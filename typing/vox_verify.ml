(* vox: verification-condition generation and discharge.

   Runs as a separate pass over the FINAL typedtree (the type checker emits no VCs; it
   backtracks internally). Walks the tree carrying a logical environment of facts; each
   [refine_] node yields the VC [facts |- p[v := name of e]]; [assume_] is reported as
   RUNTIME CHECKED (translcore compiles a check of the predicate) and
   [assume_unchecked_] as ASSUMED; neither goes to the solver. Facts come from exactly
   four places (DESIGN.md): unpacking / binders of refined type, path facts from [if],
   dependent application, and match facts on a variable scrutinee ([s = C x1 ... xn] in
   the branch that matched [C x1 ... xn]).

   VCs are discharged by a Lean 4 subprocess. Solver error, unknown, and timeout
   all count as verification FAILURE. *)

open Types
open Typedtree

(* How an obligation is discharged: [Prove] goes to the solver;
   [Runtime_check] ([assume_]) is checked at runtime by compiled code;
   [Assume] ([assume_unchecked_]) is trusted outright. *)
type vc_kind =
  | Prove
  | Runtime_check
  | Assume

type vc =
  { vc_loc : Location.t
  ; vc_facts : Refinement.pred list (* Pbound-free *)
  ; vc_goal : Refinement.pred (* Pbound-free *)
  ; vc_kind : vc_kind
  }

(* Declaration sorts for logical names, per DESIGN.md: int as Int, bool as Bool, simple
   variants as solver datatypes, anything else at a single uninterpreted sort. *)
type dsort =
  | S_int
  | S_bool
  | S_data of Path.t (* a "simple" variant, modelled with the datatype theory *)
  | S_other

(* Failure diagnostics show what the solver was given, so a failed
   obligation can be understood without re-running under -dump-vc.
   When one source name covers several stamps within a VC (shadowing),
   later ones display as name#2, name#3, ... in order of appearance,
   so a hypothesis about a shadowed variable cannot read as identical
   to the goal it fails to prove. *)
let vc_display_fun vc =
  let seen : (string, Ident.t list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun p ->
      List.iter
        (fun id ->
          let name = Ident.name id in
          let ids = try Hashtbl.find seen name with Not_found -> [] in
          if not (List.exists (Ident.same id) ids)
          then Hashtbl.replace seen name (ids @ [ id ]))
        (Refinement.free_vars p))
    (vc.vc_goal :: vc.vc_facts);
  fun id ->
    let name = Ident.name id in
    match Hashtbl.find_opt seen name with
    | Some (_ :: _ :: _ as ids) ->
      let rec index i = function
        | [] -> name
        | id' :: _ when Ident.same id id' ->
          if i = 1 then name else Printf.sprintf "%s#%d" name i
        | _ :: rest -> index (i + 1) rest
      in
      index 1 ids
    | _ -> name
;;

let with_vc_display vc k = Refinement.with_var_display (vc_display_fun vc) k

let hyps_for_error vc =
  with_vc_display vc (fun () ->
    match vc.vc_facts with
    | [] -> "\nHypotheses: <none>"
    | fs ->
      "\nHypotheses:"
      ^ String.concat "" (List.map (fun f -> "\n  " ^ Refinement.to_string f) fs))
;;

let goal_for_error vc = with_vc_display vc (fun () -> Refinement.to_string vc.vc_goal)

let vcs : vc list ref = ref []
let name_sorts : (Ident.t, dsort) Hashtbl.t = Hashtbl.create 64

(* Fresh unknowns minted by the pass itself; always "in scope".
   Numbered so distinct unknowns are distinguishable in diagnostics. *)
let synthetic_names : (Ident.t, unit) Hashtbl.t = Hashtbl.create 16
let unknown_counter = ref 0

(* The solver-side declaration of a "simple" type: a variant becomes a free
   datatype; a record becomes a single-constructor datatype with named
   selectors (a Lean [structure]). *)
type dt_decl =
  | Dt_variant of (string * dsort list) list (* constructor, field sorts *)
  | Dt_record of (string * dsort) list (* label, sort *)

(* Simple-variant/record datatypes used by the current module's (or toplevel
   session's) VCs, in dependency order (the datatypes of a datatype's fields
   precede it; self-recursion is fine).  Mutual recursion is not supported:
   detecting a back-edge POISONS the type being registered, which then sorts
   as [S_other] everywhere (sound: facts about its structure become
   ill-sorted and verification fails). *)
let datatypes : (Path.t * dt_decl) list ref = ref []
let registering : Path.t list ref = ref []
let poisoned : Path.t list ref = ref []
let find_datatype p = List.find_opt (fun (q, _) -> Path.same p q) !datatypes

(* Reflected definitions ([total_] bindings) of the current module (or
   toplevel session), in definition order; emitted into the solver input
   between the datatypes and the [-vox-prelude], so prelude lemmas may
   reference them. *)
let spec_defs : Vox_reflect.spec_def list ref = ref []

(* Embedded solver blocks ([%%vox.lean ...]) of the module (or
   toplevel session) being verified, in source order: text (ending in
   a newline) and the block's location (solver errors inside a block
   are reported there).  See the collection functions below. *)
let embedded_preludes : (string * Location.t) list ref = ref []

(* Solver blocks imported from other units' .cmis ([%%vox.lean]
   in their interfaces): unit name and blocks, in dependency order
   (a unit's blocks after the units it imports).  Gathered from the
   persistent env at verification time; the definition travels with
   the defining module, so a client can never verify against a
   DIFFERENT version of a spec function used in an imported signature
   (the .cmi CRC forces re-verification when the spec changes). *)
let imported_preludes : (string * Cmi_format.vox_prelude_export) list ref =
  ref []

let reset () =
  vcs := [];
  Hashtbl.reset name_sorts;
  Hashtbl.reset synthetic_names;
  datatypes := [];
  registering := [];
  poisoned := [];
  spec_defs := [];
  unknown_counter := 0;
  embedded_preludes := [];
  imported_preludes := []
;;

(* Expansion can fail on exotic types (e.g. stage errors inside quotations); fall back to
   no expansion, which is conservative. *)
let safe_expand_head env ty =
  match Ctype.expand_head env ty with
  | ty' -> ty'
  | exception _ -> ty
;;

(* A STABLE string for a type path: no stamps, and a path rooted in the
   current unit is prefixed with the unit's name, so the same type gets
   the same solver-side name in its defining module and in every client
   (a [-vox-prelude] can then refer to it).  Distinct paths that map to
   the same string (e.g. types in shadowed local modules) are detected
   at registration and rejected. *)
let rec path_uname (p : Path.t) =
  match p with
  | Path.Pident id ->
    if Ident.is_global_or_predef id
    then Ident.name id
    else Env.get_current_unit_name () ^ "." ^ Ident.name id
  | Path.Pdot (q, s) -> path_uname q ^ "." ^ s
  | Path.Papply (q, r) -> path_uname q ^ "(" ^ path_uname r ^ ")"
  | Path.Pextra_ty (q, _) -> path_uname q ^ ".#extra"
;;

(* The sort of the type at path [p], registering it as a datatype (with its
   field datatypes, recursively) on first sight. *)
let rec datatype_sort env p =
  if Path.same p Predef.path_int
  then S_int
  else if Path.same p Predef.path_bool
  then S_bool
  else if List.exists (Path.same p) !poisoned
  then S_other
  else if find_datatype p <> None
  then S_data p
  else if List.exists (Path.same p) !registering
  then (
    match !registering with
    | q :: _ when Path.same p q -> S_data p (* self-recursion *)
    | _ ->
      (* mutual recursion: poison the back-edge's target *)
      poisoned := p :: !poisoned;
      S_other)
  else (
    registering := p :: !registering;
    (* The pop must survive exceptions: at the toplevel the vox globals
       persist across phrases, and a stale [registering] entry would
       spuriously poison later phrases as mutual recursion. *)
    let decl =
      Fun.protect
        ~finally:(fun () -> registering := List.tl !registering)
        (fun () ->
          match Ctype.vox_simple_variant env p with
          | Some cstrs ->
            Some
              (Dt_variant
                 (List.map
                    (fun (cd : Types.constructor_declaration) ->
                      ( Ident.name cd.cd_id
                      , List.map
                          (dsort_of_type env)
                          (Types.tys_of_constr_args cd.cd_args) ))
                    cstrs))
          | None ->
            (match Ctype.vox_simple_record env p with
             | Some lbls ->
               Some
                 (Dt_record
                    (List.map
                       (fun (ld : Types.label_declaration) ->
                         Ident.name ld.ld_id, dsort_of_type env ld.ld_type)
                       lbls))
             | None -> None))
    in
    match decl with
    | None -> S_other
    | Some decl ->
      if List.exists (Path.same p) !poisoned
      then S_other
      else (
        (* Solver-side names are stamp-free: reject a distinct path that
           would alias an already-registered datatype's name. *)
        List.iter
          (fun (q, _) ->
            if String.equal (path_uname p) (path_uname q)
            then
              Location.raise_errorf
                "vox: two distinct types would share the solver-side name \
                 %s; rename one of them"
                (path_uname p))
          !datatypes;
        datatypes := !datatypes @ [ p, decl ];
        S_data p))

and dsort_of_type env ty =
  match get_desc (safe_expand_head env ty) with
  | Tconstr (p, [], _) -> datatype_sort env p
  | Trefine (skel, _) -> dsort_of_type env skel
  | _ -> S_other
;;

let record_name env id ty = Hashtbl.replace name_sorts id (dsort_of_type env ty)

(* Register the datatypes of any constructor application in [p].  Called
   wherever a predicate enters the fact/goal stream; a path that fails to
   register (not a simple variant here, or mutually recursive) is caught at
   discharge time. *)
let register_pred_paths env p =
  List.iter (fun q -> ignore (datatype_sort env q)) (Refinement.constr_paths p)
;;

(* Register a [total_] binding: translate its body into an
   equation-style definition (Vox_reflect.translate_def) and queue it
   for emission.  Solver-side names are the source names, so two
   reflected functions may not share one; the definition's datatypes
   are registered so its emission never degrades to VoxU. *)
let register_spec_def env (vb : Typedtree.value_binding) =
  let d = Vox_reflect.translate_def vb in
  List.iter
    (fun (d' : Vox_reflect.spec_def) ->
      if String.equal d'.sd_name d.sd_name
      then
        Location.raise_errorf
          ~loc:d.sd_loc
          "vox: two reflected functions would share the solver-side name %s; \
           rename one of them"
          d.sd_name)
    !spec_defs;
  List.iter
    (fun p -> ignore (datatype_sort env p))
    (Vox_reflect.def_datatype_paths d);
  spec_defs := !spec_defs @ [ d ]
;;

(* Register the datatypes an exported refinement is ABOUT: refined
   skeletons plus constructor applications in the predicates.  Used to
   compute the .cmi's spec export, so a client that never mentions
   these types itself still receives their declarations alongside the
   spec blocks that reference them.  The walk is structural (like
   [uses_vox]): a refinement hidden behind a type alias is missed, so
   its datatype is not exported -- a client whose spec needs it then
   fails at the solver (closed), never falsely verifies. *)
let register_type_specs env ty =
  let rec go ty visited =
    if List.memq ty visited
    then ()
    else begin
      let visited = ty :: visited in
      match get_desc ty with
      | Trefine (skel, p) ->
        ignore (dsort_of_type env skel : dsort);
        register_pred_paths env p;
        go skel visited
      | Tarrow (_, a, r, _) ->
        go a visited;
        go r visited
      | _ -> List.iter (fun t -> go t visited) (Vox_dep.children ty)
    end
  in
  go ty []
;;

let has_vox_attr name attrs =
  List.exists (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name) attrs
;;

(* The refinement of a type, if any. *)
let refinement_of_type env ty =
  match get_desc (safe_expand_head env ty) with
  | Trefine (_, p) -> Some p
  | _ -> None
;;

(* The refinement of an arrow PARAMETER type (a contract, DESIGN.md),
   looking under the [Tpoly] wrapper arrow domains carry.  A genuinely
   polymorphic domain (non-empty univars) is NOT a contract -- typing
   leaves those rigid -- so the walker must not report one: it would
   emit obligations typing never stripped for. *)
let param_refinement env ty =
  match get_desc (safe_expand_head env ty) with
  | Tpoly (t, []) -> refinement_of_type env t
  | Trefine (_, p) -> Some p
  | _ -> None
;;

(* Facts contributed by the binders of a pattern: every binder is recorded (for its
   declaration sort); binders of refined type contribute their refinement instantiated at
   the binder. *)
let binder_facts : type k. Env.t -> k general_pattern -> Refinement.pred list =
  fun env pat ->
  List.concat_map
    (fun (id, _, ty, _, _) ->
      record_name env id ty;
      match refinement_of_type env ty with
      | Some p ->
        register_pred_paths env p;
        [ Refinement.subst_bound ~by:(Refinement.Pvar id) p ]
      | None -> [])
    (pat_bound_idents_full pat)
;;

(* The unpack fact: a pattern marked [refine_ x] binds [x] at the skeleton and contributes
   the SCRUTINEE's refinement at [x]. *)
let unpack_fact
  : type k. Env.t -> k general_pattern -> scrut:type_expr -> Refinement.pred list
  =
  fun env pat ~scrut ->
  if not (has_vox_attr "vox.refine" pat.pat_attributes)
  then []
  else (
    match pat_bound_idents pat, refinement_of_type env scrut with
    | [ id ], Some p ->
      register_pred_paths env p;
      [ Refinement.subst_bound ~by:(Refinement.Pvar id) p ]
    | _ -> [])
;;

(* The logical name of an expression: variables denote their stamp; expressions
   in the translatable int/bool fragment their logic translation (Vox_reflect);
   applications of simple-variant constructors their constructor term (over the
   names of their arguments -- "constructors get the usual refinements", and
   the arguments are themselves named, so translatable arithmetic reflects
   inside them); anything else is a fresh unknown. *)
let fresh_unknown env (e : expression) =
  incr unknown_counter;
  let id = Ident.create_local (Printf.sprintf "*unknown%d*" !unknown_counter) in
  record_name env id e.exp_type;
  Hashtbl.replace synthetic_names id ();
  Refinement.Pvar id
;;

(* The stable logical name of an argument, if it has one: variables
   denote their stamp, literals themselves.  Used for dependent-binder
   substitution, mirroring [vox_open_dependent_arrow] exactly (typing
   already rejected mutable variables and compound expressions
   there). *)
let stable_arg_name (a : expression) : Refinement.pred option =
  match a.exp_desc with
  | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
  | Texp_constant (Const_int n) -> Some (Refinement.Pint n)
  | Texp_construct ({ txt = Longident.Lident "true"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool true)
  | Texp_construct ({ txt = Longident.Lident "false"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool false)
  | _ -> None
;;

let rec name_of_expr env (e : expression) : Refinement.pred =
  match Vox_reflect.translate e with
  | Some p ->
    (* The translation may contain field projections; register their
       record types so the structure declarations reach the solver. *)
    register_pred_paths env p;
    p
  | None ->
    (match e.exp_desc with
     | Texp_construct (_, cstr, _, args, _) ->
       let path = Data_types.cstr_res_type_path cstr in
       (match datatype_sort env path with
        | S_data _ ->
          Refinement.Pconstr
            ( path
            , cstr.cstr_name
            , List.map (fun (_, a) -> name_of_expr env a) args )
        | S_int | S_bool | S_other -> fresh_unknown env e)
     | Texp_record { fields; extended_expression; _ }
       when Array.length fields > 0 ->
       (* A record literal names the constructor term ["mk"] (a reserved
          lowercase name: real constructors are capitalized); in a
          functional update [{ b with l = e }], kept fields project out
          of the base's name. *)
       let path =
         Data_types.lbl_res_type_path (match fields.(0) with lbl, _, _ -> lbl)
       in
       (match datatype_sort env path with
        | S_data _ ->
          let base =
            Option.map
              (fun (be, _, _) -> name_of_expr env be)
              extended_expression
          in
          let arg_of (lbl, _, def) =
            match def, base with
            | Overridden (_, ex), _ -> name_of_expr env ex
            | Kept _, Some b ->
              Refinement.Pfield (path, lbl.Data_types.lbl_name, b)
            | Kept _, None ->
              (* unreachable: [Kept] implies a functional update *)
              fresh_unknown env e
          in
          Refinement.Pconstr (path, "mk", List.map arg_of (Array.to_list fields))
        | S_int | S_bool | S_other -> fresh_unknown env e)
     | Texp_field { record; label; _ } ->
       (* Mostly subsumed: [Vox_reflect.translate] projects immutable
          fields of simple records when the base itself translates.
          This fallback still fires when the base is only NAMEABLE
          (e.g. a field of a just-constructed record). *)
       let path = Data_types.lbl_res_type_path label in
       (match label.lbl_mut, datatype_sort env path with
        | Types.Immutable, S_data _ ->
          Refinement.Pfield (path, label.lbl_name, name_of_expr env record)
        | _ -> fresh_unknown env e)
     | _ -> fresh_unknown env e)
;;

(* The logical context of a program point: facts, plus the stamps in
   scope there.  A fact mentioning an out-of-scope stamp must not be
   used: the same dead stamp can reach several unrelated points (e.g.
   through a refinement in a function's inferred result type that
   mentions the function's own parameters), and equating them would
   prove false facts.  Out-of-scope facts are dropped (sound: fewer
   hypotheses); out-of-scope goals are errors. *)
type ctx =
  { cfacts : Refinement.pred list
  ; cscope : Ident.t list
  }

let in_scope ctx id =
  List.exists (Ident.same id) ctx.cscope || Hashtbl.mem synthetic_names id

let pred_in_scope ctx p = List.for_all (in_scope ctx) (Refinement.free_vars p)

(* Predicates the compiled runtime check cannot evaluate: constructor
   terms and field projections (structural operations at datatype sorts
   are future work), spec functions (solver-side only, no runtime
   denotation), and division/modulo (the logic's T-division is total,
   [tdiv x 0 = 0], where the program raises: a faithful check cannot
   be compiled). *)
let rec pred_unreflectable (p : Refinement.pred) =
  match p with
  | Refinement.Pconstr _ | Refinement.Pfun _ | Refinement.Pfield _
  | Refinement.Pis _ -> true
  | Refinement.Pbinop ((Refinement.Div | Refinement.Mod), _, _) -> true
  | Refinement.Pbound | Refinement.Pvar _ | Refinement.Pint _
  | Refinement.Pbool _ -> false
  | Refinement.Pbinop (_, a, b)
  | Refinement.Pand (a, b)
  | Refinement.Por (a, b) -> pred_unreflectable a || pred_unreflectable b
  | Refinement.Pnot a -> pred_unreflectable a
;;

let emit_vc ~loc ~ctx ~goal ~kind =
  (* Facts mentioning out-of-scope stamps (including any dependent
     binder a substitution failed to open) are dropped (sound: fewer
     hypotheses); such goals cannot be discharged and are errors.  The
     same scope requirement applies to runtime-checked goals: the
     compiled check reads those variables at run time. *)
  (match kind with
   | Prove ->
     if not (pred_in_scope ctx goal)
     then
       Location.raise_errorf ~loc
         "vox: this obligation mentions a variable that has escaped its scope"
   | Runtime_check ->
     if not (pred_in_scope ctx goal)
     then
       Location.raise_errorf ~loc
         "vox: assume_ compiles a runtime check of this refinement, but it \
          mentions a variable that is not in scope here; use \
          assume_unchecked_";
     if pred_unreflectable goal
     then
       Location.raise_errorf ~loc
         "vox: assume_ compiles a runtime check of this refinement, but it \
          involves a constructor, field projection, spec function, or \
          division, which the compiled check cannot evaluate faithfully; \
          use assume_unchecked_";
     (* The compiled check compares machine words, which agrees with the
        logic only for int- and bool-sorted operands: other sorts are
        uninterpreted, and physical equality is stricter than logical
        equality (a coherent assumption could fail at run time). *)
     let int_or_bool id =
       match Hashtbl.find_opt name_sorts id with
       | Some (S_int | S_bool) -> true
       | Some (S_data _ | S_other) | None -> false
     in
     (match
        List.find_opt
          (fun id -> not (int_or_bool id))
          (Refinement.free_vars goal)
      with
      | Some id ->
        Location.raise_errorf ~loc
          "vox: assume_ compiles a runtime check of this refinement, but %s \
           is not an int or bool, so the runtime comparison would not agree \
           with the logic's equality; use assume_unchecked_"
          (if Hashtbl.mem synthetic_names id
           then "the checked value"
           else Ident.name id)
      | None -> ())
   | Assume -> ());
  let facts = List.filter (pred_in_scope ctx) ctx.cfacts in
  vcs := { vc_loc = loc; vc_facts = facts; vc_goal = goal; vc_kind = kind } :: !vcs
;;

(* Escaped refinements (DESIGN: "escape is an error").  A binder's type
   may not carry refinements mentioning program variables that are not
   in scope at the binding: the same stamp can name a different value
   at another point (recursion re-binds it; unification propagates
   types across scopes), so such facts would be unsound.  At the module
   level the rule is stricter: refinements in exported types may
   mention no program variables at all (predicates in .cmis are
   self-contained; stamps do not survive a compilation unit).
   Dependent-arrow binders are exempt where the type itself binds them
   ([iter_refinement_preds] reports them as [bound]). *)
type escape_mode =
  | Module_level
  | In_scope of ctx * Ident.t list (* extra idents treated as in scope *)

let check_type_escapes ~loc ~what mode ty =
  Vox_dep.iter_refinement_preds ty (fun ~bound p ->
    List.iter
      (fun v ->
        let bad =
          if List.exists (Ident.same v) bound
          then false
          else (
            match mode with
            | Module_level -> true
            | In_scope (ctx, extra) ->
              not (in_scope ctx v || List.exists (Ident.same v) extra))
        in
        if bad
        then
          Location.raise_errorf ~loc
            "vox: the type of %s carries a refinement mentioning %s, which \
             %s; annotate with a dependent arrow ((%s : ...) -> ...) or a \
             self-contained refinement"
            what
            (Ident.name v)
            (match mode with
             | Module_level -> "may not appear in a module-level type"
             | In_scope _ -> "is not in scope here")
            (Ident.name v))
      (Refinement.free_vars p))
;;

let check_binder_escape ~toplevel ctx ~extra_scope (pat : _ general_pattern) id ty =
  let mode = if toplevel then Module_level else In_scope (ctx, extra_scope) in
  check_type_escapes ~loc:pat.pat_loc ~what:(Ident.name id) mode ty
;;

(* Backstop for binders the walker does not model (inside local or
   nested module structures, try handlers, letops, ...): they
   contribute no facts, but their types must still be escape-checked --
   a stored closure typed with another activation's variable would
   otherwise smuggle false facts (the pattern's own binders count as in
   scope; siblings bound by the unmodeled construct do not, which is
   conservative). *)
(* Reflected definitions live at the top level of the current module:
   that is where walk_items registers and emits them.  A marked binding
   anywhere else -- a local let, or a structure item of a nested or
   local module -- would be registered in the typing-time table (so its
   calls would translate) but never emitted, and a local one could
   capture enclosing variables; reject them all. *)
let reject_local_reflect (vb : Typedtree.value_binding) =
  if Vox_reflect.is_total_binding vb
  then
    Location.raise_errorf
      ~loc:vb.vb_loc
      "vox: total_ is only supported on top-level bindings of the current \
       module"
;;

let backstop_pat : type k. ctx -> k general_pattern -> unit =
  fun ctx pat ->
  let bound = pat_bound_idents pat in
  List.iter
    (fun (id, _, ty, _, _) ->
      check_binder_escape ~toplevel:false ctx ~extra_scope:bound pat id ty)
    (pat_bound_idents_full pat)
;;

(* Every type reachable from any exported item of a signature --
   values, type manifests, record fields, constructor arguments,
   extension constructors, submodules, module types, classes.  Used
   both for the self-containment check and for computing the .cmi's
   spec export. *)
let rec iter_signature_types ~f (sg : Types.signature) =
  List.iter (iter_signature_item_types ~f) sg

and iter_signature_item_types ~f (item : Types.signature_item) =
  let check = f in
  let check_constructor_arguments ~what = function
    | Types.Cstr_tuple args ->
      List.iter
        (fun (ca : Types.constructor_argument) ->
          check ~loc:ca.ca_loc ~what ca.ca_type)
        args
    | Types.Cstr_record lbls ->
      List.iter
        (fun (ld : Types.label_declaration) ->
          check ~loc:ld.ld_loc ~what ld.ld_type)
        lbls
  in
  match item with
  | Sig_value (id, vd, _) ->
    check ~loc:vd.val_loc ~what:(Ident.name id) vd.val_type
  | Sig_type (id, decl, _, _) ->
    let what = "type " ^ Ident.name id in
    Option.iter (check ~loc:decl.type_loc ~what) decl.type_manifest;
    (match decl.type_kind with
     | Type_abstract _ | Type_open -> ()
     | Type_record (lbls, _, _) | Type_record_unboxed_product (lbls, _, _) ->
       List.iter
         (fun (ld : Types.label_declaration) ->
           check ~loc:ld.ld_loc ~what ld.ld_type)
         lbls
     | Type_variant (cds, _, _) ->
       List.iter
         (fun (cd : Types.constructor_declaration) ->
           check_constructor_arguments ~what cd.cd_args)
         cds)
  | Sig_typext (id, ext, _, _) ->
    check_constructor_arguments ~what:(Ident.name id) ext.ext_args
  | Sig_module (_, _, md, _, _) -> iter_module_type_types ~f md.md_type
  | Sig_modtype (_, mtd, _) -> Option.iter (iter_module_type_types ~f) mtd.mtd_type
  | Sig_class (id, cd, _, _) ->
    iter_class_type_types ~f ~loc:cd.cty_loc ~what:(Ident.name id) cd.cty_type
  | Sig_class_type (id, ctd, _, _) ->
    iter_class_type_types ~f ~loc:ctd.clty_loc ~what:(Ident.name id)
      ctd.clty_type
  | Sig_jkind _ -> ()

and iter_module_type_types ~f = function
  | Mty_ident _ | Mty_alias _ -> ()
  | Mty_signature sg -> iter_signature_types ~f sg
  | Mty_functor (param, res, _) ->
    (match param with
     | Unit -> ()
     | Named (_, mty, _) -> iter_module_type_types ~f mty);
    iter_module_type_types ~f res
  | Mty_strengthen (mty, _, _) -> iter_module_type_types ~f mty

and iter_class_type_types ~f ~loc ~what = function
  | Cty_constr (_, args, cty) ->
    List.iter (f ~loc ~what) args;
    iter_class_type_types ~f ~loc ~what cty
  | Cty_signature csig ->
    f ~loc ~what csig.csig_self;
    Vars.iter (fun _ (_, _, ty) -> f ~loc ~what ty) csig.csig_vars
  | Cty_arrow (_, ty, cty) ->
    f ~loc ~what ty;
    iter_class_type_types ~f ~loc ~what cty
;;

(* Module-level self-containment, applied to a whole signature
   (implementation, interface, or toplevel phrase): every refinement
   reachable from any exported item must be free of program variables.
   This is what makes .cmi predicates self-contained: stamps do not
   survive a compilation unit, so an imported [Pvar] can collide with
   an unrelated local stamp and prove false facts. *)
let check_signature (sg : Types.signature) =
  iter_signature_types sg
    ~f:(fun ~loc ~what ty -> check_type_escapes ~loc ~what Module_level ty)
;;

(* vox match facts ("the match refines the thing we matched on"): matching a
   variable scrutinee [sid] against a SIMPLE pattern contributes facts to the
   case's guard and body:
   - one constructor of a simple variant over variables/wildcards gives
     [sid = C x1 ... xn] (wildcards name fresh unknowns);
   - a simple-record pattern gives [xi = sid.li] per VARIABLE sub-pattern
     (per-field, so partial patterns are fine; non-variable fields
     contribute nothing).
   Anything deeper (nesting, aliases, or-patterns, constants) contributes
   nothing, which is sound.  This is the constructor analogue of the [if]
   path fact; [let p = x in ...] gets the same facts. *)
let match_facts
  : type k. Env.t -> Ident.t -> k general_pattern -> Refinement.pred list
  =
  fun env sid pat ->
  let arg_name (_, (p : value general_pattern)) =
    match p.pat_desc with
    | Tpat_var { id; _ } -> Some (Refinement.Pvar id)
    | Tpat_any ->
      let id = Ident.create_local "*vox-wild*" in
      record_name env id p.pat_type;
      Hashtbl.replace synthetic_names id ();
      Some (Refinement.Pvar id)
    | _ -> None
  in
  let constructor_facts cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path with
    | S_int | S_bool | S_other -> []
    | S_data _ ->
      let rec name_args acc = function
        | [] -> Some (List.rev acc)
        | a :: rest ->
          (match arg_name a with
           | Some n -> name_args (n :: acc) rest
           | None -> None)
      in
      (match name_args [] args with
       | Some names ->
         [ Refinement.Pbinop
             ( Refinement.Eq
             , Refinement.Pvar sid
             , Refinement.Pconstr (path, cstr.Data_types.cstr_name, names) )
         ]
       | None -> [])
  in
  let record_facts (fields : (_ * Data_types.label_description * _) list) =
    match fields with
    | [] -> []
    | (_, lbl0, _) :: _ ->
      let path = Data_types.lbl_res_type_path lbl0 in
      (match datatype_sort env path with
       | S_int | S_bool | S_other -> []
       | S_data _ ->
         List.filter_map
           (fun (_, (lbl : Data_types.label_description), sub) ->
             match (sub : value general_pattern).pat_desc with
             | Tpat_var { id; _ } ->
               Some
                 (Refinement.Pbinop
                    ( Refinement.Eq
                    , Refinement.Pvar id
                    , Refinement.Pfield (path, lbl.lbl_name, Refinement.Pvar sid)
                    ))
             | _ -> None)
           fields)
  in
  let value_facts (p : value general_pattern) =
    match p.pat_desc with
    | Tpat_construct (_, cstr, _, args, _) -> constructor_facts cstr args
    | Tpat_record (fields, _, _, _) -> record_facts fields
    | _ -> []
  in
  match pat.pat_desc with
  | Tpat_value p -> value_facts (p :> value general_pattern)
  | Tpat_construct (_, cstr, _, args, _) -> constructor_facts cstr args
  | Tpat_record (fields, _, _, _) -> record_facts fields
  | _ -> []
;;

(* Negative match facts: if control reaches an arm, every EARLIER arm
   failed to match.  That is a usable fact only when the earlier arm's
   failure is decided by its constructor head alone: a guard-free arm
   whose pattern is one constructor of a simple variant over variables
   or wildcards (the same shape that earns a positive fact).  Such an
   arm contributes [not (s is C)] to every later arm.  Guarded arms
   contribute nothing (the pattern may have matched with the guard
   false); deeper patterns contribute nothing (the head may have
   matched while a sub-pattern refuted). *)
let pattern_negation
  : type k. Env.t -> Ident.t -> k general_pattern -> Refinement.pred option
  =
  fun env sid pat ->
  let head_negation cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path with
    | S_int | S_bool | S_other -> None
    | S_data _ ->
      let simple (_, (p : value general_pattern)) =
        match p.pat_desc with
        | Tpat_var _ | Tpat_any -> true
        | _ -> false
      in
      if List.for_all simple args
      then
        Some
          (Refinement.Pnot
             (Refinement.Pis
                (path, cstr.Data_types.cstr_name, Refinement.Pvar sid)))
      else None
  in
  match pat.pat_desc with
  | Tpat_value p ->
    (match (p :> value general_pattern).pat_desc with
     | Tpat_construct (_, cstr, _, args, _) -> head_negation cstr args
     | _ -> None)
  | Tpat_construct (_, cstr, _, args, _) -> head_negation cstr args
  | _ -> None
;;

(* Extend the context at a binding pattern: new stamps come into scope;
   refined binders contribute their facts (plus the scrutinee's
   refinement for unpack patterns). *)
let extend_pat
  : type k. ?toplevel:bool -> ?scrut:type_expr -> Env.t -> ctx -> k general_pattern -> ctx
  =
  fun ?(toplevel = false) ?scrut env ctx pat ->
  let bound = pat_bound_idents pat in
  List.iter
    (fun (id, _, ty, _, _) ->
      check_binder_escape ~toplevel ctx ~extra_scope:bound pat id ty)
    (pat_bound_idents_full pat);
  let unpack =
    match scrut with
    | Some s -> unpack_fact env pat ~scrut:s
    | None -> []
  in
  { cfacts = unpack @ binder_facts env pat @ ctx.cfacts
  ; cscope = bound @ ctx.cscope
  }
;;

(* Walk an expression under a logical context, collecting VCs. *)
let rec walk_expr env ctx (e : expression) =
  (* Intro forms: the node itself carries the vox attribute and the refined type. *)
  let kind =
    if has_vox_attr "vox.refine" e.exp_attributes
    then Some Prove
    else if has_vox_attr "vox.assume" e.exp_attributes
    then Some Runtime_check
    else if has_vox_attr "vox.assume_unchecked" e.exp_attributes
    then Some Assume
    else None
  in
  (match kind with
   | Some kind ->
     (match refinement_of_type env e.exp_type with
      | Some p ->
        register_pred_paths env p;
        let n = name_of_expr env e in
        emit_vc ~loc:e.exp_loc ~ctx ~goal:(Refinement.subst_bound ~by:n p) ~kind
      | None -> ())
   | None -> ());
  match e.exp_desc with
  | Texp_apply (funct, args, _, _, _) ->
    walk_expr env ctx funct;
    (* Contract obligations (parameters as preconditions): each
       argument for a refined parameter must satisfy the predicate at
       its logical name; an intro-form argument
       ([refine_]/[assume_]/[assume_unchecked_]) carries its own
       obligation instead (the explicit-cast spelling).  The dependent
       binder is substituted by the (syntactically enforced) variable
       or literal argument's name as the spine is walked, mirroring
       the application-site opening done at typing time. *)
    let arrow_ty = ref funct.exp_type in
    List.iter
      (fun (_lbl, (arg : apply_arg)) ->
        let arg_expr =
          match arg with
          | Arg (a, _) -> Some a
          | Omitted _ -> None
        in
        Option.iter (walk_expr env ctx) arg_expr;
        match get_desc (safe_expand_head env !arrow_ty) with
        | Tarrow ((_, _, _, binder), dom, ret, _) ->
          (match arg_expr with
           | Some a ->
             (match param_refinement env dom with
              | Some p
                when not
                       (has_vox_attr "vox.refine" a.exp_attributes
                        || has_vox_attr "vox.assume" a.exp_attributes
                        || has_vox_attr "vox.assume_unchecked"
                             a.exp_attributes) ->
                register_pred_paths env p;
                emit_vc
                  ~loc:a.exp_loc
                  ~ctx
                  ~goal:(Refinement.subst_bound ~by:(name_of_expr env a) p)
                  ~kind:Prove
              | _ -> ());
             (match binder, stable_arg_name a with
              | Some b, Some by ->
                arrow_ty := Vox_dep.subst_binder b ~by ret
              | _ -> arrow_ty := ret)
           | None -> arrow_ty := ret)
        | _ -> ())
      args
  | Texp_let (_rec_flag, vbs, body) ->
    (* Reflected definitions are global; a local one could capture
       enclosing variables (translate_def's closedness check would also
       catch that, but the restriction is the honest one). *)
    List.iter reject_local_reflect vbs;
    List.iter (fun vb -> walk_expr env ctx vb.vb_expr) vbs;
    let ctx' = List.fold_left (fun ctx vb -> extend_pat env ctx vb.vb_pat) ctx vbs in
    (* A destructuring let of a variable gets the same facts a match
       case would: [let { x; y } = r in ...]. *)
    let ctx' =
      List.fold_left
        (fun ctx vb ->
          match vb.vb_expr.exp_desc with
          | Texp_ident { path = Path.Pident id; _ } ->
            { ctx with cfacts = match_facts env id vb.vb_pat @ ctx.cfacts }
          | _ -> ctx)
        ctx'
        vbs
    in
    walk_expr env ctx' body
  | Texp_match (scrut, _sort, comp_cases, val_cases, _partial) ->
    walk_expr env ctx scrut;
    let scrut_id =
      match scrut.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> Some id
      | _ -> None
    in
    let do_case : type k. Refinement.pred list -> k case -> unit =
      fun negs c ->
      let ctx' = extend_pat ~scrut:scrut.exp_type env ctx c.c_lhs in
      let ctx' =
        match scrut_id with
        | Some sid ->
          { ctx' with
            cfacts = match_facts env sid c.c_lhs @ negs @ ctx'.cfacts
          }
        | None -> ctx'
      in
      Option.iter (walk_expr env ctx') c.c_guard;
      walk_expr env ctx' c.c_rhs
    in
    (* Arms additionally see the negations of the guard-free simple
       arms ABOVE them.  All ordinary arms -- value and exception, in
       source order -- arrive as computation cases (value patterns
       wrapped in [Tpat_value]); [val_cases] holds effect-handler arms.
       Exception and effect arms never contribute a negation (their
       patterns are not simple-variant constructors: exception and
       effect types are open), and their RECEIVING facts is vacuously
       sound under the variable-scrutinee gate: evaluating a variable
       can neither raise nor perform. *)
    let run_cases : type k. k case list -> unit =
      fun cases ->
      ignore
        (List.fold_left
           (fun negs c ->
             do_case negs c;
             match scrut_id, c.c_guard with
             | Some sid, None ->
               (match pattern_negation env sid c.c_lhs with
                | Some n -> negs @ [ n ]
                | None -> negs)
             | _ -> negs)
           []
           cases
          : Refinement.pred list)
    in
    run_cases comp_cases;
    run_cases val_cases
  | Texp_ifthenelse (cond, e_then, e_else) ->
    walk_expr env ctx cond;
    (* The path fact is the condition's logic translation when it has
       one (a variable, or a translatable int/bool expression);
       untranslatable conditions contribute nothing. *)
    let cond_fact = Vox_reflect.translate cond in
    Option.iter (register_pred_paths env) cond_fact;
    let with_fact f ctx =
      match cond_fact with
      | None -> ctx
      | Some c -> { ctx with cfacts = f c :: ctx.cfacts }
    in
    walk_expr env (with_fact (fun c -> c) ctx) e_then;
    Option.iter (walk_expr env (with_fact (fun c -> Refinement.Pnot c) ctx)) e_else
  | Texp_function { params; body; _ } ->
    (* Contract facts (parameters as preconditions): a refined arrow
       DOMAIN contributes its predicate at the parameter's name -- the
       parameter itself is bound at the skeleton, and every caller
       discharged the predicate at its argument.  The arrow's dependent
       binder is substituted by the parameter's stamp as the spine is
       walked, mirroring the definition-site opening done at typing
       time.  (A parameter whose PATTERN still carries the refined type
       -- the pattern-annotation spelling -- contributes through
       [binder_facts] instead; the guard avoids the duplicate.) *)
    let arrow_ty = ref e.exp_type in
    let ctx' =
      List.fold_left
        (fun ctx fp ->
          let pat, is_default =
            match fp.fp_kind with
            | Tparam_pat pat -> pat, false
            | Tparam_optional_default (pat, default, _) ->
              walk_expr env ctx default;
              pat, true
          in
          let ctx = extend_pat env ctx pat in
          match get_desc (safe_expand_head env !arrow_ty) with
          | Tarrow ((_, _, _, binder), dom, ret, _) ->
            let id_opt =
              match pat.pat_desc with
              | Tpat_var { id; _ } -> Some id
              | _ -> None
            in
            let ctx =
              match param_refinement env dom with
              | Some p
                when (not is_default)
                     && Option.is_none (refinement_of_type env pat.pat_type) ->
                let name =
                  match id_opt with
                  | Some id -> Refinement.Pvar id
                  | None ->
                    incr unknown_counter;
                    let s =
                      Ident.create_local
                        (Printf.sprintf "*param%d*" !unknown_counter)
                    in
                    record_name env s pat.pat_type;
                    Hashtbl.replace synthetic_names s ();
                    Refinement.Pvar s
                in
                register_pred_paths env p;
                { ctx with
                  cfacts = Refinement.subst_bound ~by:name p :: ctx.cfacts
                }
              | _ -> ctx
            in
            (match binder, id_opt with
             | Some b, Some id ->
               arrow_ty := Vox_dep.subst_binder b ~by:(Refinement.Pvar id) ret
             | _ -> arrow_ty := ret);
            ctx
          | _ -> ctx)
        ctx
        params
    in
    (match body with
     | Tfunction_body e -> walk_expr env ctx' e
     | Tfunction_cases { fc_cases; _ } ->
       List.iter
         (fun c ->
           let ctx'' = extend_pat env ctx' c.c_lhs in
           Option.iter (walk_expr env ctx'') c.c_guard;
           walk_expr env ctx'' c.c_rhs)
         fc_cases)
  | _ ->
    (* Generic traversal of children under the same context.  Patterns
       reached this way belong to constructs the walker does not model
       (try handlers, letops, local module structures, ...); they are
       escape-checked but contribute no facts.  Value bindings reached
       this way (structure items of local modules) cannot host
       reflected definitions. *)
    let it =
      { Tast_iterator.default_iterator with
        expr = (fun _ e' -> walk_expr env ctx e')
      ; pat =
          (fun sub (type k) (p : k general_pattern) ->
            backstop_pat ctx p;
            Tast_iterator.default_iterator.pat sub p)
      ; value_binding =
          (fun sub vb ->
            reject_local_reflect vb;
            Tast_iterator.default_iterator.value_binding sub vb)
      }
    in
    Tast_iterator.default_iterator.expr it e
;;

(* ------------------------------------------------------------------ *)
(* Serialization helpers *)

(* Arity of constructor [c] of the registered variant at [p]; testers of
   unregistered paths never reach serialization (usability filter). *)
let constr_arity p c =
  match find_datatype p with
  | Some (_, Dt_variant constrs) ->
    (match List.assoc_opt c constrs with
     | Some fields -> List.length fields
     | None -> 0)
  | Some (_, Dt_record _) | None -> 0
;;


let sort_is_other = function
  | S_other -> true
  | S_int | S_bool | S_data _ -> false
;;

let datatype_field_needs_voxu () =
  List.exists
    (fun (_, decl) ->
      match decl with
      | Dt_variant constrs ->
        List.exists (fun (_, fields) -> List.exists sort_is_other fields) constrs
      | Dt_record fields -> List.exists (fun (_, fs) -> sort_is_other fs) fields)
    !datatypes
;;

let free_vars_of_vc vc = List.concat_map Refinement.free_vars (vc.vc_goal :: vc.vc_facts)

(* Embedded solver blocks: [%%vox.lean {lean|...|lean}] structure
   items carry solver-side text directly in the OCaml source.  They
   are not "preludes": reflected definitions precede them, so a block
   may state lemmas about the module's own total_ functions.  Blocks
   travel: an .mli's blocks -- and an mli-less unit's -- ride the
   .cmi's spec export to every client. *)

type prelude_kind =
  | Not_prelude
  | Prelude
  | Bad_backend of string

let prelude_extension_kind txt =
  if String.equal txt "vox.lean" then Prelude
  else if String.length txt >= 4 && String.equal (String.sub txt 0 4) "vox."
  then
    (* Claim the whole vox.* item-extension namespace, so a misspelled
       block gets a vox error rather than "uninterpreted extension". *)
    Bad_backend txt
  else Not_prelude
;;

(* Whether Typemod should claim this extension item (including
   misspelled backends, so they get the vox error, not "uninterpreted
   extension"). *)
let is_prelude_extension_name txt =
  match prelude_extension_kind txt with
  | Prelude | Bad_backend _ -> true
  | Not_prelude -> false
;;

(* Validates and extracts the text of a [%%vox.prelude] payload; used
   by Typemod (to accept the item) and by the collection below. *)
let prelude_extension_text (({txt; loc}, payload) : Parsetree.extension) =
  match prelude_extension_kind txt with
  | Not_prelude -> None
  | Bad_backend b ->
    Location.raise_errorf ~loc
      "vox: unknown block extension %S (expected \"vox.lean\")" b
  | Prelude ->
    (match payload with
     | Parsetree.PStr
         [ { pstr_desc =
               Pstr_eval
                 ( { pexp_desc =
                       Pexp_constant {pconst_desc = Pconst_string (s, _, _); _}
                   ; _ }
                 , [] )
           ; _ } ] ->
       Some s
     | _ ->
       Location.raise_errorf ~loc
         "vox: a solver block takes a single string literal, e.g. \
          [%%%%vox.lean {lean|...|lean}]")
;;

let normalize_block s =
  if String.length s > 0 && s.[String.length s - 1] = '\n' then s else s ^ "\n"
;;

let collect_preludes (str : structure) =
  List.filter_map
    (fun item ->
      match item.str_desc with
      | Tstr_attribute ({attr_name = {txt; _}; attr_payload; attr_loc} : attribute)
        when is_prelude_extension_name txt ->
        (match prelude_extension_text ({txt; loc = attr_loc}, attr_payload) with
         | Some s -> Some (normalize_block s, attr_loc)
         | None -> None)
      | _ -> None)
    str.str_items
;;

(* Blocks of an INTERFACE ([%%vox.prelude] in an .mli): collected by
   the .mli's compilation and saved into the .cmi (see Typemod), so
   they reach every client -- and the unit's own implementation, whose
   verification reads the interface's .cmi like any other import. *)
let collect_preludes_sig (sg : Typedtree.signature) =
  List.filter_map
    (fun item ->
      match item.sig_desc with
      | Tsig_attribute ({attr_name = {txt; _}; attr_payload; attr_loc}
                        : attribute)
        when is_prelude_extension_name txt ->
        (match
           prelude_extension_text ({txt; loc = attr_loc}, attr_payload)
         with
         | Some s -> Some (normalize_block s)
         | None -> None)
      | _ -> None)
    sg.sig_items
;;

(* Imported spec exports in dependency order (a unit's spec after the
   units it imports; name order breaks ties, for determinism). *)
let gather_imported_preludes () =
  let all =
    Env.vox_imported_preludes ()
    |> List.map (fun (name, export, deps) ->
      ( Compilation_unit.Name.to_string name
      , export
      , List.map Compilation_unit.Name.to_string deps ))
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  let carriers = List.map (fun (n, _, _) -> n) all in
  let visited = ref [] in
  let out = ref [] in
  let rec visit n =
    if not (List.exists (String.equal n) !visited)
    then (
      visited := n :: !visited;
      match List.find_opt (fun (m, _, _) -> String.equal m n) all with
      | None -> ()
      | Some (_, export, deps) ->
        List.iter visit (List.filter (fun d -> List.exists (String.equal d) carriers) deps);
        out := (n, export) :: !out)
  in
  List.iter (fun (n, _, _) -> visit n) all;
  List.rev !out
;;

(* Datatype names already declared by imported exports: a client
   skips re-declaring these (stable names guarantee they denote the
   same declarations). *)
let imported_unames () =
  List.concat_map
    (fun (_, vp) ->
      List.map (fun (n, _) -> n) vp.Cmi_format.vp_datatypes)
    !imported_preludes
;;

let imported_need_voxu () =
  List.exists
    (fun (_, vp) -> vp.Cmi_format.vp_needs_voxu)
    !imported_preludes
;;

(* A datatype of THIS module whose stable name matches an imported
   declaration is not re-declared (see the emitters' [~skip]) -- which
   is only sound if it really is the same declaration.  The renderers
   are deterministic, so comparing rendered text detects a local type
   shadowing an imported one at the same solver-side name. *)
let check_imported_datatype_clashes ~render =
  List.iter
    (fun ((p, _) as dt) ->
      let uname = path_uname p in
      List.iter
        (fun (unit, vp) ->
          List.iter
            (fun (n, leand) ->
              if String.equal n uname
                 && not (String.equal (render dt : string) leand)
              then
                Location.raise_errorf
                  "vox: the type %s would share the solver-side name %s \
                   with a different datatype imported from unit %s; \
                   rename one of them"
                  uname
                  uname
                  unit)
            vp.Cmi_format.vp_datatypes)
        !imported_preludes)
    !datatypes
;;

(* Where a line of generated solver input came from, for error
   attribution. *)
type block_src =
  | Local_block of Location.t
  | Imported_block of string (* unit name *)
  | Reflected_def of Vox_reflect.spec_def

let count_lines s = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 s

(* The [-vox-prelude] file: user-written solver-side definitions (spec
   functions such as measures), inserted verbatim into every generated
   solver input just after the datatype declarations.  Normalized to
   end in a newline; an unreadable file is a verification failure. *)
let prelude_cache : string option ref = ref None

let prelude () =
  match !prelude_cache with
  | Some c -> c
  | None ->
    let c =
      if String.equal !Clflags.vox_prelude ""
      then ""
      else (
        match
          let ic = open_in_bin !Clflags.vox_prelude in
          let n = in_channel_length ic in
          let c = really_input_string ic n in
          close_in ic;
          c
        with
        | c ->
          if String.length c > 0 && c.[String.length c - 1] = '\n'
          then c
          else c ^ "\n"
        | exception Sys_error msg ->
          Location.raise_errorf "vox: cannot read -vox-prelude file: %s" msg)
    in
    prelude_cache := Some c;
    c
;;

let prelude_lines () =
  String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 (prelude ())
;;

let vc_uses_spec_fun vc =
  List.exists Refinement.mentions_spec_fun (vc.vc_goal :: vc.vc_facts)
;;

(* ------------------------------------------------------------------ *)
(* Solver harness: [Sys.command] + temp files; no unix dependency.  A
   wedged process is out of scope for v0. *)

let lean_command () =
  if not (String.equal !Clflags.vox_solver_path "") then !Clflags.vox_solver_path
  else
    match Sys.getenv_opt "VOX_LEAN" with
    | Some s -> s
    | None -> "lean"
;;

(* ------------------------------------------------------------------ *)
(* Lean backend: VCs become Lean 4 theorems proved by [grind], batched
   one file per module, one theorem per line so failing line numbers
   map back to VCs.  Int-sorted names are [Int], bool-sorted names are
   modelled as [Prop] (equality between boolean-valued predicates
   becomes [↔]), everything else lives in an opaque type [VoxU]. *)

let lean_sanitize s =
  let b = Bytes.of_string s in
  Bytes.iteri
    (fun i c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ()
      | _ -> Bytes.set b i '_')
    b;
  Bytes.to_string b
;;

let lean_name id = "v_" ^ lean_sanitize (Ident.unique_name id)
let lean_dt_name p = "Vox_" ^ lean_sanitize (path_uname p)
let lean_constr_name p c = lean_dt_name p ^ "." ^ lean_sanitize c

let lean_sort = function
  | S_int -> "Int"
  | S_bool -> "Prop"
  | S_other -> "VoxU"
  | S_data p ->
    (match find_datatype p with
     | Some _ -> lean_dt_name p
     | None -> "VoxU" (* unregistered: degrade, sound *))
;;

(* One declaration, on a single line (the error-line mapping counts
   lines); self-recursion within a line is fine.  Variants are
   inductives; records are structures, whose projections come built
   in. *)
let lean_datatype_decl (p, decl) =
  let buf = Buffer.create 128 in
  (match decl with
   | Dt_variant constrs ->
     Buffer.add_string
       buf
       (Printf.sprintf "inductive %s : Type where" (lean_dt_name p));
     List.iter
       (fun (cname, fields) ->
         Buffer.add_string
           buf
           (Printf.sprintf " | %s : " (lean_sanitize cname));
         List.iter
           (fun fs -> Buffer.add_string buf (lean_sort fs ^ " -> "))
           fields;
         Buffer.add_string buf (lean_dt_name p))
       constrs
   | Dt_record fields ->
     Buffer.add_string
       buf
       (Printf.sprintf "structure %s where" (lean_dt_name p));
     List.iter
       (fun (l, fs) ->
         Buffer.add_string
           buf
           (Printf.sprintf " (%s : %s)" (lean_sanitize l) (lean_sort fs)))
       fields);
  Buffer.add_char buf '\n';
  Buffer.contents buf
;;

(* All registered datatypes except [skip] (already declared by an
   imported export), in dependency order. *)
let lean_datatype_decls buf ~skip =
  List.iter
    (fun ((p, _) as dt) ->
      if not (List.exists (String.equal (path_uname p)) skip)
      then Buffer.add_string buf (lean_datatype_decl dt))
    !datatypes
;;

let boolish p =
  let open Refinement in
  match p with
  | Pbool _ | Pbinop ((Eq | Neq | Lt | Le | Gt | Ge), _, _)
  | Pand _ | Por _ | Pnot _ -> true
  | Pvar id ->
    (match Hashtbl.find_opt name_sorts id with
     | Some S_bool -> true
     | _ -> false)
  | Pfield (p, l, _) ->
    (* a bool-sorted field is a [Prop] in the Lean model *)
    (match find_datatype p with
     | Some (_, Dt_record fields) ->
       (match List.assoc_opt l fields with
        | Some S_bool -> true
        | Some (S_int | S_data _ | S_other) | None -> false)
     | Some (_, Dt_variant _) | None -> false)
  | Pis _ -> true
  | Pbound | Pint _ | Pconstr _ | Pfun _
  | Pbinop ((Add | Sub | Mul | Div | Mod), _, _) -> false
;;

let rec lean_of_pred buf (p : Refinement.pred) =
  let open Refinement in
  let bin op a b =
    Buffer.add_char buf '(';
    lean_of_pred buf a;
    Buffer.add_string buf (" " ^ op ^ " ");
    lean_of_pred buf b;
    Buffer.add_char buf ')'
  in
  match p with
  | Pbound -> assert false
  | Pvar id -> Buffer.add_string buf (lean_name id)
  | Pint n ->
    if n >= 0
    then Buffer.add_string buf (Printf.sprintf "(%d : Int)" n)
    else Buffer.add_string buf (Printf.sprintf "((%d : Int))" n)
  | Pbool b -> Buffer.add_string buf (if b then "True" else "False")
  | Pconstr (p, c, []) -> Buffer.add_string buf (lean_constr_name p c)
  | Pconstr (p, c, args) ->
    Buffer.add_string buf ("(" ^ lean_constr_name p c);
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pfun (f, []) -> Buffer.add_string buf f
  | Pfun (f, args) ->
    (* Spec function, emitted verbatim (no quoting is needed: every
       OCaml lowercase identifier, [']s included, is a valid Lean
       identifier); defined by the [-vox-prelude]. *)
    Buffer.add_string buf ("(" ^ f);
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pfield (p, l, a) ->
    (* structure projection *)
    Buffer.add_string buf ("(" ^ lean_dt_name p ^ "." ^ lean_sanitize l ^ " ");
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Pis (p, c, a) ->
    (* existential tester; the exhaustiveness hypothesis emitted per
       tester subject (lean_theorem) lets grind case on it *)
    let n = constr_arity p c in
    Buffer.add_char buf '(';
    if n > 0
    then begin
      Buffer.add_string buf "∃";
      for i = 0 to n - 1 do
        Buffer.add_string buf (Printf.sprintf " e%d" i)
      done;
      Buffer.add_string buf ", "
    end;
    lean_of_pred buf a;
    Buffer.add_string buf (" = " ^ if n > 0 then "(" else "");
    Buffer.add_string buf (lean_constr_name p c);
    for i = 0 to n - 1 do
      Buffer.add_string buf (Printf.sprintf " e%d" i)
    done;
    if n > 0 then Buffer.add_char buf ')';
    Buffer.add_char buf ')'
  | Pbinop (Eq, a, b) -> bin (if boolish a || boolish b then "↔" else "=") a b
  | Pbinop (Neq, a, b) ->
    Buffer.add_string buf "(¬ ";
    bin (if boolish a || boolish b then "↔" else "=") a b;
    Buffer.add_char buf ')'
  | Pbinop (Add, a, b) -> bin "+" a b
  | Pbinop (Sub, a, b) -> bin "-" a b
  | Pbinop (Mul, a, b) -> bin "*" a b
  | Pbinop ((Div | Mod) as op, a, b) ->
    (* OCaml's [/] and [mod] truncate toward zero: exactly [Int.tdiv]
       and [Int.tmod]. *)
    Buffer.add_string buf (if op = Div then "(Int.tdiv " else "(Int.tmod ");
    lean_of_pred buf a;
    Buffer.add_char buf ' ';
    lean_of_pred buf b;
    Buffer.add_char buf ')' 
  | Pbinop (Lt, a, b) -> bin "<" a b
  | Pbinop (Le, a, b) -> bin "≤" a b
  | Pbinop (Gt, a, b) -> bin ">" a b
  | Pbinop (Ge, a, b) -> bin "≥" a b
  | Pand (a, b) -> bin "∧" a b
  | Por (a, b) -> bin "∨" a b
  | Pnot a ->
    Buffer.add_string buf "(¬ ";
    lean_of_pred buf a;
    Buffer.add_char buf ')'
;;

(* Reflected definitions, emitted between the datatypes and the
   prelude.  [@[grind] def] registers the defining equations with
   grind.  Termination is Lean's to check: structural recursion needs
   nothing, and a [@@vox.decreases e] metric becomes
   [termination_by (e).toNat] with an omega [decreasing_by] (the branch
   guards are in context for those goals).  The def name is the source
   name, so a [-vox-prelude] can state lemmas about it. *)
let lean_rsort (s : Vox_reflect.rsort) =
  match s with
  | Vox_reflect.Rint -> "Int"
  | Vox_reflect.Rbool -> "Prop"
  | Vox_reflect.Rdata p -> lean_sort (S_data p)
;;

let rec lean_def_body buf (b : Vox_reflect.def_body) =
  match b with
  | Vox_reflect.Bpred p -> lean_of_pred buf p
  | Vox_reflect.Bite (c, a, b') ->
    Buffer.add_string buf "(if ";
    lean_of_pred buf c;
    Buffer.add_string buf " then ";
    lean_def_body buf a;
    Buffer.add_string buf " else ";
    lean_def_body buf b';
    Buffer.add_char buf ')'
  | Vox_reflect.Bcase (x, clauses) ->
    Buffer.add_string buf ("(match " ^ lean_name x ^ " with");
    List.iter
      (fun (cl : Vox_reflect.def_clause) ->
        Buffer.add_string buf (" | " ^ lean_constr_name cl.dc_path cl.dc_cstr);
        List.iter
          (fun f -> Buffer.add_string buf (" " ^ lean_name f))
          cl.dc_fields;
        Buffer.add_string buf " => ";
        lean_def_body buf cl.dc_rhs)
      clauses;
    Buffer.add_char buf ')'
;;

let lean_spec_def buf (d : Vox_reflect.spec_def) =
  Buffer.add_string buf ("@[grind] def " ^ d.sd_name);
  List.iter
    (fun (id, s) ->
      Buffer.add_string
        buf
        (Printf.sprintf " (%s : %s)" (lean_name id) (lean_rsort s)))
    d.sd_params;
  Buffer.add_string buf (" : " ^ lean_rsort d.sd_ret ^ " := ");
  lean_def_body buf d.sd_body;
  Buffer.add_char buf '\n';
  match d.sd_decreases with
  | None -> ()
  | Some m ->
    Buffer.add_string buf "termination_by (";
    lean_of_pred buf m;
    Buffer.add_string buf ").toNat\ndecreasing_by all_goals omega\n"
;;

(* The .cmi spec export of a unit: its reflected definitions
   (pre-rendered, as lean-only blocks ahead of the user's blocks, which
   may state lemmas about them), its blocks, plus pre-rendered
   declarations of the datatypes its exported refinements and
   definitions mention.  Computed from a FRESH registration pass over
   the exported signature (batch compilation may leave another unit's
   datatype state in the globals), restored afterwards.  No blocks and
   no definitions, no export: without spec functions clients register
   datatypes on demand as before. *)
let cmi_export env (sg : Types.signature) ~defs ~blocks =
  let def_blocks =
    List.map
      (fun (d : Vox_reflect.spec_def) ->
        let b = Buffer.create 128 in
        lean_spec_def b d;
        Buffer.contents b)
      defs
  in
  let blocks = def_blocks @ blocks in
  if blocks = []
  then None
  else begin
    let saved = !datatypes, !registering, !poisoned in
    datatypes := [];
    registering := [];
    poisoned := [];
    Misc.try_finally
      ~always:(fun () ->
        let d, r, po = saved in
        datatypes := d;
        registering := r;
        poisoned := po)
      (fun () ->
        iter_signature_types sg ~f:(fun ~loc:_ ~what:_ ty ->
          register_type_specs env ty);
        List.iter
          (fun d ->
            List.iter
              (fun p -> ignore (datatype_sort env p))
              (Vox_reflect.def_datatype_paths d))
          defs;
        let dts =
          List.map
            (fun ((p, _) as dt) -> path_uname p, lean_datatype_decl dt)
            !datatypes
        in
        Some
          { Cmi_format.vp_datatypes = dts
          ; vp_needs_voxu = datatype_field_needs_voxu ()
          ; vp_blocks = blocks
          })
  end
;;

(* Save-site entry points (see Typemod / Compile_common).  Reflected
   definitions are exported only from the cmi a unit writes itself: for
   a unit with an .mli the cmi comes from the interface, which has no
   bodies to reflect -- there, total_ functions stay private to the
   implementation (clients' calls degrade to unknowns; sound). *)
let cmi_export_of_signature (tsg : Typedtree.signature) =
  cmi_export tsg.sig_final_env tsg.sig_type ~defs:[]
    ~blocks:(collect_preludes_sig tsg)
;;

let cmi_export_of_structure (str : structure) (sg : Types.signature) =
  cmi_export str.str_final_env sg ~defs:!spec_defs
    ~blocks:(List.map fst (collect_preludes str))
;;


(* Emits every definition; also returns the total line count and, per
   definition, its 0-based line span within the block (so a Lean error
   inside a definition -- typically a failed termination proof -- is
   reported against that definition, not blamed on some VC). *)
let lean_theorem buf i vc =
  Buffer.add_string buf (Printf.sprintf "theorem vc_%d " i);
  let seen = Hashtbl.create 16 in
  List.iter
    (fun id ->
      if not (Hashtbl.mem seen id)
      then (
        Hashtbl.add seen id ();
        let sort =
          match Hashtbl.find_opt name_sorts id with
          | Some ds -> lean_sort ds
          | None -> "VoxU"
        in
        Buffer.add_string buf
          (Printf.sprintf "(%s : %s) " (lean_name id) sort)))
    (free_vars_of_vc vc);
  List.iteri
    (fun j f ->
      Buffer.add_string buf (Printf.sprintf "(h_%d : " j);
      lean_of_pred buf f;
      Buffer.add_string buf ") ")
    vc.vc_facts;
  (* Exhaustiveness hypotheses: for each tester subject among the facts,
     tell grind the subject IS one of its constructors, so it can case
     on the negations.
     The disjunction is [Por] over the positive testers, so it reuses
     the serializer; validated shape: (∃ a, s = K a) ∨ ... ∨ s = M. *)
  let seen_subj = Hashtbl.create 4 in
  let exh = ref 0 in
  List.iter
    (fun f ->
      let rec collect (q : Refinement.pred) =
        (match q with
         | Refinement.Pis (path, _, Refinement.Pvar id) ->
           let key = Ident.unique_name id ^ "|" ^ path_uname path in
           if not (Hashtbl.mem seen_subj key)
           then (
             Hashtbl.add seen_subj key ();
             match find_datatype path with
             | Some (_, Dt_variant constrs) ->
               let disj =
                 match
                   List.map
                     (fun (cname, _) ->
                       Refinement.Pis (path, cname, Refinement.Pvar id))
                     constrs
                 with
                 | [] -> assert false (* simple variants are non-empty *)
                 | t :: ts ->
                   List.fold_left (fun acc t' -> Refinement.Por (acc, t')) t ts
               in
               incr exh;
               Buffer.add_string buf (Printf.sprintf "(h_exh%d : " !exh);
               lean_of_pred buf disj;
               Buffer.add_string buf ") "
             | Some (_, Dt_record _) | None -> ())
         | _ -> ());
        match q with
        | Refinement.Pis (_, _, a)
        | Refinement.Pfield (_, _, a)
        | Refinement.Pnot a -> collect a
        | Refinement.Pconstr (_, _, args) | Refinement.Pfun (_, args) ->
          List.iter collect args
        | Refinement.Pbinop (_, a, b)
        | Refinement.Pand (a, b)
        | Refinement.Por (a, b) ->
          collect a;
          collect b
        | Refinement.Pbound | Refinement.Pvar _ | Refinement.Pint _
        | Refinement.Pbool _ -> ()
      in
      collect f)
    vc.vc_facts;
  Buffer.add_string buf ": ";
  lean_of_pred buf vc.vc_goal;
  Buffer.add_string buf " := by grind\n"
;;

(* Returns the file contents and, per theorem, the 1-based line it
   occupies (for mapping lean's error locations back to VCs). *)
let lean_file vcs =
  let buf = Buffer.create 1024 in
  let needs_voxu =
    List.exists
      (fun vc ->
        List.exists
          (fun id ->
            match Hashtbl.find_opt name_sorts id with
            | Some (S_int | S_bool) -> false
            | Some (S_data p) -> find_datatype p = None
            | Some S_other | None -> true)
          (free_vars_of_vc vc))
      vcs
    || datatype_field_needs_voxu ()
  in
  (* Header, as (text, provenance) SEGMENTS so line accounting cannot
     drift from what is emitted: VoxU (referenced by datatype fields,
     so first); each imported unit's datatype declarations in
     dependency order (deduplicated across units by stable name); this
     module's remaining datatypes; then -- only when some VC applies a
     spec function -- the prelude text: imported blocks in dependency
     order, the [-vox-prelude] file, this module's own blocks in
     source order; finally the elaboration bound.  Theorems follow,
     one per line.  A solver error inside a block is reported at the
     block's own location (or its defining unit). *)
  let needs_voxu = needs_voxu || imported_need_voxu () in
  let want_prelude = List.exists vc_uses_spec_fun vcs in
  let segments = ref [] in
  let seg ?src text = if text <> "" then segments := (text, src) :: !segments in
  if needs_voxu then seg "opaque VoxU : Type\n";
  let seen = ref [] in
  List.iter
    (fun (unit, vp) ->
      List.iter
        (fun (n, leand) ->
          if not (List.exists (String.equal n) !seen)
          then (
            seen := n :: !seen;
            seg ~src:(Imported_block unit) leand))
        vp.Cmi_format.vp_datatypes)
    !imported_preludes;
  let own_decls = Buffer.create 256 in
  lean_datatype_decls own_decls ~skip:!seen;
  seg (Buffer.contents own_decls);
  (* Imported blocks and the [-vox-prelude] file come BEFORE this
     module's reflected definitions: a definition may call an imported
     reflected function (whose definition rides the exporting unit's
     blocks).  They are therefore also needed whenever this module has
     definitions, not only when a VC applies a spec function. *)
  if want_prelude || !spec_defs <> []
  then (
    List.iter
      (fun (unit, vp) ->
        List.iter
          (fun text -> seg ~src:(Imported_block unit) text)
          vp.Cmi_format.vp_blocks)
      !imported_preludes;
    seg (prelude ()));
  (* Reflected definitions, unconditionally: they are checked
     (termination included) even when nothing else needs the
     prelude.  This module's own blocks follow them, so a block may
     state lemmas about them. *)
  List.iter
    (fun (d : Vox_reflect.spec_def) ->
      let b = Buffer.create 128 in
      lean_spec_def b d;
      seg ~src:(Reflected_def d) (Buffer.contents b))
    !spec_defs;
  if want_prelude
  then
    List.iter
      (fun (s, loc) -> seg ~src:(Local_block loc) s)
      !embedded_preludes;
  (* Bound elaboration per theorem: a diverging [grind] must count as
     a verification failure, not hang the build.  (A wedged process
     outside elaboration remains out of scope.)  Emitted
     after the prelude so a prelude may begin with [import], which
     Lean requires to be the first command in the file. *)
  seg "set_option maxHeartbeats 400000\n";
  let segments = List.rev !segments in
  let block_ranges, first_line =
    List.fold_left
      (fun (ranges, start) (text, src) ->
        let n = count_lines text in
        let ranges =
          match src with
          | Some src -> (start, n, src) :: ranges
          | None -> ranges
        in
        (ranges, start + n))
      ([], 1)
      segments
  in
  List.iter (fun (text, _) -> Buffer.add_string buf text) segments;
  List.iteri (fun i vc -> lean_theorem buf i vc) vcs;
  let block_of_line line =
    List.find_map
      (fun (start, n, src) ->
        if start <= line && line < start + n
        then Some (src, line - start + 1)
        else None)
      block_ranges
  in
  ( Buffer.contents buf,
    (fun line ->
       (* An error on a header line maps to no VC (and a negative
          index would make [List.nth_opt] raise). *)
       if line < first_line then None else List.nth_opt vcs (line - first_line)),
    block_of_line )
;;

(* Counterexample rendering: a failed [grind] prints, among its goal
   diagnostics, the arithmetic model its linear solver ended on
   ("[assign] v_n_308 := 7").  Rewritten to source names, that model is
   usually a concrete input on which the goal is false -- the single
   most useful thing a failure message can carry.  Lines still
   mentioning internal [v_...] names after rewriting (values the VC
   cannot name) are dropped; ["a ^ 2"]-style bracketed ring monomials
   too. *)
let replace_all ~sub ~by s =
  let n = String.length sub in
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i <= String.length s - n do
    if String.equal (String.sub s !i n) sub
    then (
      Buffer.add_string buf by;
      i := !i + n)
    else (
      Buffer.add_char buf s.[!i];
      incr i)
  done;
  Buffer.add_substring buf s !i (String.length s - !i);
  Buffer.contents buf
;;

let counterexample_for_error vc assigns =
  match assigns with
  | [] -> ""
  | _ ->
    let display = vc_display_fun vc in
    let vars =
      List.fold_left
        (fun acc id -> if List.exists (Ident.same id) acc then acc else id :: acc)
        []
        (free_vars_of_vc vc)
    in
    let subs =
      List.sort
        (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
        (List.map (fun id -> lean_name id, display id) vars)
    in
    let rewrite s =
      List.fold_left (fun s (sub, by) -> replace_all ~sub ~by s) s subs
    in
    let contains ~sub s =
      let n = String.length sub in
      let rec at i =
        i + n <= String.length s
        && (String.equal (String.sub s i n) sub || at (i + 1))
      in
      at 0
    in
    let shown =
      List.filter_map
        (fun l ->
          let l = rewrite l in
          if contains ~sub:"v_" l
          then None
          else (
            (* Nonlinear monomials and other theory atoms print in
               corner brackets ("[x * y] := 1"); keep them, brackets
               stripped -- dropping them could show a partial model
               that satisfies the goal. *)
            let l = replace_all ~sub:"\xe3\x80\x8c" ~by:"" l in
            let l = replace_all ~sub:"\xe3\x80\x8d" ~by:"" l in
            Some (replace_all ~sub:" := " ~by:" = " l)))
        assigns
    in
    let shown =
      if List.length shown > 12
      then List.filteri (fun i _ -> i < 12) shown @ [ "..." ]
      else shown
    in
    (match shown with
     | [] -> ""
     | _ ->
       "\nPossible counterexample:"
       ^ String.concat "" (List.map (fun l -> "\n  " ^ l) shown))
;;

let run_lean vcs =
  (* Reflected definitions are checked (termination included) even when
     the module has no VCs of its own: a rejected definition must fail
     its defining module, not lie in wait. *)
  match vcs, !spec_defs with
  | [], [] -> ()
  | _ ->
    let fallback_loc =
      match vcs, !spec_defs with
      | vc :: _, _ -> vc.vc_loc
      | [], d :: _ -> d.Vox_reflect.sd_loc
      | [], [] -> assert false
    in
    let contents, vc_of_line, block_of_line = lean_file vcs in
    let in_file = Filename.temp_file "vox" ".lean" in
    let out_file = Filename.temp_file "vox" ".out" in
    Misc.try_finally
      ~always:(fun () ->
        Misc.remove_file in_file;
        Misc.remove_file out_file)
      (fun () ->
        let oc = open_out in_file in
        output_string oc contents;
        close_out oc;
        let cmd =
          Printf.sprintf "%s %s > %s 2>&1"
            (Filename.quote (lean_command ()))
            (Filename.quote in_file) (Filename.quote out_file)
        in
        let status = Sys.command cmd in
        if status <> 0
        then begin
          (* Find the first "<file>:LINE:COL: error: ..." and map it
             back.  Lean also emits "warning:" lines in the same
             format (e.g. for unused hypotheses); attributing the
             failure to the first WARNING would blame the wrong VC, so
             only "error:" lines count. *)
          (* Lean prints "<file>:L:C: error: ..." or, with a kind,
             "<file>:L:C: error(lean.some.kind): ...".  Warnings use
             the same shapes with "warning"; only errors count (a
             warning line before the real error must not steal the
             attribution). *)
          let error_marker l =
            let needle = " error" in
            let n = String.length needle in
            let rec at i =
              if i + n > String.length l
              then None
              else if
                String.equal (String.sub l i n) needle
                && i + n < String.length l
                && (l.[i + n] = ':' || l.[i + n] = '(')
              then Some (i + 1)
              else at (i + 1)
            in
            at 0
          in
          let ic = open_in out_file in
          let error_line = ref None in
          let msg = ref "" in
          let first_output = ref "" in
          (* The grind diagnostics that follow the first error include
             the arithmetic model ("[assign] x := 7") that refuted the
             goal; collect it until the next per-location message. *)
          let assigns = ref [] in
          let assigns_done = ref false in
          let is_file_line l =
            String.length l > String.length in_file
            && String.equal (String.sub l 0 (String.length in_file)) in_file
          in
          (try
             while true do
               let l = input_line ic in
               if String.equal !first_output "" then first_output := l;
               match !error_line with
               | None ->
                 (match String.index_opt l ':' with
                  | Some _ when is_file_line l && error_marker l <> None ->
                    let rest =
                      String.sub l (String.length in_file + 1)
                        (String.length l - String.length in_file - 1)
                    in
                    (match String.index_opt rest ':' with
                     | Some i ->
                       (match int_of_string_opt (String.sub rest 0 i) with
                        | Some n ->
                          error_line := Some n;
                          msg := l
                        | None -> ())
                     | None -> ())
                  | _ -> ())
               | Some _ ->
                 if is_file_line l
                 then assigns_done := true
                 else if not !assigns_done
                 then (
                   let t = String.trim l in
                   let tag = "[assign] " in
                   if String.length t > String.length tag
                      && String.equal (String.sub t 0 (String.length tag)) tag
                   then
                     assigns
                     := String.sub t (String.length tag)
                          (String.length t - String.length tag)
                        :: !assigns)
             done
           with
           | End_of_file -> ());
          close_in ic;
          let assigns = List.rev !assigns in
          (* Strip the (nondeterministic) temp-file prefix from the
             message; keep from "error"/"error(kind)" onward. *)
          let strip_msg m =
            match error_marker m with
            | Some i -> String.sub m i (String.length m - i)
            | None -> m
          in
          match !error_line with
          | None ->
            (* No per-theorem diagnostic: the solver itself failed
               (missing binary, crash, bad flags).  Blaming a VC would
               hide the real cause. *)
            Location.raise_errorf ~loc:fallback_loc
              "vox: verification failed (lean solver error, exit %d): %s"
              status
              (if String.equal !first_output ""
               then "<no output>"
               else !first_output)
          | Some line ->
            (match block_of_line line with
             | Some (Local_block block_loc, rel_line) ->
               (* The error is inside an embedded [%%vox.lean]
                  block: report it there, not at a VC. *)
               Location.raise_errorf ~loc:block_loc
                 "vox: error in this solver block (line %d of the \
                  block):@ %s"
                 rel_line
                 (strip_msg !msg)
             | Some (Imported_block unit, rel_line) ->
               (* The error is inside a spec prelude imported from
                  another unit's interface (e.g. two units exporting
                  the same spec-function name).  There is no local
                  source position; anchor at the current file. *)
               Location.raise_errorf
                 ~loc:(Location.in_file !Location.input_name)
                 "vox: error in the spec block imported from unit \
                  %s (line %d of its block):@ %s"
                 unit
                 rel_line
                 (strip_msg !msg)
             | Some (Reflected_def d, _) ->
               (* The definition itself was rejected -- most often Lean
                  could not establish termination. *)
               let msg = strip_msg !msg in
               Location.raise_errorf ~loc:d.Vox_reflect.sd_loc
                 "vox: the reflected definition of %s was rejected by the \
                  solver (is it terminating?  int-indexed recursion needs a \
                  [@@vox.decreases] metric)%s"
                 d.Vox_reflect.sd_name
                 (if String.equal msg "" then "" else "\n(lean: " ^ msg ^ ")")
             | None ->
               let msg = strip_msg !msg in
               (match vc_of_line line, vcs with
                | Some vc, _ | None, vc :: _ ->
                  Location.raise_errorf ~loc:vc.vc_loc
                    "vox: verification failed (lean).@ Goal: %s%s%s%s"
                    (goal_for_error vc)
                    (hyps_for_error vc)
                    (counterexample_for_error vc assigns)
                    (if String.equal msg "" then ""
                     else "\n(lean: " ^ msg ^ ")")
                | None, [] ->
                  Location.raise_errorf ~loc:fallback_loc
                    "vox: verification failed (lean): %s"
                    (if String.equal msg "" then "<no output>" else msg)))
        end)
;;

(* ------------------------------------------------------------------ *)

let print_pred ppf p = Format.pp_print_string ppf (Refinement.to_string p)

let dump_vc ppf vc =
  with_vc_display vc @@ fun () ->
  Format.fprintf
    ppf
    "@[<v 2>%a: vox VC%s:@ goal: %a@ hypotheses:%t@]@."
    Location.print_loc
    vc.vc_loc
    (match vc.vc_kind with
     | Prove -> ""
     | Runtime_check -> " (RUNTIME CHECKED)"
     | Assume -> " (ASSUMED)")
    print_pred
    vc.vc_goal
    (fun ppf ->
      if vc.vc_facts = []
      then Format.fprintf ppf " <none>"
      else List.iter (fun f -> Format.fprintf ppf "@ %a" print_pred f) vc.vc_facts)
;;

let discharge () =
  check_imported_datatype_clashes ~render:lean_datatype_decl;
  let all = List.rev !vcs in
  (* A constructor application whose datatype failed to register (the type is
     not a simple variant here, or is mutually recursive) cannot be declared
     to the solver: such a goal is an error, such a fact is dropped (sound). *)
  let pred_usable p =
    List.for_all (fun q -> find_datatype q <> None) (Refinement.constr_paths p)
  in
  let all =
    List.map
      (fun vc ->
        let needs_solver =
          match vc.vc_kind with
          | Prove -> true
          | Runtime_check | Assume -> false
        in
        if needs_solver && not (pred_usable vc.vc_goal)
        then
          Location.raise_errorf
            ~loc:vc.vc_loc
            "vox: this obligation mentions constructors of a type that is \
             not usable here (not a simple variant, or mutually recursive)";
        { vc with vc_facts = List.filter pred_usable vc.vc_facts })
      all
  in
  if !Clflags.vox_dump_vc then List.iter (dump_vc Format.err_formatter) all;
  if !Clflags.vox_dry_run
  then ()
  else (
    let needs_proof vc =
      match vc.vc_kind with
      | Prove -> true
      | Runtime_check | Assume -> false
    in
    run_lean (List.filter needs_proof all))
;;

(* Entry point: called on the final typedtree of an implementation. *)
(* VCs arise only from [refine_]/[assume_] expressions and [refine_] patterns, all of
   which carry a "vox." attribute. Programs without any are skipped entirely: the pass
   must not even inspect (and via [Ctype.expand_head], mutate) the types of unannotated
   programs. *)
let uses_vox (str : structure) =
  (* Applications to contract parameters carry no vox syntax; the type
     checker flags them ([Vox_dep.contract_use_seen]) at the point it
     strips the parameter refinement, where the domain is already being
     expanded at the correct stage.  Read-and-clear per unit/phrase. *)
  let contract_use = !Vox_dep.contract_use_seen in
  Vox_dep.contract_use_seen := false;
  let found = ref contract_use in
  let has_vox attrs =
    List.exists
      (fun (a : Parsetree.attribute) ->
        String.length a.attr_name.txt >= 4
        && String.equal (String.sub a.attr_name.txt 0 4) "vox.")
      attrs
  in
  (* A structural (no-expansion) check: a binder can have a refined type
     with no vox syntax of its own (e.g. it was bound to a refined value
     from another phrase or module) and must still contribute facts and
     be escape-checked.  Aliases hiding a [Trefine] behind [Tconstr] are
     missed; expanding here would mutate the types of programs that
     never opted into vox, which this gate exists to prevent. *)
  let type_has_refine ty =
    let refined = ref false in
    Vox_dep.iter_refinement_preds ty (fun ~bound:_ _ -> refined := true);
    !refined
  in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e ->
          if has_vox e.exp_attributes then found := true;
          Tast_iterator.default_iterator.expr sub e)
    ; pat =
        (fun sub (type k) (p : k general_pattern) ->
          if has_vox p.pat_attributes || type_has_refine p.pat_type
          then found := true;
          Tast_iterator.default_iterator.pat sub p)
    ; value_binding =
        (fun sub vb ->
          (* [total_] bindings need the pass even when no other
             vox syntax appears (their definitions must be registered,
             translated, and checked). *)
          if has_vox vb.vb_attributes then found := true;
          Tast_iterator.default_iterator.value_binding sub vb)
    }
  in
  it.structure it str;
  (* A phrase (or module) whose only vox content is a [%%vox.lean]
     block has no vox expressions, patterns, or bindings, but must
     still be walked: at the toplevel a prelude-only FIRST phrase would
     otherwise be skipped and its block silently dropped from every
     later phrase's solver input (the spec functions then elaborate as
     unbound identifiers, failing obligations for the wrong reason). *)
  !found
  || List.exists
       (fun item ->
         match item.str_desc with
         | Tstr_attribute (a : attribute) ->
           is_prelude_extension_name a.attr_name.txt
         | _ -> false)
       str.str_items
;;

let walk_items (str : structure) ctx =
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_value (_rec_flag, vbs) ->
        (match vbs with
         | _ :: _ :: _
           when List.exists
                  Vox_reflect.is_total_binding
                  vbs ->
           (* Emission order is definition order, so a group could
              forward-reference; mutual recursion is not supported
              (matching the datatype restriction). *)
           Location.raise_errorf
             ~loc:(List.hd vbs).vb_loc
             "vox: total_ is not supported on multi-binding groups \
              (mutually recursive reflected functions are not supported)"
         | _ -> ());
        List.iter
          (fun vb ->
            if Vox_reflect.is_total_binding vb
            then register_spec_def str.str_final_env vb)
          vbs;
        List.iter (fun vb -> walk_expr str.str_final_env !ctx vb.vb_expr) vbs;
        ctx
        := List.fold_left
             (fun ctx vb ->
               extend_pat ~toplevel:true str.str_final_env ctx vb.vb_pat)
             !ctx
             vbs
      | _ ->
        let it =
          { Tast_iterator.default_iterator with
            expr = (fun _ e -> walk_expr str.str_final_env !ctx e)
          ; pat =
              (fun sub (type k) (p : k general_pattern) ->
                backstop_pat !ctx p;
                Tast_iterator.default_iterator.pat sub p)
          ; value_binding =
              (fun sub vb ->
                reject_local_reflect vb;
                Tast_iterator.default_iterator.value_binding sub vb)
          }
        in
        it.structure_item it item)
    str.str_items
;;

let check_implementation (str : structure) (sg : Types.signature) =
  (* The signature check is unconditional: a refined type can appear in
     an exported item (a type manifest, an exception, an external) with
     no vox syntax in any expression, and it must still be
     self-contained.  It only reads types structurally, so it cannot
     perturb programs that never use vox. *)
  check_signature sg;
  if not (uses_vox str)
  then ()
  else (
    reset ();
    (* Blocks anywhere in the module are available to all of its VCs
       (they are emitted, in source order, into every solver input);
       blocks exported by imported units' interfaces -- including this
       unit's own .mli -- come from their .cmis. *)
    embedded_preludes := collect_preludes str;
    imported_preludes := gather_imported_preludes ();
    let ctx = ref { cfacts = []; cscope = [] } in
    walk_items str ctx;
    discharge ())
;;

(* Toplevel entry point (also the expect-test runner): phrases arrive one at a
   time, so the logical context persists across phrases, mirroring how facts
   accumulate down the items of an implementation.  Skipping is per-session
   rather than per-phrase: once any phrase has used vox, later phrases are
   walked even without vox attributes of their own, so that their toplevel
   binders (which may carry refinements copied from earlier phrases)
   contribute facts. *)
let toplevel_ctx = ref { cfacts = []; cscope = [] }
let toplevel_preludes : (string * Location.t) list ref = ref []
let toplevel_active = ref false

let check_toplevel_phrase (str : structure) ~(sig_acc : Types.signature)
      (sg : Types.signature) =
  (* [sig_acc] (the session's accumulated signature) is re-checked on
     every phrase: typing this phrase can instantiate a weak type
     variable in an EARLIER phrase's signature with a refined type
     mentioning one of this phrase's variables -- e.g. a stored closure
     in a module-wrapped cell -- which phrase-local checks miss. *)
  check_signature sig_acc;
  check_signature sg;
  if !toplevel_active || uses_vox str
  then (
    toplevel_active := true;
    vcs := [];
    (* The session's committed blocks plus this phrase's; committed
       (like the facts below) only if the phrase discharges. *)
    embedded_preludes := !toplevel_preludes @ collect_preludes str;
    imported_preludes := gather_imported_preludes ();
    (* Reflected definitions and datatype registrations are committed
       the same way: a failed phrase is backtracked, so its rejected
       definition must not be re-emitted (and re-fail, blamed at the
       OLD location) by every later phrase, and its datatypes must not
       collide -- at their stamp-free solver-side names -- with the
       retried phrase's. *)
    let saved_spec_defs = !spec_defs in
    let saved_datatypes = !datatypes in
    let ctx = ref !toplevel_ctx in
    Misc.try_finally
      ~exceptionally:(fun () ->
        spec_defs := saved_spec_defs;
        datatypes := saved_datatypes)
      (fun () ->
        walk_items str ctx;
        (* Discharge before committing the phrase's facts: if
           verification fails, the toplevel backtracks the phrase, so
           its bindings never exist and their facts (e.g. a refuted
           contradictory refinement) must not be available to later
           phrases. *)
        discharge ());
    toplevel_ctx := !ctx;
    toplevel_preludes := !embedded_preludes)
;;
