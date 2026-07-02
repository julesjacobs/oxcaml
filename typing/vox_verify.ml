(* vox: verification-condition generation and discharge.

   Runs as a separate pass over the FINAL typedtree (the type checker emits no VCs; it
   backtracks internally). Walks the tree carrying a logical environment of facts; each
   [refine_] node yields the VC [facts |- p[v := name of e]]; [assume_] is reported as
   RUNTIME CHECKED (translcore compiles a check of the predicate) and
   [assume_unchecked_] as ASSUMED; neither goes to the solver. Facts come from exactly
   four places (DESIGN.md): unpacking / binders of refined type, path facts from [if],
   dependent application, and match facts on a variable scrutinee ([s = C x1 ... xn] in
   the branch that matched [C x1 ... xn]).

   VCs are discharged by a Z3 subprocess over SMT-LIB2. Solver error, unknown, and timeout
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
let with_vc_display vc k =
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
  let display id =
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
  in
  Refinement.with_var_display display k
;;

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

let reset () =
  vcs := [];
  Hashtbl.reset name_sorts;
  Hashtbl.reset synthetic_names;
  datatypes := [];
  registering := [];
  poisoned := [];
  unknown_counter := 0
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

let has_vox_attr name attrs =
  List.exists (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name) attrs
;;

(* The refinement of a type, if any. *)
let refinement_of_type env ty =
  match get_desc (safe_expand_head env ty) with
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

let rec name_of_expr env (e : expression) : Refinement.pred =
  match Vox_reflect.translate e with
  | Some p -> p
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
   are future work) and spec functions (solver-side only, no runtime
   denotation). *)
let rec pred_unreflectable (p : Refinement.pred) =
  match p with
  | Refinement.Pconstr _ | Refinement.Pfun _ | Refinement.Pfield _ -> true
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
          involves a constructor or spec function, which the compiled check \
          cannot evaluate; use assume_unchecked_";
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
let backstop_pat : type k. ctx -> k general_pattern -> unit =
  fun ctx pat ->
  let bound = pat_bound_idents pat in
  List.iter
    (fun (id, _, ty, _, _) ->
      check_binder_escape ~toplevel:false ctx ~extra_scope:bound pat id ty)
    (pat_bound_idents_full pat)
;;

(* Module-level self-containment, applied to a whole signature
   (implementation, interface, or toplevel phrase): every refinement
   reachable from any exported item -- values, type manifests, record
   fields, constructor arguments, extension constructors, submodules,
   module types, classes -- must be free of program variables.  This is
   what makes .cmi predicates self-contained: stamps do not survive a
   compilation unit, so an imported [Pvar] can collide with an
   unrelated local stamp and prove false facts. *)
let rec check_signature (sg : Types.signature) =
  List.iter check_signature_item sg

and check_signature_item (item : Types.signature_item) =
  let check ~loc ~what ty = check_type_escapes ~loc ~what Module_level ty in
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
  | Sig_module (_, _, md, _, _) -> check_module_type md.md_type
  | Sig_modtype (_, mtd, _) -> Option.iter check_module_type mtd.mtd_type
  | Sig_class (id, cd, _, _) ->
    check_class_type ~loc:cd.cty_loc ~what:(Ident.name id) cd.cty_type
  | Sig_class_type (id, ctd, _, _) ->
    check_class_type ~loc:ctd.clty_loc ~what:(Ident.name id) ctd.clty_type
  | Sig_jkind _ -> ()

and check_module_type = function
  | Mty_ident _ | Mty_alias _ -> ()
  | Mty_signature sg -> check_signature sg
  | Mty_functor (param, res, _) ->
    (match param with
     | Unit -> ()
     | Named (_, mty, _) -> check_module_type mty);
    check_module_type res
  | Mty_strengthen (mty, _, _) -> check_module_type mty

and check_class_type ~loc ~what = function
  | Cty_constr (_, args, cty) ->
    List.iter (check_type_escapes ~loc ~what Module_level) args;
    check_class_type ~loc ~what cty
  | Cty_signature csig ->
    check_type_escapes ~loc ~what Module_level csig.csig_self;
    Vars.iter
      (fun _ (_, _, ty) -> check_type_escapes ~loc ~what Module_level ty)
      csig.csig_vars
  | Cty_arrow (_, ty, cty) ->
    check_type_escapes ~loc ~what Module_level ty;
    check_class_type ~loc ~what cty
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
  | Texp_let (_rec_flag, vbs, body) ->
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
    let do_case : type k. k case -> unit =
      fun c ->
      let ctx' = extend_pat ~scrut:scrut.exp_type env ctx c.c_lhs in
      let ctx' =
        match scrut_id with
        | Some sid ->
          { ctx' with cfacts = match_facts env sid c.c_lhs @ ctx'.cfacts }
        | None -> ctx'
      in
      Option.iter (walk_expr env ctx') c.c_guard;
      walk_expr env ctx' c.c_rhs
    in
    List.iter do_case comp_cases;
    List.iter do_case val_cases
  | Texp_ifthenelse (cond, e_then, e_else) ->
    walk_expr env ctx cond;
    (* The path fact is the condition's logic translation when it has
       one (a variable, or a translatable int/bool expression);
       untranslatable conditions contribute nothing. *)
    let cond_fact = Vox_reflect.translate cond in
    let with_fact f ctx =
      match cond_fact with
      | None -> ctx
      | Some c -> { ctx with cfacts = f c :: ctx.cfacts }
    in
    walk_expr env (with_fact (fun c -> c) ctx) e_then;
    Option.iter (walk_expr env (with_fact (fun c -> Refinement.Pnot c) ctx)) e_else
  | Texp_function { params; body; _ } ->
    let ctx' =
      List.fold_left
        (fun ctx fp ->
          match fp.fp_kind with
          | Tparam_pat pat -> extend_pat env ctx pat
          | Tparam_optional_default (pat, default, _) ->
            walk_expr env ctx default;
            extend_pat env ctx pat)
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
       escape-checked but contribute no facts. *)
    let it =
      { Tast_iterator.default_iterator with
        expr = (fun _ e' -> walk_expr env ctx e')
      ; pat =
          (fun sub (type k) (p : k general_pattern) ->
            backstop_pat ctx p;
            Tast_iterator.default_iterator.pat sub p)
      }
    in
    Tast_iterator.default_iterator.expr it e
;;

(* ------------------------------------------------------------------ *)
(* SMT-LIB2 serialization *)

(* SMT-LIB2 quoted symbols may contain any character except '|' and
   '\'; operator identifiers (e.g. [( || )]) would otherwise break the
   quoting.  Replacement cannot collide: [Ident.unique_name] includes
   the stamp, and distinct idents never share one. *)
let smt_name id =
  let s =
    String.map
      (fun c ->
        match c with
        | '|' | '\\' -> '_'
        | _ -> c)
      (Ident.unique_name id)
  in
  "|" ^ s ^ "|"
;;

let smt_escape s =
  String.map
    (fun c ->
      match c with
      | '|' | '\\' -> '_'
      | _ -> c)
    s
;;

let smt_dt_name p = "|dt:" ^ smt_escape (path_uname p) ^ "|"
let smt_constr_name p c = "|c:" ^ smt_escape (path_uname p ^ "." ^ c) ^ "|"

let smt_sel_name p c i =
  "|s:" ^ smt_escape (path_uname p ^ "." ^ c) ^ "." ^ Int.to_string i ^ "|"
;;

let smt_field_name p l = "|s:" ^ smt_escape (path_uname p ^ "." ^ l) ^ "|"


let smt_sort = function
  | S_int -> "Int"
  | S_bool -> "Bool"
  | S_other -> "VoxU"
  | S_data p ->
    (match find_datatype p with
     | Some _ -> smt_dt_name p
     | None -> "VoxU" (* unregistered: degrade, sound *))
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

(* All registered datatypes, one [declare-datatypes] block each, already in
   dependency order (self-recursion within a block is fine).  A record is a
   single ["mk"] constructor whose selectors are the labels. *)
let smt_datatype_decls buf =
  List.iter
    (fun (p, decl) ->
      Buffer.add_string
        buf
        (Printf.sprintf "(declare-datatypes ((%s 0)) ((" (smt_dt_name p));
      (match decl with
       | Dt_variant constrs ->
         List.iteri
           (fun k (cname, fields) ->
             if k > 0 then Buffer.add_char buf ' ';
             Buffer.add_string buf ("(" ^ smt_constr_name p cname);
             List.iteri
               (fun i fs ->
                 Buffer.add_string
                   buf
                   (Printf.sprintf
                      " (%s %s)"
                      (smt_sel_name p cname i)
                      (smt_sort fs)))
               fields;
             Buffer.add_char buf ')')
           constrs
       | Dt_record fields ->
         Buffer.add_string buf ("(" ^ smt_constr_name p "mk");
         List.iter
           (fun (l, fs) ->
             Buffer.add_string
               buf
               (Printf.sprintf " (%s %s)" (smt_field_name p l) (smt_sort fs)))
           fields;
         Buffer.add_char buf ')');
      Buffer.add_string buf ")))\n")
    !datatypes
;;

let rec smt_of_pred buf (p : Refinement.pred) =
  let open Refinement in
  match p with
  | Pbound -> assert false (* always substituted before discharge *)
  | Pvar id -> Buffer.add_string buf (smt_name id)
  | Pint n ->
    if n >= 0
    then Buffer.add_string buf (Int.to_string n)
    else (
      (* Strip the sign rather than negating: [-min_int] overflows. *)
      let s = Int.to_string n in
      Buffer.add_string buf
        (Printf.sprintf "(- %s)" (String.sub s 1 (String.length s - 1))))
  | Pbool b -> Buffer.add_string buf (Bool.to_string b)
  | Pconstr (p, c, []) -> Buffer.add_string buf (smt_constr_name p c)
  | Pconstr (p, c, args) ->
    Buffer.add_string buf ("(" ^ smt_constr_name p c);
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        smt_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  (* Spec functions are defined by the [-vox-prelude].  The name is
     pipe-quoted: [|f|] denotes the same SMT symbol as a plain [f], so
     preludes declaring the plain name keep working, while names that
     are not simple SMT symbols (e.g. [len']) stay well-formed. *)
  | Pfun (f, []) -> Buffer.add_string buf ("|" ^ smt_escape f ^ "|")
  | Pfun (f, args) ->
    Buffer.add_string buf ("(|" ^ smt_escape f ^ "|");
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        smt_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pfield (p, l, a) ->
    Buffer.add_string buf ("(" ^ smt_field_name p l ^ " ");
    smt_of_pred buf a;
    Buffer.add_char buf ')'
  | Pbinop (Neq, a, b) ->
    Buffer.add_string buf "(not (= ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_string buf "))"
  | Pbinop (op, a, b) ->
    let s =
      match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Eq -> "="
      | Lt -> "<"
      | Le -> "<="
      | Gt -> ">"
      | Ge -> ">="
      | Neq -> assert false
    in
    Buffer.add_string buf ("(" ^ s ^ " ");
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Pand (a, b) ->
    Buffer.add_string buf "(and ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Por (a, b) ->
    Buffer.add_string buf "(or ";
    smt_of_pred buf a;
    Buffer.add_char buf ' ';
    smt_of_pred buf b;
    Buffer.add_char buf ')'
  | Pnot a ->
    Buffer.add_string buf "(not ";
    smt_of_pred buf a;
    Buffer.add_char buf ')'
;;

let free_vars_of_vc vc = List.concat_map Refinement.free_vars (vc.vc_goal :: vc.vc_facts)

(* The [-vox-prelude] file: user-written solver-side definitions (spec
   functions such as measures), inserted verbatim into every generated
   solver input just after the datatype declarations.  Written for
   whichever backend [-vox-solver] selects.  Normalized to end in a
   newline; an unreadable file is a verification failure. *)
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

let smt_script vc =
  let buf = Buffer.create 512 in
  let seen = Hashtbl.create 16 in
  let needs_other =
    List.exists
      (fun id ->
        match Hashtbl.find_opt name_sorts id with
        | Some (S_int | S_bool) -> false
        | Some (S_data p) -> find_datatype p = None
        | Some S_other | None -> true)
      (free_vars_of_vc vc)
    || datatype_field_needs_voxu ()
  in
  if needs_other then Buffer.add_string buf "(declare-sort VoxU 0)\n";
  smt_datatype_decls buf;
  if vc_uses_spec_fun vc then Buffer.add_string buf (prelude ());
  List.iter
    (fun id ->
      if not (Hashtbl.mem seen id)
      then (
        Hashtbl.add seen id ();
        let s =
          match Hashtbl.find_opt name_sorts id with
          | Some ds -> smt_sort ds
          | None -> "VoxU"
        in
        Buffer.add_string buf (Printf.sprintf "(declare-const %s %s)\n" (smt_name id) s)))
    (free_vars_of_vc vc);
  List.iter
    (fun f ->
      Buffer.add_string buf "(assert ";
      smt_of_pred buf f;
      Buffer.add_string buf ")\n")
    vc.vc_facts;
  Buffer.add_string buf "(assert (not ";
  smt_of_pred buf vc.vc_goal;
  Buffer.add_string buf "))\n(check-sat)\n";
  Buffer.contents buf
;;

(* ------------------------------------------------------------------ *)
(* Z3 harness: [Sys.command] + temp files; no unix dependency. The solver's own timeout
   flag bounds runtime. A wedged process is out of scope for v0. *)

type verdict =
  | Valid
  | Invalid
  | Unknown of string

let z3_command () =
  if not (String.equal !Clflags.vox_solver_path "") then !Clflags.vox_solver_path
  else
    match Sys.getenv_opt "VOX_Z3" with
    | Some s -> s
    | None -> "z3"
;;

let lean_command () =
  if not (String.equal !Clflags.vox_solver_path "") then !Clflags.vox_solver_path
  else
    match Sys.getenv_opt "VOX_LEAN" with
    | Some s -> s
    | None -> "lean"
;;

let run_z3 script =
  let in_file = Filename.temp_file "vox" ".smt2" in
  let out_file = Filename.temp_file "vox" ".out" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file in_file;
      Misc.remove_file out_file)
    (fun () ->
      let oc = open_out in_file in
      output_string oc script;
      close_out oc;
      let cmd =
        Printf.sprintf
          "%s -T:10 %s > %s 2>&1"
          (Filename.quote (z3_command ()))
          (Filename.quote in_file)
          (Filename.quote out_file)
      in
      let status = Sys.command cmd in
      let first_line =
        let ic = open_in out_file in
        let l =
          try input_line ic with
          | End_of_file -> ""
        in
        close_in ic;
        l
      in
      match first_line with
      | "unsat" -> Valid
      | "sat" -> Invalid
      | "timeout" -> Unknown "solver timeout"
      | "unknown" -> Unknown "solver returned unknown"
      | other ->
        Unknown
          (Printf.sprintf
             "solver error (exit %d): %s"
             status
             (if String.equal other "" then "<no output>" else other)))
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

(* One declaration per line (the error-line mapping counts lines), in
   dependency order; self-recursion within a line is fine.  Variants are
   inductives; records are structures, whose projections come built in. *)
let lean_datatype_decls buf =
  List.iter
    (fun (p, decl) ->
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
      Buffer.add_char buf '\n')
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
  | Pbound | Pint _ | Pconstr _ | Pfun _ | Pbinop ((Add | Sub | Mul), _, _) ->
    false
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
    (* Spec function, emitted verbatim (unlike the SMT side, no quoting
       is needed: every OCaml lowercase identifier, [']s included, is a
       valid Lean identifier); defined by the [-vox-prelude]. *)
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
  | Pbinop (Eq, a, b) -> bin (if boolish a || boolish b then "↔" else "=") a b
  | Pbinop (Neq, a, b) ->
    Buffer.add_string buf "(¬ ";
    bin (if boolish a || boolish b then "↔" else "=") a b;
    Buffer.add_char buf ')'
  | Pbinop (Add, a, b) -> bin "+" a b
  | Pbinop (Sub, a, b) -> bin "-" a b
  | Pbinop (Mul, a, b) -> bin "*" a b
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
  (* Header: VoxU (referenced by datatype fields, so first), then one
     declaration per line, then the [-vox-prelude] -- only when some VC
     applies a spec function: the prelude may reference another
     module's datatypes, which do not exist in this input.  Theorems
     follow, one per line. *)
  let want_prelude = List.exists vc_uses_spec_fun vcs in
  let first_line =
    1
    + (if needs_voxu then 1 else 0)
    + List.length !datatypes
    + (if want_prelude then prelude_lines () else 0)
  in
  let first_line = first_line + 1 in
  if needs_voxu then Buffer.add_string buf "opaque VoxU : Type\n";
  lean_datatype_decls buf;
  if want_prelude then Buffer.add_string buf (prelude ());
  (* Bound elaboration per theorem: a diverging [grind] must count as
     a verification failure, not hang the build.  (A wedged process
     outside elaboration remains out of scope, as for z3.)  Emitted
     after the prelude so a prelude may begin with [import], which
     Lean requires to be the first command in the file. *)
  Buffer.add_string buf "set_option maxHeartbeats 400000\n";
  List.iteri (fun i vc -> lean_theorem buf i vc) vcs;
  ( Buffer.contents buf,
    fun line ->
      (* An error on a header line maps to no VC (and a negative
         index would make [List.nth_opt] raise). *)
      if line < first_line then None else List.nth_opt vcs (line - first_line)
  )
;;

let run_lean vcs =
  match vcs with
  | [] -> ()
  | first :: _ ->
    let contents, vc_of_line = lean_file vcs in
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
          let contains_error l =
            let needle = "error:" in
            let n = String.length needle in
            let rec at i =
              i + n <= String.length l
              && (String.equal (String.sub l i n) needle || at (i + 1))
            in
            at 0
          in
          let ic = open_in out_file in
          let error_line = ref None in
          let msg = ref "" in
          let first_output = ref "" in
          (try
             while true do
               let l = input_line ic in
               if String.equal !first_output "" then first_output := l;
               match !error_line with
               | None ->
                 (match String.index_opt l ':' with
                  | Some _
                    when String.length l > String.length in_file
                         && String.equal
                              (String.sub l 0 (String.length in_file))
                              in_file
                         && contains_error l ->
                    let rest =
                      String.sub l (String.length in_file + 1)
                        (String.length l - String.length in_file - 1)
                    in
                    let is_error =
                      (* skip warnings: only "<file>:L:C: error: ..." *)
                      let needle = " error: " in
                      let rec find i =
                        if i + String.length needle > String.length l
                        then false
                        else
                          String.equal (String.sub l i (String.length needle)) needle
                          || find (i + 1)
                      in
                      find 0
                    in
                    (match String.index_opt rest ':' with
                     | Some i when is_error ->
                       (match int_of_string_opt (String.sub rest 0 i) with
                        | Some n ->
                          error_line := Some n;
                          msg := l
                        | None -> ())
                     | Some _ | None -> ())
                  | _ -> ())
               | Some _ -> ()
             done
           with
           | End_of_file -> ());
          close_in ic;
          match !error_line with
          | None ->
            (* No per-theorem diagnostic: the solver itself failed
               (missing binary, crash, bad flags).  Blaming a VC would
               hide the real cause. *)
            Location.raise_errorf ~loc:first.vc_loc
              "vox: verification failed (lean solver error, exit %d): %s"
              status
              (if String.equal !first_output ""
               then "<no output>"
               else !first_output)
          | Some line ->
            let vc =
              match vc_of_line line with
              | Some vc -> vc
              | None -> first
            in
            (* Strip the (nondeterministic) temp-file prefix from the
               message; keep from "error:" onward. *)
            let msg =
              let m = !msg in
              let needle = "error:" in
              let rec find i =
                if i + String.length needle > String.length m
                then m
                else if
                  String.equal (String.sub m i (String.length needle)) needle
                then String.sub m i (String.length m - i)
                else find (i + 1)
              in
              find 0
            in
            Location.raise_errorf ~loc:vc.vc_loc
              "vox: verification failed (lean).@ Goal: %s%s%s"
              (goal_for_error vc)
              (hyps_for_error vc)
              (if String.equal msg "" then "" else "\n(lean: " ^ msg ^ ")")
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
    match !Clflags.vox_solver with
    | "lean" -> run_lean (List.filter needs_proof all)
    | "z3" ->
      List.iter
        (fun vc ->
          if needs_proof vc
          then (
            match run_z3 (smt_script vc) with
            | Valid -> ()
            | Invalid ->
              Location.raise_errorf
                ~loc:vc.vc_loc
                "vox: verification failed.@ Unprovable goal: %s%s"
                (goal_for_error vc)
                (hyps_for_error vc)
            | Unknown reason ->
              Location.raise_errorf
                ~loc:vc.vc_loc
                "vox: verification failed (%s).@ Goal: %s%s"
                reason
                (goal_for_error vc)
                (hyps_for_error vc)))
        all
    | other ->
      (match all with
       | [] -> ()
       | vc :: _ ->
         Location.raise_errorf ~loc:vc.vc_loc
           "vox: unknown solver %S (expected \"z3\" or \"lean\")" other))
;;

(* Entry point: called on the final typedtree of an implementation. *)
(* VCs arise only from [refine_]/[assume_] expressions and [refine_] patterns, all of
   which carry a "vox." attribute. Programs without any are skipped entirely: the pass
   must not even inspect (and via [Ctype.expand_head], mutate) the types of unannotated
   programs. *)
let uses_vox (str : structure) =
  let found = ref false in
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
    }
  in
  it.structure it str;
  !found
;;

let walk_items (str : structure) ctx =
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_value (_rec_flag, vbs) ->
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
    let ctx = ref !toplevel_ctx in
    walk_items str ctx;
    (* Discharge before committing the phrase's facts: if verification
       fails, the toplevel backtracks the phrase, so its bindings never
       exist and their facts (e.g. a refuted contradictory refinement)
       must not be available to later phrases. *)
    discharge ();
    toplevel_ctx := !ctx)
;;
