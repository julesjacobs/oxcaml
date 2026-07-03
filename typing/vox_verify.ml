(* vox: verification-condition generation and discharge.

   Runs as a separate pass over the FINAL typedtree (the type checker emits no VCs; it
   backtracks internally). Walks the tree carrying a logical environment of facts; each
   [refine_] node yields the VC [facts |- p[v := name of e]]; [assume_] is reported as
   RUNTIME CHECKED (translcore compiles a check of the predicate) and
   [assume_unchecked_] as ASSUMED; neither goes to the solver. Facts come from the
   channels DESIGN.md enumerates -- binders (their refinements and contracts),
   selfification equations, unpacking, path facts from [if], dependent application,
   and match facts (positive and negative) -- deduplicated at emission.

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
  | S_tuple of dsort list
    (* an unlabeled tuple, modelled with one polymorphic product
       datatype per ARITY (VoxT2, VoxT3, ...) instantiated at the
       component sorts *)
  | S_iarray
    (* [int iarray], modelled by the built-in theory: an opaque sort
       VoxIA with Vox_ia_len/Vox_ia_get (Refinement.ia_len/ia_get)
       and the length-nonnegativity axiom, emitted when used *)
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

(* Module-level values named by this unit's VCs ([Pglobal]s): sort per
   path, with the import's .cmi refinement as a global fact (see
   [register_global]). *)
let globals : (string, Path.t * dsort) Hashtbl.t = Hashtbl.create 16
let global_facts : Refinement.pred list ref = ref []

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

(* Tuple ARITIES in use (types of names, tuple terms in predicates):
   each needs its product datatype (VoxT<n>) declared.  Tuples are
   structural, so unlike [datatypes] there is nothing to render per
   type -- the arity determines the declaration. *)
let tuple_arities : int list ref = ref []

let register_tuple_arity n =
  if not (List.mem n !tuple_arities)
  then tuple_arities := !tuple_arities @ [ n ]
;;

let register_pred_tuple_arities p =
  List.iter register_tuple_arity (Refinement.tuple_arities p)
;;

(* Reflected definitions ([total_] bindings) of the current module (or
   toplevel session), in definition order; emitted into the solver input
   between the [-vox-prelude] and the module's own embedded blocks, so
   those blocks may state lemmas about them (a prelude FILE precedes
   them: a definition may call an imported reflected function). *)
let spec_defs : Vox_reflect.spec_def list ref = ref []

(* Embedded solver blocks ([%%vox.lean ...]) of the module (or
   toplevel session) being verified, in source order: text (ending in
   a newline) and the block's location (solver errors inside a block
   are reported there).  See the collection functions below. *)
let embedded_blocks : (string * Location.t) list ref = ref []

(* Solver blocks imported from other units' .cmis ([%%vox.lean]
   in their interfaces): unit name and blocks, in dependency order
   (a unit's blocks after the units it imports).  Gathered from the
   persistent env at verification time; the definition travels with
   the defining module, so a client can never verify against a
   DIFFERENT version of a spec function used in an imported signature
   (the .cmi CRC forces re-verification when the spec changes). *)
let imported_specs : (string * Cmi_format.vox_spec_export) list ref =
  ref []

(* SSA versions for [let mutable] variables (flow-sensitive mutation).
   [mut_versions] maps each LIVE mutable binder to its current logical
   version -- a synthetic ident, so always in scope -- together with the
   binder's declared type; reads name the version and every write mints
   a fresh one.  A version's facts are eternal truths about a VALUE,
   never about the cell, so they may flow anywhere downstream on the
   control path; the walker threads contexts (and saves/restores this
   table around branches) so they flow nowhere else.  [mut_counts]
   numbers versions per binder for display and never rolls back. *)
let mut_versions : (Ident.t, Ident.t * Types.type_expr) Hashtbl.t =
  Hashtbl.create 16

let mut_counts : (Ident.t, int) Hashtbl.t = Hashtbl.create 16

(* Definitional equations [version = rhs-name], one per assignment.
   Unlike the declared-refinement instances (which are theorems proved
   under the assignment's path condition and stay path-scoped), these
   are Skolem-style definitions -- each version is defined once, as a
   function of strictly earlier names -- so adding them is a
   conservative extension in EVERY execution: an execution that never
   performs the assignment simply interprets the version by its
   equation.  They are pulled into each VC by relevance (emit_vc). *)
let mut_defs : Refinement.pred list ref = ref []

let reset () =
  vcs := [];
  Hashtbl.reset name_sorts;
  Hashtbl.reset synthetic_names;
  Hashtbl.reset mut_versions;
  Hashtbl.reset mut_counts;
  mut_defs := [];
  datatypes := [];
  registering := [];
  poisoned := [];
  tuple_arities := [];
  spec_defs := [];
  unknown_counter := 0;
  embedded_blocks := [];
  imported_specs := [];
  Hashtbl.reset globals;
  global_facts := []
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

(* An abstract type may declare that its LOGICAL REPRESENTATIVE is its
   value at a base sort ([@@vox.sort int]): values of the type are
   modelled as opaque Ints (or Bools) rather than at VoxU, so
   refinements can use them directly as the values they stand for --
   ghost types whose denotation IS the value (prophecies; refs
   denoting their contents).  TRUSTED: the declaring library asserts
   that every fact it issues about such values is true of that
   interpretation.  The attribute must appear on the declaration in
   both the interface and the implementation (sorts are computed
   per-compilation from the visible declaration). *)
let vox_sort_of_attribute (a : Parsetree.attribute) =
  if not (String.equal a.attr_name.txt "vox.sort")
  then None
  else (
    match a.attr_payload with
    | PStr
        [ { pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_ident { txt = Longident.Lident s; _ }; _ }, _)
          ; _
          }
        ] ->
      (match s with
       | "int" -> Some S_int
       | "bool" -> Some S_bool
       | _ ->
         Location.raise_errorf
           ~loc:a.attr_loc
           "vox: unknown vox.sort %S (expected \"int\" or \"bool\")"
           s)
    | _ ->
      Location.raise_errorf
        ~loc:a.attr_loc
        "vox: vox.sort takes a single sort name, e.g. [@@@@vox.sort int]")
;;

(* Eager validation: a malformed [@@vox.sort] is an error even when no
   value of the type ever reaches a VC (a typo on a ghost type must not
   be silent), and so is the attribute on a pure ALIAS, where it would
   be silently ignored (sorting expands aliases to their definition
   first).  Run wherever declarations pass by. *)
let validate_vox_sort_attributes ?(alias = false) (attrs : Parsetree.attributes) =
  List.iter
    (fun (a : Parsetree.attribute) ->
      match vox_sort_of_attribute a with
      | None -> ()
      | Some _ ->
        if alias
        then
          Location.raise_errorf
            ~loc:a.attr_loc
            "vox: vox.sort on a type alias has no effect (an alias expands \
             to its definition before sorting); put the attribute on the \
             definition")
    attrs
;;

let vox_sort_attribute env p =
  match Env.find_type p env with
  | exception Not_found -> None
  | decl -> List.find_map vox_sort_of_attribute decl.type_attributes
;;

(* The sort of the type at path [p], registering it as a datatype (with its
   field datatypes, recursively) on first sight. *)
let rec datatype_sort env p =
  if Path.same p Predef.path_int
  then S_int
  else if Path.same p Predef.path_bool
  then S_bool
  else (
    match vox_sort_attribute env p with
    | Some s -> s
    | None -> datatype_sort_unattributed env p)

and datatype_sort_unattributed env p =
  if List.exists (Path.same p) !poisoned
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
                ~loc:(Location.in_file !Location.input_name)
                "vox: two distinct types would share the solver-side name \
                 %s; rename one of them"
                (path_uname p))
          !datatypes;
        datatypes := !datatypes @ [ p, decl ];
        S_data p))

and dsort_of_type ?(visited = []) env ty =
  let ty = Ctype.vox_expand_head env ty in
  (* A -rectypes cycle can run through a tuple with no nominal type on
     the path; revisiting a node degrades to the uninterpreted sort
     (sound). *)
  if List.mem (get_id ty) visited
  then S_other
  else (
    let visited = get_id ty :: visited in
    match get_desc ty with
    | Tconstr (p, [], _) -> datatype_sort env p
    | Tconstr (p, [ elt ], _)
      when Path.same p Predef.path_iarray
           && (match get_desc (Ctype.vox_expand_head env elt) with
               | Tconstr (e, [], _) -> Path.same e Predef.path_int
               | _ -> false) -> S_iarray
    | Trefine (skel, _) -> dsort_of_type ~visited env skel
    | Ttuple comps
      when List.length comps >= 2
           && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
      register_tuple_arity (List.length comps);
      S_tuple (List.map (fun (_, t) -> dsort_of_type ~visited env t) comps)
    | _ -> S_other)
;;

let record_name env id ty = Hashtbl.replace name_sorts id (dsort_of_type env ty)

(* Register the datatypes of any constructor application in [p].  Called
   wherever a predicate enters the fact/goal stream; a path that fails to
   register (not a simple variant here, or mutually recursive) is caught at
   discharge time. *)
let register_pred_paths env p =
  List.iter (fun q -> ignore (datatype_sort env q)) (Refinement.constr_paths p);
  register_pred_tuple_arities p
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
  List.iter
    register_pred_tuple_arities
    (Vox_reflect.body_preds
       (Option.to_list d.Vox_reflect.sd_decreases)
       d.Vox_reflect.sd_body);
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
  match get_desc (Ctype.vox_expand_head env ty) with
  | Trefine (_, p) -> Some p
  | _ -> None
;;

(* The refinement of an arrow PARAMETER type (a contract, DESIGN.md),
   looking under the [Tpoly] wrapper arrow domains carry.  A genuinely
   polymorphic domain (non-empty univars) is NOT a contract -- typing
   leaves those rigid -- so the walker must not report one: it would
   emit obligations typing never stripped for. *)
let param_refinement env ty =
  match get_desc (Ctype.vox_expand_head env ty) with
  | Tpoly (t, []) -> refinement_of_type env t
  | Trefine (_, p) -> Some p
  | _ -> None
;;

let mut_read id =
  Option.map (fun (v, _) -> Refinement.Pvar v) (Hashtbl.find_opt mut_versions id)
;;

let mut_fresh env id ty =
  let k =
    match Hashtbl.find_opt mut_counts id with
    | Some k -> k + 1
    | None -> 0
  in
  Hashtbl.replace mut_counts id k;
  let name =
    if k = 0 then Ident.name id else Printf.sprintf "%s@%d" (Ident.name id) k
  in
  let v = Ident.create_local name in
  record_name env v ty;
  Hashtbl.replace synthetic_names v ();
  Hashtbl.replace mut_versions id (v, ty);
  v
;;

(* The declared refinement, instantiated at a fresh version: sound
   because rigid typing forced every write (and the initialization)
   through [refine_] at that type. *)
let mut_invariant env ty v =
  match refinement_of_type env ty with
  | Some p ->
    register_pred_paths env p;
    [ Refinement.subst_bound ~by:(Refinement.Pvar v) p ]
  | None -> []
;;

(* [m <- e] and initialization: the fresh version's definitional
   equation joins the global stream (sound everywhere); the declared
   refinement is returned for the PATH-SCOPED context. *)
let mut_assign env id ty ~rhs =
  let v = mut_fresh env id ty in
  mut_defs
  := Refinement.Pbinop (Refinement.Eq, Refinement.Pvar v, rhs) :: !mut_defs;
  mut_invariant env ty v
;;

(* Havoc: a fresh, unconstrained version (joins, loops, and constructs
   the walker does not model).  Only the declared refinement survives:
   it holds at every program point. *)
let mut_havoc env id =
  match Hashtbl.find_opt mut_versions id with
  | None -> []
  | Some (_, ty) ->
    let v = mut_fresh env id ty in
    mut_invariant env ty v
;;

let save_versions () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) mut_versions []

let restore_versions saved =
  Hashtbl.reset mut_versions;
  List.iter (fun (k, v) -> Hashtbl.replace mut_versions k v) saved
;;

let version_in snapshot id =
  List.find_map
    (fun (k, (v, _)) -> if Ident.same k id then Some v else None)
    snapshot
;;

(* The mutable variables (tracked at this point) that [e] assigns
   anywhere in its subtree.  Complete because closures cannot capture
   mutable variables: every mutation is a syntactic [Texp_setmutvar]. *)
let written_mutables (e : expression) =
  let acc = ref [] in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e' ->
          (match e'.exp_desc with
           | Texp_setmutvar ({ txt = id; _ }, _, _) ->
             if Hashtbl.mem mut_versions id
                && not (List.exists (Ident.same id) !acc)
             then acc := id :: !acc
           | _ -> ());
          Tast_iterator.default_iterator.expr sub e')
    }
  in
  it.expr it e;
  !acc
;;

let mut_havoc_written env e =
  List.concat_map (mut_havoc env) (written_mutables e)
;;

(* The tracked mutable variables [e] READS ([Texp_mutvar]) anywhere in
   its subtree; complete for the same reason as [written_mutables]. *)
let read_mutables (e : expression) =
  let acc = ref [] in
  let it =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e' ->
          (match e'.exp_desc with
           | Texp_mutvar { txt = id; _ } ->
             if Hashtbl.mem mut_versions id
                && not (List.exists (Ident.same id) !acc)
             then acc := id :: !acc
           | _ -> ());
          Tast_iterator.default_iterator.expr sub e')
    }
  in
  it.expr it e;
  !acc
;;

(* Havoc facts for one unordered CHILD (application arguments, let-and
   right-hand sides, generic traversal): only the subtree-written
   variables the child itself reads get a fresh version.  A child blind
   to a variable needs no name for it, and skipping the mint keeps
   version numbering readable.  Call with the version table already
   restored to the construct's entry state. *)
let sibling_havoc env ~written child =
  List.concat_map
    (mut_havoc env)
    (List.filter
       (fun id -> List.exists (Ident.same id) written)
       (read_mutables child))
;;

(* Loop invariants ([@vox.invariant p]): a FORMULA over program
   variables, living in the logical environment -- not a refinement
   type: it never travels and is never compared.  The elaborated
   template is instantiated at each boundary point by closing every
   mutable mention over the variable's current version (Thrust-style:
   the logic only ever sees stable names).  Discipline (the classical
   quadruple): ASSERT over the entry versions; havoc; ASSUME over the
   head versions; ASSERT over the body-exit versions at the back-edge;
   after the loop, the head assumption stands alongside the negated
   guard. *)
let loop_invariant (e : expression) =
  let all =
    List.filter_map
      (fun (a : Parsetree.attribute) ->
        if String.equal a.attr_name.txt "vox.invariant"
        then (
          match a.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (pred, _); _ } ] ->
            Some (pred, a.attr_loc)
          | _ ->
            Location.raise_errorf ~loc:a.attr_loc
              "vox: malformed [@vox.invariant] payload (expected a predicate)")
        else None)
      e.exp_attributes
  in
  match all with
  | [] -> None
  | (_, loc0) :: _ -> Some (List.map fst all, loc0)
;;

(* Close a formula template over the current versions of the mutable
   variables it mentions. *)
let close_over_versions p =
  Hashtbl.fold
    (fun id (v, _) p -> Refinement.subst_var id ~by:(Refinement.Pvar v) p)
    mut_versions
    p
;;

(* [ienv] is the environment the formula elaborates in: the loop
   expression's for a while loop, the BODY's for a for loop (where the
   index is bound). *)
let elab_loop_invariant ienv (e : expression) =
  match loop_invariant e with
  | None -> None
  | Some (preds, attr_loc) ->
    (* several [@vox.invariant] attributes conjoin *)
    let elab pred =
      let template, mentioned = Typetexp.elab_vox_invariant ienv pred in
      List.iter
        (fun id ->
          if not (Hashtbl.mem mut_versions id)
          then
            Location.raise_errorf ~loc:attr_loc
              "vox: the invariant mentions the mutable variable %s, which is \
               not tracked here (is it defined outside the enclosing \
               function?)"
              (Ident.name id))
        mentioned;
      register_pred_paths ienv template;
      template
    in
    let template =
      match List.map elab preds with
      | [] -> assert false
      | t :: ts -> List.fold_left (fun acc t' -> Refinement.Pand (acc, t')) t ts
    in
    Some (template, attr_loc)
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

(* The name a dependent argument was opened at by the type checker:
   [Vox_reflect.translate] is the typed twin of the surface translation
   [vox_open_dependent_arrow] substituted (the surface fragment is a
   subset of the typed one, and both key primitives and total_
   functions on what the identifier resolves to), so the walker's
   instantiation of the remaining contracts agrees with the types. *)
let stable_arg_name (a : expression) : Refinement.pred option =
  Vox_reflect.translate a
;;

(* Register a module-level value on first sight: its sort (for the
   solver declaration) and, if its scheme carries a refinement, the
   .cmi fact at [Pglobal p], pulled into exactly the VCs that mention
   the path.  The registry is the emit-time chokepoint: every channel
   that can put a [Pglobal] into a predicate (reflection of an ident,
   dependent substitution, an imported predicate) funnels through
   [emit_vc], which scans and registers.  Two paths to one value
   register separately (both facts true, equality not assumed). *)
let rec register_global env (p : Path.t) =
  let key = path_uname p in
  if not (Hashtbl.mem globals key)
  then (
    match Env.find_value p env with
    | vd ->
      let vd = Subst.Lazy.force_value_description vd in
      Hashtbl.replace globals key (p, dsort_of_type env vd.val_type);
      (match refinement_of_type env vd.val_type with
       | Some pr ->
         register_pred_paths env pr;
         let fact = Refinement.subst_bound ~by:(Refinement.Pglobal p) pr in
         List.iter (register_global env) (Refinement.free_globals fact);
         global_facts := fact :: !global_facts
       | None -> ())
    | exception Not_found ->
      (* Unresolvable here (e.g. a stale path): declare at the
         uninterpreted sort; no fact. *)
      Hashtbl.replace globals key (p, S_other))
;;

let rec name_of_expr env (e : expression) : Refinement.pred =
  match Vox_reflect.translate ~mutvar:mut_read e with
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
        | S_int | S_bool | S_tuple _ | S_iarray | S_other -> fresh_unknown env e)
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
        | S_int | S_bool | S_tuple _ | S_iarray | S_other -> fresh_unknown env e)
     | Texp_tuple (comps, _)
       when List.length comps >= 2
            && List.for_all (fun (lbl, _) -> Option.is_none lbl) comps ->
       (* An unlabeled tuple names its product term over the components'
          names ("constructors get the usual refinements"). *)
       register_tuple_arity (List.length comps);
       Refinement.Ptuple (List.map (fun (_, a) -> name_of_expr env a) comps)
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

(* Selfification: a let binder names its RHS's value, so a binding
   whose pattern is a single variable contributes [x = name(rhs)]
   whenever the RHS has a stable logical name (its reflection, a
   constructor term, an immutable field read) -- fresh unknowns are
   skipped as pure noise.  Sound because the binding IS the evaluation:
   if the RHS raised (division), [x] is never bound and the fact holds
   vacuously.  This makes the aliasing idiom implicit: [let s = l + r]
   carries [s = l + r], with no [refine_] in sight, and an unpack
   [let refine_ x = e] additionally remembers WHICH value it opened. *)
let self_fact env id (rhs : expression) =
  match name_of_expr env rhs with
  | Refinement.Pvar u when Hashtbl.mem synthetic_names u -> []
  | n -> [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, n) ]
;;

let binding_self_facts env (vb : value_binding) =
  match vb.vb_pat.pat_desc with
  | Tpat_var { id; _ } -> self_fact env id vb.vb_expr
  | _ -> []
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
  | Refinement.Ptuple _ | Refinement.Pproj _
  | Refinement.Pis _ | Refinement.Pquant _ | Refinement.Pglobal _ -> true
  | Refinement.Pbinop ((Refinement.Div | Refinement.Mod), _, _) -> true
  | Refinement.Pbound | Refinement.Pvar _ | Refinement.Pint _
  | Refinement.Pbool _ -> false
  | Refinement.Pbinop (_, a, b)
  | Refinement.Pand (a, b)
  | Refinement.Por (a, b)
  | Refinement.Pimp (a, b) -> pred_unreflectable a || pred_unreflectable b
  | Refinement.Pnot a -> pred_unreflectable a
;;

let emit_vc ~env ~loc ~ctx ~goal ~kind =
  (* Register every module-level value this VC mentions: its solver
     declaration (sort) and its .cmi refinement as a global fact --
     the single chokepoint for all channels that can produce a
     [Pglobal] (reflection, dependent substitution, imported
     predicates). *)
  List.iter
    (register_global env)
    (List.concat_map Refinement.free_globals (goal :: ctx.cfacts));
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
          involves %s, which the compiled check cannot evaluate \
          faithfully; use assume_unchecked_"
         Refinement.unreflectable_what;
     (* The compiled check compares machine words, which agrees with the
        logic only for int- and bool-sorted operands: other sorts are
        uninterpreted, and physical equality is stricter than logical
        equality (a coherent assumption could fail at run time). *)
     let int_or_bool id =
       match Hashtbl.find_opt name_sorts id with
       | Some (S_int | S_bool) -> true
       | Some (S_data _ | S_tuple _ | S_iarray | S_other) | None -> false
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
  (* Pull in the definitional equations reachable from the goal and
     facts (transitively through their right-hand sides); definitions
     mentioning out-of-scope program variables are dropped, which only
     weakens. *)
  let defs =
    let needed = Hashtbl.create 8 in
    let note p =
      List.iter
        (fun id -> Hashtbl.replace needed (Ident.unique_name id) ())
        (Refinement.free_vars p)
    in
    note goal;
    List.iter note facts;
    let rec grow acc remaining =
      let take, keep =
        List.partition
          (fun d ->
            match d with
            | Refinement.Pbinop (Refinement.Eq, Refinement.Pvar v, _) ->
              Hashtbl.mem needed (Ident.unique_name v)
            | _ -> false)
          remaining
      in
      if take = []
      then acc
      else (
        List.iter note take;
        grow (take @ acc) keep)
    in
    List.filter (pred_in_scope ctx) (grow [] !mut_defs)
  in
  let facts = facts @ defs in
  (* Global facts (the .cmi refinements of module-level values named in
     this VC) arrive by NEED: an import's fact appears exactly in the
     VCs that mention its name. *)
  let facts =
    let mentioned = Hashtbl.create 8 in
    let note p =
      List.iter
        (fun id -> Hashtbl.replace mentioned (Ident.unique_name id) ())
        (Refinement.free_vars p);
      List.iter
        (fun gp -> Hashtbl.replace mentioned (path_uname gp) ())
        (Refinement.free_globals p)
    in
    note goal;
    List.iter note facts;
    facts
    @ List.filter
        (fun g ->
          List.exists
            (fun id -> Hashtbl.mem mentioned (Ident.unique_name id))
            (Refinement.free_vars g)
          || List.exists
               (fun gp -> Hashtbl.mem mentioned (path_uname gp))
               (Refinement.free_globals g))
        !global_facts
  in
  (* Several fact channels can deliver the same fact (a binder fact and
     its selfification equation, say); keep the first occurrence.
     Quadratic, but hypothesis lists are small. *)
  let facts =
    List.fold_left
      (fun acc f -> if List.exists (Refinement.equal f) acc then acc else f :: acc)
      []
      facts
    |> List.rev
  in
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
(* [@@vox.sort] hygiene over an exported signature: malformed payloads
   are errors even when no value of the type ever reaches a VC (a typo
   on a ghost type must not be silent). *)
let rec validate_signature_sorts (sg : Types.signature) =
  List.iter
    (fun (item : Types.signature_item) ->
      match item with
      | Sig_type (_, decl, _, _) ->
        let alias =
          (match decl.type_kind with
           | Type_abstract _ -> true
           | _ -> false)
          && Option.is_some decl.type_manifest
        in
        validate_vox_sort_attributes ~alias decl.type_attributes
      | Sig_module (_, _, md, _, _) ->
        (match md.md_type with
         | Mty_signature sub -> validate_signature_sorts sub
         | _ -> ())
      | _ -> ())
    sg
;;

let check_signature (sg : Types.signature) =
  validate_signature_sorts sg;
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
  : type k. Env.t -> Refinement.pred -> k general_pattern -> Refinement.pred list
  =
  fun env subject pat ->
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
    | S_int | S_bool | S_tuple _ | S_iarray | S_other -> []
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
             , subject
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
       | S_int | S_bool | S_tuple _ | S_iarray | S_other -> []
       | S_data _ ->
         List.filter_map
           (fun (_, (lbl : Data_types.label_description), sub) ->
             match (sub : value general_pattern).pat_desc with
             | Tpat_var { id; _ } ->
               Some
                 (Refinement.Pbinop
                    ( Refinement.Eq
                    , Refinement.Pvar id
                    , Refinement.Pfield (path, lbl.lbl_name, subject)
                    ))
             | _ -> None)
           fields)
  in
  let tuple_facts (comps : (string option * value general_pattern) list) =
    (* [xi = proj_i sid] per VARIABLE sub-pattern, like records
       (per-component, so wildcards and deeper sub-patterns simply
       contribute nothing).  Labeled tuples are not modelled. *)
    if List.exists (fun (lbl, _) -> Option.is_some lbl) comps
    then []
    else begin
      let n = List.length comps in
      register_tuple_arity n;
      List.mapi (fun i (_, sub) -> i, sub) comps
      |> List.filter_map (fun (i, (sub : value general_pattern)) ->
        match sub.pat_desc with
        | Tpat_var { id; _ } ->
          Some
            (Refinement.Pbinop
               ( Refinement.Eq
               , Refinement.Pvar id
               , Refinement.Pproj (n, i, subject) ))
        | _ -> None)
    end
  in
  let value_facts (p : value general_pattern) =
    match p.pat_desc with
    | Tpat_construct (_, cstr, _, args, _) -> constructor_facts cstr args
    | Tpat_record (fields, _, _, _) -> record_facts fields
    | Tpat_tuple comps -> tuple_facts comps
    | Tpat_var { id; _ }
      when not (Refinement.equal (Refinement.Pvar id) subject) ->
      (* A variable pattern aliases the scrutinee: [match s with y ->]
         (and a [function y ->] case, whose scrutinee is [fc_param])
         learns [y = s]; [let refine_ x = m] (which desugars to a
         match) ties the binder to a mutable scrutinee's version.  The
         self-alias guard: [fc_param] IS the first variable case's
         ident (see [Typecore.name_cases]), and [x = x] is noise. *)
      [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject) ]
    | _ -> []
  in
  match pat.pat_desc with
  | Tpat_value p -> value_facts (p :> value general_pattern)
  | Tpat_construct (_, cstr, _, args, _) -> constructor_facts cstr args
  | Tpat_record (fields, _, _, _) -> record_facts fields
  | Tpat_tuple comps -> tuple_facts comps
  | Tpat_var { id; _ }
    when not (Refinement.equal (Refinement.Pvar id) subject) ->
    (* Bare value patterns (let bindings and [function]-case arms reach
       here unwrapped).  The self-alias guard: [fc_param] IS the first
       variable case's ident (see [Typecore.name_cases]), and [x = x]
       is noise. *)
    [ Refinement.Pbinop (Refinement.Eq, Refinement.Pvar id, subject) ]
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
  : type k. Env.t -> Refinement.pred -> k general_pattern -> Refinement.pred option
  =
  fun env subject pat ->
  let head_negation cstr args =
    let path = Data_types.cstr_res_type_path cstr in
    match datatype_sort env path with
    | S_int | S_bool | S_tuple _ | S_iarray | S_other -> None
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
                (path, cstr.Data_types.cstr_name, subject)))
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

(* The instantiated RESULT type of an application: walk the arrow
   spine from the function's type, substituting each dependent binder
   by its argument's stable name -- the same opening the application
   site performed at typing time. *)
let apply_result_type env funct (args : (_ * apply_arg) list) =
  let arrow_ty = ref funct.exp_type in
  List.iter
    (fun (_lbl, (arg : apply_arg)) ->
      match get_desc (Ctype.vox_expand_head env !arrow_ty) with
      | Tarrow ((_, _, _, binder), _dom, ret, _) ->
        (match arg, binder with
         | Arg (a, _), Some b ->
           (match stable_arg_name a with
            | Some by -> arrow_ty := Vox_dep.subst_binder b ~by ret
            | None -> arrow_ty := ret)
         | _ -> arrow_ty := ret)
      | _ -> ())
    args;
  !arrow_ty
;;

(* Whether a computation pattern is free of exception patterns: only
   then does matching it guarantee the scrutinee ran to completion. *)
let rec exceptionless (p : computation general_pattern) =
  match p.pat_desc with
  | Tpat_value _ -> true
  | Tpat_exception _ -> false
  | Tpat_or (a, b, _) -> exceptionless a && exceptionless b
;;

(* The single value arm of a match, when it has exactly one arm (a
   computation case wrapping a value pattern, as unpack and
   destructuring lets desugar to).  An arm containing an exception
   pattern does not qualify: it can be reached with the scrutinee
   interrupted between writes, so its state may not be threaded. *)
let single_arm
  : computation case list -> value case list -> computation case option
  =
  fun comp_cases val_cases ->
  match comp_cases, val_cases with
  | [ c ], [] when exceptionless c.c_lhs -> Some c
  | _ -> None
;;

(* Walk an expression under a logical context, collecting VCs.  Returns
   the context for the expression's CONTINUATION: mutable-variable
   assignments extend it with the fresh version's definitional equation
   (and declared-refinement instance), and joins extend it with join
   facts.  Everything is path-scoped -- facts proved under a branch's
   hypotheses never reach a sibling branch -- and the version table is
   saved and restored around branching so each branch names the state it
   actually sees. *)
let rec walk_expr _outer_env ctx (e : expression) : ctx =
  (* Use the node's OWN env, re-derived at every recursive call: an env
     threaded from the enclosing structure misses type declarations
     introduced by let-module (and friends) inside the expression, whose
     types would then silently sort at VoxU -- same bug class as the
     walk_items nested-module fix. *)
  let env = e.exp_env in
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
        (* An intro-marked APPLICATION re-proves the expected
           refinement of a value whose own instantiated result
           refinement is a fact: selfify that refinement at the node's
           name -- the inline unpack that [let q = f x in q] used to
           spell.  Sound: the value satisfies its type, and on any
           path where the goal matters the call has returned. *)
        let self_hyps =
          match e.exp_desc with
          | Texp_apply (funct, args, _, _, _) ->
            (match
               refinement_of_type env (apply_result_type env funct args)
             with
             | Some ps when not (Refinement.equal ps p) ->
               register_pred_paths env ps;
               [ Refinement.subst_bound ~by:n ps ]
             | _ -> [])
          | _ -> []
        in
        emit_vc
          ~env
          ~loc:e.exp_loc
          ~ctx:{ ctx with cfacts = self_hyps @ ctx.cfacts }
          ~goal:(Refinement.subst_bound ~by:n p)
          ~kind
      | None -> ())
   | None -> ());
  (* A [@vox.invariant] anywhere but on a loop would otherwise be
     SILENTLY unchecked -- the worst failure mode for a verification
     annotation. *)
  (match e.exp_desc, loop_invariant e with
   | (Texp_while _ | Texp_for _), _ | _, None -> ()
   | _, Some (_, attr_loc) ->
     Location.raise_errorf ~loc:attr_loc
       "vox: [@vox.invariant] is only supported on while and for loops");
  match e.exp_desc with
  | Texp_apply (funct, args, _, _, _) ->
    (* The function and its arguments evaluate in unspecified order
       (right-to-left in practice): as in the generic traversal, each
       child starts from the entry versions with everything this
       application writes havocked, and the continuation havocs it
       again.  For pure applications ([written] empty) this is
       identical to walking every child under the entry context. *)
    let saved = save_versions () in
    let written = written_mutables e in
    let child_ctx child =
      restore_versions saved;
      { ctx with cfacts = sibling_havoc env ~written child @ ctx.cfacts }
    in
    ignore (walk_expr env (child_ctx funct) funct : ctx);
    (* Contract obligations (parameters as preconditions): each
       argument for a refined parameter must satisfy the predicate at
       its logical name; an intro-form argument
       ([refine_]/[assume_]/[assume_unchecked_]) carries its own
       obligation instead (the explicit-cast spelling).  The dependent
       binder is substituted by the argument's translation (a variable,
       literal, or pure reflected expression -- enforced at typing
       time) as the spine is walked, mirroring the application-site
       opening.  The obligation is emitted under the argument's child
       context, whose version state is what [name_of_expr] reads. *)
    let arrow_ty = ref funct.exp_type in
    List.iter
      (fun (_lbl, (arg : apply_arg)) ->
        let arg_expr =
          match arg with
          | Arg (a, _) -> Some a
          | Omitted _ -> None
        in
        let actx =
          match arg_expr with
          | Some a ->
            let actx = child_ctx a in
            ignore (walk_expr env actx a : ctx);
            actx
          | None -> ctx
        in
        match get_desc (Ctype.vox_expand_head env !arrow_ty) with
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
                  ~env
                  ~loc:a.exp_loc
                  ~ctx:actx
                  ~goal:(Refinement.subst_bound ~by:(name_of_expr env a) p)
                  ~kind:Prove
              | _ -> ());
             (match binder, stable_arg_name a with
              | Some b, Some by ->
                arrow_ty := Vox_dep.subst_binder b ~by ret
              | _ -> arrow_ty := ret)
           | None -> arrow_ty := ret)
        | _ -> ())
      args;
    restore_versions saved;
    { ctx with cfacts = mut_havoc_written env e @ ctx.cfacts }
  | Texp_let (rec_flag, [ vb ], body) ->
    (* Reflected definitions are global; a local one could capture
       enclosing variables (translate_def's closedness check would also
       catch that, but the restriction is the honest one). *)
    reject_local_reflect vb;
    let ctx0 = walk_expr env ctx vb.vb_expr in
    let ctx' = extend_pat env ctx0 vb.vb_pat in
    (* A destructuring let of a variable gets the same facts a match
       case would: [let { x; y } = r in ...].  A let of a MUTABLE
       variable additionally pins its current value to the immutable
       binder ([let x = m]) -- the only way to name a mutable
       variable's value, since mutable stamps may not appear in
       refinements or dependent applications.  A plain [let y = x] of
       an immutable variable is skipped: its alias fact is the SELF
       fact below (the variable arm of [match_facts] would duplicate
       it). *)
    let ctx' =
      match vb.vb_expr.exp_desc, vb.vb_pat.pat_desc with
      | Texp_ident _, Tpat_var _ -> ctx'
      | Texp_ident { path = Path.Pident id; _ }, _ ->
        { ctx' with
          cfacts =
            match_facts env (Refinement.Pvar id) vb.vb_pat @ ctx'.cfacts
        }
      | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ }, _ ->
        { ctx' with
          cfacts =
            match_facts env (Refinement.Pglobal p) vb.vb_pat @ ctx'.cfacts
        }
      | Texp_mutvar { txt = mid; _ }, _ ->
        (match Hashtbl.find_opt mut_versions mid with
         | Some (v, _) ->
           (* [match_facts] ties a variable pattern to the version
              directly and destructures records/constructors through
              it. *)
           { ctx' with
             cfacts =
               match_facts env (Refinement.Pvar v) vb.vb_pat @ ctx'.cfacts
           }
         | None -> ctx')
      | _, _ -> ctx'
    in
    (* Selfification (no self fact for a RECURSIVE binding: a cyclic
       constructor equation is unsatisfiable in the datatype theory). *)
    let ctx' =
      match rec_flag with
      | Recursive -> ctx'
      | Nonrecursive ->
        { ctx' with cfacts = binding_self_facts env vb @ ctx'.cfacts }
    in
    walk_expr env ctx' body
  | Texp_let (rec_flag, vbs, body) ->
    List.iter reject_local_reflect vbs;
    (* [let .. and ..]: sibling evaluation order is unspecified, so each
       right-hand side walks under the ENTRY context and every mutable
       variable any of them writes is havocked. *)
    let saved = save_versions () in
    let written = List.concat_map (fun vb -> written_mutables vb.vb_expr) vbs in
    List.iter
      (fun vb ->
        restore_versions saved;
        let hfacts = sibling_havoc env ~written vb.vb_expr in
        ignore
          (walk_expr env { ctx with cfacts = hfacts @ ctx.cfacts } vb.vb_expr
            : ctx))
      vbs;
    restore_versions saved;
    let havoc = List.concat_map (mut_havoc env) written in
    let ctx' = List.fold_left (fun ctx vb -> extend_pat env ctx vb.vb_pat) ctx vbs in
    let ctx' = { ctx' with cfacts = havoc @ ctx'.cfacts } in
    let ctx' =
      List.fold_left
        (fun ctx vb ->
          match vb.vb_expr.exp_desc, vb.vb_pat.pat_desc with
          | Texp_ident _, Tpat_var _ -> ctx
          | Texp_ident { path = Path.Pident id; _ }, _ ->
            { ctx with
              cfacts =
                match_facts env (Refinement.Pvar id) vb.vb_pat @ ctx.cfacts
            }
          | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ }, _ ->
            { ctx with
              cfacts =
                match_facts env (Refinement.Pglobal p) vb.vb_pat @ ctx.cfacts
            }
          | _ -> ctx)
        ctx'
        vbs
    in
    let ctx' =
      (* RECURSIVE bindings contribute no self fact: a cyclic
         constructor equation ([let rec ones = 1 :: ones]) is
         unsatisfiable in the solver's well-founded datatype theory,
         which would make the hypotheses inconsistent.  A group that
         writes mutable variables contributes none either: sibling
         order makes its RHS names unstable. *)
      match rec_flag with
      | Nonrecursive when written = [] ->
        List.fold_left
          (fun ctx vb ->
            { ctx with cfacts = binding_self_facts env vb @ ctx.cfacts })
          ctx'
          vbs
      | Recursive | Nonrecursive -> ctx'
    in
    walk_expr env ctx' body
  | Texp_letmutable (vb, body) ->
    let ctx0 = walk_expr env ctx vb.vb_expr in
    backstop_pat ctx0 vb.vb_pat;
    (match vb.vb_pat.pat_desc with
     | Tpat_var { id; _ } ->
       let ty = vb.vb_pat.pat_type in
       let rhs = name_of_expr env vb.vb_expr in
       let facts = mut_assign env id ty ~rhs in
       let out = walk_expr env { ctx0 with cfacts = facts @ ctx0.cfacts } body in
       (* the binder's scope ends; its versions (synthetic) live on *)
       Hashtbl.remove mut_versions id;
       out
     | _ ->
       (* the extension only allows single-variable patterns; stay
          conservative if that ever changes *)
       walk_expr env ctx0 body)
  | Texp_setmutvar ({ txt = id; _ }, _, rhs) ->
    let ctx0 = walk_expr env ctx rhs in
    (match Hashtbl.find_opt mut_versions id with
     | Some (_, ty) ->
       (* name the right-hand side BEFORE minting: its reads use the
          version being replaced *)
       let rhs_name = name_of_expr env rhs in
       { ctx0 with cfacts = mut_assign env id ty ~rhs:rhs_name @ ctx0.cfacts }
     | None -> ctx0)
  | Texp_mutvar _ -> ctx
  | Texp_sequence (e1, _, e2) ->
    let ctx1 = walk_expr env ctx e1 in
    walk_expr env ctx1 e2
  | Texp_match (scrut, _sort, comp_cases, val_cases, _partial) ->
    let saved_pre = save_versions () in
    let ctx0 = walk_expr env ctx scrut in
    let scrut_id =
      match scrut.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
      | Texp_ident { path = (Path.Pdot _ | Path.Papply _) as p; _ } ->
        (* A module-level scrutinee matches like a local one: its path
           name receives the match facts (loads are pure, so receiving
           facts stays vacuously sound for exception and effect
           arms). *)
        Some (Refinement.Pglobal p)
      | Texp_mutvar { txt = id; _ } ->
        (* the version pins the value read by the match *)
        Option.map
          (fun (v, _) -> Refinement.Pvar v)
          (Hashtbl.find_opt mut_versions id)
      | _ -> None
    in
    (match single_arm comp_cases val_cases with
     | Some c ->
       (* A single-arm match (unpacks [let refine_ x = e] and
          destructuring lets desugar to these) is straight-line code:
          the arm's out-context IS the continuation's state -- thread
          it, versions included, instead of joining.  Sound also for a
          partial single-arm match: on pattern failure the continuation
          is unreachable. *)
       let ctx' = extend_pat ~scrut:scrut.exp_type env ctx0 c.c_lhs in
       let ctx' =
         match scrut_id with
         | Some sid ->
           { ctx' with cfacts = match_facts env sid c.c_lhs @ ctx'.cfacts }
         | None -> ctx'
       in
       let gctx =
         match c.c_guard with
         | None -> ctx'
         | Some g -> walk_expr env ctx' g
       in
       walk_expr env gctx c.c_rhs
     | None ->
    let saved = save_versions () in
    let do_case : type k. interrupted:bool -> Refinement.pred list -> k case -> unit =
      fun ~interrupted negs c ->
      let base =
        if interrupted
        then (
          (* the arm can be reached with [scrut] interrupted between
             writes: neither its threaded versions nor its facts are
             valid here.  Start from the pre-scrutinee state, with
             everything the scrutinee writes havocked. *)
          restore_versions saved_pre;
          { ctx with
            cfacts =
              List.concat_map (mut_havoc env) (written_mutables scrut)
              @ ctx.cfacts
          })
        else (
          restore_versions saved;
          ctx0)
      in
      let ctx' =
        if interrupted
        then extend_pat env base c.c_lhs
        else extend_pat ~scrut:scrut.exp_type env base c.c_lhs
      in
      let ctx' =
        match scrut_id with
        | Some sid when not interrupted ->
          { ctx' with
            cfacts = match_facts env sid c.c_lhs @ negs @ ctx'.cfacts
          }
        | _ -> ctx'
      in
      let gctx =
        match c.c_guard with
        | None -> ctx'
        | Some g -> walk_expr env ctx' g
      in
      ignore (walk_expr env gctx c.c_rhs : ctx)
    in
    (* Arms additionally see the negations of the guard-free simple
       arms ABOVE them.  All ordinary arms -- value and exception, in
       source order -- arrive as computation cases (value patterns
       wrapped in [Tpat_value]); [val_cases] holds effect-handler arms.
       Exception and effect arms never contribute a negation (their
       patterns are not simple-variant constructors: exception and
       effect types are open), and they are INTERRUPTED arms: control
       reaches them with the scrutinee stopped between writes, so they
       receive the pre-scrutinee state (writes havocked) rather than
       the scrutinee's threaded facts and versions. *)
    let run_cases : type k. (k general_pattern -> bool) -> k case list -> unit =
      fun is_interrupted cases ->
      ignore
        (List.fold_left
           (fun negs c ->
             do_case ~interrupted:(is_interrupted c.c_lhs) negs c;
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
    run_cases (fun p -> not (exceptionless p)) comp_cases;
    run_cases (fun _ -> true) val_cases;
    restore_versions saved;
    (* havoc-join across arms in v1: written variables get a fresh
       version, keeping only the declared refinement.  When some arm
       is interrupted, the continuation can be reached without the
       scrutinee having completed, so its facts may not be kept
       either. *)
    let base =
      if List.exists (fun c -> not (exceptionless c.c_lhs)) comp_cases
         || val_cases <> []
      then ctx
      else ctx0
    in
    { base with cfacts = mut_havoc_written env e @ base.cfacts })
  | Texp_ifthenelse (cond, e_then, e_else) ->
    let ctx0 = walk_expr env ctx cond in
    (* The path fact is the condition's logic translation when it has
       one (a variable, or a translatable int/bool expression);
       untranslatable conditions contribute nothing.  Translatable
       implies pure, so the versions its reads name are stable. *)
    let cond_fact = Vox_reflect.translate ~mutvar:mut_read cond in
    Option.iter (register_pred_paths env) cond_fact;
    let with_fact f ctx =
      match cond_fact with
      | None -> ctx
      | Some c -> { ctx with cfacts = f c :: ctx.cfacts }
    in
    let saved = save_versions () in
    ignore (walk_expr env (with_fact (fun c -> c) ctx0) e_then : ctx);
    let vers_then = save_versions () in
    restore_versions saved;
    (match e_else with
     | Some e2 ->
       ignore (walk_expr env (with_fact (fun c -> Refinement.Pnot c) ctx0) e2 : ctx)
     | None -> ());
    let vers_else = save_versions () in
    restore_versions saved;
    (* Conditional join: a variable written by either branch gets a
       fresh version equated with the surviving branch's version under
       the reflected condition (havoc when the condition did not
       reflect). *)
    let join_facts =
      List.concat_map
        (fun (id, (v0, ty)) ->
          let vt = Option.value (version_in vers_then id) ~default:v0 in
          let ve = Option.value (version_in vers_else id) ~default:v0 in
          if Ident.same vt v0 && Ident.same ve v0
          then []
          else (
            let vj = mut_fresh env id ty in
            let inv = mut_invariant env ty vj in
            match cond_fact with
            | Some c ->
              Refinement.Por
                ( Refinement.Pand
                    ( c
                    , Refinement.Pbinop
                        (Refinement.Eq, Refinement.Pvar vj, Refinement.Pvar vt)
                    )
                , Refinement.Pand
                    ( Refinement.Pnot c
                    , Refinement.Pbinop
                        (Refinement.Eq, Refinement.Pvar vj, Refinement.Pvar ve)
                    ) )
              :: inv
            | None -> inv))
        saved
    in
    { ctx0 with cfacts = join_facts @ ctx0.cfacts }
  | Texp_while { wh_cond; wh_body; _ } ->
    (* Head state: havoc everything the loop writes; head versions
       denote any iteration's entry, and the declared refinements
       re-attach (every write re-proved them).  A [@vox.invariant]
       formula additionally follows the classical quadruple: ASSERTED
       over the entry versions, ASSUMED over the head versions, ASSERTED
       over the body-exit versions at the back-edge; after the loop the
       head assumption stands with the negated guard.  The body walks
       under the reflected condition; normal exit happens at the test,
       so the continuation sees the head state plus its negation. *)
    let inv = elab_loop_invariant e.exp_env e in
    (match inv with
     | Some (template, attr_loc) ->
       (* entry: the first iteration's head state is the current one *)
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx
         ~goal:(close_over_versions template)
         ~kind:Prove
     | None -> ());
    let head = mut_havoc_written env e in
    let head =
      match inv with
      | Some (template, _) -> close_over_versions template :: head
      | None -> head
    in
    let hctx = { ctx with cfacts = head @ ctx.cfacts } in
    let cctx = walk_expr env hctx wh_cond in
    let cond_fact = Vox_reflect.translate ~mutvar:mut_read wh_cond in
    let saved = save_versions () in
    let bctx =
      match cond_fact with
      | Some c -> { cctx with cfacts = c :: cctx.cfacts }
      | None -> cctx
    in
    let bctx_out = walk_expr env bctx wh_body in
    (match inv with
     | Some (template, attr_loc) ->
       (* back-edge: the next iteration's head state is the body's exit
          state *)
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:bctx_out
         ~goal:(close_over_versions template)
         ~kind:Prove
     | None -> ());
    restore_versions saved;
    (match cond_fact with
     | Some c -> { cctx with cfacts = Refinement.Pnot c :: cctx.cfacts }
     | None -> cctx)
  | Texp_for { for_id; for_from; for_to; for_dir; for_body; _ } ->
    let c0 = walk_expr env ctx for_from in
    let c1 = walk_expr env c0 for_to in
    (* Bounds are evaluated once, before any body write: NAME them
       (their reflection when translatable, a fresh unknown otherwise)
       before havocking.  One name per bound serves the head bounds and
       the entry/post-loop index instances alike, so even an opaque
       bound yields a consistent quadruple. *)
    let from_n = name_of_expr env for_from in
    let to_n = name_of_expr env for_to in
    record_name env for_id for_from.exp_type;
    (* The invariant elaborates in the BODY's environment, where the
       index is bound.  An index mention makes the quadruple
       index-aware: the entry assertion instantiates the index at the
       FIRST value, the back-edge assertion at the NEXT value (what it
       establishes is the next iteration's head state), and after the
       loop the head assumption stands at the one-past-the-end value
       when the loop ran -- at the first value otherwise (the entry
       assertion, over unchanged variables). *)
    let inv = elab_loop_invariant for_body.exp_env e in
    let mentions_index =
      match inv with
      | Some (template, _) ->
        List.exists (Ident.same for_id) (Refinement.free_vars template)
      | None -> false
    in
    let step p =
      let op =
        match for_dir with
        | Upto -> Refinement.Add
        | Downto -> Refinement.Sub
      in
      Refinement.Pbinop (op, p, Refinement.Pint 1)
    in
    let at_index by template =
      if mentions_index
      then (
        match by with
        | `First -> Refinement.subst_var for_id ~by:from_n template
        | `Past -> Refinement.subst_var for_id ~by:(step to_n) template
        | `Next ->
          Refinement.subst_var for_id ~by:(step (Refinement.Pvar for_id)) template)
      else template
    in
    (match inv with
     | Some (template, attr_loc) ->
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:c1
         ~goal:(close_over_versions (at_index `First template))
         ~kind:Prove
     | None -> ());
    let head_havoc = mut_havoc_written env e in
    let head_inv =
      match inv with
      | Some (template, _) -> [ close_over_versions template ]
      | None -> []
    in
    (* The post-loop instance of the invariant: over the head (havoc)
       versions, which also denote the final state.  With an index
       mention it splits on whether the loop ran; the empty case keeps
       the entry instance, sound because nothing was written. *)
    let post_inv =
      match inv with
      | None -> []
      | Some (template, _) ->
        if not mentions_index
        then head_inv
        else (
          let ran, empty =
            match for_dir with
            | Upto ->
              ( Refinement.Pbinop (Refinement.Le, from_n, to_n)
              , Refinement.Pbinop (Refinement.Gt, from_n, to_n) )
            | Downto ->
              ( Refinement.Pbinop (Refinement.Ge, from_n, to_n)
              , Refinement.Pbinop (Refinement.Lt, from_n, to_n) )
          in
          [ Refinement.Por
              ( Refinement.Pand
                  (empty, close_over_versions (at_index `First template))
              , Refinement.Pand
                  (ran, close_over_versions (at_index `Past template)) )
          ])
    in
    let bounds =
      let lo, hi =
        match for_dir with
        | Upto -> from_n, to_n
        | Downto -> to_n, from_n
      in
      [ Refinement.Pbinop (Refinement.Le, lo, Refinement.Pvar for_id)
      ; Refinement.Pbinop (Refinement.Le, Refinement.Pvar for_id, hi)
      ]
    in
    let bctx =
      { cfacts = bounds @ head_inv @ head_havoc @ c1.cfacts
      ; cscope = for_id :: c1.cscope
      }
    in
    let saved = save_versions () in
    let bctx_out = walk_expr env bctx for_body in
    (match inv with
     | Some (template, attr_loc) ->
       emit_vc
         ~env
         ~loc:attr_loc
         ~ctx:bctx_out
         ~goal:(close_over_versions (at_index `Next template))
         ~kind:Prove
     | None -> ());
    restore_versions saved;
    { c1 with cfacts = post_inv @ head_havoc @ c1.cfacts }
  | Texp_function { params; body; _ } ->
    (* A function body runs at call time: outer mutable variables are
       not live inside it (closures cannot capture them), so suspend
       the version table -- reads cannot occur, and invariants inside
       the body mentioning outer mutables are rejected by the liveness
       check rather than silently mis-instantiated. *)
    let suspended = save_versions () in
    Hashtbl.reset mut_versions;
    Fun.protect ~finally:(fun () -> restore_versions suspended)
    @@ fun () ->
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
              ignore (walk_expr env ctx default : ctx);
              pat, true
          in
          let ctx = extend_pat env ctx pat in
          match get_desc (Ctype.vox_expand_head env !arrow_ty) with
          | Tarrow ((_, _, _, binder), dom, ret, _) ->
            let id_opt =
              match pat.pat_desc with
              | Tpat_var { id; _ } -> Some id
              | _ -> None
            in
            let ctx =
              (* The dedup guard keys on the BINDERS' types, not the
                 pattern's: a refined pattern annotation keeps the
                 refined [pat_type] (that is what flows to the arrow)
                 while binding its variable at the skeleton, and its
                 fact must come from here; only a binder that itself
                 carries the refined type (inference-refined
                 parameters) contributes through [binder_facts]
                 instead. *)
              match param_refinement env dom with
              | Some p
                when (not is_default)
                     && List.for_all
                          (fun (_, _, ty, _, _) ->
                            Option.is_none (refinement_of_type env ty))
                          (pat_bound_idents_full pat) ->
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
     | Tfunction_body e -> ignore (walk_expr env ctx' e : ctx)
     | Tfunction_cases { fc_cases; fc_param; _ } ->
       (* The cases consume one more arrow, whose parameter is
          [fc_param]: a refined domain contributes its contract at
          [fc_param]'s stamp (the patterns were typed at the skeleton,
          like the other parameter spellings), and the cases are a
          match on [fc_param] -- they get match facts and the
          negations of earlier guard-free simple arms, exactly as
          [Texp_match] on a variable scrutinee. *)
       let ctx' =
         match get_desc (Ctype.vox_expand_head env !arrow_ty) with
         | Tarrow (_, dom, _, _) ->
           record_name env fc_param dom;
           (* [fc_param] is compiler-introduced: like the unnamed-param
              synthetics, it is always in scope for the cases. *)
           Hashtbl.replace synthetic_names fc_param ();
           (match param_refinement env dom with
            | Some p ->
              register_pred_paths env p;
              { ctx' with
                cfacts =
                  Refinement.subst_bound ~by:(Refinement.Pvar fc_param) p
                  :: ctx'.cfacts
              }
            | None -> ctx')
         | _ -> ctx'
       in
       ignore
         (List.fold_left
            (fun negs c ->
              let ctx'' = extend_pat env ctx' c.c_lhs in
              let ctx'' =
                { ctx'' with
                  cfacts =
                    match_facts env (Refinement.Pvar fc_param) c.c_lhs
                    @ negs
                    @ ctx''.cfacts
                }
              in
              let gctx =
                match c.c_guard with
                | None -> ctx''
                | Some g -> walk_expr env ctx'' g
              in
              ignore (walk_expr env gctx c.c_rhs : ctx);
              match c.c_guard with
              | None ->
                (match pattern_negation env (Refinement.Pvar fc_param) c.c_lhs with
                 | Some n -> negs @ [ n ]
                 | None -> negs)
              | Some _ -> negs)
            []
            fc_cases
           : Refinement.pred list));
    (* a function body runs at call time, not here: the continuation
       keeps the entry state (closures cannot capture mutable
       variables, so the body cannot write any variable we track) *)
    ctx
  | Texp_try (tried, cases, eff_cases) ->
    (* [tried] walks as straight-line code for its own VCs, but a
       handler arm runs with it interrupted between writes: arms
       receive the pre-try state with everything [tried] writes
       havocked (like the exception arms of a match), and the
       continuation -- reachable through either path -- keeps the
       entry facts plus the havoc-join. *)
    let saved = save_versions () in
    ignore (walk_expr env ctx tried : ctx);
    restore_versions saved;
    let hctx =
      { ctx with
        cfacts =
          List.concat_map (mut_havoc env) (written_mutables tried)
          @ ctx.cfacts
      }
    in
    let hsaved = save_versions () in
    let do_handler (c : value case) =
      restore_versions hsaved;
      let ctx' = extend_pat env hctx c.c_lhs in
      let gctx =
        match c.c_guard with
        | None -> ctx'
        | Some g -> walk_expr env ctx' g
      in
      ignore (walk_expr env gctx c.c_rhs : ctx)
    in
    List.iter do_handler cases;
    List.iter do_handler eff_cases;
    restore_versions saved;
    { ctx with cfacts = mut_havoc_written env e @ ctx.cfacts }
  | _ ->
    (* Generic traversal of children under the same context.  Patterns
       reached this way belong to constructs the walker does not model
       (letops, local module structures, ...); they are escape-checked
       but contribute no facts.  Children may evaluate in ANY order
       (arguments right-to-left in practice), so a child may neither
       see a sibling's threaded version nor keep an entry version a
       sibling may overwrite first: each child starts from the entry
       versions with everything this subtree writes havocked, and the
       continuation havocs it again. *)
    let saved = save_versions () in
    let written = written_mutables e in
    let it =
      { Tast_iterator.default_iterator with
        expr =
          (fun _ e' ->
            restore_versions saved;
            let hfacts = sibling_havoc env ~written e' in
            ignore
              (walk_expr env { ctx with cfacts = hfacts @ ctx.cfacts } e' : ctx))
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
    Tast_iterator.default_iterator.expr it e;
    restore_versions saved;
    let havoc = mut_havoc_written env e in
    if havoc = [] then ctx else { ctx with cfacts = havoc @ ctx.cfacts }
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


(* The stable name of the product datatype for tuple arity [n]: the
   same in every unit, like [path_uname]s, so clients deduplicate
   imported declarations by name. *)
let tuple_uname n = "VoxT" ^ Int.to_string n

(* Does rendering this sort mention VoxU -- directly, inside a tuple
   instantiation, or as the degraded rendering of an unregistered
   datatype? *)
let rec sort_needs_voxu = function
  | S_other -> true
  | S_int | S_bool | S_iarray -> false
  | S_tuple comps -> List.exists sort_needs_voxu comps
  | S_data p -> find_datatype p = None
;;

(* Same question for the built-in iarray theory (VoxIA and its
   operations), which is emitted only when something uses it. *)
let rec sort_needs_iarray = function
  | S_iarray -> true
  | S_int | S_bool | S_other -> false
  | S_tuple comps -> List.exists sort_needs_iarray comps
  | S_data _ -> false
;;

(* The built-in iarray theory, emitted (right after VoxU) when
   anything in the input uses it: an S_iarray-sorted name, a datatype
   field at VoxIA, or a predicate applying the reserved operations.
   (An IMPORTED datatype decl referencing VoxIA in a module with no
   own iarray use is not detected -- the solver's unknown-identifier
   error fails closed there.)  [get] is total in the logic, like
   division; length nonnegativity is the theory's one axiom,
   pattern-registered so grind instantiates it at every [len] term. *)
let lean_iarray_theory =
  "opaque VoxIA : Type\n\
   opaque Vox_ia_len : VoxIA -> Int\n\
   opaque Vox_ia_get : VoxIA -> Int -> Int\n\
   axiom Vox_ia_len_nonneg (a : VoxIA) : 0 <= Vox_ia_len a\n\
   grind_pattern Vox_ia_len_nonneg => Vox_ia_len a\n"
;;

let datatype_field_needs_iarray () =
  List.exists
    (fun (_, decl) ->
      match decl with
      | Dt_variant constrs ->
        List.exists
          (fun (_, fields) -> List.exists sort_needs_iarray fields)
          constrs
      | Dt_record fields ->
        List.exists (fun (_, fs) -> sort_needs_iarray fs) fields)
    !datatypes
;;

let datatype_field_needs_voxu () =
  List.exists
    (fun (_, decl) ->
      match decl with
      | Dt_variant constrs ->
        List.exists
          (fun (_, fields) -> List.exists sort_needs_voxu fields)
          constrs
      | Dt_record fields -> List.exists (fun (_, fs) -> sort_needs_voxu fs) fields)
    !datatypes
;;

let free_vars_of_vc vc = List.concat_map Refinement.free_vars (vc.vc_goal :: vc.vc_facts)

(* Embedded solver blocks: [%%vox.lean {lean|...|lean}] structure
   items carry solver-side text directly in the OCaml source.  They
   are not "preludes": reflected definitions precede them, so a block
   may state lemmas about the module's own total_ functions.  Blocks
   travel: an .mli's blocks -- and an mli-less unit's -- ride the
   .cmi's spec export to every client. *)

type vox_block_kind =
  | Not_a_block
  | Block
  | Bad_backend of string

let vox_block_of_extension txt =
  if String.equal txt "vox.lean" then Block
  else if String.length txt >= 4 && String.equal (String.sub txt 0 4) "vox."
  then
    (* Claim the whole vox.* item-extension namespace, so a misspelled
       block gets a vox error rather than "uninterpreted extension". *)
    Bad_backend txt
  else Not_a_block
;;

(* Whether Typemod should claim this extension item (including
   misspelled backends, so they get the vox error, not "uninterpreted
   extension"). *)
let is_vox_block_name txt =
  match vox_block_of_extension txt with
  | Block | Bad_backend _ -> true
  | Not_a_block -> false
;;

(* Validates and extracts the text of a [%%vox.lean] payload; used
   by Typemod (to accept the item) and by the collection below. *)
let vox_block_text (({txt; loc}, payload) : Parsetree.extension) =
  match vox_block_of_extension txt with
  | Not_a_block -> None
  | Bad_backend b ->
    Location.raise_errorf ~loc
      "vox: unknown block extension %S (expected \"vox.lean\")" b
  | Block ->
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

let collect_blocks (str : structure) =
  List.filter_map
    (fun item ->
      match item.str_desc with
      | Tstr_attribute ({attr_name = {txt; _}; attr_payload; attr_loc} : attribute)
        when is_vox_block_name txt ->
        (match vox_block_text ({txt; loc = attr_loc}, attr_payload) with
         | Some s -> Some (normalize_block s, attr_loc)
         | None -> None)
      | _ -> None)
    str.str_items
;;

(* Blocks of an INTERFACE ([%%vox.lean] in an .mli): collected by
   the .mli's compilation and saved into the .cmi (see Typemod), so
   they reach every client -- and the unit's own implementation, whose
   verification reads the interface's .cmi like any other import. *)
let collect_blocks_sig (sg : Typedtree.signature) =
  List.filter_map
    (fun item ->
      match item.sig_desc with
      | Tsig_attribute ({attr_name = {txt; _}; attr_payload; attr_loc}
                        : attribute)
        when is_vox_block_name txt ->
        (match
           vox_block_text ({txt; loc = attr_loc}, attr_payload)
         with
         | Some s -> Some (normalize_block s)
         | None -> None)
      | _ -> None)
    sg.sig_items
;;

(* Imported spec exports in dependency order (a unit's spec after the
   units it imports; name order breaks ties, for determinism). *)
let gather_imported_specs () =
  let all =
    Env.vox_imported_specs ()
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

let imported_need_voxu () =
  List.exists
    (fun (_, vp) -> vp.Cmi_format.vp_needs_voxu)
    !imported_specs
;;

(* A datatype of THIS module whose stable name matches an imported
   declaration is not re-declared (see the emitters' [~skip]) -- which
   is only sound if it really is the same declaration.  The renderers
   are deterministic, so comparing rendered text detects a local type
   shadowing an imported one at the same solver-side name.  [render]
   is a parameter only because the Lean renderer it must be (the
   export stores the Lean rendering) is defined later in this file. *)
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
                  ~loc:(Location.in_file !Location.input_name)
                  "vox: the type %s would share the solver-side name %s \
                   with a different datatype imported from unit %s; \
                   rename one of them"
                  uname
                  uname
                  unit)
            vp.Cmi_format.vp_datatypes)
        !imported_specs)
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
          Location.raise_errorf
            ~loc:(Location.in_file !Location.input_name)
            "vox: cannot read -vox-prelude file: %s" msg)
    in
    prelude_cache := Some c;
    c
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

let rec lean_sort = function
  | S_int -> "Int"
  | S_bool -> "Prop"
  | S_other -> "VoxU"
  | S_iarray -> "VoxIA"
  | S_tuple comps ->
    "(" ^ tuple_uname (List.length comps) ^ " "
    ^ String.concat " " (List.map lean_sort comps)
    ^ ")"
  | S_data p ->
    (match find_datatype p with
     | Some _ -> lean_dt_name p
     | None -> "VoxU" (* unregistered: degrade, sound *))
;;

(* The product structure for one tuple arity, universe-polymorphic over
   [Sort] so a Prop component (the Lean model of bool) instantiates as
   readily as a Type one -- the shape of core Lean's [PProd], with
   explicit universe binders so no auto-binding is relied on.  One
   line, like the other declarations (the error-line mapping counts
   lines). *)
let lean_tuple_decl n =
  let buf = Buffer.create 128 in
  Buffer.add_string buf (Printf.sprintf "structure %s.{" (tuple_uname n));
  for i = 1 to n do
    if i > 1 then Buffer.add_string buf ", ";
    Buffer.add_string buf (Printf.sprintf "u%d" i)
  done;
  Buffer.add_string buf "}";
  for i = 1 to n do
    Buffer.add_string buf (Printf.sprintf " (t%d : Sort u%d)" i i)
  done;
  let univ =
    let rec go i = if i > n then "1" else Printf.sprintf "max u%d (%s)" i (go (i + 1)) in
    go 1
  in
  Buffer.add_string buf (Printf.sprintf " : Sort (%s) where" univ);
  for i = 1 to n do
    Buffer.add_string buf (Printf.sprintf " (p%d : t%d)" i i)
  done;
  Buffer.add_char buf '\n';
  Buffer.contents buf
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
  | Pand _ | Por _ | Pnot _ | Pimp _ -> true
  | Pglobal p ->
    (match Hashtbl.find_opt globals (path_uname p) with
     | Some (_, S_bool) -> true
     | _ -> false)
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
        | Some (S_int | S_data _ | S_tuple _ | S_iarray | S_other) | None -> false)
     | Some (_, Dt_variant _) | None -> false)
  | Pis _ | Pquant _ -> true
  (* A bool-sorted tuple COMPONENT is a Prop the model cannot see from
     the (untyped) projection alone: [=] between Props is emitted
     there, a sharp edge grind still handles via propext. *)
  | Pbound | Pint _ | Pconstr _ | Pfun _ | Ptuple _ | Pproj _
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
  | Pglobal p -> Buffer.add_string buf ("g_" ^ lean_sanitize (path_uname p))
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
       identifier); defined by a prelude (file, embedded block, or
       imported spec export) or a [total_] definition. *)
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
  | Ptuple args ->
    Buffer.add_string buf ("(" ^ tuple_uname (List.length args) ^ ".mk");
    List.iter
      (fun a ->
        Buffer.add_char buf ' ';
        lean_of_pred buf a)
      args;
    Buffer.add_char buf ')'
  | Pproj (n, i, a) ->
    Buffer.add_string buf (Printf.sprintf "(%s.p%d " (tuple_uname n) (i + 1));
    lean_of_pred buf a;
    Buffer.add_char buf ')'
  | Pquant (q, id, a) ->
    (* The binder is unannotated -- predicates are untyped, and Lean
       infers its sort from the body, exactly as for the existential
       encoding of [Pis] below; an uninferable binder is a solver
       error, i.e. a verification failure. *)
    Buffer.add_string
      buf
      ((match q with
        | Qforall -> "(∀ "
        | Qexists -> "(∃ ")
       ^ lean_name id
       ^ ", ");
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
  | Pimp (a, b) -> bin "→" a b
;;

(* Reflected definitions, emitted between the datatypes and the
   prelude.  [@[grind] def] registers the defining equations with
   grind.  Termination is Lean's to check: structural recursion needs
   nothing, and a [@@vox.decreases e] metric becomes
   [termination_by (e).toNat] with an omega [decreasing_by], falling
   back to [grind] for the goals omega leaves opaque -- a recursion on
   [n / 2] decreases through [Int.tdiv], which omega treats as an atom
   (the branch guards are in context for those goals either way).  The
   def name is the source name, so a [-vox-prelude] can state lemmas
   about it. *)
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
    Buffer.add_string buf
      ").toNat\ndecreasing_by all_goals (first | omega | grind)\n"
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
    let saved = !datatypes, !registering, !poisoned, !tuple_arities in
    datatypes := [];
    registering := [];
    poisoned := [];
    tuple_arities := [];
    Misc.try_finally
      ~always:(fun () ->
        let d, r, po, ta = saved in
        datatypes := d;
        registering := r;
        poisoned := po;
        tuple_arities := ta)
      (fun () ->
        iter_signature_types sg ~f:(fun ~loc:_ ~what:_ ty ->
          register_type_specs env ty);
        List.iter
          (fun d ->
            List.iter
              (fun p -> ignore (datatype_sort env p))
              (Vox_reflect.def_datatype_paths d);
            List.iter
              register_pred_tuple_arities
              (Vox_reflect.body_preds
                 (Option.to_list d.Vox_reflect.sd_decreases)
                 d.Vox_reflect.sd_body))
          defs;
        let dts =
          List.map
            (fun ((p, _) as dt) -> path_uname p, lean_datatype_decl dt)
            !datatypes
        in
        (* Tuple product declarations FIRST: the datatype declarations
           may reference them in field sorts. *)
        let dts =
          List.map
            (fun n -> tuple_uname n, lean_tuple_decl n)
            !tuple_arities
          @ dts
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
    ~blocks:(collect_blocks_sig tsg)
;;

let cmi_export_of_structure (str : structure) (sg : Types.signature) =
  cmi_export str.str_final_env sg ~defs:!spec_defs
    ~blocks:(List.map fst (collect_blocks str))
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
  let seen_g = Hashtbl.create 4 in
  List.iter
    (fun gp ->
      let key = path_uname gp in
      if not (Hashtbl.mem seen_g key)
      then (
        Hashtbl.add seen_g key ();
        let sort =
          match Hashtbl.find_opt globals key with
          | Some (_, ds) -> lean_sort ds
          | None -> "VoxU"
        in
        Buffer.add_string buf
          (Printf.sprintf "(g_%s : %s) " (lean_sanitize key) sort)))
    (List.concat_map Refinement.free_globals (vc.vc_goal :: vc.vc_facts));
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
         | Refinement.Pis
             (path, _, ((Refinement.Pvar _ | Refinement.Pglobal _) as subj)) ->
           let skey =
             match subj with
             | Refinement.Pvar id -> Ident.unique_name id
             | _ -> Refinement.to_string subj
           in
           let key = skey ^ "|" ^ path_uname path in
           if not (Hashtbl.mem seen_subj key)
           then (
             Hashtbl.add seen_subj key ();
             match find_datatype path with
             | Some (_, Dt_variant constrs) ->
               let disj =
                 match
                   List.map
                     (fun (cname, _) ->
                       Refinement.Pis (path, cname, subj))
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
        | Refinement.Pproj (_, _, a)
        | Refinement.Pquant (_, _, a)
        | Refinement.Pnot a -> collect a
        | Refinement.Pconstr (_, _, args)
        | Refinement.Pfun (_, args)
        | Refinement.Ptuple args ->
          List.iter collect args
        | Refinement.Pbinop (_, a, b)
        | Refinement.Pand (a, b)
        | Refinement.Por (a, b)
        | Refinement.Pimp (a, b) ->
          collect a;
          collect b
        | Refinement.Pbound | Refinement.Pvar _ | Refinement.Pglobal _
        | Refinement.Pint _
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
            | Some s -> sort_needs_voxu s
            | None -> true)
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
  let want_spec_text = List.exists vc_uses_spec_fun vcs in
  (* Block text (imported blocks, -vox-prelude, own blocks) declares
     spec functions AT VoxU (e.g. [opaque f : VoxU -> Int]); if VoxU
     itself were not declared, Lean's autobound implicits would
     silently generalize those signatures ([{VoxU : Sort u} -> ...]),
     turning ill-sorted applications into polymorphic ones instead of
     errors.  So the prelude implies VoxU. *)
  let needs_voxu = needs_voxu || imported_need_voxu () || want_spec_text in
  let needs_iarray =
    List.exists
      (fun vc ->
        List.exists
          (fun id ->
            match Hashtbl.find_opt name_sorts id with
            | Some s -> sort_needs_iarray s
            | None -> false)
          (free_vars_of_vc vc)
        || List.exists
             (fun p ->
               Refinement.mentions_fun Refinement.ia_len p
               || Refinement.mentions_fun Refinement.ia_get p)
             (vc.vc_goal :: vc.vc_facts))
      vcs
    || datatype_field_needs_iarray ()
  in
  let segments = ref [] in
  let seg ?src text = if text <> "" then segments := (text, src) :: !segments in
  if needs_voxu then seg "opaque VoxU : Type\n";
  if needs_iarray then seg lean_iarray_theory;
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
    !imported_specs;
  (* Tuple product structures precede this module's datatypes (whose
     fields may be tuple-sorted); imported exports carry their own,
     deduplicated by the stable per-arity name. *)
  List.iter
    (fun n ->
      if not (List.exists (String.equal (tuple_uname n)) !seen)
      then (
        seen := tuple_uname n :: !seen;
        seg (lean_tuple_decl n)))
    !tuple_arities;
  let own_decls = Buffer.create 256 in
  lean_datatype_decls own_decls ~skip:!seen;
  seg (Buffer.contents own_decls);
  (* Imported blocks and the [-vox-prelude] file come BEFORE this
     module's reflected definitions: a definition may call an imported
     reflected function (whose definition rides the exporting unit's
     blocks).  They are therefore also needed whenever this module has
     definitions, not only when a VC applies a spec function. *)
  if want_spec_text || !spec_defs <> []
  then (
    List.iter
      (fun (unit, vp) ->
        List.iter
          (fun text -> seg ~src:(Imported_block unit) text)
          vp.Cmi_format.vp_blocks)
      !imported_specs;
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
  if want_spec_text
  then
    List.iter
      (fun (s, loc) -> seg ~src:(Local_block loc) s)
      !embedded_blocks;
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
          (* Find the first error and map it back.  Lean prints
             "<file>:L:C: error: ..." or, with a kind,
             "<file>:L:C: error(lean.some.kind): ...".  Warnings use
             the same shapes with "warning" (e.g. for unused
             hypotheses); only errors count (a warning line before the
             real error must not steal the attribution). *)
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
           is_vox_block_name a.attr_name.txt
         | _ -> false)
       str.str_items
;;

let walk_items (str : structure) ctx =
  List.iter
    (fun item ->
      match item.str_desc with
      | Tstr_value (rec_flag, vbs) ->
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
        List.iter
          (fun vb -> ctx := walk_expr str.str_final_env !ctx vb.vb_expr)
          vbs;
        ctx
        := List.fold_left
             (fun ctx vb ->
               extend_pat ~toplevel:true str.str_final_env ctx vb.vb_pat)
             !ctx
             vbs;
        (match rec_flag with
         | Recursive ->
           (* No self facts for recursive bindings (cyclic constructor
              equations are unsatisfiable in the datatype theory). *)
           ()
         | Nonrecursive ->
           ctx
           := List.fold_left
                (fun ctx vb ->
                  { ctx with
                    cfacts =
                      binding_self_facts str.str_final_env vb @ ctx.cfacts
                  })
                !ctx
                vbs)
      | _ ->
        let it =
          { Tast_iterator.default_iterator with
            (* the expression's OWN env, not the top-level structure's:
               inside a nested module, locally declared types (their
               attributes, constructors, labels) are only findable in
               the inner env -- with the outer env they silently sort
               at VoxU *)
            expr = (fun _ e -> ignore (walk_expr e.exp_env !ctx e : ctx))
          ; pat =
              (fun sub (type k) (p : k general_pattern) ->
                backstop_pat !ctx p;
                Tast_iterator.default_iterator.pat sub p)
          ; value_binding =
              (fun sub vb ->
                reject_local_reflect vb;
                Tast_iterator.default_iterator.value_binding sub vb)
          ; type_declaration =
              (fun sub td ->
                (* eager [@@vox.sort] validation for LOCAL declarations
                   (exported ones are covered by check_signature) *)
                let alias =
                  (match td.typ_kind with
                   | Ttype_abstract -> true
                   | _ -> false)
                  && Option.is_some td.typ_manifest
                in
                validate_vox_sort_attributes ~alias td.typ_attributes;
                Tast_iterator.default_iterator.type_declaration sub td)
          }
        in
        it.structure_item it item)
    str.str_items
;;

(* An interface/implementation pair must agree on [@@vox.sort] for
   every exported type: sorts are computed per-compilation from the
   VISIBLE declaration, so a mismatch would let clients reason at one
   sort against an implementation verified at another. *)
let check_sort_consistency (str : structure) (sg : Types.signature) =
  List.iter
    (fun (item : Types.signature_item) ->
      match item with
      | Sig_type (id, decl, _, _) ->
        let sig_sort =
          List.find_map vox_sort_of_attribute decl.type_attributes
        in
        (match
           Env.find_type_by_name
             (Longident.Lident (Ident.name id))
             str.str_final_env
         with
         | exception Not_found -> ()
         | _, impl_decl ->
           let impl_sort =
             List.find_map vox_sort_of_attribute impl_decl.type_attributes
           in
           if not (sig_sort = impl_sort)
           then
             Location.raise_errorf
               ~loc:impl_decl.type_loc
               "vox: the vox.sort of type %s differs between the interface \
                and the implementation; the attribute must appear \
                identically on both declarations"
               (Ident.name id))
      | _ -> ())
    sg
;;

let check_implementation ?intf (str : structure) (sg : Types.signature) =
  (* The signature check is unconditional: a refined type can appear in
     an exported item (a type manifest, an exception, an external) with
     no vox syntax in any expression, and it must still be
     self-contained.  It only reads types structurally, so it cannot
     perturb programs that never use vox. *)
  check_signature sg;
  (* [intf] is the .mli's signature when one exists (the inferred [sg]
     always agrees with the struct trivially). *)
  Option.iter (check_sort_consistency str) intf;
  if not (uses_vox str)
  then ()
  else (
    reset ();
    (* Blocks anywhere in the module are available to all of its VCs
       (they are emitted, in source order, into every solver input);
       blocks exported by imported units' interfaces -- including this
       unit's own .mli -- come from their .cmis. *)
    embedded_blocks := collect_blocks str;
    imported_specs := gather_imported_specs ();
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
let toplevel_blocks : (string * Location.t) list ref = ref []
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
    embedded_blocks := !toplevel_blocks @ collect_blocks str;
    imported_specs := gather_imported_specs ();
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
    toplevel_blocks := !embedded_blocks)
;;
