(* vox: dependent-arrow support over the type graph.

   A dependent arrow stores its binder [Ident.t] in its [arrow_desc]
   (like [Tpoly] stores its univars); refinements in the codomain
   reference it as an ordinary [Refinement.Pvar].  Consuming an arrow
   (at an application or when a lambda binds the parameter)
   substitutes the binder's stamp by the argument's (respectively the
   parameter's) stamp throughout the remaining type: no positional
   arithmetic is involved, so partial, labelled, and commuted
   applications are all handled by construction.

   Nodes are rebuilt only along paths whose predicates change; shared
   subtrees are reused.  Cycles are cut with a physical visited list
   (refinements on cyclic paths are unsupported in v0). *)

open Types

let rec subst id ~by ty visited =
  if List.memq ty visited
  then ty
  else begin
    let visited = ty :: visited in
    match get_desc ty with
    | Trefine (skel, p) ->
      let p' = Refinement.subst_var id ~by p in
      let skel' = subst id ~by skel visited in
      if p' == p && skel' == skel
      then ty
      else Btype.newty2 ~level:(get_level ty) (Trefine (skel', p'))
    | Tarrow (d, a, r, c) ->
      let a' = subst id ~by a visited in
      let r' = subst id ~by r visited in
      if a' == a && r' == r
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tarrow (d, a', r', c))
    | Tconstr (p, args, _) ->
      let args' = List.map (fun t -> subst id ~by t visited) args in
      if List.for_all2 ( == ) args args'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tconstr (p, args', ref Mnil))
    | Ttuple l ->
      let l' = List.map (fun (lbl, t) -> lbl, subst id ~by t visited) l in
      if List.for_all2 (fun (_, t) (_, t') -> t == t') l l'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Ttuple l')
    | Tunboxed_tuple l ->
      let l' = List.map (fun (lbl, t) -> lbl, subst id ~by t visited) l in
      if List.for_all2 (fun (_, t) (_, t') -> t == t') l l'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tunboxed_tuple l')
    | Tpoly (t, vars) ->
      let t' = subst id ~by t visited in
      if t' == t
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tpoly (t', vars))
    | _ -> ty
  end
;;

(* Open the binder [id]: replace it by [by] throughout [ty].  Non-
   destructive: rebuilds only changed spines, so annotation types and
   other instances are unaffected.

   [subst] has no notion of shadowing, so it relies on this INVARIANT:
   within any type graph it can reach, binder stamps are distinct.
   In-unit this holds because typetexp mints a fresh ident per arrow
   (and copies share, never re-bind, binders).  Across units it is
   delicate: stamps restart per compiler process, so two .cmis
   routinely contain COLLIDING [Scoped] binder stamps ([Ident.same]
   already keeps them apart from every [Local] program variable).  Two
   things keep a foreign colliding binder out of reach: signature
   self-containment (Vox_verify.check_signature) forces every
   .cmi-crossing binder reference to sit under its own arrow, and
   [subst] walks [Tconstr] ARGUMENTS only, never a constructor's
   expansion, so another unit's binder can only be reached through its
   own (freshly copied, consistently stamped) arrow.  See
   testsuite/tests/vox/stamp_collide.ml. *)
let subst_binder id ~by ty = subst id ~by ty []

(* Refinements can hide inside object fields, polymorphic-variant
   arguments and package constraints too; [subst] does not rebuild
   those (dependent substitution through them is unsupported), so a
   binder reference left behind there surfaces as an escape error
   rather than being missed. *)
let children ty =
  match get_desc ty with
  | Tconstr (_, args, _) -> args
  | Ttuple l | Tunboxed_tuple l -> List.map snd l
  | Tpoly (t, _) -> [ t ]
  | Tobject (fields, _) -> [ fields ]
  | Tfield (_, _, t1, t2) -> [ t1; t2 ]
  | Tvariant row ->
    row_more row
    :: List.concat_map
         (fun (_, f) ->
           match row_field_repr f with
           | Rpresent (Some t) -> [ t ]
           | Reither (_, tl, _) -> tl
           | Rpresent None | Rabsent -> [])
         (row_fields row)
  | Tpackage pack -> List.map snd pack.pack_cstrs
  | _ -> []
;;

(* Iterate over every refinement predicate in [ty], with the binders
   of the arrows enclosing it WITHIN [ty]: a [Pvar] of a bound ident
   is a dependent-parameter reference, anything else is a free
   program variable. *)
let rec iter_preds ~bound ty visited f =
  if List.memq ty visited
  then ()
  else begin
    let visited = ty :: visited in
    match get_desc ty with
    | Trefine (skel, p) ->
      f ~bound p;
      iter_preds ~bound skel visited f
    | Tarrow ((_, _, _, binder), a, r, _) ->
      iter_preds ~bound a visited f;
      let bound =
        match binder with
        | Some id -> id :: bound
        | None -> bound
      in
      iter_preds ~bound r visited f
    | Tconstr _ | Ttuple _ | Tunboxed_tuple _ | Tpoly _ | Tobject _ | Tfield _
    | Tvariant _ | Tpackage _ ->
      List.iter (fun t -> iter_preds ~bound t visited f) (children ty)
    | _ -> ()
  end
;;

let iter_refinement_preds ty f = iter_preds ~bound:[] ty [] f

(* Does any refinement in [ty] reference [id]?  Used to normalize away
   unused binders and to detect dependence on an argument. *)
let mentions_ident id ty =
  let found = ref false in
  iter_refinement_preds ty (fun ~bound:_ p ->
    if Refinement.mem_var id p then found := true);
  !found
;;
