(* vox: dependent-arrow support over the type graph.

   Dependent-arrow parameters are de Bruijn indices inside [Trefine]
   predicates ([Refinement.Pparam]), counting enclosing arrows outward
   (the walk enters an arrow's RESULT at depth+1; its argument stays at
   the same depth).  Consuming the outermost arrow of a function type
   substitutes, in its result, every parameter reference to that arrow
   by the (variable) argument's stamp.  Indices never shift: smaller
   indices keep referring to inner arrows, so partial application is
   automatically correct.

   [subst_outer_param ~offset ~by ty] replaces [Pparam (depth + offset)]
   throughout [ty] (depth 0 at the root of [ty]); [offset] supports
   post-hoc substitution after several arrows were consumed at once.
   Nodes are rebuilt only along paths whose predicates change; shared
   subtrees are reused.  Cycles are cut with a physical visited list
   (refinements on cyclic paths are unsupported in v0). *)

open Types

let rec pred_mentions ~depth ~offset ty visited =
  if List.memq ty visited
  then false
  else begin
    let visited = ty :: visited in
    match get_desc ty with
    | Trefine (skel, p) ->
      Refinement.mem_param (depth + offset) p
      || pred_mentions ~depth ~offset skel visited
    | Tarrow (_, a, r, _) ->
      pred_mentions ~depth ~offset a visited
      || pred_mentions ~depth:(depth + 1) ~offset r visited
    | Tconstr _ | Ttuple _ | Tunboxed_tuple _ | Tpoly _ ->
      List.exists
        (fun t -> pred_mentions ~depth ~offset t visited)
        (children ty)
    | _ -> false
  end

and children ty =
  match get_desc ty with
  | Tconstr (_, args, _) -> args
  | Ttuple l | Tunboxed_tuple l -> List.map snd l
  | Tpoly (t, _) -> [ t ]
  | _ -> []
;;

(* Does [ty] mention the parameter of the arrow sitting [offset] levels
   above [ty]'s root?  With [offset = 0], whether an arrow's result
   depends on that arrow's own parameter. *)
let mentions_outer_param ~offset ty = pred_mentions ~depth:0 ~offset ty []

let result_is_dependent ret = mentions_outer_param ~offset:0 ret

let rec subst ~depth ~offset ~by ty visited =
  if List.memq ty visited
  then ty
  else begin
    let visited = ty :: visited in
    match get_desc ty with
    | Trefine (skel, p) ->
      let p' = Refinement.subst_param ~index:(depth + offset) ~by p in
      let skel' = subst ~depth ~offset ~by skel visited in
      if p' == p && skel' == skel
      then ty
      else Btype.newty2 ~level:(get_level ty) (Trefine (skel', p'))
    | Tarrow (d, a, r, c) ->
      let a' = subst ~depth ~offset ~by a visited in
      let r' = subst ~depth:(depth + 1) ~offset ~by r visited in
      if a' == a && r' == r
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tarrow (d, a', r', c))
    | Tconstr (p, args, _) ->
      let args' = List.map (fun t -> subst ~depth ~offset ~by t visited) args in
      if List.for_all2 ( == ) args args'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tconstr (p, args', ref Mnil))
    | Ttuple l ->
      let l' =
        List.map (fun (lbl, t) -> lbl, subst ~depth ~offset ~by t visited) l
      in
      if List.for_all2 (fun (_, t) (_, t') -> t == t') l l'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Ttuple l')
    | Tunboxed_tuple l ->
      let l' =
        List.map (fun (lbl, t) -> lbl, subst ~depth ~offset ~by t visited) l
      in
      if List.for_all2 (fun (_, t) (_, t') -> t == t') l l'
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tunboxed_tuple l')
    | Tpoly (t, vars) ->
      let t' = subst ~depth ~offset ~by t visited in
      if t' == t
      then ty
      else Btype.newty2 ~level:(get_level ty) (Tpoly (t', vars))
    | _ -> ty
  end
;;

(* Replace, throughout [ty], the parameter references that point
   [offset] arrows above [ty]'s root by [by]. *)
let subst_outer_param ~offset ~by ty = subst ~depth:0 ~offset ~by ty []
