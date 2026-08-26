(* TEST
 include ocamlcommon;
 expect;
*)

open Types

let () =
  let loc = Location.none in
  let expr rexp_desc =
    { rexp_desc; rexp_type = Predef.type_int; rexp_loc = loc }
  in
  let pat rpat_desc =
    { rpat_desc; rpat_type = Predef.type_int; rpat_loc = loc }
  in
  let variable = Ident.create_local "x" in
  let bound = Ident.create_local "bound" in
  let alias = Ident.create_local "alias" in
  let owner = Path.Pident (Ident.create_local "Owner") in
  let constructor = Path.Pdot (owner, "C") in
  let dependency = Path.Pident (Ident.create_local "dependency") in
  let pattern =
    pat (Rpat_alias
      (pat (Rpat_tuple
        [None, pat (Rpat_construct (constructor, [pat (Rpat_var bound)]));
         None, pat Rpat_any]), alias))
  in
  let binding e =
    { rb_kind = Rbind_value; rb_ident = bound;
      rb_type = Predef.type_bool; rb_expr = e }
  in
  let contexts e =
    [expr (Rexp_apply (e, [Asttypes.Nolabel, e]));
     expr (Rexp_tuple [None, e]);
     expr (Rexp_construct (constructor, [e]));
     expr (Rexp_record ([owner, "field", e], Some e));
     expr (Rexp_record_unboxed_product ([owner, "field", e], Some e));
     expr (Rexp_array (Asttypes.Immutable, [e]));
     expr (Rexp_field (e, owner, "field"));
     expr (Rexp_ifthenelse (e, e, Some e));
     expr (Rexp_sequence (e, e));
     expr (Rexp_let (binding e, e));
     expr (Rexp_fun (bound, Predef.type_bool, e));
     expr (Rexp_match (e, [{rc_lhs = pattern; rc_guard = Some e; rc_rhs = e}]))]
  in
  let check_types tree =
    let mapped = ref 0 in
    ignore (Refinement_predicate.map
      ~type_expr:(fun ty -> incr mapped; ty) tree);
    let folded = Refinement_predicate.fold_types (fun n _ -> n + 1) 0 tree in
    assert (folded = !mapped)
  in
  List.iter (fun tree ->
    check_types tree;
    let expected = match tree.rexp_desc with
      | Rexp_let _ | Rexp_fun _ -> [bound]
      | Rexp_match _ -> [bound; alias]
      | _ -> []
    in
    assert (Ident.Set.equal (Refinement_predicate.bound_idents tree)
      (Ident.Set.of_list expected));
    assert (Option.is_some (Refinement_predicate.find_ident
      (Ident.Set.singleton variable) tree)))
    (contexts (expr (Rexp_var variable)));
  List.iter (fun tree ->
    let found = Refinement_predicate.find_dependency_path
      (fun path -> if Path.same path dependency then Some path else None) tree
    in
    assert (Option.is_some found))
    (contexts (expr (Rexp_ident dependency)));
  let tree = expr (Rexp_match (expr (Rexp_ident dependency),
    [{rc_lhs = pattern; rc_guard = None; rc_rhs = expr (Rexp_var bound)}]))
  in
  let ids = Refinement_predicate.bound_idents tree in
  assert (Ident.Set.equal ids (Ident.Set.of_list [bound; alias]));
  let visited = ref [] in
  ignore (Refinement_predicate.find_dependency_path
    (fun path -> visited := path :: !visited; None) tree);
  assert (List.for_all2 Path.same (List.rev !visited)
    [constructor; dependency]);
  let tree = expr (Rexp_let (binding (expr (Rexp_var variable)),
                           expr (Rexp_var bound))) in
  let annotations = Refinement_predicate.fold_types
    (fun types ty -> ty :: types) [] tree |> List.rev
  in
  assert (List.for_all2 eq_type annotations
    [Predef.type_int; Predef.type_bool; Predef.type_int; Predef.type_int]);;
[%%expect{|
|}]
