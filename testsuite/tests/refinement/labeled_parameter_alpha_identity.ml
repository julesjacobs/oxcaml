(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement
module Facts = Vox_vc.Fact_env

let node type_ rexp_desc = R.create ~loc:Location.none ~type_ rexp_desc

let bound id =
  node Predef.type_int (Rexp_ident (Rbound id))

let pair first second =
  node Predef.type_int
    (Rexp_tuple [None, bound first; None, bound second])

let binder id = { rb_id = id; rb_type = Predef.type_int }

let () =
  let left_outer = Ident.create_local "left_outer" in
  let left_inner = Ident.create_local "left_inner" in
  let right_outer = Ident.create_local "right_outer" in
  let right_inner = Ident.create_local "right_inner" in
  let left = pair left_outer left_inner in
  let right = pair right_outer right_inner in
  let swapped = pair right_inner right_outer in
  let equal_type _ _ = true in
  assert
    (R.alpha_equal ~equal_type
       ~binders:
         [ binder left_outer, binder right_outer;
           binder left_inner, binder right_inner;
         ]
       left right);
  assert
    (not
       (R.alpha_equal ~equal_type
          ~binders:
            [ binder left_outer, binder right_outer;
              binder left_inner, binder right_inner;
            ]
          left swapped));
  assert (not (R.alpha_equal ~equal_type left swapped))

let equal_to_zero id =
  node Predef.type_bool
    (Rexp_apply
       ( node Predef.type_int (Rexp_ident (Rfree (Rfun "="))),
         [ Nolabel, bound id;
           Nolabel, node Predef.type_int (Rexp_constant (Const_int 0));
         ] ))

let fact_origin : Vox_vc.fact_origin =
  { kind = "identity-regression"; name = None; span = None }

let fact id = Facts.add ~origin:fact_origin (equal_to_zero id) Facts.empty

let () =
  let left = fact (Ident.create_local "left_key") in
  let right = fact (Ident.create_local "right_key") in
  assert (Facts.facts (Facts.intersect left right) = [])
