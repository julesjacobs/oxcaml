(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement

let node type_ rexp_desc = R.create ~loc:Location.none ~type_ rexp_desc

let refinement binder =
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "view";
      rb_type = Predef.type_unit;
    }
  in
  let ref_pred =
    node Predef.type_bool
      (Rexp_tuple
         [ None, node Predef.type_int (Rexp_ident (Rbound binder));
           None, node Predef.type_int (Rexp_ident (Rbound binder));
         ])
  in
  Btype.newgenty
    (Trefine { ref_skeleton = Predef.type_unit; ref_view; ref_pred })

let arrow binder result =
  Btype.newgenty
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy, Some binder),
         Ctype.newmono Predef.type_int,
         result,
         commu_ok ))

let occurrences_of_refinement type_ =
  match get_desc type_ with
  | Trefine
      { ref_pred =
          { rexp_desc =
              Rexp_tuple
                [ None, { rexp_desc = Rexp_ident (Rbound first); _ };
                  None, { rexp_desc = Rexp_ident (Rbound second); _ };
                ];
            _ };
        _
      } ->
    first, second
  | _ -> failwith "expected a bound refinement"

let identities_of_arrow type_ =
  match get_desc type_ with
  | Tarrow ((_, _, _, Some binder), _, result, _) ->
    let first, second = occurrences_of_refinement result in
    binder, first, second
  | _ -> failwith "expected an arrow"

let import type_ = Subst.type_expr (Subst.for_loading_cmi ()) type_

let () =
  let source = Ident.create_local "shared" in
  let original = arrow source (refinement source) in
  let first_binder, first_left, first_right =
    identities_of_arrow (import original)
  in
  assert (Ident.same first_binder first_left);
  assert (Ident.same first_left first_right);
  assert (not (Ident.same source first_binder));

  let second_binder, second_left, second_right =
    identities_of_arrow (import original)
  in
  assert (Ident.same second_binder second_left);
  assert (Ident.same second_left second_right);
  assert (not (Ident.same first_left second_left));

  let outer = Ident.create_local "duplicate" in
  let inner = Ident.create_local "duplicate" in
  let nested = arrow outer (arrow inner (refinement inner)) in
  match get_desc (import nested) with
  | Tarrow ((_, _, _, Some imported_outer), _, imported_inner_arrow, _) ->
    let imported_inner, occurrence1, occurrence2 =
      identities_of_arrow imported_inner_arrow
    in
    assert (not (Ident.same imported_outer imported_inner));
    assert (Ident.same imported_inner occurrence1);
    assert (Ident.same occurrence1 occurrence2)
  | _ -> failwith "expected nested arrows"
