(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement

let node type_ rexp_desc =
  R.create ~loc:Location.none ~type_ rexp_desc

let arrow argument result =
  Btype.newgenty
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy),
         argument,
         result,
         commu_ok ))

let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))

let free reference =
  node Predef.type_bool (Rexp_ident (Rfree reference))

let path module_id name = Path.Pdot (Path.Pident module_id, name)

let make_refined module_id =
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "view";
      rb_type = Predef.type_int;
    }
  in
  let lambda_binder =
    { rb_id = Ident.create_scoped ~scope:1 "argument";
      rb_type = Predef.type_int;
    }
  in
  let lambda =
    node (arrow Predef.type_int Predef.type_int)
      (Rexp_function
         { arg_label = Nolabel;
           param = lambda_binder;
           body = bound lambda_binder;
         })
  in
  let constructor =
    node Predef.type_int
      (Rexp_construct
         ( { rconstr_type_path = path module_id "t";
             rconstr_name = "C";
           },
           [] ))
  in
  let field =
    node Predef.type_int
      (Rexp_field
         ( node Predef.type_int (Rexp_constant (Const_int 0)),
           { rfield_type_path = path module_id "t";
             rfield_name = "field";
           } ))
  in
  let ref_pred =
    node Predef.type_bool
      (Rexp_tuple
         [ None, bound ref_view;
           None, lambda;
           None, free (Rapp (path module_id "operation"));
           None, free (Rglobal (path module_id "value"));
           None, constructor;
           None, field;
         ])
  in
  Btype.newgenty
    (Trefine { ref_skeleton = Predef.type_int; ref_view; ref_pred })

let refinement type_ =
  match get_desc type_ with
  | Trefine refinement -> refinement
  | _ -> failwith "expected a refinement"

let predicate_parts refinement =
  match refinement.ref_pred.rexp_desc with
  | Rexp_tuple
      [ _, { rexp_desc = Rexp_ident (Rbound view_occurrence); _ };
        _, { rexp_desc =
               Rexp_function
                 { param = lambda_binder;
                   body = { rexp_desc = Rexp_ident (Rbound lambda_occurrence);
                            _ };
                   _
                 };
             _ };
        _, { rexp_desc = Rexp_ident (Rfree (Rapp application)); _ };
        _, { rexp_desc = Rexp_ident (Rfree (Rglobal global)); _ };
        _, { rexp_desc = Rexp_construct (constructor, []); _ };
        _, { rexp_desc = Rexp_field (_, field); _ };
      ] ->
    ( view_occurrence,
      lambda_binder.rb_id,
      lambda_occurrence,
      application,
      global,
      constructor.rconstr_type_path,
      field.rfield_type_path )
  | _ -> failwith "unexpected predicate"

let check_freshened ~original copied =
  let original = refinement original in
  let copied = refinement copied in
  let view_occurrence, lambda, lambda_occurrence, _, _, _, _ =
    predicate_parts copied
  in
  let _, original_lambda, _, _, _, _, _ = predicate_parts original in
  assert (not (Ident.same copied.ref_view.rb_id original.ref_view.rb_id));
  assert (Ident.same copied.ref_view.rb_id view_occurrence);
  assert (not (Ident.same lambda original_lambda));
  assert (Ident.same lambda lambda_occurrence)

let () =
  let parameter = Ident.create_scoped ~scope:1 "Parameter" in
  let argument = Ident.create_scoped ~scope:1 "Argument" in
  let original = make_refined parameter in
  let substitution =
    Subst.add_module parameter (Path.Pident argument) Subst.identity
  in
  let first = Subst.type_expr substitution original in
  let second = Subst.type_expr substitution original in
  check_freshened ~original first;
  check_freshened ~original second;
  let first_refinement = refinement first in
  let second_refinement = refinement second in
  assert
    (not
       (Ident.same
          first_refinement.ref_view.rb_id second_refinement.ref_view.rb_id));
  let _, first_lambda, _, application, global, constructor, field =
    predicate_parts first_refinement
  in
  let _, second_lambda, _, _, _, _, _ =
    predicate_parts second_refinement
  in
  assert (not (Ident.same first_lambda second_lambda));
  let expected name = path argument name in
  assert (Path.same application (expected "operation"));
  assert (Path.same global (expected "value"));
  assert (Path.same constructor (expected "t"));
  assert (Path.same field (expected "t"));
  print_endline "substitution: value/type paths rewritten";
  print_endline "functor applications: binder stamps disjoint"

let value_description type_ =
  { val_type = type_;
    val_modalities = Mode.Modality.undefined;
    val_kind =
      Val_reg Jkind.Sort.(of_const Const.for_continuation);
    val_lpoly = Lpoly.determined [];
    val_loc = Location.none;
    val_zero_alloc = Zero_alloc.default;
    val_attributes = [];
    val_uid = Uid.internal_not_actually_unique;
  }

let () =
  let value_id = Ident.create_scoped ~scope:1 "value" in
  let refined = make_refined value_id in
  let refined =
    let refinement = refinement refined in
    let ref_pred = free (Rglobal (Path.Pident value_id)) in
    Btype.newgenty (Trefine { refinement with ref_pred })
  in
  let signature =
    [Sig_value (value_id, value_description refined, Exported)]
  in
  match Subst.signature Keep Subst.identity signature with
  | [Sig_value (renamed, description, Exported)] ->
    let copied = refinement description.val_type in
    begin match copied.ref_pred.rexp_desc with
    | Rexp_ident (Rfree (Rglobal (Path.Pident reference))) ->
      assert (Ident.same reference renamed);
      assert (not (Ident.same reference value_id))
    | _ -> failwith "value path was not rewritten"
    end;
    print_endline "signature renaming: value path recorded"
  | _ -> failwith "unexpected signature"

let () =
  let imported_module = Ident.create_scoped ~scope:1 "Imported" in
  let original = make_refined imported_module in
  let imported = Subst.type_expr (Subst.for_loading_cmi ()) original in
  check_freshened ~original imported;
  print_endline "cmi loading: all predicate binders freshened"
