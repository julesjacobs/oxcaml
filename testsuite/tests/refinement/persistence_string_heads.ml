(* TEST include ocamlcommon;
*)

(* String-headed references are now produced by lowering.  This test also
   builds them directly and pushes them through module substitution, the
   save/load (cmi) boundary, and a literal marshal round-trip, so the four-head
   rewriting remains covered. *)

open Types
module R = Types.Refinement

let node type_ rexp_desc = R.create ~loc:Location.none ~type_ rexp_desc

let arrow argument result =
  Btype.newgenty
    (Tarrow ((Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy), argument, result, commu_ok))
;;

let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))
let free reference = node Predef.type_bool (Rexp_ident (Rfree reference))
let path module_id name = Path.Pdot (Path.Pident module_id, name)

let make_refined module_id =
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "view"; rb_type = Predef.type_int }
  in
  let lambda_binder =
    { rb_id = Ident.create_scoped ~scope:1 "argument"; rb_type = Predef.type_int }
  in
  let lambda =
    node
      (arrow Predef.type_int Predef.type_int)
      (Rexp_function
         { arg_label = Nolabel; param = lambda_binder; body = bound lambda_binder })
  in
  let ref_pred =
    node
      Predef.type_bool
      (Rexp_tuple
         [ None, bound ref_view
         ; None, lambda
         ; None, free (Rsibling "sibling")
         ; None, free (Rfun "self")
         ; None, free (Rglobal (path module_id "value"))
         ])
  in
  Btype.newgenty (Trefine { ref_skeleton = Predef.type_int; ref_view; ref_pred })
;;

let refinement type_ =
  match get_desc type_ with
  | Trefine refinement -> refinement
  | _ -> failwith "expected a refinement"
;;

let predicate_parts refinement =
  match refinement.ref_pred.rexp_desc with
  | Rexp_tuple
      [ (_, { rexp_desc = Rexp_ident (Rbound view_occurrence); _ })
      ; ( _
        , { rexp_desc =
              Rexp_function
                { param = lambda_binder
                ; body = { rexp_desc = Rexp_ident (Rbound lambda_occurrence); _ }
                ; _
                }
          ; _
          } )
      ; (_, { rexp_desc = Rexp_ident (Rfree (Rsibling sibling)); _ })
      ; (_, { rexp_desc = Rexp_ident (Rfree (Rfun self)); _ })
      ; (_, { rexp_desc = Rexp_ident (Rfree (Rglobal global)); _ })
      ] -> view_occurrence, lambda_binder.rb_id, lambda_occurrence, sibling, self, global
  | _ -> failwith "unexpected predicate"
;;

(* String heads survive rewriting unchanged; the path head is rewritten. *)
let () =
  let parameter = Ident.create_scoped ~scope:1 "Parameter" in
  let argument = Ident.create_scoped ~scope:1 "Argument" in
  let original = make_refined parameter in
  let substitution = Subst.add_module parameter (Path.Pident argument) Subst.identity in
  let copied = Subst.type_expr substitution original in
  let _, _, _, sibling, self, global = predicate_parts (refinement copied) in
  assert (String.equal sibling "sibling");
  assert (String.equal self "self");
  assert (Path.same global (path argument "value"));
  print_endline "substitution: string heads preserved, path head rewritten"
;;

(* The save (Prepare_for_saving) boundary preserves binder stamps and string heads; the
   subsequent load (for_loading_cmi) freshens binders and still preserves the string
   heads. *)
let () =
  let module_id = Ident.create_scoped ~scope:1 "Saved" in
  let original = make_refined module_id in
  let original_refinement = refinement original in
  Subst.reset_additional_action_id ();
  let saved =
    Subst.type_expr
      (Subst.with_additional_action Prepare_for_saving Subst.identity)
      original
  in
  let saved_refinement = refinement saved in
  assert (Ident.same saved_refinement.ref_view.rb_id original_refinement.ref_view.rb_id);
  let _, _, _, saved_sibling, saved_self, _ = predicate_parts saved_refinement in
  assert (String.equal saved_sibling "sibling");
  assert (String.equal saved_self "self");
  let loaded = Subst.type_expr (Subst.for_loading_cmi ()) saved in
  let loaded_refinement = refinement loaded in
  assert (
    not (Ident.same loaded_refinement.ref_view.rb_id saved_refinement.ref_view.rb_id));
  let _, _, _, loaded_sibling, loaded_self, _ = predicate_parts loaded_refinement in
  assert (String.equal loaded_sibling "sibling");
  assert (String.equal loaded_self "self");
  print_endline "save/load: stamps preserved then freshened, string heads intact"
;;

(* A literal marshal round-trip of the predicate, as a cmi write/read would perform,
   leaves the string heads byte-for-byte intact. *)
let () =
  let module_id = Ident.create_scoped ~scope:1 "Marshalled" in
  let refinement = refinement (make_refined module_id) in
  let bytes = Marshal.to_string refinement.ref_pred [] in
  let restored : R.t = Marshal.from_string bytes 0 in
  let restored = { refinement with ref_pred = restored } in
  let _, _, _, sibling, self, _ = predicate_parts restored in
  assert (String.equal sibling "sibling");
  assert (String.equal self "self");
  print_endline "marshal round-trip: string heads intact"
;;
