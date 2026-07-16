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

let bound binder =
  node binder.rb_type (Rexp_ident (Rbound binder.rb_id))

let predicate binder head =
  let function_type = arrow binder.rb_type Predef.type_bool in
  let function_ = node function_type (Rexp_ident (Rfree (Rfun head))) in
  node Predef.type_bool
    (Rexp_apply (function_, [Nolabel, bound binder]))

let refined skeleton binder_name head =
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 binder_name; rb_type = skeleton }
  in
  let ref_pred = predicate ref_view head in
  Btype.newgenty (Trefine { ref_skeleton = skeleton; ref_view; ref_pred })

let refinement_exn type_ =
  match get_desc type_ with
  | Trefine refinement -> refinement
  | _ -> failwith "expected Trefine"

let expect_unify name left right =
  match Ctype.unify Env.empty left right with
  | () -> Printf.printf "unify accepts: %s\n" name
  | exception Ctype.Unify _ -> failwith ("unify rejected " ^ name)

let expect_unify_failure name left right =
  match Ctype.unify Env.empty left right with
  | () -> failwith ("unify unexpectedly accepted " ^ name)
  | exception Ctype.Unify _ -> Printf.printf "unify rejects: %s\n" name

let () =
  expect_unify
    "alpha-equivalent view binders"
    (refined Predef.type_int "left" "positive")
    (refined Predef.type_int "right" "positive");
  expect_unify_failure
    "different predicates"
    (refined Predef.type_int "value" "positive")
    (refined Predef.type_int "value" "nonzero");
  expect_unify_failure
    "refined versus unrefined"
    (refined Predef.type_int "value" "positive")
    Predef.type_int

let cyclic_refined head =
  let cycle =
    Btype.newgenvar
      (Jkind.Builtin.value ~why:(Jkind.History.Unknown "unification cycle"))
  in
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "cycle"; rb_type = cycle }
  in
  let ref_pred = predicate ref_view head in
  set_type_desc cycle
    (Trefine { ref_skeleton = cycle; ref_view; ref_pred });
  cycle

let () =
  expect_unify
    "distinct equal cyclic refinements"
    (cyclic_refined "cyclic")
    (cyclic_refined "cyclic");
  expect_unify_failure
    "distinct unequal cyclic refinements"
    (cyclic_refined "left")
    (cyclic_refined "right")

let () =
  let generic =
    Btype.newgenvar
      (Jkind.Builtin.value ~why:(Jkind.History.Unknown "refinement test"))
  in
  let original = refined generic "value" "property" in
  let original_refinement = refinement_exn original in
  let copied = Ctype.instance original in
  let copied_refinement = refinement_exn copied in
  if eq_type copied_refinement.ref_skeleton generic then
    failwith "instance shared a generic skeleton";
  if not
       (eq_type
          copied_refinement.ref_skeleton copied_refinement.ref_view.rb_type)
  then failwith "instance detached the view type from its skeleton";
  if not
       (Ident.same
          original_refinement.ref_view.rb_id copied_refinement.ref_view.rb_id)
  then failwith "ordinary instance freshened the refinement view binder";
  let occurrence_type =
    match copied_refinement.ref_pred.rexp_desc with
    | Rexp_apply (_, [_, { rexp_desc = Rexp_ident (Rbound _); rexp_type; _ }]) ->
      rexp_type
    | _ -> failwith "unexpected copied predicate"
  in
  if not (eq_type occurrence_type copied_refinement.ref_skeleton) then
    failwith "instance did not map a predicate type annotation";
  print_endline "instance: types copied consistently; binder stamp preserved"

let () =
  let type_ = refined Predef.type_int "value" "positive" in
  let refinement = refinement_exn type_ in
  let direct_children = ref [] in
  Btype.iter_type_expr
    (fun child -> direct_children := child :: !direct_children)
    type_;
  let require name type_ =
    if not (List.memq type_ !direct_children) then
      failwith ("missing refinement graph child: " ^ name)
  in
  require "skeleton" refinement.ref_skeleton;
  require "view" refinement.ref_view.rb_type;
  require "predicate root" refinement.ref_pred.rexp_type;
  begin match
    R.validate
      ~equal_type:eq_type
      ~bool_type:Predef.type_bool
      ~binders:[refinement.ref_view]
      refinement.ref_pred
  with
  | Ok () -> ()
  | Error error ->
    failwith (Format.asprintf "invalid test refinement: %a"
                R.print_validation_error error)
  end;
  print_endline "graph traversal: skeleton, view, and predicate annotations"

let () =
  let variable =
    Ctype.newvar
      (Jkind.Builtin.value ~why:(Jkind.History.Unknown "refinement occurs"))
  in
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "value";
      rb_type = Predef.type_int;
    }
  in
  let ref_pred = node variable (Rexp_constant (Const_int 0)) in
  let containing =
    Btype.newgenty
      (Trefine
         { ref_skeleton = Predef.type_int; ref_view; ref_pred })
  in
  begin match Ctype.unify Env.empty variable containing with
  | () -> failwith "occurs check ignored a refinement predicate annotation"
  | exception Ctype.Unify _ -> ()
  end;
  print_endline "occurs check: predicate annotation participates"

let () =
  let cycle =
    Btype.newgenvar
      (Jkind.Builtin.value ~why:(Jkind.History.Unknown "refinement cycle"))
  in
  let ref_view =
    { rb_id = Ident.create_scoped ~scope:1 "cycle"; rb_type = cycle }
  in
  let ref_pred = predicate ref_view "cyclic" in
  set_type_desc cycle
    (Trefine { ref_skeleton = cycle; ref_view; ref_pred });
  with_type_mark (fun mark -> Btype.mark_type mark cycle);
  let copied = Ctype.instance cycle in
  let copied_refinement = refinement_exn copied in
  if not (eq_type copied copied_refinement.ref_skeleton) then
    failwith "cyclic instance did not preserve its back edge";
  print_endline "rectypes: traversal and instance are cycle-safe"

let () =
  let type_ = refined Predef.type_int "value" "positive" in
  let printed =
    Printtyp.wrap_printing_env ~error:false Env.empty (fun () ->
      Format.asprintf "%a" Printtyp.type_expr type_)
  in
  Printf.printf "printer: %s\n" printed
