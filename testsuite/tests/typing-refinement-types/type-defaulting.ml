(* TEST
 flags = "-extension refinement_types";
 include ocamlcommon;
 expect;
*)

open Types

let () =
  let variable () = Ctype.new_rep_var ~why:Jkind.History.Function_argument () in
  let child, child_sort = variable () in
  let argument_mode = Mode.Alloc.newvar () in
  let return_mode = Mode.Alloc.newvar () in
  let arrow = Ctype.newty (Tarrow
    ((Nolabel, argument_mode, return_mode, None),
     Ctype.newmono child, child, commu_ok))
  in
  Ctype.default_mode_and_jkind_variables_in_node arrow;
  assert (Mode.Alloc.to_const_exn argument_mode = Mode.Alloc.Const.legacy);
  assert (Mode.Alloc.to_const_exn return_mode = Mode.Alloc.Const.legacy);
  assert (Jkind.Sort.to_const_opt child_sort = None);
  Ctype.remove_mode_and_jkind_variables arrow;
  assert (Jkind.Sort.to_const_opt child_sort =
    Some Jkind.Sort.Const.scannable);
  let jkind, sort = Jkind.of_new_sort_var
    ~why:Jkind.History.Function_argument ~level:(Ctype.get_current_level ())
  in
  let universal = Ctype.newty (Tunivar {name = None; jkind}) in
  Ctype.default_mode_and_jkind_variables_in_node universal;
  assert (Jkind.Sort.to_const_opt sort = Some Jkind.Sort.Const.scannable);
  let payload, payload_sort = variable () in
  let evidence, evidence_sort = variable () in
  let binder = Ident.create_local "x" in
  let predicate =
    { rexp_desc = Rexp_var binder;
      rexp_type = evidence; rexp_loc = Location.none }
  in
  let refinement = Ctype.newty (Trefine
    {ref_binder = binder; ref_payload = payload; ref_pred = predicate})
  in
  Ctype.remove_mode_and_jkind_variables refinement;
  assert (Jkind.Sort.to_const_opt payload_sort =
    Some Jkind.Sort.Const.scannable);
  assert (Jkind.Sort.to_const_opt evidence_sort = None);
  let payload, payload_sort = variable () in
  let syntax = Parse.expression (Lexing.from_string "let _y = x in true") in
  ignore (!Typetexp.type_refinement_predicate (Lazy.force Env.initial)
    Ident.Set.empty binder payload syntax);
  assert (Jkind.Sort.to_const_opt payload_sort = None);
  let argument_mode = Mode.Alloc.newvar () in
  let return_mode = Mode.Alloc.newvar () in
  let payload = Ctype.newty (Tarrow
    ((Nolabel, argument_mode, return_mode, None),
     Ctype.newmono Predef.type_int, Predef.type_int, commu_ok))
  in
  ignore (!Typetexp.type_refinement_predicate (Lazy.force Env.initial)
    Ident.Set.empty binder payload syntax);
  assert (Mode.Alloc.zap_to_ceil argument_mode = Mode.Alloc.Const.max);
  assert (Mode.Alloc.zap_to_ceil return_mode = Mode.Alloc.Const.max);;
[%%expect{|
|}]
