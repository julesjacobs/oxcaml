module Name = Flambda2_identifiers.Name
module Variable = Flambda2_identifiers.Variable
module Name_mode = Flambda2_nominal.Name_mode
module Simple = Flambda2_term_basics.Simple
module Target_ocaml_int = Flambda2_numbers.Target_ocaml_int
module T = Flambda2_types__Type_grammar
module MTC = Flambda2_types__More_type_creators
module TE = Flambda2_types__Typing_env
module Level = Flambda2_types__Typing_env_level
module Cache = Flambda2_types__Cached_level
module BT = Flambda2_types__Binding_time
module K = Flambda2_kinds.Flambda_kind

let concrete n =
  let ty =
    T.tag_immediate
      (T.this_naked_immediate (Target_ocaml_int.of_int Sixty_four n))
  in
  match T.get_alias_exn ty with
  | exception Not_found -> ty
  | _ -> failwith "Expected a concrete type without a top-level alias"

let expect_invalid f =
  match f () with
  | exception Misc.Fatal_error -> ()
  | exception Assert_failure _ -> ()
  | _ -> failwith "Expected rejected equation"

let () =
  Clflags.flambda_invariant_checks := Clflags.Heavy_checks;
  let cu = Compilation_unit.of_string "Typing_env_equation_sharing_tests" in
  Env.set_current_unit
    (Unit_info.make_dummy ~input_name:"Typing_env_equation_sharing_tests" cu);
  let var = Variable.create "value" K.value in
  let name = Name.var var in
  let ty = concrete 17 in
  let fresh = concrete 17 in
  assert (fresh != ty);
  assert (Format.asprintf "%a" T.print fresh = Format.asprintf "%a" T.print ty);
  let time = BT.succ BT.earliest_var in
  let cache =
    Cache.add_or_replace_binding Cache.empty name ty time Name_mode.in_types
  in
  let binding = Name.Map.find name (Cache.names_to_types cache) in
  assert (Cache.replace_variable_binding cache var ty == cache);
  assert (Name.Map.find name (Cache.names_to_types cache) == binding);
  let replaced = Cache.replace_variable_binding cache var fresh in
  assert (replaced != cache);
  let replaced_ty, replaced_mode =
    Name.Map.find name (Cache.names_to_types replaced)
  in
  assert (replaced_ty == fresh);
  assert (replaced_mode == snd binding);
  assert (BT.equal (BT.With_name_mode.binding_time replaced_mode) time);
  assert (
    Name_mode.equal
      (BT.With_name_mode.name_mode replaced_mode)
      Name_mode.in_types);
  let missing = Name.var (Variable.create "missing" K.value) in
  let unknown = MTC.unknown K.value in
  let level = Level.add_or_replace_equation Level.empty name ty in
  assert (Level.add_or_replace_equation level name ty == level);
  assert (Level.add_or_replace_equation level name fresh != level);
  assert (Level.add_or_replace_equation level missing unknown == level);
  let unknown_level = Level.add_or_replace_equation level name unknown in
  assert (
    Level.add_or_replace_equation unknown_level name unknown == unknown_level);
  let recursive = T.alias_type_of K.value (Simple.name name) in
  expect_invalid (fun () -> Level.add_or_replace_equation level name recursive);
  let calls = ref 0 in
  let env =
    TE.create ~machine_width:Sixty_four ~resolver:(fun _ ->
        incr calls;
        None)
  in
  let env = TE.add_variable_definition env var K.value Name_mode.normal in
  let initial = TE.find env name None in
  let updated = TE.replace_equation env name ty in
  assert (TE.replace_equation updated name ty == updated);
  assert (TE.find env name None == initial);
  let revised = TE.replace_equation updated name fresh in
  assert (revised != updated);
  assert (TE.find revised name None == fresh);
  assert (TE.find updated name None == ty);
  let scoped = TE.increment_scope revised in
  let scoped = TE.replace_equation scoped name fresh in
  assert (TE.replace_equation scoped name fresh == scoped);
  assert (TE.find revised name None == fresh);
  expect_invalid (fun () -> TE.replace_equation revised name recursive);
  let env = ref revised in
  for n = 0 to 999 do
    let old = !env in
    let previous = TE.find old name None in
    let next = if n mod 5 = 0 then unknown else concrete n in
    env := TE.replace_equation old name next;
    assert (TE.find old name None == previous);
    assert (TE.find !env name None == next);
    assert (TE.replace_equation !env name next == !env);
    assert (TE.mem ~min_name_mode:Name_mode.normal !env name)
  done;
  assert (!calls = 0)
