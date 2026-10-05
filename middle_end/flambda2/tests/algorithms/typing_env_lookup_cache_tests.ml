module Name = Flambda2_identifiers.Name
module Symbol = Flambda2_identifiers.Symbol
module Variable = Flambda2_identifiers.Variable
module Name_mode = Flambda2_nominal.Name_mode
module Target_ocaml_int = Flambda2_numbers.Target_ocaml_int
module Tag = Flambda2_kinds.Tag
module T = Flambda2_types__Type_grammar
module TC = Flambda2_types__More_type_creators
module TE = Flambda2_types__Typing_env
module K = Flambda2_kinds.Flambda_kind

let () = Clflags.flambda_invariant_checks := Clflags.Heavy_checks

let set_unit name =
  let comp_unit = Compilation_unit.of_string name in
  Env.set_current_unit (Unit_info.make_dummy ~input_name:name comp_unit)

let create_env resolver = TE.create ~resolver ~machine_width:Sixty_four

let block_type n =
  TC.immutable_block ~machine_width:Sixty_four ~is_unique:false Tag.zero
    ~shape:(K.Block_shape.Scannable Value_only)
    Flambda2_bound_identifiers.Alloc_mode.For_types.heap
    ~fields:[T.this_tagged_immediate (Target_ocaml_int.of_int Sixty_four n)]

let () =
  set_unit "Typing_env_lookup_cache_tests";
  let env = create_env (fun _ -> None) in
  let var = Variable.create "value" K.value in
  let name = Name.var var in
  let env = TE.add_variable_definition env var K.value Name_mode.normal in
  let original = TE.find env name None in
  let ty = block_type 17 in
  let updated = TE.replace_equation env name ty in
  let bottom = TE.make_bottom updated in
  let closure = TE.closure_env updated in
  for _ = 1 to 100 do
    assert (TE.find updated name None == ty);
    assert (TE.find env name None == original);
    assert (TE.find updated name (Some K.value) == ty);
    assert (T.is_obviously_bottom (TE.find bottom name (Some K.value)));
    assert (TE.find updated name None == ty);
    assert (TE.mem ~min_name_mode:Name_mode.normal updated name);
    assert (not (TE.mem ~min_name_mode:Name_mode.normal closure name));
    assert (TE.mem ~min_name_mode:Name_mode.in_types closure name);
    assert (TE.mem ~min_name_mode:Name_mode.normal updated name)
  done;
  (match TE.find updated name (Some K.naked_immediate) with
  | exception Misc.Fatal_error -> ()
  | _ -> failwith "Expected kind mismatch after cached lookup");
  let missing = Name.var (Variable.create "missing" K.value) in
  assert (not (TE.mem env missing));
  (match TE.find env missing None with
  | exception Misc.Fatal_error -> ()
  | _ -> failwith "Expected missing local variable lookup to fail");
  TE.reset_lookup_cache ();
  assert (TE.find updated name None == ty);
  set_unit "Typing_env_lookup_cache_import";
  let imported = Name.var (Variable.create "imported" K.value) in
  set_unit "Typing_env_lookup_cache_tests";
  let calls = ref 0 in
  let imported_env =
    create_env (fun _ ->
        incr calls;
        None)
  in
  for _ = 1 to 10 do
    ignore (TE.find imported_env imported (Some K.value))
  done;
  assert (!calls = 10);
  TE.reset_lookup_cache ()

let () =
  let env = create_env (fun _ -> None) in
  let env, bindings =
    List.fold_left
      (fun (env, bindings) n ->
        let var = Variable.create (string_of_int n) K.value in
        let name = Name.var var in
        let ty = block_type n in
        let env = TE.add_variable_definition env var K.value Name_mode.normal in
        TE.replace_equation env name ty, (name, ty) :: bindings)
      (env, []) (List.init 64 Fun.id)
  in
  for _ = 1 to 3 do
    List.iter
      (fun (name, ty) ->
        assert (TE.find env name None == ty);
        assert (TE.mem env name))
      bindings
  done;
  let symbol =
    Symbol.create
      (Current_unit.get_cu_exn ())
      (Linkage_name.of_string "late_symbol")
  in
  let name = Name.symbol symbol in
  for _ = 1 to 2 do
    match TE.find env name None with
    | exception Misc.Fatal_error -> ()
    | _ -> failwith "Expected repeated missing symbol lookup to fail"
  done;
  let env_with_symbol = TE.add_symbol_definition env symbol in
  assert (T.is_obviously_unknown (TE.find env_with_symbol name None));
  (match TE.find env name None with
  | exception Misc.Fatal_error -> ()
  | _ -> failwith "Expected missing symbol in original environment");
  set_unit "Typing_env_lookup_cache_foreign";
  let var = Variable.create "foreign" K.value in
  let name = Name.var var in
  set_unit "Typing_env_lookup_cache_tests";
  let calls = ref 0 in
  let env =
    create_env (fun _ ->
        incr calls;
        None)
  in
  ignore (TE.find env name None);
  assert (!calls = 1);
  set_unit "Typing_env_lookup_cache_foreign";
  (match TE.find env name None with
  | exception Misc.Fatal_error -> ()
  | _ -> failwith "Expected current-unit variable lookup to fail");
  assert (!calls = 1);
  let symbol =
    Symbol.create
      (Current_unit.get_cu_exn ())
      (Linkage_name.of_string "foreign_symbol")
  in
  set_unit "Typing_env_lookup_cache_tests";
  let imported = ref None in
  let calls = ref 0 in
  let env =
    create_env (fun _ ->
        incr calls;
        !imported)
  in
  let name = Name.symbol symbol in
  ignore (TE.find env name None);
  imported := Some (TE.Serializable.predefined_exceptions Symbol.Set.empty);
  (match TE.find env name None with
  | exception Misc.Fatal_error -> ()
  | _ -> failwith "Expected undefined imported symbol lookup to fail");
  imported
    := Some
         (TE.Serializable.predefined_exceptions (Symbol.Set.singleton symbol));
  ignore (TE.find env name None);
  assert (!calls = 3);
  TE.reset_lookup_cache ()

let[@inline never] cached_type_weak_reference n =
  let env = create_env (fun _ -> None) in
  let var = Variable.create "retained" K.value in
  let name = Name.var var in
  let ty = block_type n in
  let env = TE.add_variable_definition env var K.value Name_mode.normal in
  let env = TE.replace_equation env name ty in
  assert (TE.find env name None == ty);
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some ty);
  weak

let () =
  TE.reset_lookup_cache ();
  let weak =
    (Sys.opaque_identity cached_type_weak_reference)
      (Sys.opaque_identity 1000003)
  in
  Gc.full_major ();
  assert (Weak.check weak 0);
  let env = create_env (fun _ -> None) in
  let var = Variable.create "switch_map" K.value in
  let env = TE.add_variable_definition env var K.value Name_mode.normal in
  ignore (TE.find env (Name.var var) None);
  Gc.full_major ();
  assert (not (Weak.check weak 0));
  let weak =
    (Sys.opaque_identity cached_type_weak_reference)
      (Sys.opaque_identity 1000004)
  in
  Gc.full_major ();
  assert (Weak.check weak 0);
  Flambda2.reset_symbol_tables ();
  Gc.full_major ();
  assert (not (Weak.check weak 0));
  set_unit "Typing_env_lookup_cache_tests_after_reset";
  let env = create_env (fun _ -> None) in
  let var = Variable.create "value" K.value in
  let env = TE.add_variable_definition env var K.value Name_mode.normal in
  assert (T.is_obviously_unknown (TE.find env (Name.var var) None));
  TE.reset_lookup_cache ()
