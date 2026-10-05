(* TEST
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/file_formats";
 native;
*)

let () =
  let hidden = "Hidden_cache" in
  let loads = ref 0 in
  let conversions = ref 0 in
  let missing_available = ref false in
  Persistent_env.Persistent_signature.load :=
    (fun ~allow_hidden:_ ~unit_name ->
      incr loads;
      let head = Compilation_unit.Name.to_string unit_name in
      if head = "Missing_cache" && not !missing_available then None
      else
        let cu =
          Compilation_unit.create Compilation_unit.Prefix.empty unit_name
        in
        let visibility =
          if head = hidden then Load_path.Hidden
          else Load_path.Visible { cmx_guaranteed = false }
        in
        Some
          { filename = head ^ ".cmi";
            visibility;
            cmi =
              { cmi_name = unit_name;
                cmi_kind = Normal { cmi_impl = cu; cmi_arg_for = None };
                cmi_globals = [||];
                cmi_sign = Subst.Lazy.of_signature [], Dynamic;
                cmi_params = [];
                cmi_crcs = [||];
                cmi_flags = [] } });
  let convert _ _ _ ~shape:_ ~address:_ ~flags:_ =
    incr conversions;
    ref !conversions
  in
  let find ?(allow_hidden = false) env name =
    Persistent_env.find ~allow_hidden env convert name ~allow_excess_args:true
  in
  let must_be_missing f =
    match f () with
    | _ -> failwith "Expected Not_found"
    | exception Not_found -> ()
  in
  let env = Persistent_env.empty () in
  let other_env = Persistent_env.empty () in
  let a = Global_module.Name.create_no_args "Cache_a" in
  let b = Global_module.Name.create_no_args "Cache_A" in
  let first_a = find env a in
  let first_b = find env b in
  for _ = 1 to 10 do
    assert (find env a == first_a);
    assert (find env b == first_b);
    assert (Persistent_env.find_in_cache env a = Some first_a)
  done;
  assert (!loads = 2 && !conversions = 2);
  let equal_a = Global_module.Name.create_no_args "Cache_a" in
  assert (a != equal_a);
  assert (find env equal_a == first_a);
  assert (find env a == first_a);
  assert (!loads = 2 && !conversions = 2);
  let other_a = find other_env a in
  assert (first_a != other_a);
  assert (find env a == first_a);
  assert (find other_env a == other_a);
  let hidden_name = Global_module.Name.create_no_args hidden in
  let hidden_value = find ~allow_hidden:true env hidden_name in
  for _ = 1 to 10 do
    must_be_missing (fun () -> find env hidden_name);
    assert (find ~allow_hidden:true env hidden_name == hidden_value)
  done;
  let missing = Global_module.Name.create_no_args "Missing_cache" in
  must_be_missing (fun () -> find ~allow_hidden:true env missing);
  missing_available := true;
  must_be_missing (fun () -> find ~allow_hidden:true env missing);
  Persistent_env.clear_missing env;
  ignore (find env missing);
  Persistent_env.clear env;
  assert (Persistent_env.find_in_cache env a = None);
  let second_a = find env a in
  assert (second_a != first_a);
  assert (find env a == second_a);
  assert (find other_env a == other_a)
