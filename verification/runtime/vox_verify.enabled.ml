let installed = ref false

let dump_vc = ref false

let dump_smtlib = ref false

let executable = ref Vox_smt_solver.default_config.executable

let timeout_ms = ref Vox_smt_solver.default_config.timeout_ms

let prove check loc query =
  let int_width = if Target_system.is_64_bit () then 63 else 31 in
  if int_width <> 63
  then
    Location.raise_errorf ~loc
      "Refinement verification requires a 63-bit integer target";
  if !timeout_ms <= 0
  then Location.raise_errorf ~loc "Refinement solver timeout must be positive";
  if !dump_vc
  then begin
    Format.eprintf "%a:@." Location.print_loc loc;
    List.iteri
      (fun i s ->
        Format.eprintf "  v%d: %s (%s)@." i (Vox_smt.Symbol.label s)
          (match Vox_smt.Symbol.sort s with Bool -> "bool" | Int63 -> "int"))
      query.Vox_smt.symbols;
    List.iteri
      (fun i f -> Format.eprintf "  f%d: %s@." i (Vox_smt.Function.label f))
      query.Vox_smt.functions;
    Format.eprintf "%s@."
      (Vox_smt.to_smtlib ~int_width ~timeout_ms:!timeout_ms query)
  end;
  let result : Vox_smt_solver.result = check query in
  match result.validity with
  | Vox_smt.Valid -> ()
  | Invalid _ ->
    Location.raise_errorf ~loc "Refinement could not be proved (counterexample)"
  | Unknown reason ->
    Location.raise_errorf ~loc "Refinement solver returned unknown%s"
      (match reason with None -> "" | Some r -> ": " ^ r)
  | Timeout -> Location.raise_errorf ~loc "Refinement solver timed out"
  | Failure reason ->
    Location.raise_errorf ~loc "Refinement solver failed: %s" reason

let install () =
  if not !installed
  then begin
    installed := true;
    let with_prover f =
      let int_width = if Target_system.is_64_bit () then 63 else 31 in
      let dump = if !dump_smtlib then Some (fun bytes -> Format.eprintf "%s%!" bytes) else None in
      Vox_smt_solver.with_session
        ~config:{ executable = !executable; timeout_ms = !timeout_ms }
        ?dump ~int_width (fun check -> f (prove check))
    in
    Verification.install (fun structure -> with_prover (fun prove -> Vox_vc.generate ~prove structure));
    Verification.install_termination (fun ~self ~fn ~measure ->
      with_prover (fun prove -> Vox_vc.check_termination ~prove ~self ~fn ~measure));
    Clflags.add_arguments __LOC__
      [ "-dvc", Arg.Set dump_vc, " Dump refinement verification conditions";
        ( "-dsmtlib",
          Arg.Set dump_smtlib,
          " Dump commands sent to the refinement solver" );
        ( "-smt-solver",
          Arg.Set_string executable,
          "<path> Refinement solver executable (default z3)" );
        ( "-smt-timeout",
          Arg.Set_int timeout_ms,
          "<ms> Refinement solver deadline (default 5000)" ) ]
  end
