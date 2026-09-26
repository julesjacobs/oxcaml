let installed = ref false

let dump_vc = ref false

let dump_smtlib = ref false

let executable = ref Vox_smt_solver.default_config.executable

let timeout_ms = ref Vox_smt_solver.default_config.timeout_ms

let budget_ms = ref 0

exception Budget_exceeded

let abstract_multiplication (query : Vox_smt.query) =
  let open Vox_smt in
  let bitwise = ref false and abstract = ref false in
  let rec visit = function
    | Vox_smt.App (op, args) ->
      (match op, args with
      | (Bit_and | Bit_or | Bit_xor | Shift_right_logical), _ -> bitwise := true
      | Mul, [Integer _; _] | Mul, [_; Integer _] -> ()
      | Mul, _ -> abstract := true
      | _ -> ());
      List.iter visit args
    | Call (_, args) | Construct (_, args) -> List.iter visit args
    | Is (_, term) | Select (_, _, term) -> visit term
    | Boolean _ | Integer _ | Big_integer _ | Var _ -> ()
  in
  List.iter (fun f -> visit f.Vox_smt.term) (query.Vox_smt.goal :: query.facts);
  !abstract && not !bitwise

let prove poll check loc query =
  poll ();
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
          (match Vox_smt.Symbol.sort s with
          | Bool -> "bool"
          | Int63 -> "int"
          | Int -> "bigint"
          | Opaque _ -> "opaque"
          | Datatype datatype -> Vox_smt.Datatype.label datatype))
      query.Vox_smt.symbols;
    List.iteri
      (fun i f -> Format.eprintf "  f%d: %s@." i (Vox_smt.Function.label f))
      query.Vox_smt.functions;
    Format.eprintf "%s@."
      (Vox_smt.to_smtlib ~poll ~int_width ~timeout_ms:!timeout_ms query)
  end;
  let result : Vox_smt_solver.result = check query in
  match result.validity with
  | Vox_smt.Valid -> ()
  | Invalid model ->
    raise
      (Vox_vc.Unproved
         (if !dump_vc
          then
            Location.errorf ~loc "Refinement could not be proved.\n%s"
              (Vox_smt.explain_invalid query model)
          else
            Location.errorf ~loc "Refinement could not be proved (%s)"
              (if abstract_multiplication query
               then "countermodel for abstract multiplication"
               else "counterexample")))
  | Unknown reason ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc "Refinement solver returned unknown%s"
            (match reason with None -> "" | Some r -> ": " ^ r)))
  | Timeout ->
    raise (Vox_vc.Unproved (Location.errorf ~loc "Refinement solver timed out"))
  | Failure reason ->
    Location.raise_errorf ~loc "Refinement solver failed: %s" reason

let install () =
  if not !installed
  then begin
    installed := true;
    let with_prover f =
      let int_width = if Target_system.is_64_bit () then 63 else 31 in
      let dump =
        if !dump_smtlib
        then Some (fun bytes -> Format.eprintf "%s%!" bytes)
        else None
      in
      if !budget_ms < 0
      then
        Location.raise_errorf
          "Refinement verification budget must be nonnegative";
      let started = Vox_smt_solver.monotonic_time () in
      let poll () =
        if
          !budget_ms > 0
          && (Vox_smt_solver.monotonic_time () -. started) *. 1000.
             >= float !budget_ms
        then raise Budget_exceeded
      in
      try
        Vox_smt_solver.with_session
          ~config:{ executable = !executable; timeout_ms = !timeout_ms }
          ~cancelled:(fun () ->
            poll ();
            false)
          ?dump ~int_width
          (fun check -> f poll (prove poll check))
      with Budget_exceeded ->
        Location.raise_errorf "Refinement verification budget exhausted"
    in
    Verification.install (fun structure ->
        with_prover (fun poll prove -> Vox_vc.generate ~poll ~prove structure));
    Verification.install_termination (fun ~self ~fn ~measure ->
        with_prover (fun poll prove ->
            Vox_vc.check_termination ~poll ~prove ~self ~fn ~measure));
    Clflags.add_arguments __LOC__
      [ ( "-smt-budget",
          Arg.Set_int budget_ms,
          "<ms> Overall budget per verification pass (0: unlimited)" );
        "-dvc", Arg.Set dump_vc, " Dump refinement verification conditions";
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
