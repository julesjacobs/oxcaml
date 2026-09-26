let installed = ref false

let dump_vc = ref false

let dump_smtlib = ref false

let dump_resources = ref false

let executable = ref Vox_smt_solver.default_config.executable

(* Proofs are bounded by Z3's resource count, which does not depend on machine
   load; the wall-clock deadline is only a backstop. Z3 4.16 spends about 8
   million units per second on an Apple M4 Max core, so the defaults are about
   0.1 s (warning) and 5 s (limit). Almost every library obligation needs under
   0.1 s; one that needs more usually has an encoding problem. *)
let resource_warning = ref 1_000_000

let resource_limit = ref 40_000_000

let timeout_ms = ref 60_000

let budget_ms = ref 0

let assume_verified = ref false

(* Cleared when a proof is slow enough to warn, so that the warning is reported
   again rather than skipped by the cache. *)
let cacheable = ref true

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

(* With VOX_VERIFY_CACHE set, verification results are cached at two levels. A
   unit is not verified again when the compiler, the source, the interfaces it
   imports, the flags and the solver are all unchanged. A solver query is not
   sent again when its SMT-LIB text and the solver are unchanged; this covers
   termination checks and the unchanged parts of an edited unit. Only clean
   successes are recorded. *)
let cache_directory () =
  match Sys.getenv_opt "VOX_VERIFY_CACHE" with
  | None | Some "" -> None
  | Some _ when !dump_vc || !dump_smtlib || !dump_resources -> None
  | directory -> directory

let record_entry file contents =
  try
    let directory = Filename.dirname file in
    if not (Sys.file_exists directory) then Sys.mkdir directory 0o755;
    let temporary = Filename.temp_file ~temp_dir:directory "entry" ".tmp" in
    Out_channel.with_open_bin temporary (fun channel ->
        output_string channel contents);
    Sys.rename temporary file
  with Sys_error _ -> ()

let unit_cache_file ~whole_unit =
  let arguments = Array.to_list Sys.argv in
  match cache_directory () with
  | None -> None
  (* Only a whole unit compiled from a file named on the command line is keyed
     by its inputs. A toplevel phrase's environment includes earlier phrases,
     which are not part of this key. *)
  | Some _ when not (whole_unit && List.mem !Location.input_name arguments) ->
    None
  | Some directory -> (
    match Unix.stat Sys.executable_name, Digest.file !Location.input_name with
    | exception (Unix.Unix_error _ | Sys_error _) -> None
    | compiler, source ->
      let rec flags = function
        | ("-o" | "-I" | "-use-runtime") :: _ :: rest -> flags rest
        | argument :: rest
          when String.length argument > 0
               && argument.[0] <> '-'
               && Sys.file_exists argument ->
          flags rest
        | argument :: rest -> argument :: flags rest
        | [] -> []
      in
      let imports =
        List.map
          (fun import ->
            Compilation_unit.Name.to_string (Import_info.name import)
            ^ "="
            ^ Option.value (Import_info.crc import) ~default:"")
          (Env.imports ())
      in
      let key =
        String.concat "\000"
          ([ "vox-verify-1";
             Sys.executable_name;
             string_of_int compiler.st_size;
             Printf.sprintf "%.6f" compiler.st_mtime;
             Digest.to_hex source;
             !executable;
             Option.value (Sys.getenv_opt "OCAMLPARAM") ~default:"" ]
          @ List.sort compare imports
          @ flags (match arguments with _ :: rest -> rest | [] -> []))
      in
      Some (Filename.concat directory (Digest.to_hex (Digest.string key))))

type outcome =
  | Proved of int option  (** with the resources used, when known *)
  | Refuted
  | Exhausted
  | Inconclusive of string option

let validity_name : Vox_smt.validity -> string = function
  | Valid -> "valid"
  | Invalid _ -> "invalid"
  | Unknown _ -> "unknown"
  | Timeout -> "timeout"
  | Failure _ -> "failure"

let prove poll check ~batch loc query =
  poll ();
  let int_width = if Target_system.is_64_bit () then 63 else 31 in
  if int_width <> 63
  then
    Location.raise_errorf ~loc
      "Refinement verification requires a 63-bit integer target";
  if !timeout_ms <= 0
  then Location.raise_errorf ~loc "Refinement solver timeout must be positive";
  if !resource_limit < 0 || !resource_warning < 0
  then
    Location.raise_errorf ~loc "Refinement resource bounds must be nonnegative";
  let positive n = if n > 0 then Some n else None in
  (* A batch that is not proved within the warning threshold is retried one
     obligation at a time, so slow obligations are reported where they are. *)
  let limit =
    match batch, positive !resource_warning, positive !resource_limit with
    | true, Some warning, Some limit -> Some (min warning limit)
    | true, Some warning, None -> Some warning
    | _, _, limit -> limit
  in
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
      (Vox_smt.to_smtlib ~poll ?resource_limit:limit ~int_width
         ~timeout_ms:!timeout_ms query)
  end;
  let cached =
    Option.map
      (fun directory ->
        let text =
          Vox_smt.to_smtlib ~poll ?resource_limit:limit ~int_width
            ~timeout_ms:!timeout_ms query
        in
        Filename.concat directory
          ("query-"
          ^ Digest.to_hex (Digest.string (!executable ^ "\000" ^ text))))
      (cache_directory ())
  in
  (* Resource limits make every outcome except a wall-clock timeout or a solver
     failure reproducible, so failures are cached too. *)
  let recorded =
    match cached with
    | Some file when Sys.file_exists file -> (
      match In_channel.with_open_bin file In_channel.input_all with
      | "proved" -> Some (Proved None)
      | "refuted" -> Some Refuted
      | "exhausted" -> Some Exhausted
      | "unknown" -> Some (Inconclusive None)
      | entry -> (
        match String.index_opt entry ' ' with
        | Some i when String.sub entry 0 i = "proved" ->
          Option.map
            (fun n -> Proved (Some n))
            (int_of_string_opt
               (String.sub entry (i + 1) (String.length entry - i - 1)))
        | Some i when String.sub entry 0 i = "unknown" ->
          Some
            (Inconclusive
               (Some (String.sub entry (i + 1) (String.length entry - i - 1))))
        | _ -> None)
      | exception Sys_error _ -> None)
    | _ -> None
  in
  let outcome =
    match recorded with
    | Some outcome -> outcome
    | None ->
      let result : Vox_smt_solver.result = check ?resource_limit:limit query in
      if !dump_resources
      then
        Format.eprintf
          "%a: %s %s, %s resource units, %.3f s encoding, %.3f s solving@."
          Location.print_loc loc
          (if batch then "batch" else "obligation")
          (validity_name result.validity)
          (match result.resources with
          | Some n -> string_of_int n
          | None -> "?")
          result.encoding_seconds result.solving_seconds;
      let exhausted =
        match limit, result.resources with
        | Some limit, Some resources -> resources >= limit
        | _ -> false
      in
      let outcome =
        match result.validity with
        | Vox_smt.Valid -> Proved result.resources
        | (Unknown _ | Timeout) when exhausted -> Exhausted
        | Invalid model when !dump_vc ->
          raise
            (Vox_vc.Unproved
               (Location.errorf ~loc "Refinement could not be proved.\n%s"
                  (Vox_smt.explain_invalid query model)))
        | Invalid _ -> Refuted
        | Unknown reason -> Inconclusive reason
        | Timeout ->
          raise
            (Vox_vc.Unproved
               (Location.errorf ~loc "Refinement solver timed out"))
        | Failure reason ->
          Location.raise_errorf ~loc "Refinement solver failed: %s" reason
      in
      Option.iter
        (fun file ->
          record_entry file
            (match outcome with
            | Proved None -> "proved"
            | Proved (Some n) -> "proved " ^ string_of_int n
            | Refuted -> "refuted"
            | Exhausted -> "exhausted"
            | Inconclusive None -> "unknown"
            | Inconclusive (Some reason) -> "unknown " ^ reason))
        cached;
      outcome
  in
  match outcome with
  | Proved (Some resources)
    when (not batch) && !resource_warning > 0 && resources > !resource_warning
    ->
    cacheable := false;
    let warning =
      Warnings.Slow_refinement
        { resources; threshold = !resource_warning; limit = !resource_limit }
    in
    (* Resource counts differ between platforms, so a test harness collects
       slow proofs in a report rather than in compiler output. *)
    (match Sys.getenv_opt "VOX_SLOW_PROOFS" with
    | Some file when file <> "" ->
      if Warnings.is_active warning
      then begin
        try
          Out_channel.with_open_gen [Open_append; Open_creat; Open_text]
            0o644 file (fun channel ->
              Printf.fprintf channel "%s:%d: %d resource units\n"
                loc.Location.loc_start.Lexing.pos_fname
                loc.Location.loc_start.Lexing.pos_lnum resources)
        with Sys_error _ -> ()
      end
    | _ -> Location.prerr_warning loc warning)
  | Proved _ -> ()
  | Exhausted ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc
            "Refinement solver exceeded its resource limit (%d units)"
            (Option.value limit ~default:0)))
  | Refuted ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc "Refinement could not be proved (%s)"
            (if abstract_multiplication query
             then "countermodel for abstract multiplication"
             else "counterexample")))
  | Inconclusive reason ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc "Refinement solver returned unknown%s"
            (match reason with None -> "" | Some r -> ": " ^ r)))

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
    Verification.install (fun ~whole_unit structure ->
        if not !assume_verified
        then
          match unit_cache_file ~whole_unit with
          | Some file when Sys.file_exists file -> ()
          | file ->
            cacheable := true;
            with_prover (fun poll prove ->
                Vox_vc.generate ~poll ~prove structure);
            if !cacheable
            then Option.iter (fun file -> record_entry file "verified") file);
    Verification.install_termination (fun ~self ~fn ~measure ->
        if not !assume_verified
        then
          with_prover (fun poll prove ->
              Vox_vc.check_termination ~poll ~prove ~self ~fn ~measure));
    Clflags.add_arguments __LOC__
      [ ( "-smt-budget",
          Arg.Set_int budget_ms,
          "<ms> Overall budget per verification pass (0: unlimited)" );
        "-dvc", Arg.Set dump_vc, " Dump refinement verification conditions";
        ( "-smt-assume-verified",
          Arg.Set assume_verified,
          " Skip refinement verification; for build scripts that verify the \
           same unit in another compilation" );
        ( "-dsmt-resources",
          Arg.Set dump_resources,
          " Print the solver resources and time used by each refinement query" );
        ( "-smt-resource-warning",
          Arg.Set_int resource_warning,
          Printf.sprintf
            "<n> Warn when a refinement proof uses more than n solver resource \
             units (default %d; 0: never)"
            !resource_warning );
        ( "-smt-resource-limit",
          Arg.Set_int resource_limit,
          Printf.sprintf
            "<n> Fail a refinement proof that uses more than n solver resource \
             units (default %d; 0: unlimited)"
            !resource_limit );
        ( "-dsmtlib",
          Arg.Set dump_smtlib,
          " Dump commands sent to the refinement solver" );
        ( "-smt-solver",
          Arg.Set_string executable,
          "<path> Refinement solver executable (default z3)" );
        ( "-smt-timeout",
          Arg.Set_int timeout_ms,
          Printf.sprintf
            "<ms> Wall-clock deadline per refinement query (default %d)"
            !timeout_ms ) ]
  end
