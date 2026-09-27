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

let precise_unused_steps = ref false

(* Cleared when a proof is slow enough to warn, so that the warning is reported
   again rather than skipped by the cache. *)
let cacheable = ref true

(* The unused proof steps a unit reported to VOX_UNUSED_STEPS, recorded with
   its cache entry and reported again when the entry is used. *)
let reported_steps = Buffer.create 256

let append_report file text =
  try
    Out_channel.with_open_gen [Open_append; Open_creat; Open_text] 0o644 file
      (fun channel -> output_string channel text)
  with Sys_error _ -> ()

exception Budget_exceeded

(* A test harness collects unused proof steps in a report, like slow proofs,
   whether or not the warning is enabled; VOX_UNUSED_STEPS_PRECISE=1 then
   selects the precise mode. *)
let unused_steps_report () =
  match Sys.getenv_opt "VOX_UNUSED_STEPS" with
  | Some file when file <> "" -> Some file
  | _ -> None

let abstract_multiplication (query : Vox_smt.query) =
  let open Vox_smt in
  let bitwise = ref false and abstract = ref false in
  let rec visit = function
    | Vox_smt.App (op, args) ->
      (match op, args with
      | ( ( Bit_and | Bit_or | Bit_xor | Shift_right_logical | Shift_left
          | Shift_right_arithmetic ),
          _ ) ->
        bitwise := true
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
   sent again when its SMT-LIB text, the solver, the source names of its
   variables and the kind of attempt (batch or single obligation, exact or
   with bitwise operations abstracted) are unchanged: a recorded refutation
   holds counterexample values named after the source and found by a search
   that only single exact obligations make, and neither is in the text. This
   covers termination checks and the unchanged parts of an edited unit, also
   across compiler rebuilds. Unit entries record only clean successes. The
   compiler is identified by a digest of its executable and the solver by the
   version it reports and the platform, both computed once per process; when
   either is unavailable, the caches that need it are not used. *)
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

(* The solver's configured name, the version it reports and the platform it
   runs on. A solver that does not answer [-version] within a few seconds is
   treated as having no version. The platform is part of the identity because
   resource counts differ between builds of one version: the same query took
   821,439 units with Z3 4.16.0 on macOS arm64 and 681,198 on Linux x86-64, so
   a cache shared between machines could otherwise replay a proof or an
   exhausted limit obtained under different counts. *)
let solver_identity =
  lazy
    (let deadline = Unix.gettimeofday () +. 5. in
     match Unix.pipe ~cloexec:true () with
     | exception Unix.Unix_error _ -> None
     | output, input -> (
       let pid =
         try
           let null = Unix.openfile "/dev/null" [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
           Fun.protect
             ~finally:(fun () -> Unix.close null)
             (fun () ->
               Some
                 (Unix.create_process !executable
                    [| !executable; "-version" |]
                    null input null))
         with Unix.Unix_error _ -> None
       in
       Unix.close input;
       let buffer = Buffer.create 64 and bytes = Bytes.create 256 in
       let rec read () =
         let remaining = deadline -. Unix.gettimeofday () in
         if remaining <= 0. || Buffer.length buffer > 4096
         then false
         else
           match Unix.select [output] [] [] remaining with
           | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
           | [], _, _ -> false
           | _ -> (
             match Unix.read output bytes 0 (Bytes.length bytes) with
             | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
             | 0 -> true
             | n ->
               Buffer.add_subbytes buffer bytes 0 n;
               read ())
       in
       let finished = Option.is_some pid && read () in
       Unix.close output;
       match pid with
       | None -> None
       | Some pid ->
         (* The solver may also close its output and keep running. *)
         let rec wait finished =
           match
             Unix.waitpid (if finished then [Unix.WNOHANG] else []) pid
           with
           | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait finished
           | 0, _ when Unix.gettimeofday () < deadline ->
             Unix.sleepf 0.01;
             wait finished
           | 0, _ ->
             (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
             ignore (wait false);
             None
           | _, status -> Some status
         in
         if not finished
         then (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
         let version = String.trim (Buffer.contents buffer) in
         (match wait finished with
         | Some (Unix.WEXITED 0) when finished && version <> "" ->
           Some (String.concat "\000" [!executable; version; Config.host])
         | _ | (exception Unix.Unix_error _) -> None)))

let compiler_digest =
  lazy
    (match Digest.file Sys.executable_name with
    | exception Sys_error _ -> None
    | digest -> Some (Digest.to_hex digest))

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
    match
      ( Lazy.force compiler_digest,
        Lazy.force solver_identity,
        Digest.file !Location.input_name )
    with
    | exception Sys_error _ -> None
    | None, _, _ | _, None, _ -> None
    | Some compiler, Some solver, source ->
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
          ([ "vox-verify-2";
             compiler;
             solver;
             Digest.to_hex source;
             Option.value (Sys.getenv_opt "OCAMLPARAM") ~default:"" ]
          @ (if Option.is_some (unused_steps_report ())
             then
               [ "unused-steps"
                 ^ Option.value ~default:""
                     (Sys.getenv_opt "VOX_UNUSED_STEPS_PRECISE") ]
             else [])
          @ List.sort compare imports
          @ flags (match arguments with _ :: rest -> rest | [] -> []))
      in
      Some (Filename.concat directory (Digest.to_hex (Digest.string key))))

type outcome =
  | Proved of int option  (** with the resources used, when known *)
  | Refuted of string option  (** with the counterexample's source values *)
  | Exhausted
  | Inconclusive of string option

(* VC generation labels symbols that do not name a source variable by their
   role. *)
let source_name label =
  let internal =
    [ "value";
      "reachable";
      "observation";
      "pattern";
      "result";
      "refinement_function";
      "recursive";
      "condition";
      "argument" ]
  in
  label <> ""
  && (not (List.mem label internal))
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       label

(* Z3's models often pick extreme integers, and its builds for different
   platforms pick differently. A countermodel found under extra assumptions is
   still a countermodel of the goal, so look for one with every source integer
   within 0, then 1, 10 and 100 of zero, and keep [model] if there is none.
   Trying the tightest bound first also makes the reported values agree across
   platforms in most cases. *)
let smaller_model check ?resource_limit (query : Vox_smt.query) model =
  let bound small symbol : Vox_smt.term option =
    let v = Vox_smt.Var symbol in
    match Vox_smt.Symbol.sort symbol with
    | Int63 ->
      Some
        (App
           ( And,
             [ App (Le, [Integer (Int64.neg small); v]);
               App (Le, [v; Integer small]) ] ))
    | Int ->
      let bound = Int64.to_string small in
      Some
        (App
           ( And,
             [ App (Int_le, [Big_integer (Int64.to_string (Int64.neg small)); v]);
               App (Int_le, [v; Big_integer bound]) ] ))
    | Bool | Opaque _ | Datatype _ -> None
  in
  let magnitude (symbol, (value : Vox_smt.value)) =
    if not (source_name (Vox_smt.Symbol.label symbol))
    then None
    else
      match value with
      | Int_value n -> Some (Int64.abs n)
      | Bigint_value n ->
        Some
          (match Int64.of_string_opt n with
          | Some n -> Int64.abs n
          | None -> Int64.max_int)
      | Bool_value _ -> None
  in
  let largest = List.fold_left max 0L (List.filter_map magnitude model) in
  let within small =
    match
      List.filter_map
        (fun symbol ->
          if source_name (Vox_smt.Symbol.label symbol)
          then bound small symbol
          else None)
        query.symbols
    with
    | [] -> None
    | first :: rest -> (
      let term =
        List.fold_left (fun a b -> Vox_smt.App (And, [a; b])) first rest
      in
      let query =
        { query with
          Vox_smt.facts =
            query.facts @ [{ Vox_smt.label = "small values"; term }]
        }
      in
      match (check ?resource_limit query : Vox_smt_solver.result).validity with
      | Invalid (Some smaller) -> Some smaller
      | _ -> None)
  in
  let rec search = function
    | small :: rest when small < largest -> (
      match within small with Some smaller -> smaller | None -> search rest)
    | _ -> model
  in
  search [0L; 1L; 10L; 100L]

(* The counterexample restricted to variables named in the source. *)
let counterexample model =
  let show : Vox_smt.value -> string = function
    | Bool_value b -> string_of_bool b
    | Int_value n -> Int64.to_string n
    | Bigint_value n -> n ^ "Z"
  in
  match model with
  | None -> None
  | Some model -> (
    let bindings =
      List.filter_map
        (fun (symbol, value) ->
          let label = Vox_smt.Symbol.label symbol in
          if source_name label then Some (label ^ " = " ^ show value) else None)
        model
    in
    let bindings =
      List.rev
        (List.fold_left
           (fun shown b -> if List.mem b shown then shown else b :: shown)
           [] bindings)
    in
    match bindings with [] -> None | _ -> Some (String.concat ", " bindings))

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
  (* A query with bitwise operations is first tried with them abstracted, which
     avoids bit-blasting every integer in it. Only a proof counts: any other
     outcome is replaced by that of the exact query, which has its own budget.
     The abstract attempt is limited like a batch, so a failed attempt costs at
     most the warning threshold. *)
  let attempt ~exact ~limit query =
    if !dump_vc
    then begin
      Format.eprintf "%a:%s@." Location.print_loc loc
        (if exact then "" else " (bitwise operations abstracted)");
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
      match cache_directory () with
      | None -> None
      | Some directory ->
        Option.map
          (fun solver ->
            let text =
              Vox_smt.to_smtlib ~poll ?resource_limit:limit ~int_width
                ~timeout_ms:!timeout_ms query
            in
            let attempt =
              (if batch then "batch" else "obligation")
              ^ if exact then "" else " abstract"
            in
            let labels =
              List.map Vox_smt.Symbol.label query.Vox_smt.symbols
            in
            (* The version names the entry format: bump it when an outcome
               records more, or is shown differently, so older entries are not
               replayed. *)
            Filename.concat directory
              ("query-5-"
              ^ Digest.to_hex
                  (Digest.string
                     (String.concat "\000"
                        (solver :: attempt :: text :: labels)))))
          (Lazy.force solver_identity)
    in
    (* Resource limits make every outcome except a wall-clock timeout or a
       solver failure reproducible, so failures are cached too. *)
    let recorded =
      match cached with
      | Some file when Sys.file_exists file -> (
        match In_channel.with_open_bin file In_channel.input_all with
        | "proved" -> Some (Proved None)
        | "refuted" -> Some (Refuted None)
        | "exhausted" -> Some Exhausted
        | "unknown" -> Some (Inconclusive None)
        | entry -> (
          match String.index_opt entry ' ' with
          | Some i when String.sub entry 0 i = "proved" ->
            Option.map
              (fun n -> Proved (Some n))
              (int_of_string_opt
                 (String.sub entry (i + 1) (String.length entry - i - 1)))
          | Some i when String.sub entry 0 i = "refuted" ->
            Some
              (Refuted
                 (Some (String.sub entry (i + 1) (String.length entry - i - 1))))
          | Some i when String.sub entry 0 i = "unknown" ->
            Some
              (Inconclusive
                 (Some (String.sub entry (i + 1) (String.length entry - i - 1))))
          | _ -> None)
        | exception Sys_error _ -> None)
      | _ -> None
    in
    match recorded with
    | Some outcome -> outcome
    | None -> (
      let result : Vox_smt_solver.result = check ?resource_limit:limit query in
      if !dump_resources
      then
        Format.eprintf
          "%a: %s%s %s, %s resource units, %.3f s encoding, %.3f s solving@."
          Location.print_loc loc
          (if batch then "batch" else "obligation")
          (if exact then "" else " (bitwise operations abstracted)")
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
        | Vox_smt.Valid -> Some (Proved result.resources)
        | (Unknown _ | Timeout) when exhausted -> Some Exhausted
        | Invalid _ when not exact -> Some (Refuted None)
        | (Timeout | Failure _) when not exact -> None
        | Invalid model when !dump_vc ->
          raise
            (Vox_vc.Unproved
               (Location.errorf ~loc "Refinement could not be proved.\n%s"
                  (Vox_smt.explain_invalid query model)))
        | Invalid (Some model) when not batch ->
          Some
            (Refuted
               (counterexample
                  (Some (smaller_model check ?resource_limit:limit query model))))
        | Invalid model -> Some (Refuted (counterexample model))
        | Unknown reason -> Some (Inconclusive reason)
        | Timeout ->
          raise
            (Vox_vc.Unproved
               (Location.errorf ~loc "Refinement solver timed out"))
        | Failure reason ->
          Location.raise_errorf ~loc "Refinement solver failed: %s" reason
      in
      match outcome with
      | None -> Inconclusive None
      | Some outcome ->
        Option.iter
          (fun file ->
            record_entry file
              (match outcome with
              | Proved None -> "proved"
              | Proved (Some n) -> "proved " ^ string_of_int n
              | Refuted None -> "refuted"
              | Refuted (Some values) -> "refuted " ^ values
              | Exhausted -> "exhausted"
              | Inconclusive None -> "unknown"
              | Inconclusive (Some reason) -> "unknown " ^ reason))
          cached;
        outcome)
  in
  let outcome =
    let exact () = attempt ~exact:true ~limit query in
    match Vox_smt.abstract_bitwise query with
    | None -> exact ()
    | Some abstract -> (
      let limit =
        match positive !resource_warning, limit with
        | Some warning, Some limit -> Some (min warning limit)
        | Some warning, None -> Some warning
        | None, limit -> limit
      in
      match attempt ~exact:false ~limit abstract with
      | Proved _ as proved -> proved
      | Refuted _ | Exhausted | Inconclusive _ -> exact ())
  in
  match outcome with
  | Proved (Some resources)
    when (not batch) && !resource_warning > 0 && resources > !resource_warning
    -> (
    cacheable := false;
    let warning =
      Warnings.Slow_refinement
        { resources; threshold = !resource_warning; limit = !resource_limit }
    in
    (* Resource counts differ between platforms, so a test harness collects slow
       proofs in a report rather than in compiler output. *)
    match Sys.getenv_opt "VOX_SLOW_PROOFS" with
    | Some file when file <> "" ->
      if Warnings.is_active warning
      then
        begin try
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
  | Refuted values ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc "Refinement could not be proved (%s%s)"
            (if abstract_multiplication query
             then "countermodel for abstract multiplication"
             else "counterexample")
            (match values with None -> "" | Some values -> ": " ^ values)))
  | Inconclusive reason ->
    raise
      (Vox_vc.Unproved
         (Location.errorf ~loc "Refinement solver returned unknown%s"
            (match reason with None -> "" | Some r -> ": " ^ r)))

(* The unsat core of a query that was proved, for the unused proof steps check
   (warning 227): the [assumptions] its proof used, or [None] when it is not
   proved with them. A query with bitwise operations is first tried with them
   abstracted, as in [prove]. Outcomes are cached like proofs. The precise
   mode's proofs without a step ([deletion]) get the warning threshold as
   their budget: a step whose removal makes a proof that much slower is worth
   keeping. *)
let core poll check loc (query : Vox_smt.query) ~assumptions ~deletion =
  poll ();
  let int_width = 63 in
  let positive n = if n > 0 then Some n else None in
  let attempt ~exact ~limit query =
    let text () =
      try
        Vox_smt.to_smtlib ~poll ?resource_limit:limit ~assumptions ~int_width
          ~timeout_ms:!timeout_ms query
      with Vox_smt.Sort_error message -> "sort error: " ^ message
    in
    let cached =
      match cache_directory () with
      | None -> None
      | Some directory ->
        Option.map
          (fun solver ->
            let attempt =
              if exact then "unsat core" else "unsat core abstract"
            in
            let labels = List.map Vox_smt.Symbol.label query.Vox_smt.symbols in
            Filename.concat directory
              ("query-5-"
              ^ Digest.to_hex
                  (Digest.string
                     (String.concat "\000"
                        (solver :: attempt :: text () :: labels)))))
          (Lazy.force solver_identity)
    in
    let indexed = List.mapi (fun i symbol -> i, symbol) assumptions in
    let recorded =
      match cached with
      | Some file when Sys.file_exists file -> (
        (* An entry records the resources used, then the core's indices in
           [assumptions]. *)
        match
          String.split_on_char ' '
            (In_channel.with_open_bin file In_channel.input_all)
        with
        | ["unproved"; _] -> Some None
        | "core" :: _ :: indices -> (
          match
            List.map
              (fun i -> List.assoc (int_of_string i) indexed)
              (List.filter (( <> ) "") indices)
          with
          | core -> Some (Some core)
          | exception (Not_found | Failure _) -> None)
        | _ -> None
        | exception Sys_error _ -> None)
      | _ -> None
    in
    match recorded with
    | Some outcome -> outcome
    | None ->
      let result : Vox_smt_solver.result =
        (* The check is a diagnostic: a malformed second query counts its
           steps as used rather than failing the compilation. *)
        try check ?resource_limit:limit ?assumptions:(Some assumptions) query
        with Vox_smt.Sort_error message ->
          { validity = Failure message;
            stderr = "";
            resources = None;
            core = None;
            encoding_seconds = 0.;
            solving_seconds = 0.
          }
      in
      if !dump_resources
      then
        Format.eprintf
          "%a: unsat core%s %s, %s resource units, %.3f s encoding, %.3f s \
           solving@."
          Location.print_loc loc
          (if exact then "" else " (bitwise operations abstracted)")
          (match result.core with
          | Some core ->
            Printf.sprintf "of %d/%d facts" (List.length core)
              (List.length assumptions)
          | None -> validity_name result.validity)
          (match result.resources with
          | Some n -> string_of_int n
          | None -> "?")
          result.encoding_seconds result.solving_seconds;
      let outcome =
        match result.validity, result.core with
        | Valid, Some core -> Some core
        | _ -> None
      in
      (match result.validity with
      | Timeout | Failure _ -> ()
      | Valid | Invalid _ | Unknown _ ->
        Option.iter
          (fun file ->
            let resources =
              match result.resources with
              | Some n -> string_of_int n
              | None -> "?"
            in
            record_entry file
              (match outcome with
              | None -> "unproved " ^ resources
              | Some core ->
                String.concat " "
                  ("core" :: resources
                  :: List.filter_map
                       (fun (i, symbol) ->
                         if List.memq symbol core
                         then Some (string_of_int i)
                         else None)
                       indexed)))
          cached);
      outcome
  in
  let limit =
    match deletion, positive !resource_warning, positive !resource_limit with
    | true, Some warning, Some limit -> Some (min warning limit)
    | true, Some warning, None -> Some warning
    | _, _, limit -> limit
  in
  match Vox_smt.abstract_bitwise query with
  | None -> attempt ~exact:true ~limit query
  | Some abstract -> (
    let abstract_limit =
      match positive !resource_warning, limit with
      | Some warning, Some limit -> Some (min warning limit)
      | Some warning, None -> Some warning
      | None, limit -> limit
    in
    match attempt ~exact:false ~limit:abstract_limit abstract with
    | Some _ as core -> core
    | None -> attempt ~exact:true ~limit query)

let unused_steps poll check =
  let report = unused_steps_report () in
  { Vox_proof_steps.core = core poll check;
    precise = !precise_unused_steps;
    precise_unreported =
      Option.is_some report
      && Sys.getenv_opt "VOX_UNUSED_STEPS_PRECISE" = Some "1";
    all_steps = Option.is_some report;
    abandoned = (function Budget_exceeded -> true | _ -> false);
    report =
      (fun loc warning ->
        (* Reported again rather than skipped by the unit cache. *)
        if Warnings.is_active warning
        then begin
          cacheable := false;
          Location.prerr_warning loc warning
        end
        else
          match report, warning with
          | Some file, Unused_proof_step step ->
            let step =
              match step with
              | Unused_lemma_call name -> "lemma call " ^ name
              | Unused_assume -> "assume_"
              | Unused_argument name -> "argument " ^ name
            in
            let start = loc.Location.loc_start
            and stop = loc.Location.loc_end in
            (* The expect tool names no file and numbers lines from each
               phrase, but its offsets count from the start of the file it
               parsed. *)
            let file_name, position =
              match start.pos_fname, !Location.input_lexbuf with
              | "", Some lexbuf ->
                let text = lexbuf.Lexing.lex_buffer in
                let position (p : Lexing.position) =
                  let line = ref 1 and bol = ref 0 in
                  for i = 0 to min p.pos_cnum (Bytes.length text) - 1 do
                    if Bytes.get text i = '\n'
                    then begin
                      incr line;
                      bol := i + 1
                    end
                  done;
                  !line, p.pos_cnum - !bol
                in
                ( List.fold_left
                    (fun name argument ->
                      let source =
                        Option.value ~default:argument
                          (Filename.chop_suffix_opt ~suffix:".corrected"
                             argument)
                      in
                      if Filename.check_suffix source ".ml"
                      then source
                      else name)
                    "" (Array.to_list Sys.argv),
                  position )
              | name, _ ->
                name, fun p -> p.pos_lnum, p.pos_cnum - p.pos_bol
            in
            let start_line, start_column = position start
            and stop_line, stop_column = position stop in
            let line =
              Printf.sprintf "%s:%d:%d-%d:%d: unused %s\n" file_name
                start_line start_column stop_line stop_column step
            in
            Buffer.add_string reported_steps line;
            append_report file line
          | _ -> ())
  }

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
          (fun check ->
            f poll
              (prove poll (fun ?resource_limit query ->
                   check ?resource_limit query))
              (unused_steps poll check))
      with Budget_exceeded ->
        Location.raise_errorf "Refinement verification budget exhausted"
    in
    Verification.install (fun ~whole_unit structure ->
        if not !assume_verified
        then
          match unit_cache_file ~whole_unit with
          | Some file when Sys.file_exists file -> (
            match
              ( unused_steps_report (),
                In_channel.with_open_bin file In_channel.input_all )
            with
            | Some report, entry -> (
              match String.index_opt entry '\n' with
              | Some i ->
                append_report report
                  (String.sub entry (i + 1) (String.length entry - i - 1))
              | None -> ())
            | None, _ -> ()
            | exception Sys_error _ -> ())
          | file ->
            cacheable := true;
            Buffer.clear reported_steps;
            with_prover (fun poll prove unused_steps ->
                Vox_vc.generate ~poll ~unused_steps ~prove structure);
            if !cacheable
            then
              Option.iter
                (fun file ->
                  record_entry file
                    ("verified\n" ^ Buffer.contents reported_steps))
                file);
    Verification.install_termination (fun ~self ~fn ~measure ->
        if not !assume_verified
        then
          with_prover (fun poll prove unused_steps ->
              Vox_vc.check_termination ~unused_steps ~poll ~prove ~self ~fn
                ~measure ()));
    Clflags.add_arguments __LOC__
      [ ( "-smt-budget",
          Arg.Set_int budget_ms,
          "<ms> Overall budget per verification pass (0: unlimited)" );
        "-dvc", Arg.Set dump_vc, " Dump refinement verification conditions";
        ( "-smt-unused-steps-precise",
          Arg.Set precise_unused_steps,
          " With warning 227 (unused-proof-step), also prove each query again \
           without each proof step in its unsat core, to find more unused \
           steps (slower)" );
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
