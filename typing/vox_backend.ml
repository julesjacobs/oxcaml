type backend =
  | Lean
  | Z3
  | Oxsmt

type selection =
  | Single of backend
  | Cross

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Unknown
  | Solver_error
  | Unavailable

type fact_usage_capability =
  | Fact_usage
  | No_fact_usage

type capabilities =
  { fact_usage : fact_usage_capability;
  }

type obligation =
  { env : Env.t;
    condition : Vox_vc.t;
    prove_contents : string option;
  }

type backend_result =
  { backend : backend;
    capabilities : capabilities;
    verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
  }

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
    backend_results : backend_result list;
  }

module type S = sig
  val backend : backend
  val capabilities : capabilities

  val cache_key : command:string option -> obligation -> string option

  val discharge :
    command:string option -> obligation -> backend_result
end

let backend_name = function
  | Lean -> "lean"
  | Z3 -> "z3"
  | Oxsmt -> "oxsmt"

external cache_directory_is_private : string -> bool
  = "caml_vox_cache_directory_is_private"

external cache_file_is_private : string -> bool
  = "caml_vox_cache_file_is_private"

external file_stamp : string -> string = "caml_vox_file_stamp"

external is_regular_executable : string -> bool
  = "caml_vox_file_is_executable"

(* A binary's identity, for deciding whether a cached result was produced by
   this exact compiler and this exact solver.  Its content digest answers the
   same question and costs 0.10s per compiler invocation: 42 MB of compiler
   and 22 MB of solver, hashed before the first obligation can be looked up,
   and paid in full by a module carrying a single obligation.

   The stamp is device, inode, size, modification time and change time, each
   to the nanosecond.  It changes whenever the file's content could have
   changed, which is the property the key needs.  It cannot be carried over a
   rebuild: an install writes new content, so size or modification time move,
   and even a copy that restores the modification time cannot restore the
   change time, because setting the one sets the other.  It also cannot be
   staler than the digest it replaces, because the digest was already trusted
   only for as long as this stamp held: version 1 memoised the content digest
   against exactly these bytes. *)
let file_identity path =
  let stamp = file_stamp path in
  if String.equal stamp "" then None else Some (path ^ ":" ^ stamp)

let resolve_executable executable =
  if Filename.is_implicit executable then
    match Sys.getenv_opt "PATH" with
    | None -> None
    | Some path ->
      String.split_on_char ':' path
      |> List.find_map (fun directory ->
        let candidate = Filename.concat directory executable in
        if is_regular_executable candidate then Some candidate else None)
  else if is_regular_executable executable then Some executable
  else None

let running_compiler_identity =
  lazy (Option.bind (resolve_executable Sys.executable_name) file_identity)

(* The running executable cannot change underneath this process, so its
   identity is taken once.  The declared identity partitions that further
   rather than standing in for it: the reason to want it to stand in was the
   cost of the digest, and the digest is gone. *)
let compiler_implementation_identity () =
  match Lazy.force running_compiler_identity with
  | None -> None
  | Some identity ->
    Some
      (match Sys.getenv_opt "VOX_SOLVER_CACHE_COMPILER_IDENTITY" with
       | Some declared when not (String.equal declared "") ->
         identity ^ "|declared=" ^ declared
       | Some _ | None -> identity)

module Persistent_cache = struct
  (* Version 1 of this store gave every result a file of its own, holding the
     whole proof obligation twice over: once hex-encoded as the comparison
     key, and once more inside the solver log kept as the detail.  A
     nine-byte verdict cost six kilobytes.  Version 2 records a digest of the
     key rather than the key itself, keeps only short details, and gathers
     the results of one compiled source file into a single append-only log. *)

  let schema = "vox-solver-cache-v2"

  (* A record is about a hundred bytes, so this ceiling still holds far more
     results than any plausible workspace produces. *)
  let default_max_bytes = 16 * 1024 * 1024

  (* A log is rewritten in compacted form once it grows past this size, and
     is ignored entirely past the read ceiling. *)
  let max_log_bytes = 1024 * 1024
  let max_read_bytes = 8 * 1024 * 1024

  (* Only a disproof carries a detail, and fewer than three results in a
     hundred are disproofs.  A detail past this bound is left uncached rather
     than truncated, so that a hit always reproduces the exact text the
     solver gave; the bound is set well above the longest detail observed so
     that no result loses its entry in practice. *)
  let max_detail_bytes = 64 * 1024

  let key_digest_length = 64
  let checksum_length = 16

  type entry =
    { verdict : verdict;
      detail : string option;
      unused_facts : int list option;
    }

  let hex_digit value =
    if value < 10 then Char.chr (Char.code '0' + value)
    else Char.chr (Char.code 'a' + value - 10)

  let hex_of_string string =
    let result = Bytes.create (2 * String.length string) in
    String.iteri
      (fun index character ->
        let byte = Char.code character in
        Bytes.set result (2 * index) (hex_digit (byte lsr 4));
        Bytes.set result ((2 * index) + 1) (hex_digit (byte land 0xf)))
      string;
    Bytes.unsafe_to_string result

  let value_of_hex_digit = function
    | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
    | 'a' .. 'f' as digit -> Some (Char.code digit - Char.code 'a' + 10)
    | _ -> None

  let string_of_hex hex =
    let length = String.length hex in
    if length mod 2 <> 0 then None
    else
      let result = Bytes.create (length / 2) in
      let rec loop index =
        if index = length then Some (Bytes.unsafe_to_string result)
        else
          match
            value_of_hex_digit hex.[index], value_of_hex_digit hex.[index + 1]
          with
          | Some high, Some low ->
            Bytes.set result (index / 2) (Char.chr ((high lsl 4) lor low));
            loop (index + 2)
          | (Some _ | None), (Some _ | None) -> None
      in
      loop 0

  (* The key is no longer stored, so two distinct keys sharing a digest would
     exchange verdicts.  BLAKE2b-256 keeps that beyond reach: the birthday
     bound over a million entries is below 10^-64, and no colliding pair is
     constructible either. *)
  let key_digest key = Digest.BLAKE256.to_hex (Digest.BLAKE256.string key)

  (* Appends from two compilers can in principle interleave.  A record that
     did not survive intact fails this check and is skipped, so damage costs
     a recomputation rather than a wrong verdict. *)
  let record_checksum body =
    String.sub
      (Digest.BLAKE128.to_hex (Digest.BLAKE128.string body))
      0 checksum_length

  (* Only a decision about the obligation itself is written down.  Not-proved
     and unknown are as often a report about the machine as about the goal --
     a solver that ran out of its thirty seconds under load returns them --
     so storing one would turn a slow afternoon into a permanent rejection of
     correct code.  Solver-error and unavailable say the solver could not be
     run at all.  None of the four has a reader that recomputing would not
     serve better. *)
  let cache_string_of_verdict = function
    | Proved -> "p"
    | Disproved -> "d"
    | Not_proved | Unknown | Solver_error | Unavailable ->
      invalid_arg "non-cacheable solver verdict"

  let verdict_of_cache_string = function
    | "p" -> Some Proved
    | "d" -> Some Disproved
    | _ -> None

  let cache_string_of_unused_facts = function
    | None -> "-"
    | Some indices -> "u" ^ String.concat "," (List.map string_of_int indices)

  let unused_facts_of_cache_string = function
    | "-" -> Some None
    | "u" -> Some (Some [])
    | text when String.starts_with ~prefix:"u" text ->
      let suffix = String.sub text 1 (String.length text - 1) in
      let rec parse acc = function
        | [] -> Some (Some (List.rev acc))
        | index :: rest ->
          begin
            match int_of_string_opt index with
            | Some index when index >= 0 -> parse (index :: acc) rest
            | Some _ | None -> None
          end
      in
      parse [] (String.split_on_char ',' suffix)
    | _ -> None

  let cache_string_of_detail = function
    | None -> "-"
    | Some detail -> "d" ^ hex_of_string detail

  let detail_of_cache_string = function
    | "-" -> Some None
    | text when String.starts_with ~prefix:"d" text ->
      Option.map (fun detail -> Some detail)
        (string_of_hex (String.sub text 1 (String.length text - 1)))
    | _ -> None

  (* A record is one line of printable text whose only spaces are the four
     field separators, so a reader resynchronizes on the next newline however
     badly the previous line was damaged.

     Every field is here because something reads it back, and no field is
     here for any other reason.  The digest decides whether this record
     answers the obligation in hand.  The verdict is the answer.  The
     unused-fact indices become the per-fact [used] flag in the dumped
     verification condition, which is what fades a hypothesis no proof needed
     ([Vox_verify.json_fact]).  The detail is the text of a refutation, which
     reaches the reader twice, as the [counterexample] field of that dump and
     as the message of the failure the compiler reports
     ([Vox_verify.counterexample] and [Vox_verify.failure_text]); dropping it
     would make a cached run print something an uncached run does not.  The
     checksum is what makes an interleaved append cost a recomputation rather
     than a wrong verdict.

     There is no version tag.  A log is named for the digest of its schema,
     so a record written under one version is never opened by a reader of
     another, and a tag in the line would have had no reader. *)
  let record_body ~digest ~verdict ~unused_facts ~detail =
    String.concat " " [digest; verdict; unused_facts; detail]

  let encoded_record ~digest (entry : entry) =
    let body =
      record_body ~digest
        ~verdict:(cache_string_of_verdict entry.verdict)
        ~unused_facts:(cache_string_of_unused_facts entry.unused_facts)
        ~detail:(cache_string_of_detail entry.detail)
    in
    body ^ " " ^ record_checksum body ^ "\n"

  let parse_record line =
    match String.split_on_char ' ' line with
    | [digest; verdict; unused_facts; detail; checksum]
      when String.length digest = key_digest_length
           && String.equal checksum
                (record_checksum
                   (record_body ~digest ~verdict ~unused_facts ~detail)) ->
      begin
        match
          verdict_of_cache_string verdict,
          detail_of_cache_string detail,
          unused_facts_of_cache_string unused_facts
        with
        | Some verdict, Some detail, Some unused_facts ->
          Some (digest, { verdict; detail; unused_facts })
        | (Some _ | None), (Some _ | None), (Some _ | None) -> None
      end
    | _ -> None

  let debug message =
    match Sys.getenv_opt "VOX_SOLVER_CACHE_DEBUG" with
    | Some "1" -> Format.eprintf "vox solver cache: %s@." message
    | Some _ | None -> ()

  let enabled () =
    match Sys.getenv_opt "VOX_SOLVER_CACHE" with
    | Some ("0" | "false" | "no") -> false
    | Some _ | None -> true

  let cache_dir () =
    match Sys.getenv_opt "VOX_SOLVER_CACHE_DIR" with
    | Some directory when not (String.equal directory "") -> Some directory
    | Some _ -> None
    | None ->
      begin
        match Sys.getenv_opt "XDG_CACHE_HOME", Sys.getenv_opt "HOME" with
        | Some root, _ when not (String.equal root "") ->
          Some (Filename.concat root "vox2/solver-v2")
        | _, Some home when not (String.equal home "") ->
          Some (Filename.concat home ".cache/vox2/solver-v2")
        | (Some _ | None), (Some _ | None) -> None
      end

  let max_bytes () =
    match Sys.getenv_opt "VOX_SOLVER_CACHE_MAX_BYTES" with
    | None -> default_max_bytes
    | Some value ->
      begin
        match int_of_string_opt value with
        | Some bytes when bytes >= 0 -> bytes
        | Some _ | None -> default_max_bytes
      end

  let rec ensure_directory directory =
    if Sys.file_exists directory then ()
    else begin
      let parent = Filename.dirname directory in
      if not (String.equal parent directory) then ensure_directory parent;
      try Sys.mkdir directory 0o700 with
      | Sys_error _ when Sys.file_exists directory -> ()
    end

  let directory () =
    if not (enabled ()) then None
    else
      match cache_dir () with
      | None -> None
      | Some directory ->
        begin
          try
            ensure_directory directory;
            if cache_directory_is_private directory then Some directory
            else None
          with
          | Sys_error _ -> None
        end

  (* One log per compiled source file keeps a compilation to a single read.
     The compiler identity is part of every key already; naming the log after
     it as well means a rebuilt compiler starts fresh logs rather than
     reading past ones it can never hit. *)
  let log_basename ~backend_name =
    match compiler_implementation_identity () with
    | None -> None
    | Some compiler ->
      let source =
        match !Location.input_name with
        | "" -> "-"
        | name -> name
      in
      Some
        (Digest.BLAKE128.to_hex
           (Digest.BLAKE128.string
              (String.concat "\000" [schema; source; backend_name; compiler]))
         ^ ".log")

  let log_path ~backend_name =
    match directory (), log_basename ~backend_name with
    | Some directory, Some basename ->
      Some (Filename.concat directory basename)
    | (Some _ | None), (Some _ | None) -> None

  let file_size filename =
    try
      let channel = open_in_bin filename in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> in_channel_length channel)
    with Sys_error _ -> 0

  let read_log filename =
    let table = Hashtbl.create 64 in
    if cache_file_is_private filename then begin
      try
        let channel = open_in_bin filename in
        Fun.protect
          ~finally:(fun () -> close_in_noerr channel)
          (fun () ->
            if in_channel_length channel <= max_read_bytes then begin
              let rec loop () =
                match input_line channel with
                | exception End_of_file -> ()
                | line ->
                  begin
                    match parse_record line with
                    | Some (digest, entry) ->
                      Hashtbl.replace table digest entry
                    | None -> ()
                  end;
                  loop ()
              in
              loop ()
            end)
      with
      | Sys_error _
      | End_of_file
      | Failure _
      | Invalid_argument _ -> ()
    end;
    table

  (* A compilation looks up hundreds of results in one log.  The parsed log is
     kept for the life of the process and revalidated against the file's
     stamp, so a lookup costs one stat while no other process has touched the
     file, and re-reads honestly once one has. *)
  let logs_lock = Mutex.create ()

  let logs : (string, string * (string, entry) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 4

  let compacted : (string, unit) Hashtbl.t = Hashtbl.create 4

  let with_logs body =
    Mutex.lock logs_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock logs_lock) body

  let loaded filename =
    let stamp = file_stamp filename in
    match Hashtbl.find_opt logs filename with
    | Some (loaded_stamp, table)
      when (not (String.equal stamp "")) && String.equal loaded_stamp stamp ->
      table
    | Some _ | None ->
      let table = read_log filename in
      Hashtbl.replace logs filename (stamp, table);
      table

  let append filename record =
    if Sys.file_exists filename && not (cache_file_is_private filename) then
      (* The directory is private, so an unreadable log is one this compiler
         left behind under a permissive umask.  Replace it rather than append
         to a file no lookup will ever read. *)
      Misc.remove_file filename;
    let channel =
      open_out_gen
        [Open_wronly; Open_append; Open_creat; Open_binary] 0o600 filename
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        (* A single buffered write of about a hundred bytes: an interleaved
           append from another compiler lands before or after this record,
           and were it ever to land inside it the checksum rejects both. *)
        output_string channel record;
        flush channel)

  (* Rewriting keeps one record per distinct key.  Records another process
     appended between this read and the rename are lost, which costs those
     obligations a recomputation; nothing already returned becomes wrong. *)
  let compact directory filename table =
    let rewritten = read_log filename in
    Hashtbl.iter
      (fun digest entry -> Hashtbl.replace rewritten digest entry)
      table;
    let temporary = Filename.temp_file ~temp_dir:directory "compact-" ".tmp" in
    Fun.protect
      ~finally:(fun () -> Misc.remove_file temporary)
      (fun () ->
        let channel = open_out_bin temporary in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            Hashtbl.iter
              (fun digest entry ->
                output_string channel (encoded_record ~digest entry))
              rewritten;
            flush channel);
        Sys.rename temporary filename);
    Hashtbl.replace logs filename (file_stamp filename, rewritten)

  let compact_if_large directory filename table =
    if
      (not (Hashtbl.mem compacted filename))
      && file_size filename > max_log_bytes
    then begin
      Hashtbl.replace compacted filename ();
      compact directory filename table
    end

  let is_evictable basename =
    Filename.check_suffix basename ".log"
    || (String.starts_with ~prefix:"compact-" basename
        && Filename.check_suffix basename ".tmp")

  (* [caml_vox_file_stamp] reports device, inode, size, modification time and
     change time.  Ordering by modification time discards the logs of
     abandoned builds before those of the workspace in use. *)
  let modification_order stamp =
    match String.split_on_char ':' stamp with
    | [_; _; _; seconds; nanoseconds; _; _] ->
      begin
        match int_of_string_opt seconds, int_of_string_opt nanoseconds with
        | Some seconds, Some nanoseconds -> seconds, nanoseconds
        | (Some _ | None), (Some _ | None) -> 0, 0
      end
    | _ -> 0, 0

  let evict directory =
    let limit = max_bytes () in
    try
      let files =
        Sys.readdir directory
        |> Array.to_list
        |> List.filter_map (fun basename ->
          if is_evictable basename then begin
            let filename = Filename.concat directory basename in
            let stamp = file_stamp filename in
            if String.equal stamp "" then None
            else Some (modification_order stamp, filename, file_size filename)
          end
          else None)
      in
      let total = List.fold_left (fun n (_, _, size) -> n + size) 0 files in
      if total > limit then begin
        let removal_order = List.sort compare files in
        let rec remove total = function
          | _ when total <= limit -> ()
          | [] -> ()
          | (_, filename, size) :: rest ->
            Misc.remove_file filename;
            remove (total - size) rest
        in
        remove total removal_order
      end
    with Sys_error _ -> ()

  let find ~backend_name key =
    let entry =
      try
        match log_path ~backend_name with
        | None -> None
        | Some filename ->
          let digest = key_digest key in
          with_logs (fun () -> Hashtbl.find_opt (loaded filename) digest)
      with
      | Sys_error _
      | Failure _
      | Invalid_argument _ -> None
    in
    debug
      (backend_name ^ " "
       ^ if Option.is_some entry then "hit" else "miss");
    entry

  let bytes_written_since_eviction = Atomic.make 0
  let eviction_checked_directories = Atomic.make []

  let evict_on_first_write directory =
    let rec claim () =
      let checked = Atomic.get eviction_checked_directories in
      if List.mem directory checked then false
      else if
        Atomic.compare_and_set eviction_checked_directories checked
          (directory :: checked)
      then true
      else claim ()
    in
    if claim () then evict directory

  let store ~backend_name key (result : backend_result) =
    try
      match result.verdict, directory (), log_basename ~backend_name with
      | (Proved | Disproved), Some directory, Some basename ->
        let entry =
          { verdict = result.verdict;
            detail = result.detail;
            unused_facts = result.unused_facts;
          }
        in
        let detail_is_oversized =
          match entry.detail with
          | None -> false
          | Some detail -> String.length detail > max_detail_bytes
        in
        if not detail_is_oversized then begin
          let filename = Filename.concat directory basename in
          (* Batch compilers usually write much less than the periodic
             threshold.  Scan once per directory in every process so stale
             generations and abandoned temporary files cannot accumulate
             across processes. *)
          evict_on_first_write directory;
          let digest = key_digest key in
          let record = encoded_record ~digest entry in
          let bytes =
            with_logs (fun () ->
              let table = loaded filename in
              append filename record;
              Hashtbl.replace table digest entry;
              Hashtbl.replace logs filename (file_stamp filename, table);
              compact_if_large directory filename table;
              String.length record)
          in
          let threshold = max 1 (max_bytes () / 4) in
          let previous =
            Atomic.fetch_and_add bytes_written_since_eviction bytes
          in
          let total = previous + bytes in
          if
            total >= threshold
            && Atomic.compare_and_set bytes_written_since_eviction total 0
          then begin
            evict directory
          end
        end
      | (Not_proved | Unknown | Solver_error | Unavailable), _, _
      | (Proved | Disproved), None, _
      | (Proved | Disproved), Some _, None -> ()
    with
    | Sys_error _
    | Failure _
    | Invalid_argument _ -> ()
end

module Cached (Backend : S) = struct
  let backend = Backend.backend
  let capabilities = Backend.capabilities
  let cache_key = Backend.cache_key

  let discharge ~command obligation =
    match cache_key ~command obligation with
    | None -> Backend.discharge ~command obligation
    | Some key ->
      begin
        match
          Persistent_cache.find ~backend_name:(backend_name backend) key
        with
        | Some entry ->
          { backend;
            capabilities;
            verdict = entry.verdict;
            location = obligation.condition.location;
            detail = entry.detail;
            unused_facts = entry.unused_facts;
          }
        | None ->
          let result = Backend.discharge ~command obligation in
          Persistent_cache.store ~backend_name:(backend_name backend) key
            result;
          result
      end
end

(* The log that would hold results for [backend] under the current source
   file, compiler and cache directory. *)
let cache_bucket_path backend =
  Persistent_cache.log_path ~backend_name:(backend_name backend)

let string_of_backend = backend_name

let string_of_selection = function
  | Single backend -> string_of_backend backend
  | Cross -> "cross"

let string_of_verdict = function
  | Proved -> "proved"
  | Not_proved -> "not-proved"
  | Disproved -> "disproved"
  | Unknown -> "unknown"
  | Solver_error -> "solver-error"
  | Unavailable -> "unavailable"

let backend_of_string = function
  | "lean" -> Ok Lean
  | "z3" -> Ok Z3
  | "oxsmt" -> Ok Oxsmt
  | backend ->
    Error
      (Printf.sprintf
         "unknown refinement discharge backend %S (expected lean, z3, or \
          oxsmt)"
         backend)

let selection_of_string = function
  | "cross" -> Ok Cross
  | backend ->
    Result.map (fun backend -> Single backend) (backend_of_string backend)

let capabilities = function
  | Lean | Z3 | Oxsmt -> { fact_usage = Fact_usage }

let resolve_command ~explicit ~environment ~fallback =
  match explicit with
  | Some _ as command -> command
  | None ->
    begin
      match Sys.getenv_opt environment with
      | Some _ as command -> command
      | None -> fallback ()
    end

let z3_command explicit =
  resolve_command ~explicit ~environment:"VOX_SMT_SOLVER"
    ~fallback:(fun () -> Some "z3 -in")

let oxsmt_command explicit =
  resolve_command ~explicit ~environment:"VOX_OXSMT_SOLVER"
    ~fallback:(fun () ->
      let root = Filename.dirname (Filename.dirname Config.bindir) in
      Some
        (Filename.quote
           (Filename.concat root "_build/vox_oxsmt_runner.exe")))

let legacy_oxsmt_external () =
  match Sys.getenv_opt "VOX_OXSMT_LEGACY_EXTERNAL" with
  | Some "1" -> true
  | Some _ | None -> false

let oxsmt_timeout_seconds () =
  match Sys.getenv_opt "VOX_OXSMT_TIMEOUT_SECONDS" with
  | None -> 30
  | Some timeout ->
    let timeout = int_of_string timeout in
    if timeout <= 0 then invalid_arg "VOX_OXSMT_TIMEOUT_SECONDS";
    timeout

let result ~backend ~verdict ~location ?detail ?unused_facts () =
  { backend;
    capabilities = capabilities backend;
    verdict;
    location;
    detail;
    unused_facts;
  }

let key_field name value =
  Printf.sprintf "%s:%d:%s" name (String.length value) value

let option_key_field name = function
  | None -> key_field name "none"
  | Some value -> key_field name (key_field "some" value)

let declared_solver_version backend =
  let variable =
    "VOX_" ^ String.uppercase_ascii (string_of_backend backend)
    ^ "_SOLVER_VERSION"
  in
  match Sys.getenv_opt variable with
  | Some version when not (String.equal version "") -> Some version
  | Some _ | None -> None

let command_is_simple command =
  let forbidden = "'\"`$;|&<>(){}[]*?!\n\r\t\\" in
  let tokens =
    String.split_on_char ' ' (String.trim command)
    |> List.filter (fun token -> not (String.equal token ""))
  in
  match tokens with
  | [] -> None
  | executable :: _
    when String.equal (Filename.basename executable) "env"
         || String.equal (Filename.basename executable) "sh"
         || String.equal (Filename.basename executable) "bash"
         || String.contains executable '='
         || List.mem "-c" tokens ->
    None
  | executable :: _ ->
    if
      List.exists
        (fun token ->
          String.exists (fun character -> String.contains forbidden character)
            token)
        tokens
    then None
    else Some executable

let solver_basename_matches backend executable =
  let basename = Filename.basename executable in
  match backend with
  | Z3 -> String.equal basename "z3" || String.equal basename "z3.exe"
  | Lean -> String.equal basename "lean" || String.equal basename "lean.exe"
  | Oxsmt ->
    String.equal basename "oxsmt"
    || String.equal basename "oxsmt.exe"
    || String.equal basename "vox_oxsmt_runner.exe"

let declared_command_fingerprint ~backend command =
  Option.map
    (fun version -> command ^ "|declared=" ^ version)
    (declared_solver_version backend)

let command_fingerprint ~backend = function
  | None -> None
  | Some command ->
    begin
      match command_is_simple command with
      | Some executable when solver_basename_matches backend executable ->
        Option.bind (resolve_executable executable) (fun executable ->
          Option.map (fun identity -> command ^ "|" ^ identity)
            (file_identity executable))
      | Some _ | None -> declared_command_fingerprint ~backend command
    end

let cache_key ~backend ~implementation ~solver ~options ~payload =
  Option.map
    (fun compiler_identity ->
      String.concat "\n"
        [ key_field "schema" Persistent_cache.schema;
          key_field "backend" (string_of_backend backend);
          key_field "compiler" (Config.version ^ "|" ^ compiler_identity);
          key_field "implementation" implementation;
          key_field "solver" solver;
          option_key_field "declared-solver-version"
            (declared_solver_version backend);
          key_field "options" options;
          key_field "payload" payload;
        ])
    (compiler_implementation_identity ())

module Lean_backend_uncached = struct
  let backend = Lean
  let capabilities = capabilities backend

  let cache_key ~command:_ { env; condition; prove_contents = _ } =
    match Vox_lean.emit ~env condition with
    | Error _ -> None
    | Ok payload ->
      let lean = Vox_lean.resolve_lean () in
      Option.bind lean (fun lean ->
        Option.bind (resolve_executable lean) (fun lean ->
          Option.bind (file_identity lean) (fun solver ->
          cache_key ~backend ~implementation:"lean-translation-bv63-v2" ~solver
              ~options:"timeout=30;linter=unusedVariables" ~payload)))

  let discharge ~command:_ { env; condition; prove_contents = _ } =
    let lean = Vox_lean.discharge ~env condition in
    let verdict =
      match lean.verdict with
      | Vox_lean.Proved -> Proved
      | Vox_lean.Not_proved -> Not_proved
      | Vox_lean.Disproved -> Disproved
      | Vox_lean.Solver_error -> Solver_error
    in
    { backend;
      capabilities;
      verdict;
      location = lean.location;
      detail = lean.detail;
      (* Lean declares the usage capability even when no proof is produced.
         Its legacy result then carries the conservative empty-unused set,
         which keeps every fact visible and preserves the existing JSON. *)
      unused_facts = Some lean.unused_facts;
    }
end

let verdict_of_smt = function
  | Vox_smt.Proved -> Proved
  | Vox_smt.Not_proved -> Not_proved
  | Vox_smt.Disproved -> Disproved
  | Vox_smt.Solver_error -> Solver_error
  | Vox_smt.Unavailable -> Unavailable

module Z3_backend_uncached = struct
  let backend = Z3
  let capabilities = capabilities backend

  let cache_key ~command { env; condition; prove_contents } =
    let command = z3_command command in
    let solver = command_fingerprint ~backend command in
    let payload =
      Result.to_option (Vox_smt.emit ~query:Vox_smt.Prove ~env condition)
      |> Option.map (fun obligation ->
        String.concat "\n"
          [ key_field "obligation" obligation;
            option_key_field "custom-prove" prove_contents;
          ])
    in
    Option.bind solver
      (fun solver -> Option.bind payload (fun payload ->
        cache_key ~backend ~implementation:"smt-translation-bv63-v3"
          ~solver ~options:"timeout=30;input=stdin;unsat-core=true" ~payload))

  let discharge ~command { env; condition; prove_contents } =
    let smt =
      Vox_smt.discharge ~backend:`Z3 ~command:(z3_command command)
        ?prove_contents ~input_mode:Vox_smt.Stdin ~env condition
    in
    { backend;
      capabilities;
      verdict = verdict_of_smt smt.verdict;
      location = smt.location;
      detail = smt.detail;
      unused_facts = Some smt.unused_facts;
    }
end

module Oxsmt_backend_uncached = struct
  let backend = Oxsmt
  let capabilities = capabilities backend

  let in_process_revision = "036b29692b057e98df701df6a1517991f4d98cdd"

  (* Oxsmt has additional experimental runtime levers beyond the six enabled
     by the supported profile. Keep this sorted list synchronized with every
     [Sys.getenv_opt "OXSMT_..."] read in the vendored runtime. *)
  let runtime_environment_names =
    [ "OXSMT_ARR_ROW2";
      "OXSMT_ARR_ROW2_NOINDEX";
      "OXSMT_ARR_WEQ";
      "OXSMT_ARR_WEQ_ANALYZE";
      "OXSMT_ARR_WEQ_FUEL";
      "OXSMT_ARR_WEQ_MAXIDX";
      "OXSMT_ARR_WEQ_NONARROW";
      "OXSMT_ARR_WEQ_NOROW";
      "OXSMT_ARR_WEQ_NOTRIGGER";
      "OXSMT_ARR_WEQ_SELFCHECK";
      "OXSMT_ASSUMPTION_FAST_COMPLEMENTS";
      "OXSMT_ASSUMPTION_PREPROCESS";
      "OXSMT_ASSUMPTION_PREPROCESS_PROPFOLD";
      "OXSMT_ASSUMPTION_PROFILE";
      "OXSMT_AX_OCCIDX";
      "OXSMT_BASE_L0";
      "OXSMT_BINARY_INTERFACE_EQ";
      "OXSMT_BV_REWRITE2";
      "OXSMT_BV_REWRITE2_EQSPLIT";
      "OXSMT_BV_RW3";
      "OXSMT_CG_ANTS_PCT";
      "OXSMT_CG_CUTS";
      "OXSMT_CG_CUT_GATE";
      "OXSMT_CG_MAX_CUTS";
      "OXSMT_CG_NNZ_PCT";
      "OXSMT_CHRONO";
      "OXSMT_CHRONO_INCR_UNDO";
      "OXSMT_CHRONO_T";
      "OXSMT_COMBINE_INSEARCH";
      "OXSMT_CORE_MIN_EFFORT_CAP";
      "OXSMT_CORE_MIN_EFFORT_FLOOR";
      "OXSMT_CORE_MIN_EFFORT_MULTIPLIER";
      "OXSMT_CORE_MIN_INITIAL_EFFORT_LIMIT";
      "OXSMT_CORE_MIN_LINEAR";
      "OXSMT_DIRECT_TERM_ITE";
      "OXSMT_DTLIA_BOOL_COMPLETE";
      "OXSMT_DTLIA_ELIM_COMPLETE";
      "OXSMT_DTLIA_PRED_COMPLETE";
      "OXSMT_DTLIA_PURIFY";
      "OXSMT_DT_GROUND_SIMPLIFY";
      "OXSMT_DT_INCR";
      "OXSMT_EMATCH_MGI";
      "OXSMT_EMATCH_MGI_THRESHOLD";
      "OXSMT_EUF_INCR";
      "OXSMT_EUF_SELF_CHECK";
      "OXSMT_FOREST_BALANCE";
      "OXSMT_HNF_CUTS";
      "OXSMT_LAZY_INTERFACE_DISEQ";
      "OXSMT_LEMMA_BACKJUMP";
      "OXSMT_LEMMA_FAIR";
      "OXSMT_LEMMA_GEN_BUDGET";
      "OXSMT_LEMMA_INDEX";
      "OXSMT_LEMMA_SEED";
      "OXSMT_LEMMA_STREAM";
      "OXSMT_LGC_FIXED";
      "OXSMT_LGC_INITIAL";
      "OXSMT_LGC_SIZEREL";
      "OXSMT_LIA_DISEQ_CDCL";
      "OXSMT_LIA_DL_PROP";
      "OXSMT_LIA_EQ_PROP";
      "OXSMT_LIA_GCD_CUT";
      "OXSMT_LIA_MODELFIND";
      "OXSMT_LIA_MODELFIND_BUDGET";
      "OXSMT_LIA_MODELFIND_STALL";
      "OXSMT_LIA_MODELFIND_STALL_MIN";
      "OXSMT_LIA_MODELFIND_STALL_RATIO";
      "OXSMT_LIA_MODEL_REPAIR";
      "OXSMT_LIA_TRIVIAL_EQ";
      "OXSMT_LRA";
      "OXSMT_MAX_BV_WIDTH";
      "OXSMT_NEC_PROPFOLD";
      "OXSMT_NIA";
      "OXSMT_NO_DIOPHANTINE";
      "OXSMT_NO_FABRIC";
      "OXSMT_NO_FABRIC_CALLBACKS";
      "OXSMT_PRESOLVE_CTX";
      "OXSMT_PRESOLVE_CTX_STATS";
      "OXSMT_PRESOLVE_ELIM_GROWTH";
      "OXSMT_PRESOLVE_ELIM_STATS";
      "OXSMT_PRESOLVE_EQ";
      "OXSMT_PRESOLVE_PROJ";
      "OXSMT_PRESOLVE_PROJ_MAX_STEPS";
      "OXSMT_PRESOLVE_PROJ_STATS";
      "OXSMT_PROPFOLD_LITONLY";
      "OXSMT_QUANT_PIPELINE";
      "OXSMT_RECURSIVE_MIN";
      "OXSMT_RELEVANCY";
      "OXSMT_SATCORE_MODES";
      "OXSMT_SATCORE_MODE_INIT";
      "OXSMT_SATPRE";
      "OXSMT_SATPRE_INPROC_FIRST";
      "OXSMT_SATPRE_STATS";
      "OXSMT_SAT_LINEAR_TAUTOLOGY";
      "OXSMT_SYMBREAK";
      "OXSMT_SYMBREAK_BUDGET";
      "OXSMT_SYMBREAK_STATS";
      "OXSMT_SYMBREAK_UFTAIL";
    ]

  (* Length-prefixing each name and raw value makes the encoding unambiguous;
     distinct spellings may over-partition the cache but can never collide. *)
  let runtime_environment_key () =
    runtime_environment_names
    |> List.map (fun name -> option_key_field name (Sys.getenv_opt name))
    |> String.concat ""

  let cache_key ~command { env; condition; prove_contents } =
    let timeout = oxsmt_timeout_seconds () in
    let legacy = legacy_oxsmt_external () in
    let payload =
      Result.to_option (Vox_smt.emit ~query:Vox_smt.Prove ~env condition)
      |> Option.map (fun obligation ->
        String.concat "\n"
          [ key_field "obligation" obligation;
            (if legacy then option_key_field "custom-prove" prove_contents
             else key_field "custom-prove" "ignored-by-in-process-oxsmt");
          ])
    in
    Option.bind payload
      (fun payload ->
        let solver =
          if legacy then command_fingerprint ~backend (oxsmt_command command)
          else Some ("oxsmt-" ^ in_process_revision ^ "-in-process")
        in
        Option.bind solver (fun solver ->
          cache_key ~backend ~implementation:"oxsmt-translation-bv63-v3"
            ~solver
            ~options:
              (Printf.sprintf
                 ("timeout=%d;legacy-external=%b;unsat-core=true;"
                  ^^ "runtime-environment=%s")
                 timeout legacy (runtime_environment_key ()))
            ~payload))

  let discharge ~command { env; condition; prove_contents } =
    let smt =
      if legacy_oxsmt_external () then
        Vox_smt.discharge ~backend:`Oxsmt
          ~command:(oxsmt_command command) ?prove_contents
          ~input_mode:Vox_smt.Stdin ~env condition
      else
        Vox_smt.discharge_oxsmt
          ~timeout_seconds:(oxsmt_timeout_seconds ()) ~env condition
    in
    { backend;
      capabilities;
      verdict = verdict_of_smt smt.verdict;
      location = smt.location;
      detail = smt.detail;
      unused_facts = Some smt.unused_facts;
    }
end


module Lean_backend = Cached (Lean_backend_uncached)
module Z3_backend = Cached (Z3_backend_uncached)
module Oxsmt_backend = Cached (Oxsmt_backend_uncached)

let module_for_backend = function
  | Lean -> (module Lean_backend : S)
  | Z3 -> (module Z3_backend : S)
  | Oxsmt -> (module Oxsmt_backend : S)

let command_for_backend ~smt_solver ~oxsmt_solver = function
  | Lean -> None
  | Z3 -> smt_solver
  | Oxsmt -> oxsmt_solver

let protect_discharge (module Backend : S) ~command obligation =
  try Backend.discharge ~command obligation with
  | exception_ ->
    result ~backend:Backend.backend ~verdict:Solver_error
      ~location:obligation.condition.location
      ~detail:(Printexc.to_string exception_) ()

let discharge_backend ~smt_solver ~oxsmt_solver backend obligation =
  let command = command_for_backend ~smt_solver ~oxsmt_solver backend in
  protect_discharge (module_for_backend backend) ~command obligation

let single_result (backend_result : backend_result) : result =
  { verdict = backend_result.verdict;
    location = backend_result.location;
    detail = backend_result.detail;
    unused_facts = backend_result.unused_facts;
    backend_results = [backend_result];
  }

let unavailable_lean (condition : Vox_vc.t) =
  result ~backend:Lean ~verdict:Unavailable ~location:condition.Vox_vc.location
    ~detail:"Lean executable not found" ()

let normalize_cross_result (result : backend_result) =
  match result.verdict with
  | Proved -> result
  | Not_proved ->
    { result with verdict = Unknown; unused_facts = None }
  | Disproved | Unknown | Solver_error | Unavailable ->
    { result with unused_facts = None }

let cross_summary (results : backend_result list) =
  let backend (result : backend_result) =
    string_of_backend result.backend ^ "=" ^ string_of_verdict result.verdict
  in
  "cross-check failed: " ^ String.concat ", " (List.map backend results)

let cross_result (condition : Vox_vc.t) (results : backend_result list) =
  let unused_facts =
    match results with
    | lean :: _ -> lean.unused_facts
    | [] -> None
  in
  if
    List.for_all
      (fun (result : backend_result) -> result.verdict = Proved)
      results
  then
    { verdict = Proved;
      location = condition.Vox_vc.location;
      detail = None;
      unused_facts;
      backend_results = results;
    }
  else
    { verdict = Solver_error;
      location = condition.Vox_vc.location;
      detail = Some (cross_summary results);
      unused_facts;
      backend_results = results;
    }

let discharge ~selection ~smt_solver ~oxsmt_solver ?prove_contents ~env
    condition =
  let obligation = { env; condition; prove_contents } in
  match selection with
  | Single backend ->
    discharge_backend ~smt_solver ~oxsmt_solver backend obligation
    |> single_result
  | Cross ->
    let lean =
      if Vox_lean.lean_available () then
        discharge_backend ~smt_solver ~oxsmt_solver Lean obligation
      else unavailable_lean condition
    in
    let z3 = discharge_backend ~smt_solver ~oxsmt_solver Z3 obligation in
    let oxsmt =
      discharge_backend ~smt_solver ~oxsmt_solver Oxsmt obligation
    in
    List.map normalize_cross_result [lean; z3; oxsmt]
    |> cross_result condition
