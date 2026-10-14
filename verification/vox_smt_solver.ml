open Vox_smt
open Vox_smt_response

type config =
  { executable : string;
    timeout_ms : int
  }

let default_config = { executable = "z3"; timeout_ms = 5000 }

let expected_version = Vox_smt_response.expected_version

let is_expected_version version =
  String.equal (String.trim version) "Vox SMT 0.1"
  || Vox_smt_response.is_expected_version version

external monotonic_time : unit -> float = "caml_vox_smt_monotonic_time"

type result =
  { validity : validity;
    stderr : string;
    resources : int option;
    core : Symbol.t list option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled

exception Deadline

exception Setup_exhausted

exception Callback_error of exn * Printexc.raw_backtrace

let callback f x =
  try f x
  with exn -> raise (Callback_error (exn, Printexc.get_raw_backtrace ()))

let rec waitpid flags pid =
  try Unix.waitpid flags pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

type connection =
  { pid : int;
    input : Unix.file_descr;
    output : Unix.file_descr;
    errors : Unix.file_descr
  }

(* A solver that does not answer within 30 seconds is treated as having no
   version. The limit is generous because a loaded build machine can take
   several seconds to start a process; it only delays a solver that hangs. *)
let version ~executable =
  if executable = "ocaml" then Some "Vox SMT 0.1" else
  let deadline = Unix.gettimeofday () +. 30. in
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
              (Unix.create_process executable
                 [| executable; "-version" |]
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
      | Some (Unix.WEXITED 0) when finished && version <> "" -> Some version
      | _ | (exception Unix.Unix_error _) -> None))

module Declarations = Map.Make (String)

type retained =
  { declarations : string Declarations.t;
    assertions : string array;
    cores : bool;
    resources : int option;
    scoped : bool;
    compatible_checks : int;
    observed_common : int;
    goal : string;
    base : int
  }

type session_mode = Reset | Incremental | Automatic

type session =
  { mutable connection : connection option;
    mutable retained : retained option;
    mode : session_mode;
    mutable checks : int;
    mutable bytes : int
  }

let dispose connection =
  (try Unix.kill connection.pid Sys.sigkill with Unix.Unix_error _ -> ());
  ignore (waitpid [] connection.pid);
  List.iter
    (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
    [connection.input; connection.output; connection.errors]

let incremental_input session ~poll ~cores (query : smtlib_query) =
  let blocks = ref [] and count = ref 0 in
  let buffer = Buffer.create 256 in
  let flush () =
    if !count > 0 then begin
      blocks := Buffer.contents buffer :: !blocks;
      Buffer.clear buffer;
      count := 0
    end
  in
  List.iter (fun assertion ->
    poll ();
    Buffer.add_string buffer assertion;
    incr count;
    if !count = 32 then flush ()) query.assertions;
  flush ();
  let assertions = Array.of_list (List.rev !blocks) in
  let previous = session.retained in
  let buffer = Buffer.create 256 in
  let add text = Buffer.add_string buffer text in
  let push_assertion assertion =
    poll ();
    add "(push 1)\n";
    add assertion
  in
  let pop count =
    if count > 0 then add (Printf.sprintf "(pop %d)\n" count)
  in
  let common_prefix old =
    let length = min (Array.length old) (Array.length assertions) in
    let rec loop index =
      if index = length then index else begin
        poll ();
        if old.(index) = assertions.(index) then loop (index + 1) else index
      end
    in
    loop 0
  in
  let compatible previous =
    List.for_all (fun (name, text) ->
      poll ();
      match Declarations.find_opt name previous.declarations with
      | None -> true
      | Some retained -> text = retained) query.declarations
  in
  let declare declarations =
    List.fold_left (fun declarations (name, text) ->
      poll ();
      if Declarations.mem name declarations then declarations else begin
        add text;
        Declarations.add name text declarations
      end) declarations query.declarations
  in
  let previous, common =
    match previous with
    | Some previous when previous.cores = cores && compatible previous ->
      Some previous, common_prefix previous.assertions
    | _ -> None, 0
  in
  let eligible_prefix common =
    if session.mode <> Automatic then true else begin
      let total = ref 0 and shared = ref 0 in
      Array.iteri (fun index assertion ->
        poll ();
        let bytes = String.length assertion in
        total := !total + bytes;
        if index < common then shared := !shared + bytes) assertions;
      !shared >= 16 * 1024 && !shared >= !total - !total / 4
    end
  in
  let eligible = eligible_prefix common in
  let stable_common =
    Option.fold ~none:0 ~some:(fun previous ->
      min previous.observed_common common) previous
  in
  let compatible_checks =
    if eligible then 1 + Option.fold ~none:0
      ~some:(fun previous -> previous.compatible_checks) previous
    else 0
  in
  let scoped = session.mode <> Automatic
    || (compatible_checks >= 2 && eligible_prefix stable_common) in
  let identical =
    match previous with
    | Some previous when session.mode = Automatic && not previous.scoped
        && common = Array.length assertions
        && common = Array.length previous.assertions
        && previous.goal = query.goal_assertion -> Some previous
    | _ -> None
  in
  let observed_common = common in
  match identical with
  | Some previous -> begin
      add query.limits;
      let declarations = declare previous.declarations in
      add query.check_command;
      Buffer.contents buffer,
      { declarations; assertions; cores; resources = None; scoped = false;
        base = 0; compatible_checks; observed_common;
        goal = query.goal_assertion },
      previous.resources
  end
  | None when not scoped -> begin
    add "(reset)\n";
    add query.settings;
    add query.limits;
    add "(set-logic ALL)\n";
    let declarations = declare Declarations.empty in
    Array.iter (fun assertion -> poll (); add assertion) assertions;
    add query.goal_assertion;
    add query.check_command;
    Buffer.contents buffer,
    { declarations; assertions; cores; resources = None; scoped = false;
      base = 0; compatible_checks; observed_common;
      goal = query.goal_assertion },
    Some 0
  end
  | None -> begin
    let declarations, common, base, previous_resources =
      match previous with
      | Some previous when previous.scoped && common >= previous.base ->
        add query.limits;
        pop (Array.length previous.assertions - common + 1);
        declare previous.declarations, common, previous.base, previous.resources
      | _ ->
        add "(reset)\n";
        add query.settings;
        add query.limits;
        add "(set-option :global-declarations true)\n";
        add "(set-logic ALL)\n";
        declare Declarations.empty, 0,
          (if session.mode = Automatic then stable_common else 0), Some 0
    in
    for index = common to Array.length assertions - 1 do
      if index < base then begin poll (); add assertions.(index) end
      else push_assertion assertions.(index)
    done;
    push_assertion query.goal_assertion;
    add query.check_command;
    Buffer.contents buffer,
    { declarations; assertions; cores; resources = None; scoped = true; base;
      compatible_checks; observed_common; goal = query.goal_assertion },
    previous_resources
  end

let setup_exhausted answer =
  let prefix = "(error \"" and suffix = "\")" in
  if String.starts_with ~prefix answer && String.ends_with ~suffix answer
  then begin
    let message = String.sub answer (String.length prefix)
      (String.length answer - String.length prefix - String.length suffix) in
    let digits text =
      String.length text > 0
      && String.for_all (fun c -> c >= '0' && c <= '9') text
    in
    let message =
      match String.split_on_char ' ' message with
      | "line" :: line :: "column" :: column :: message
        when digits line && String.ends_with ~suffix:":" column
             && digits (String.sub column 0 (String.length column - 1)) ->
        String.concat " " message
      | _ -> message
    in
    List.mem message
      ["push canceled"; "pop canceled"; "max. resource limit exceeded"]
  end else false

let check_impl session ?(config = default_config) ?(dump = fun _ -> ())
    ?(cancelled = fun () -> false) ?(request_model = true)
    ?resource_limit ?assumptions ~int_width q =
  if config.timeout_ms <= 0 then invalid_arg "Vox_smt_solver: timeout_ms";
  let keep = ref false in
  let started = monotonic_time () in
  let deadline = started +. (float config.timeout_ms /. 1000.) in
  let encoded = ref started and resources = ref None and core = ref None in
  let stderr = Buffer.create 128 in
  let descriptors = ref [] and child = ref None and exit_status = ref None in
  let stderr_fd = ref None in
  let output_limit = 4 * 1024 * 1024 in
  let close fd =
    descriptors := List.filter (( <> ) fd) !descriptors;
    try Unix.close fd with Unix.Unix_error _ -> ()
  in
  let pipe () =
    let r, w = Unix.pipe ~cloexec:true () in
    descriptors := r :: w :: !descriptors;
    r, w
  in
  let cleanup () =
    if not !keep
    then begin
      Option.iter dispose session.connection;
      session.connection <- None;
      session.retained <- None;
      session.checks <- 0;
      session.bytes <- 0;
      (match !child, !exit_status with
      | Some pid, None ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (waitpid [] pid)
      | _ -> ());
      (match !stderr_fd with
      | Some fd when List.mem fd !descriptors ->
        let bytes = Bytes.create 4096 in
        let rec drain () =
          let remaining = output_limit - Buffer.length stderr in
          if remaining > 0
          then
            match Unix.read fd bytes 0 (min remaining (Bytes.length bytes)) with
            | 0 -> ()
            | n ->
              Buffer.add_subbytes stderr bytes 0 n;
              drain ()
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain ()
            | exception Unix.Unix_error _ -> ()
        in
        drain ()
      | _ -> ());
      List.iter close !descriptors
    end
  in
  let poll () =
    if callback cancelled () then raise Cancelled;
    let remaining = deadline -. monotonic_time () in
    if remaining <= 0. then raise Deadline;
    min remaining 0.02
  in
  let bounded_add b s =
    let remaining = output_limit - Buffer.length b in
    Buffer.add_substring b s 0 (min remaining (String.length s));
    if String.length s > remaining then protocol "Solver output exceeds 4 MiB"
  in
  let run () =
    ignore (poll ());
    if session.mode <> Reset
       && (session.checks >= 256 || session.bytes >= 16 * 1024 * 1024)
    then begin
      Option.iter dispose session.connection;
      session.connection <- None;
      session.retained <- None;
      session.checks <- 0;
      session.bytes <- 0
    end;
    let input, retained, previous_resources =
      if session.mode <> Reset then begin
        let query =
          to_smtlib_query ~poll:(fun () -> ignore (poll ()))
            ?resource_limit ?assumptions ~int_width
            ~timeout_ms:config.timeout_ms q
        in
        let input, retained, previous_resources =
          incremental_input session ~poll:(fun () -> ignore (poll ()))
            ~cores:(Option.is_some assumptions) query
        in
        input, Some retained, previous_resources
      end else
        session_input
          (to_smtlib ~poll:(fun () -> ignore (poll ()))
             ?resource_limit ?assumptions ~int_width
             ~timeout_ms:config.timeout_ms q), None, Some 0
    in
    encoded := monotonic_time ();
    ignore (poll ());
    let connection =
      match session.connection with
      | Some connection ->
        session.connection <- None;
        descriptors := [connection.input; connection.output; connection.errors];
        connection
      | None ->
        let stdin_r, stdin_w = pipe () in
        let stdout_r, stdout_w = pipe () in
        let stderr_r, stderr_w = pipe () in
        let pid =
          Unix.create_process config.executable
            [| config.executable; "-in"; "-smt2" |]
            stdin_r stdout_w stderr_w
        in
        child := Some pid;
        List.iter close [stdin_r; stdout_w; stderr_w];
        List.iter Unix.set_nonblock [stdin_w; stdout_r; stderr_r];
        { pid; input = stdin_w; output = stdout_r; errors = stderr_r }
    in
    let { pid; input = stdin_w; output = stdout_r; errors = stderr_r } =
      connection
    in
    child := Some pid;
    stderr_fd := Some stderr_r;
    let pending = ref input and offset = ref 0 in
    let status = ref None and first_line = Buffer.create 16 in
    let response = Buffer.create 128 in
    let complete = ref false and response_line = Buffer.create 128 in
    let rec response_output s =
      match String.index_opt s '\n' with
      | None -> bounded_add response_line s
      | Some end_line ->
        bounded_add response_line (String.sub s 0 end_line);
        let line = Buffer.contents response_line in
        Buffer.clear response_line;
        if line = "vox-query-done" || line = "\"vox-query-done\""
        then complete := true
        else bounded_add response (line ^ "\n");
        let tail =
          String.sub s (end_line + 1) (String.length s - end_line - 1)
        in
        if !complete && tail <> "" then protocol "Output after query terminator";
        if tail <> "" then response_output tail
    in
    let rec output s =
      match !status with
      | Some _ -> response_output s
      | None -> (
        match String.index_opt s '\n' with
        | None -> bounded_add first_line s
        | Some end_line ->
          bounded_add first_line (String.sub s 0 end_line);
          let answer = String.trim (Buffer.contents first_line) in
          let tail =
            String.sub s (end_line + 1) (String.length s - end_line - 1)
          in
          Buffer.clear first_line;
          if answer = ""
          then output tail
          else begin
            if session.mode <> Reset && Option.is_some resource_limit
               && setup_exhausted answer
            then raise Setup_exhausted;
            let followup =
              followup ~request_model ?assumptions q.symbols answer in
            status := Some answer;
            pending
              := !pending ^ followup ^ resource_request
                 ^ "(echo \"vox-query-done\")\n";
            response_output tail
          end)
    in
    let readers = ref [stdout_r; stderr_r] and stdin_open = ref true in
    let bytes = Bytes.create 4096 in
    let read fd =
      match Unix.read fd bytes 0 (Bytes.length bytes) with
      | 0 ->
        readers := List.filter (( <> ) fd) !readers;
        close fd;
        if fd = stdout_r && !status = None && Buffer.length first_line > 0
        then output "\n"
      | n ->
        let s = Bytes.sub_string bytes 0 n in
        if fd = stdout_r then output s else bounded_add stderr s
      | exception
          Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
        ->
        ()
    in
    while (not !complete) && (!exit_status = None || !readers <> []) do
      let timeout = poll () in
      let writes =
        if !stdin_open && !offset < String.length !pending
        then [stdin_w]
        else []
      in
      let ready, writable, _ =
        try Unix.select !readers writes [] timeout
        with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], []
      in
      List.iter read ready;
      List.iter
        (fun fd ->
          try
            let n =
              Unix.write_substring fd !pending !offset
                (min 4096 (String.length !pending - !offset))
            in
            let sent = String.sub !pending !offset n in
            offset := !offset + n;
            callback dump sent
          with
          | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
            ->
            ()
          | Unix.Unix_error (Unix.EPIPE, _, _) ->
            close stdin_w;
            stdin_open := false)
        writable;
      if !exit_status = None
      then
        match waitpid [Unix.WNOHANG] pid with
        | 0, _ -> ()
        | _, status -> exit_status := Some status
    done;
    match !exit_status, !status with
    | None, Some answer when !complete ->
      let rec drain_errors () =
        match Unix.read stderr_r bytes 0 (Bytes.length bytes) with
        | 0 -> ()
        | n ->
          bounded_add stderr (Bytes.sub_string bytes 0 n);
          drain_errors ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain_errors ()
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          ()
      in
      drain_errors ();
      if !offset <> String.length !pending
      then protocol "Incomplete query write";
      ignore (poll ());
      let cumulative_resources = ref None in
      let result =
        Fun.protect
          ~finally:(fun () ->
            resources :=
              (match !cumulative_resources, previous_resources with
              | Some current, Some previous when current >= previous ->
                Some (current - previous)
              | _ -> None))
          (fun () ->
            interpret_response ~request_model ~resources:cumulative_resources
              ?assumptions
              ~core q.symbols answer (Buffer.contents response))
      in
      ignore (poll ());
      (match result with
      | Timeout | Failure _ -> ()
      | Valid | Invalid _ | Unknown _ ->
        session.connection <- Some connection;
        session.retained <- Option.map (fun retained ->
          { retained with resources = !cumulative_resources }) retained;
        if session.mode <> Reset then begin
          session.checks <- session.checks + 1;
          session.bytes <- session.bytes
            + min (String.length !pending) (16 * 1024 * 1024)
        end;
        keep := true);
      result
    | Some (Unix.WEXITED 0), _ ->
      protocol "Solver exited without completing the query"
    | Some (Unix.WEXITED code), _ ->
      protocol (Printf.sprintf "Solver exited with status %d" code)
    | Some (Unix.WSIGNALED signal | Unix.WSTOPPED signal), _ ->
      protocol (Printf.sprintf "Solver terminated by signal %d" signal)
    | None, _ -> protocol "Solver was not reaped"
  in
  let previous_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  let validity =
    match
      Fun.protect
        ~finally:(fun () ->
          Fun.protect
            ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous_sigpipe)
            cleanup)
        (fun () ->
          try run () with
          | Deadline -> Timeout
          | Setup_exhausted -> Unknown (Some "resource limit")
          | Protocol_error message -> Failure message
          | Unix.Unix_error (Unix.ENOENT, "create_process", _) ->
            Failure
              (Printf.sprintf
                 "Cannot execute %S: install Z3 %s or set the solver \
                  executable"
                 config.executable expected_version)
          | Unix.Unix_error (error, call, _) ->
            Failure (Printf.sprintf "%s: %s" call (Unix.error_message error)))
    with
    | validity -> validity
    | exception Callback_error (exn, backtrace) ->
      Printexc.raise_with_backtrace exn backtrace
  in
  let finished = monotonic_time () in
  { validity;
    stderr = Buffer.contents stderr;
    resources = !resources;
    core = (match validity with Valid -> !core | _ -> None);
    encoding_seconds = !encoded -. started;
    solving_seconds = finished -. !encoded
  }

let check_embedded ?(config = default_config) ?dump
    ?(cancelled = fun () -> false) ?(request_model = true)
    ?resource_limit ?assumptions ~int_width q =
  if config.timeout_ms <= 0 then invalid_arg "Vox_smt_solver: timeout_ms";
  let started = monotonic_time () in
  let deadline = started +. float config.timeout_ms /. 1000. in
  let cancelled () =
    let stopped = cancelled () in
    if not stopped && monotonic_time () >= deadline then raise Deadline;
    stopped
  in
  try
    Option.iter (fun dump ->
      let poll () = if cancelled () then raise Cancelled in
      let text = to_smtlib ~poll ?resource_limit ?assumptions ~int_width
          ~timeout_ms:config.timeout_ms q in
      dump text) dump;
    let result = Vox_smt_embedded.check ~request_model ~cancelled
        ?resource_limit
        ?assumptions ~clock:monotonic_time ~timeout_ms:config.timeout_ms
        ~int_width q in
    { validity = result.validity; stderr = ""; resources = result.resources;
      core = result.core;
      encoding_seconds = monotonic_time () -. started
                         -. result.solving_seconds;
      solving_seconds = result.solving_seconds }
  with
  | Vox_smt_embedded.Cancelled -> raise Cancelled
  | Deadline ->
    { validity = Timeout; stderr = ""; resources = None; core = None;
      encoding_seconds = 0.; solving_seconds = monotonic_time () -. started }

let with_session_impl ~mode ?config ?dump ?cancelled ~int_width f =
  let session =
    { connection = None; retained = None; mode; checks = 0; bytes = 0 }
  in
  let capture =
    match Sys.getenv_opt "VOX_SMT_API_CORPUS_DIR" with
    | None | Some "" -> None
    | Some directory ->
      let path, output = Filename.open_temp_file ~temp_dir:directory
        "session-" ".vapi" in
      (match Out_channel.with_open_text (path ^ ".source") (fun metadata ->
        List.iter (fun key ->
          output_string metadata
            (Option.value (Sys.getenv_opt key) ~default:"");
          output_char metadata '\n')
          ["VOX_SMT_CORPUS_SOURCE"; "VOX_SMT_CORPUS_RUN_ID";
           "VOX_SMT_CORPUS_CONFIG"]) with
      | () -> Some output
      | exception exn -> close_out_noerr output; raise exn)
  in
  let closed = ref false and busy = ref false in
  Fun.protect
    ~finally:(fun () ->
      closed := true;
      Option.iter close_out_noerr capture;
      Option.iter dispose session.connection;
      session.connection <- None;
      session.retained <- None)
    (fun () ->
      f (fun ?(request_model = true) ?resource_limit ?assumptions query ->
          if !closed then invalid_arg "Vox_smt_solver: closed session";
          if !busy then invalid_arg "Vox_smt_solver: recursive session query";
          busy := true;
          Fun.protect
            ~finally:(fun () -> busy := false)
            (fun () ->
              Option.iter (fun output ->
                Vox_smt_query_codec.write_call output
                  { int_width;
                    timeout_ms = (Option.value config
                      ~default:default_config).timeout_ms;
                    resource_limit; assumptions; query; request_model };
                flush output) capture;
              if (Option.value config ~default:default_config).executable
                 = "ocaml"
              then check_embedded ~request_model ?config ?dump ?cancelled
                  ?resource_limit
                  ?assumptions ~int_width query
              else check_impl session ~request_model ?config ?dump ?cancelled
                  ?resource_limit
                  ?assumptions ~int_width query)))

let with_session ?config ?dump ?cancelled ~int_width f =
  with_session_impl ~mode:Reset ?config ?dump ?cancelled ~int_width f

let with_incremental_session ?config ?dump ?cancelled ~int_width f =
  with_session_impl ~mode:Incremental ?config ?dump ?cancelled ~int_width f

let with_automatic_session ?config ?dump ?cancelled ~int_width f =
  with_session_impl ~mode:Automatic ?config ?dump ?cancelled ~int_width f

let check ?request_model ?config ?dump ?cancelled ?resource_limit
    ?assumptions ~int_width query =
  with_session ?config ?dump ?cancelled ~int_width (fun check ->
      check ?request_model ?resource_limit ?assumptions query)
