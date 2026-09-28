open Vox_smt
open Vox_smt_response

type config =
  { executable : string;
    timeout_ms : int
  }

let default_config = { executable = "z3"; timeout_ms = 5000 }

let expected_version = Vox_smt_response.expected_version

let is_expected_version = Vox_smt_response.is_expected_version

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

type session = { mutable connection : connection option }

let dispose connection =
  (try Unix.kill connection.pid Sys.sigkill with Unix.Unix_error _ -> ());
  ignore (waitpid [] connection.pid);
  List.iter
    (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
    [connection.input; connection.output; connection.errors]

let check_impl session ?(config = default_config) ?(dump = fun _ -> ())
    ?(cancelled = fun () -> false) ?resource_limit ?assumptions ~int_width q =
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
    let input =
      to_smtlib
        ~poll:(fun () -> ignore (poll ()))
        ?resource_limit ?assumptions ~int_width ~timeout_ms:config.timeout_ms q
    in
    encoded := monotonic_time ();
    let input = session_input input in
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
        if line = "vox-query-done"
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
            let followup = followup ?assumptions q.symbols answer in
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
      let result =
        interpret_response ~resources ?assumptions ~core q.symbols answer
          (Buffer.contents response)
      in
      ignore (poll ());
      (match result with
      | Timeout | Failure _ -> ()
      | Valid | Invalid _ | Unknown _ ->
        session.connection <- Some connection;
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

let with_session ?config ?dump ?cancelled ~int_width f =
  let session = { connection = None } in
  let closed = ref false and busy = ref false in
  Fun.protect
    ~finally:(fun () ->
      closed := true;
      Option.iter dispose session.connection;
      session.connection <- None)
    (fun () ->
      f (fun ?resource_limit ?assumptions query ->
          if !closed then invalid_arg "Vox_smt_solver: closed session";
          if !busy then invalid_arg "Vox_smt_solver: recursive session query";
          busy := true;
          Fun.protect
            ~finally:(fun () -> busy := false)
            (fun () ->
              check_impl session ?config ?dump ?cancelled ?resource_limit
                ?assumptions ~int_width query)))

let check ?config ?dump ?cancelled ?resource_limit ?assumptions ~int_width query
    =
  with_session ?config ?dump ?cancelled ~int_width (fun check ->
      check ?resource_limit ?assumptions query)
