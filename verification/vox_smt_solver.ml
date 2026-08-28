open Vox_smt

type config =
  { executable : string;
    timeout_ms : int
  }

let default_config = { executable = "z3"; timeout_ms = 5000 }

external monotonic_time : unit -> float = "caml_vox_smt_monotonic_time"

type result =
  { validity : validity;
    stderr : string
  }

exception Cancelled

exception Deadline

exception Protocol_error of string

exception Callback_error of exn * Printexc.raw_backtrace

let callback f x =
  try f x
  with exn -> raise (Callback_error (exn, Printexc.get_raw_backtrace ()))

let protocol message = raise (Protocol_error message)

type sexp =
  | Atom of string
  | String of string
  | List of sexp list

let parse text =
  let pos = ref 0 and len = String.length text in
  let rec space () =
    if !pos < len
    then
      match text.[!pos] with
      | ' ' | '\t' | '\r' | '\n' ->
        incr pos;
        space ()
      | ';' ->
        while !pos < len && text.[!pos] <> '\n' do
          incr pos
        done;
        space ()
      | _ -> ()
  in
  let rec value depth =
    if depth > 256 then protocol "Solver response is nested too deeply";
    space ();
    if !pos = len then protocol "Incomplete solver response";
    match text.[!pos] with
    | '(' ->
      incr pos;
      List (items (depth + 1) [])
    | ')' -> protocol "Unexpected ')' in solver response"
    | '"' ->
      incr pos;
      let b = Buffer.create 32 in
      let rec quoted () =
        if !pos = len then protocol "Unterminated solver string";
        let c = text.[!pos] in
        incr pos;
        if c = '"'
        then
          if !pos < len && text.[!pos] = '"'
          then (
            incr pos;
            Buffer.add_char b c;
            quoted ())
          else String (Buffer.contents b)
        else (
          Buffer.add_char b c;
          quoted ())
      in
      quoted ()
    | _ ->
      let start = !pos in
      while
        !pos < len
        && not
             (List.mem text.[!pos] [' '; '\t'; '\r'; '\n'; '('; ')'; ';'; '"'])
      do
        incr pos
      done;
      Atom (String.sub text start (!pos - start))
  and items depth acc =
    space ();
    if !pos = len then protocol "Unterminated solver list";
    if text.[!pos] = ')'
    then (
      incr pos;
      List.rev acc)
    else
      let x = value depth in
      items depth (x :: acc)
  in
  let rec all acc =
    space ();
    if !pos = len
    then List.rev acc
    else
      let x = value 0 in
      all (x :: acc)
  in
  all []

let model symbols response =
  let integer = function
    | Atom digits -> Int64.of_string_opt digits
    | List [Atom "-"; Atom digits] ->
      Option.map Int64.neg (Int64.of_string_opt digits)
    | _ -> None
  in
  let value symbol sexp =
    match Symbol.sort symbol, sexp with
    | Bool, Atom "true" -> Some (Bool_value true)
    | Bool, Atom "false" -> Some (Bool_value false)
    | Int, Atom digits when decimal_integer digits && digits.[0] <> '-' ->
      Some (Bigint_value digits)
    | Int, List [Atom "-"; Atom digits]
      when decimal_integer digits && digits.[0] <> '-' ->
      Some (Bigint_value (if digits = "0" then "0" else "-" ^ digits))
    | Int63, sexp ->
      Option.bind (integer sexp) (fun n ->
          if n < -4611686018427387904L || n > 4611686018427387903L
          then None
          else Some (Int_value n))
    | (Opaque _ | Datatype _), _ -> None
    | _ -> None
  in
  let rec bindings i acc symbols entries =
    match symbols, entries with
    | [], [] -> Some (List.rev acc)
    | s :: ss, List [Atom name; v] :: vs when name = "v" ^ string_of_int i ->
      Option.bind (value s v) (fun v -> bindings (i + 1) ((s, v) :: acc) ss vs)
    | _ -> None
  in
  match response with
  | List entries -> bindings 0 [] symbols entries
  | _ -> None

let interpret symbols status response =
  match status, parse response with
  | "unsat", [] -> Valid
  | "sat", [] -> Invalid (if symbols = [] then Some [] else None)
  | "sat", [List [Atom "error"; String _]] -> Invalid None
  | "sat", [(List entries as values)]
    when List.for_all (function List [Atom _; _] -> true | _ -> false) entries
    ->
    Invalid (model symbols values)
  | "unknown", [] -> Unknown None
  | "unknown", [List [Atom ":reason-unknown"; String reason]] ->
    if reason = "timeout" then Timeout else Unknown (Some reason)
  | "unknown", [List [Atom "error"; String _]] -> Unknown None
  | _ -> protocol "Unexpected solver response"

let rec waitpid flags pid =
  try Unix.waitpid flags pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let check ?(config = default_config) ?(dump = fun _ -> ())
    ?(cancelled = fun () -> false) ~int_width q =
  let input = to_smtlib ~int_width ~timeout_ms:config.timeout_ms q in
  let deadline = monotonic_time () +. (float config.timeout_ms /. 1000.) in
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
    stderr_fd := Some stderr_r;
    let pending = ref input and offset = ref 0 in
    let status = ref None and first_line = Buffer.create 16 in
    let response = Buffer.create 128 in
    let rec output s =
      match !status with
      | Some _ -> bounded_add response s
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
            let followup =
              match answer with
              | "unsat" -> ""
              | "sat" ->
                if q.symbols = []
                then ""
                else
                  "(get-value ("
                  ^ String.concat " "
                      (List.mapi (fun i _ -> "v" ^ string_of_int i) q.symbols)
                  ^ "))\n"
              | "unknown" -> "(get-info :reason-unknown)\n"
              | _ ->
                protocol
                  (Printf.sprintf "Expected sat, unsat or unknown; got %S"
                     (String.sub answer 0 (min 200 (String.length answer))))
            in
            status := Some answer;
            pending := !pending ^ followup ^ "(exit)\n";
            bounded_add response tail
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
    while !exit_status = None || !readers <> [] do
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
      if !stdin_open && !status <> None && !offset = String.length !pending
      then (
        close stdin_w;
        stdin_open := false);
      if !exit_status = None
      then
        match waitpid [Unix.WNOHANG] pid with
        | 0, _ -> ()
        | _, status -> exit_status := Some status
    done;
    match !exit_status, !status with
    | Some (Unix.WEXITED 0), Some answer ->
      if !offset <> String.length !pending
      then protocol "Solver closed its input before receiving the full query";
      ignore (poll ());
      let result = interpret q.symbols answer (Buffer.contents response) in
      ignore (poll ());
      result
    | Some (Unix.WEXITED 0), None -> protocol "Solver exited without an answer"
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
                 "Cannot execute %S: install Z3 4.16.0 or set the solver \
                  executable"
                 config.executable)
          | Unix.Unix_error (error, call, _) ->
            Failure (Printf.sprintf "%s: %s" call (Unix.error_message error)))
    with
    | validity -> validity
    | exception Callback_error (exn, backtrace) ->
      Printexc.raise_with_backtrace exn backtrace
  in
  { validity; stderr = Buffer.contents stderr }
