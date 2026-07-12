(* Per-file corpus classifier (board #124). Given one .smt2 file (first non-flag arg) and
   an optional [--max-effort N] (board #60), print "<outcome> <effort>" on stdout: the
   outcome token is solved-sat | solved-unsat | unknown | unknown-incremental | parse-fail
   | mismatch, and [effort] is the deterministic counted work of the check (SAT
   conflicts + decisions + seam Final-rounds; 0 for the paths that never run a check).
   [--max-effort] is the deterministic, load-independent cutoff (a would-be answer past
   the cap becomes `unknown`); absent = unbounded, and the emitted effort then feeds
   calibration. Exit 0 always (the driver treats a nonzero/killed exit as timeout/error).

   It mirrors the solver CLI's decision (Parser->Session: a single-check-sat,
   push/pop-free file is solved once; anything incremental degrades) so the numbers
   reflect the shipped path — but, unlike the CLI (which soundly folds a rejected parse
   into `unknown`), it keeps `parse-fail` DISTINCT, because that distinction is
   load-bearing for the baseline (the QF_UFLIA bignum ceiling is 76 parse-fails, tracked
   separately). It also reads the file's (set-info :status ...) label and reports
   `mismatch` when a definite verdict contradicts a definite label — the soundness
   tripwire.

   Run one process PER FILE under the driver's `timeout`: process isolation means a hard
   SIGKILL bounds each file with no shared state, sidestepping the shared-intern-table +
   SIGALRM-mid-intern determinism wart of an in-process serial sweep. Test-only, stdlib. *)

module Session = Oxsmt_interface.Session
module Sexp = Oxsmt_smtlib_parser.Sexp
module Parser = Oxsmt_smtlib_parser.Parser

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

(* Label = the first (set-info :status <v>). Scan raw text; skip ';' comment lines; after
   ":status" skip ALL whitespace (space/tab/CR/newline) before the value — a newline
   between :status and its value must NOT hide the label (else its verdict would go
   unchecked for mismatch, a silent soundness false-negative). *)
let label_of src =
  let n = String.length src in
  let is_ws c = c = ' ' || c = '\t' || c = '\r' || c = '\n' in
  let rec scan i =
    if i >= n
    then None
    else if src.[i] = ';'
    then (
      let j = ref i in
      while !j < n && src.[!j] <> '\n' do
        incr j
      done;
      scan !j)
    else if i + 7 <= n && String.sub src i 7 = ":status"
    then (
      let j = ref (i + 7) in
      while !j < n && is_ws src.[!j] do
        incr j
      done;
      let s = !j in
      while
        !j < n
        &&
        match src.[!j] with
        | 'a' .. 'z' -> true
        | _ -> false
      do
        incr j
      done;
      match String.sub src s (!j - s) with
      | "sat" -> Some "sat"
      | "unsat" -> Some "unsat"
      | "unknown" -> Some "unknown"
      | _ -> scan !j)
    else scan (i + 1)
  in
  scan 0
;;

let scan_commands sexps =
  List.fold_left
    (fun (n_checks, incr) sx ->
       (* Command keywords are UNQUOTED symbol heads; dispatch on that text, exactly as the
         parser/CLI do since the tokenizer landed ([Atom] carries a [Lexer.token], not a
         string — [Sexp.simple] extracts an unquoted symbol's text). *)
       match sx with
       | Sexp.List (head :: _) ->
         (match Sexp.simple head with
          | Some ("check-sat" | "check-sat-assuming") -> n_checks + 1, incr
          | Some ("push" | "pop") -> n_checks, true
          | _ -> n_checks, incr)
       | _ -> n_checks, incr)
    (0, false)
    sexps
;;

let () =
  (* Args: the .smt2 file (first non-flag arg) plus an optional [--max-effort N] — the
     board #60 deterministic counted cutoff. Absent = unbounded: the effort is still
     counted and emitted, so a plain (uncapped) sweep doubles as the calibration run that
     records per-file effort to pick N. *)
  let file = ref None in
  let max_effort = ref None in
  let rec parse = function
    | [] -> ()
    | "--max-effort" :: n :: rest ->
      max_effort := Some (int_of_string n);
      parse rest
    | f :: rest when !file = None ->
      file := Some f;
      parse rest
    | _ :: rest -> parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  let file =
    match !file with
    | Some f -> f
    | None ->
      prerr_endline "corpus_classify: expected a .smt2 file argument";
      exit 2
  in
  let src = read_file file in
  let token, effort =
    match Sexp.parse_many src with
    | exception Sexp.Malformed _ -> "parse-fail", 0
    | sexps ->
      let n_checks, incremental = scan_commands sexps in
      if incremental || n_checks <> 1
      then "unknown-incremental", 0
      else (
        let s = Session.create ?max_effort:!max_effort () in
        match Parser.parse_into (Session.env s) (Session.context s) src with
        | exception (Parser.Malformed _ | Parser.Unsupported _) -> "parse-fail", 0
        | parsed ->
          List.iter (Session.assert_term s) parsed.Parser.assertions;
          let v = Session.check_sat s in
          let label = label_of src in
          let tok =
            match v with
            | Session.Sat -> if label = Some "unsat" then "mismatch" else "solved-sat"
            | Session.Unsat -> if label = Some "sat" then "mismatch" else "solved-unsat"
            | Session.Unknown -> "unknown"
          in
          tok, Session.effort s)
  in
  (* Line: "<token> <effort>". [effort] is a deterministic function of the file (#60), so
     the sweep records it for calibration; the driver splits on whitespace. *)
  Printf.printf "%s %d\n" token effort
;;
