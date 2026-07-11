(* Real-solver CLI implementing the harness's SOLVER contract (tests/README.md): given a
   .smt2 file as argv[1], print exactly one (result ...) block per (check-sat), each with
   a verdict and the counter trio, plus a (model ...) for a pure-Boolean sat.

   This is DEV/BENCH tooling, NOT shipped: it links the TEST-ONLY SMT-LIB parser
   (Oxsmt_smtlib_parser) to read .smt2, and drives the shipped session layer
   (Oxsmt_interface.Session). The shipped session never links the parser — that boundary
   is why this lives under tests/ (see the dune comment).

   Driving model: the shipped parser is a whole-document reader that does not support
   incremental push/pop, and v1 solves the whole assertion set at once. So:
   - a batch file (exactly one check-sat, no push/pop) is parsed into the session's
     context and solved once — the real path exercised by every regression case;
   - an incremental file (push/pop, or multiple check-sats) degrades to one `unknown`
     block per check-sat. That is always SOUND, and matches the current corpus (the only
     such file is a theory case, already unknown under THE SOUNDNESS RULE). Full
     incremental .smt2 driving through Session.push/pop is a documented follow-up. *)

module Session = Oxsmt_interface.Session
module Sexp = Oxsmt_smtlib_parser.Sexp
module Parser = Oxsmt_smtlib_parser.Parser

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

type block =
  { verdict : string
  ; model : (string * bool) list option
  ; conflicts : int
  ; decisions : int
  ; propagations : int
  }

let unknown_block =
  { verdict = "unknown"; model = None; conflicts = 0; decisions = 0; propagations = 0 }
;;

let print_block b =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "(result";
  Printf.bprintf buf " (verdict %s)" b.verdict;
  (match b.model with
   | Some m ->
     let m = List.sort (fun (a, _) (c, _) -> String.compare a c) m in
     Buffer.add_string buf " (model (";
     List.iteri
       (fun i (name, v) ->
          if i > 0 then Buffer.add_char buf ' ';
          Printf.bprintf buf "(%s %b)" name v)
       m;
     Buffer.add_string buf "))"
   | None -> ());
  Printf.bprintf
    buf
    " (counters (conflicts %d) (decisions %d) (propagations %d)))"
    b.conflicts
    b.decisions
    b.propagations;
  print_string (Buffer.contents buf);
  print_newline ()
;;

(* Count top-level check-sats and detect incremental commands, exactly as the harness
   counts goals (check-sat / check-sat-assuming). *)
let scan_commands sexps =
  List.fold_left
    (fun (n_checks, incr) sx ->
       match sx with
       | Sexp.List (Sexp.Atom ("check-sat" | "check-sat-assuming") :: _) ->
         n_checks + 1, incr
       | Sexp.List (Sexp.Atom ("push" | "pop") :: _) -> n_checks, true
       | _ -> n_checks, incr)
    (0, false)
    sexps
;;

let verdict_string = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

(* Batch solve: one check-sat, no push/pop. Parse into the session's own context so the
   asserted terms share its tag stream, then solve once. *)
let solve_batch src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) ->
    (* out-of-subset or unparseable as a query -> sound unknown (I8) *)
    unknown_block
  | parsed ->
    List.iter (Session.assert_term s) parsed.Parser.assertions;
    let v = Session.check_sat s in
    let st = Session.stats s in
    { verdict = verdict_string v
    ; model = Session.get_model s
    ; conflicts = st.Oxsmt_solver.Sat.Stats.conflicts
    ; decisions = st.Oxsmt_solver.Sat.Stats.decisions
    ; propagations = st.Oxsmt_solver.Sat.Stats.propagations
    }
;;

let () =
  let file =
    if Array.length Sys.argv >= 2
    then Sys.argv.(1)
    else (
      prerr_endline "oxsmt_cli: expected a .smt2 file argument";
      exit 2)
  in
  let src = read_file file in
  let sexps =
    match Sexp.parse_many src with
    | s -> s
    | exception Sexp.Malformed _ -> []
  in
  let n_checks, incremental = scan_commands sexps in
  let blocks =
    if incremental || n_checks <> 1
    then List.init n_checks (fun _ -> unknown_block)
    else [ solve_batch src ]
  in
  List.iter print_block blocks
;;
