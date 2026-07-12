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
  ; model : (string * string) list option (* (name, value-token) per nullary symbol *)
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
          Printf.bprintf buf "(%s %s)" name v)
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
       | Sexp.List (head :: _) ->
         (match Sexp.simple head with
          | Some ("check-sat" | "check-sat-assuming") -> n_checks + 1, incr
          | Some ("push" | "pop") -> n_checks, true
          | _ -> n_checks, incr)
       | _ -> n_checks, incr)
    (0, false)
    sexps
;;

(* Render a model value as the token the eval Model reader types against the symbol's
   declared sort: [true]/[false] for Bool, the numeral (SMT-LIB [(- n)] for negatives) for
   Int, the element index for an uninterpreted sort. A negative is rendered by STRIPPING
   the leading '-' from [string_of_int] rather than negating, so [min_int] does not
   overflow (mirrors the shipped printer's [add_int_lit]): [-n] would wrap for [min_int]
   and emit the malformed [(- -4611686018427387904)]. The value is always renderable —
   never degrade. *)
let token_of_value = function
  | Session.VBool b -> if b then "true" else "false"
  | Session.VInt n ->
    if n >= 0
    then string_of_int n
    else (
      let s = string_of_int n in
      "(- " ^ String.sub s 1 (String.length s - 1) ^ ")")
  | Session.VUninterp i -> string_of_int i
;;

(* Symbol names are rendered through the shared SMT-LIB printer's lexical quoter (grounded
   in the one shared lexer, ADR-0008), so a name that is not a simple symbol — [|a b|],
   [|1x|] — round-trips as [|a b|] rather than the malformed bare [a b]. The §8 evaluator
   reads it back through the same lexer, so quoting cannot desync on a token boundary. *)
let render_model bindings =
  List.map
    (fun (Session.Const (name, v)) ->
       Oxsmt_smtlib.Printer.quote_symbol name, token_of_value v)
    bindings
;;

(* Batch solve: one check-sat, no push/pop. Parse into the session's own context so the
   asserted terms share its tag stream, then solve once. A [Sat] whose model cannot be
   reconstructed for the §8 self-check (a UF model that would need function tables — a v1
   limit) is soundly reported as [unknown]: we never emit a [sat] the harness cannot
   self-certify. *)
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
    let block verdict model =
      { verdict
      ; model
      ; conflicts = st.Oxsmt_solver.Sat.Stats.conflicts
      ; decisions = st.Oxsmt_solver.Sat.Stats.decisions
      ; propagations = st.Oxsmt_solver.Sat.Stats.propagations
      }
    in
    (match v with
     | Session.Sat ->
       (match Session.get_model s with
        | Some bindings ->
          (match render_model bindings with
           | model -> block "sat" (Some model)
           | exception Oxsmt_smtlib.Printer.Unsupported _ ->
             (* A model names a symbol the SMT-LIB printer cannot faithfully render: the
                empty symbol [||], or a predefined-operator collision like [|+|]. Quoting
                is purely lexical and cannot disambiguate these, so the printer's refusal
                is CORRECT (not something to make total). Emitting the name anyway would
                be malformed solver output; degrade this goal to a sound [unknown] with no
                model rather than crash the CLI. *)
             block "unknown" None)
        | None -> block "unknown" None)
     | Session.Unsat -> block "unsat" None
     | Session.Unknown -> block "unknown" None)
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
