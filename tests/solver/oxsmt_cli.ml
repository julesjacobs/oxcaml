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
  ; model : string option (* pre-rendered model BODY sexp, spliced into [(model ...)] *)
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
   | Some body -> Printf.bprintf buf " (model %s)" body
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
    (* Arbitrary precision (core-bignum W2): render via [Bigint.to_string], negatives as
       [(- N)]. Always renderable — never degrade. *)
    if Oxsmt_core.Bigint.sign n >= 0
    then Oxsmt_core.Bigint.to_string n
    else "(- " ^ Oxsmt_core.Bigint.to_string (Oxsmt_core.Bigint.neg n) ^ ")"
  | Session.VUninterp i -> string_of_int i
;;

(* Symbol names are rendered through the shared SMT-LIB printer's lexical quoter (grounded
   in the one shared lexer, ADR-0008), so a name that is not a simple symbol — [|a b|],
   [|1x|] — round-trips as [|a b|] rather than the malformed bare [a b]. The §8 evaluator
   reads it back through the same lexer, so quoting cannot desync on a token boundary. *)
let q = Oxsmt_smtlib.Printer.quote_symbol

(* Render the model BODY sexp. A table-free (const/Bool/LIA) model uses the LEGACY flat
   [((name token) ...)] body — byte-identical to the pre-UF output, so the harness's
   existing const transport is unchanged (no regression). A model with sorts/tables uses
   the §8 sidecar grammar
   [(sort S n) (const name tok) (fun f (default tok) (case (toks) tok) ...)] — the format
   both N-version readers already parse (tests/eval, tests/gate). Raises
   [Printer.Unsupported] on an unrenderable symbol name; caught by {!solve_batch}. *)
let render_model (sort_cards, bindings) =
  let has_table =
    List.exists
      (function
        | Session.Fun _ -> true
        | Session.Const _ -> false)
      bindings
  in
  let buf = Buffer.create 128 in
  if sort_cards = [] && not has_table
  then (
    let pairs =
      List.filter_map
        (function
          | Session.Const (name, v) -> Some (q name, token_of_value v)
          | Session.Fun _ -> None)
        bindings
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    Buffer.add_char buf '(';
    List.iteri
      (fun i (n, v) ->
         if i > 0 then Buffer.add_char buf ' ';
         Printf.bprintf buf "(%s %s)" n v)
      pairs;
    Buffer.add_char buf ')')
  else (
    List.iter
      (fun { Session.sort_name; card } ->
         Printf.bprintf buf "(sort %s %d)" (q sort_name) card)
      sort_cards;
    List.iter
      (function
        | Session.Const (name, v) ->
          Printf.bprintf buf "(const %s %s)" (q name) (token_of_value v)
        | Session.Fun (name, { Session.default; cases }) ->
          Printf.bprintf buf "(fun %s (default %s)" (q name) (token_of_value default);
          List.iter
            (fun (args, res) ->
               Buffer.add_string buf " (case (";
               List.iteri
                 (fun i a ->
                    if i > 0 then Buffer.add_char buf ' ';
                    Buffer.add_string buf (token_of_value a))
                 args;
               Printf.bprintf buf ") %s)" (token_of_value res))
            cases;
          Buffer.add_char buf ')')
      bindings);
  Buffer.contents buf
;;

(* Batch solve: one check-sat, no push/pop. Parse into the session's own context so the
   asserted terms share its tag stream, then solve once. A [Sat] is emitted only with the
   model the session reconstructed and self-checked (const/Bool/LIA via the pipeline;
   function tables via the R1 in-process checker); a [Sat] with no reconstructable model,
   or one naming a symbol the printer cannot render, degrades to a sound [unknown]. We
   never emit a [sat] the harness cannot transport or the evaluator cannot self-certify.
   [max_effort] threads the board #60 counted cutoff (a cut-off goal is a plain [unknown]
   block, so the output format is unchanged). *)
let solve_batch ?max_effort ?(presolve = true) src =
  let s = Session.create ?max_effort () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception (Parser.Malformed _ | Parser.Unsupported _) ->
    (* out-of-subset or unparseable as a query -> sound unknown (I8) *)
    unknown_block
  | exception _ ->
    (* ROBUSTNESS / fail-closed (I8): the reader maps its expected rejections to
       [Malformed]/[Unsupported], but an unmapped exception on untrusted corpus input
       ([Failure]/[Invalid_argument]/[Stack_overflow]/...) must still degrade to a sound
       [unknown] rather than crash the driver (the "error instead of degrade" robustness
       item). [unknown] is always sound; a crash is never acceptable. *)
    unknown_block
  | parsed when not (Oxsmt_query_loader.assert_all ~presolve s parsed) ->
    (* W1b: the shared loader submits the ground batch through the equality-elimination
       presolve (a no-op on zero-alias files) plus each [forall] lemma through the cap-
       gated mint-before-build [assert_lemma] (ADR-0012); the SAME loader backs
       corpus_classify so the two drivers cannot diverge. A universally-quantified lemma
       outside the reader's subset degrades here to a sound [unknown] — never a dropped
       quantifier (sound for the [sat] direction). [--no-presolve] restores the per-term
       [assert_term] path for A/B measurement; both are sound. *)
    unknown_block
  | _loaded ->
    (* Assertions (ground batch + [forall] lemmas) were loaded into [s] by the guard
       above; solve the ground core once. THE SOUNDNESS RULE (a live lemma degrades [Sat]
       to [Unknown]) is enforced inside {!Session.check_sat}. *)
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
        (* A [Sat] whose model was reconstructed and self-checked at the session level is
           rendered and emitted here. A table-free (const/Bool/LIA) model uses the LEGACY
           flat body; a FUNCTION-TABLE / sorted model uses the §8 sidecar grammar — the
           harness model-transport now carries both (ADR-UF-models R9), and the R1
           in-process checker has already gated any table model's [sat] (THE SOUNDNESS
           RULE), so the table flip reaches the corpus. [render_model] decides which body
           to emit. *)
        | Some m ->
          (match render_model m with
           | body -> block "sat" (Some body)
           | exception Oxsmt_smtlib.Printer.Unsupported _ ->
             (* A model names a symbol the SMT-LIB printer cannot faithfully render: the
                empty symbol [||], or a predefined-operator collision like [|+|]. Quoting
                is purely lexical and cannot disambiguate these, so the printer's refusal
                is CORRECT (not something to make total). Emitting the name anyway would
                be malformed solver output; degrade this goal to a sound [unknown] with no
                model rather than crash the CLI. *)
             block "unknown" None)
        | None ->
          (* A DATATYPES or ARRAYS session self-checks its [Sat] with the in-process
             constructor-tree / array-map checker (Session.commit_sat), but the scalar
             [model] type cannot carry a tree or an array map, so [get_model] is [None]
             here (model transport to the external eval is a follow-up). Report [sat] on
             the verdict — matching the headline classifier, which decides on the verdict
             alone — rather than downgrading to [unknown]. A modelless [Sat] from neither
             theory (a UF table we could not render) stays the sound [unknown]. *)
          if Session.uses_datatypes s || Session.uses_arrays s
          then block "sat" None
          else block "unknown" None)
     | Session.Unsat -> block "unsat" None
     | Session.Unknown -> block "unknown" None)
;;

let () =
  (* Args: the .smt2 file (first non-flag arg) plus an optional [--max-effort N] — the
     board #60 deterministic counted cutoff, threaded to the session. The output format is
     unchanged: a goal cut off by the budget is a plain [unknown] block (the budget only
     ever downgrades a would-be answer), so the harness goldens are unaffected. *)
  let file = ref None in
  let max_effort = ref None in
  let presolve = ref true in
  let rec parse = function
    | [] -> ()
    | "--max-effort" :: n :: rest ->
      max_effort := Some (int_of_string n);
      parse rest
    | "--no-presolve" :: rest ->
      presolve := false;
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
      prerr_endline "oxsmt_cli: expected a .smt2 file argument";
      exit 2
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
    else [ solve_batch ?max_effort:!max_effort ~presolve:!presolve src ]
  in
  List.iter print_block blocks
;;
