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
  ; reason : string
  (* census (task #78): WHY an [unknown] verdict was returned; "" for sat/unsat. Printed
     unconditionally to STDERR (loud, no env gate) while stdout stays SMT-LIB-clean. *)
  }

let unknown_block_with reason =
  { verdict = "unknown"
  ; model = None
  ; conflicts = 0
  ; decisions = 0
  ; propagations = 0
  ; reason
  }
;;

(* census (task #78): flatten an exception message into a short reason-safe token (the
   reason grammar is one line, stops at ')'). Keeps the sub-cause visible for buckets. *)
let san_token s =
  let b = Buffer.create 32 in
  (try
     String.iter
       (fun c ->
         if Buffer.length b >= 40 then raise Exit;
         match c with
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '-' -> Buffer.add_char b c
         | ' ' | '(' -> raise Exit
         | _ -> ())
       s
   with
   | Exit -> ());
  Buffer.contents b
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
  print_newline ();
  (* Data-gathering probe (Stage C mechanism-I measurement): when
     OXSMT_PRINT_COMBINE_STATS is set, emit the LAND-67 backstop hit count to STDERR so it
     never perturbs the stdout result line the gates match on. Unset => no read, no output
     => byte-identical. *)
  match Sys.getenv_opt "OXSMT_PRINT_COMBINE_STATS" with
  | Some ("1" | "true" | "yes") ->
    Printf.eprintf
      "(combine_stats (congruence_split_hits %d))\n"
      (Oxsmt_interface.Cdclt.combine_congruence_split_hit_count ())
  | _ -> ()
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
let real_token q =
  let num = Oxsmt_lia.Rational.num_bigint q in
  let den = Oxsmt_lia.Rational.den_bigint q in
  let negative = Oxsmt_core.Bigint.sign num < 0 in
  let num = if negative then Oxsmt_core.Bigint.neg num else num in
  let body =
    if Oxsmt_core.Bigint.equal den Oxsmt_core.Bigint.one
    then Oxsmt_core.Bigint.to_string num ^ ".0"
    else
      "(/ "
      ^ Oxsmt_core.Bigint.to_string num
      ^ " "
      ^ Oxsmt_core.Bigint.to_string den
      ^ ")"
  in
  if negative then "(- " ^ body ^ ")" else body
;;

let token_of_value = function
  | Session.VBool b -> if b then "true" else "false"
  | Session.VInt n ->
    (* Arbitrary precision (core-bignum W2): render via [Bigint.to_string], negatives as
       [(- N)]. Always renderable — never degrade. *)
    if Oxsmt_core.Bigint.sign n >= 0
    then Oxsmt_core.Bigint.to_string n
    else "(- " ^ Oxsmt_core.Bigint.to_string (Oxsmt_core.Bigint.neg n) ^ ")"
  | Session.VReal q -> real_token q
  | Session.VUninterp i -> string_of_int i
;;

(* Symbol names are rendered through the shared SMT-LIB printer's lexical quoter (grounded
   in the one shared lexer, ADR-0008), so a name that is not a simple symbol — [|a b|],
   [|1x|] — round-trips as [|a b|] rather than the malformed bare [a b]. The §8 evaluator
   reads it back through the same lexer, so quoting cannot desync on a token boundary. *)
let q = Oxsmt_smtlib.Printer.quote_symbol

(* Render one unsat-core assumption literal back to SMT-LIB source: a Bool constant prints
   as its (lexically quoted) name, its negation as [(not name)]. Assumption literals are
   Boolean constants per SMT-LIB (a declared 0-ary Bool symbol, or [true]/[false]), so a
   non-constant atom is not a well-formed literal and renders as a placeholder rather than
   crashing. *)
let render_core_lit ((atom, polarity) : Session.assumption) =
  let name =
    match atom.Oxsmt_core.Term.node with
    | Oxsmt_core.Term.App (sym, args) when Oxsmt_core.Iarr.length args = 0 ->
      q (Oxsmt_core.Symbol.name sym)
    | Oxsmt_core.Term.Bool_const b -> if b then "true" else "false"
    | _ -> "?"
  in
  if polarity then name else "(not " ^ name ^ ")"
;;

(* SMT-LIB [(get-unsat-core)] output: the parenthesized list of the core literals, in the
   input assumption order [check_sat_assuming] preserves. *)
let render_core (core : Session.assumption list) =
  "(" ^ String.concat " " (List.map render_core_lit core) ^ ")"
;;

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
let solve_batch ?max_effort ?(presolve = true) sexps =
  let s = Session.create ?max_effort () in
  match
    Parser.parse_into_sexps
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      sexps
  with
  | exception Parser.Unsupported msg ->
    (* out-of-subset construct -> sound unknown (I8); tag the construct for the census. *)
    unknown_block_with ("cli-parse-unsupported:" ^ san_token msg), None
  | exception Parser.Malformed msg ->
    (* unparseable as a query -> sound unknown (I8). *)
    unknown_block_with ("cli-parse-malformed:" ^ san_token msg), None
  | exception _ ->
    (* ROBUSTNESS / fail-closed (I8): the reader maps its expected rejections to
       [Malformed]/[Unsupported], but an unmapped exception on untrusted corpus input
       ([Failure]/[Invalid_argument]/[Stack_overflow]/...) must still degrade to a sound
       [unknown] rather than crash the driver (the "error instead of degrade" robustness
       item). [unknown] is always sound; a crash is never acceptable. *)
    unknown_block_with "cli-parse-crash", None
  | parsed when not (Oxsmt_query_loader.assert_all ~presolve s parsed) ->
    (* W1b: the shared loader submits the ground batch through the equality-elimination
       presolve (a no-op on zero-alias files) plus each [forall] lemma through the cap-
       gated mint-before-build [assert_lemma] (ADR-0012); the SAME loader backs
       corpus_classify so the two drivers cannot diverge. A universally-quantified lemma
       outside the reader's subset degrades here to a sound [unknown] — never a dropped
       quantifier (sound for the [sat] direction). [--no-presolve] restores the per-term
       [assert_term] path for A/B measurement; both are sound. *)
    unknown_block_with "cli-loader-reject", None
  | parsed ->
    (* Assertions (ground batch + [forall] lemmas) were loaded into [s] by the guard
       above; solve the ground core once. THE SOUNDNESS RULE (a live lemma degrades [Sat]
       to [Unknown]) is enforced inside {!Session.check_sat}. I8 fail-closed: a live lemma
       over the arrays/datatypes theories can reach a path with no EUF+LIA e-graph view
       ([Cdclt.egraph_view] failure); degrade any such unmapped exception to a sound
       [unknown] rather than crash the CLI (mirrors corpus_classify, keeping the two
       drivers equivalent). The degrade is LOUD (visible-failure-modes directive): a
       one-line stderr marker names the exception, so a silent completeness loss is never
       invisible. *)
    (* [(check-sat-assuming (lit ...))] drives the in-process assumption API and, on
       [Unsat], carries the returned core out for [(get-unsat-core)] printing; a plain
       [check-sat] takes the existing path with no core. Both degrade a raised exception
       to a sound [unknown] (LOUD stderr marker). *)
    let v, core_render =
      match parsed.Parser.assumptions with
      | None ->
        ( (try Oxsmt_query_loader.check_sat_refined s with
           | e ->
             Printf.eprintf
               "oxsmt_cli: check_sat degraded to unknown: %s\n"
               (Printexc.to_string e);
             Session.Unknown)
        , None )
      | Some assums ->
        (try
           let { Session.verdict; unsat_core } = Session.check_sat_assuming s assums in
           ( verdict
           , match verdict, unsat_core with
             | Session.Unsat, Some core -> Some (render_core core)
             | _ -> None )
         with
         | e ->
           Printf.eprintf
             "oxsmt_cli: check_sat_assuming degraded to unknown: %s\n"
             (Printexc.to_string e);
           Session.Unknown, None)
    in
    let st = Session.stats s in
    let block ?(reason = "") verdict model =
      { verdict
      ; model
      ; conflicts = st.Oxsmt_solver.Sat.Stats.conflicts
      ; decisions = st.Oxsmt_solver.Sat.Stats.decisions
      ; propagations = st.Oxsmt_solver.Sat.Stats.propagations
      ; reason
      }
    in
    let result_block =
      match v with
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
                 be malformed solver output; degrade this goal to a sound [unknown] with
                 no model rather than crash the CLI. *)
              block ~reason:"cli-printer-unsupported" "unknown" None)
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
           else block ~reason:"cli-unrenderable-model" "unknown" None)
      | Session.Unsat -> block "unsat" None
      | Session.Unknown -> block ~reason:(Session.last_unknown_reason s) "unknown" None
    in
    result_block, core_render
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
  (* Whether the document requested [(get-unsat-core)]; drives printing the core after the
     result block. A command keyword is an UNQUOTED symbol head (matches [scan_commands]). *)
  let wants_core =
    List.exists
      (fun sx ->
        match sx with
        | Sexp.List (head :: _) -> Sexp.simple head = Some "get-unsat-core"
        | _ -> false)
      sexps
  in
  let blocks, core_render =
    if incremental || n_checks <> 1
    then List.init n_checks (fun _ -> unknown_block_with "cli-incremental"), None
    else (
      let b, core = solve_batch ?max_effort:!max_effort ~presolve:!presolve sexps in
      [ b ], core)
  in
  List.iter print_block blocks;
  (* [(get-unsat-core)] output: the SMT-LIB paren list of the core's assumption literals,
     on its own stdout line AFTER the result block (matching an SMT-LIB solver's command
     order). Emitted only when the document asked for it AND the check was [Unsat] with a
     core (a [check-sat-assuming] that refuted); a core query on a non-[unsat] result is
     not well-formed SMT-LIB, so nothing goes to stdout (a stderr note keeps it visible
     without polluting the verdict channel). Ordinary [check-sat] documents carry no core,
     so this is a no-op there — stdout stays byte-identical on every non-assuming path. *)
  (match core_render with
   | Some core when wants_core -> print_endline core
   | _ ->
     if wants_core && Option.is_none core_render
     then Printf.eprintf "oxsmt_cli: (get-unsat-core) with no core (not unsat-assuming)\n");
  (* census (task #78, USER directive): the unknown-reason is LOUD and UNCONDITIONAL — no
     env gate. STDOUT stays SMT-LIB-clean (bare [(result (verdict unknown) ...)] via
     [print_block] above), and every self-returned [unknown] ALWAYS emits one stable
     [(unknown-reason <tag>)] line to STDERR. Verdict-parsing harnesses read stdout and
     are unaffected; the census sweep reads reasons off stderr. *)
  List.iter
    (fun b ->
      if b.verdict = "unknown"
      then
        Printf.eprintf
          "(unknown-reason %s)\n"
          (if String.length b.reason = 0 then "unclassified" else b.reason))
    blocks
;;
