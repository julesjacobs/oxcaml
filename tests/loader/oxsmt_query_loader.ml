(* Shared .smt2 -> Session loader for the two test-only drivers (oxsmt_cli and
   corpus_classify). It exists so the two drivers CANNOT diverge on how a parsed document
   is asserted — the driver-equivalence guard (tests/corpus/driver_equiv_test) treats that
   agreement as an invariant, and the quantifier path is the newest place they could
   drift. Test-only, stdlib over the smt/ libraries + the test-only parser. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Qvar = Oxsmt_ematch.Qvar
module Trigger = Oxsmt_ematch.Trigger
module Term = Oxsmt_core.Term
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Symbol = Oxsmt_core.Symbol
module Rank = Oxsmt_core.Rank
module Env = Oxsmt_core.Env

(* Is [name] already declared (any rank) in [env]? [Env.rank] raises when the symbol has
   no rank, i.e. is undeclared. [Symbol.intern] is a pure hash-cons — interning a
   candidate name does NOT declare it. Used to make a Skolem-witness name provably FRESH
   before declaring it (a same-sort collision would otherwise be silently reused =
   unsound). *)
let is_declared env name =
  match Env.rank env (Symbol.intern name) with
  | _ -> true
  | exception _ -> false
;;

(* Assert a parsed document into [s]: the ground batch through the W1b
   equality-elimination presolve (or the per-term [assert_term] stream when
   [presolve = false], for A/B), then each universally-quantified lemma through the
   cap-gated mint-before-build {!Session.assert_lemma} (ADR-0012 §1.3). One qvar is minted
   per binder and its {!Term.t} image handed to the parser's deferred [build], which reads
   the lemma body and [:pattern] triggers with the binders bound.

   SOUNDNESS: dropping a lemma would WEAKEN the assertion set — sound for [unsat] but NOT
   for [sat] (a model of the weaker set may violate the quantifier). So a lemma the reader
   cannot represent must never be silently dropped: [build] raising (a body op outside the
   subset) returns [false], and the driver degrades the whole query to [unknown]. A lemma
   that IS asserted makes [Session.has_live_lemma] true, so THE SOUNDNESS RULE degrades
   any ground [Sat] to [Unknown] automatically (§2). Returns [true] iff every lemma
   loaded. *)
let assert_all ?(presolve = true) s (parsed : Parser.t) =
  (* Total dropped assertion content: what partial assertion dropped at PARSE
     ([parsed.dropped]) plus any lemma dropped HERE because its body/trigger is outside
     the fragment (e.g. an [exists] in the body, discovered only when [build] runs). *)
  let dropped = ref parsed.Parser.dropped in
  try
    (* Install any datatype shapes BEFORE asserting, so the theory stack selects the DT
       theory when the file declared a datatype (empty registry = no-op, byte-identical on
       non-DT files). Single point for BOTH drivers + the cert gate (all route through
       this loader), superseding the per-driver set_datatypes threading. *)
    Session.set_datatypes s parsed.Parser.datatypes;
    (* Likewise install the array select/store registry (arrays lane) BEFORE asserting, so
       an array file routes onto the standalone arrays theory. No-op on non-array files. *)
    Session.set_arrays s parsed.Parser.arrays;
    if presolve
    then Session.assert_presolved s parsed.Parser.assertions
    else List.iter (Session.assert_term s) parsed.Parser.assertions;
    (* Skolem-FUNCTION minter for positive existentials nested in a [forall] body
       (lemmas-climb chunk 2b): each such binder becomes a FRESH uninterpreted function of
       the enclosing universals ([args] = the lemma's qvar images), so the lemma stays
       universal and EQUISATISFIABLE with the original (standard Skolemization). Freshness
       is load-bearing exactly as for the ground witnesses below — a name colliding with a
       same-rank user symbol would be silently reused ([declare_fun] is idempotent),
       constraining the Skolem function to that symbol (unsound) — so each [skf!N] name is
       checked against the env and bumped, then declared through the ordinary user door
       (never the reserved namespace). The [skf!]/[sk!] prefixes are disjoint, so the two
       minters cannot collide. *)
    let skf_counter = ref 0 in
    let skolem ~cod ~args =
      let env = Session.env s in
      let rec pick () =
        let name = Printf.sprintf "skf!%d" !skf_counter in
        incr skf_counter;
        if is_declared env name then pick () else name
      in
      let dom = List.map (fun (t : Term.t) -> t.Term.sort) args in
      let sym = Session.declare_fun s (pick ()) (Rank.create dom cod) in
      Context.app (Session.context s) sym args
    in
    (* Ground-occurrence counts of head symbols across the ground assertions (lemmas-climb
       chunk 2c): fed to auto-trigger inference so it PREFERS a trigger whose head
       actually occurs in a ground term. Without it, a Skolem function minted for a nested
       existential (chunk 2b) — which occurs only in that lemma's body, never in a ground
       term — can win the trigger by size/tag and leave the lemma inert (it can never
       match). Computed ONCE over the pre-presolve assertions; a heuristic seed, never a
       soundness input. *)
    let ground_occurrences = Trigger.ground_head_counts parsed.Parser.assertions in
    List.iter
      (fun (lem : Parser.lemma_src) ->
        match
          Session.assert_lemma s ~qvars:lem.Parser.qvars ~build:(fun qv ->
            let body, triggers = lem.Parser.build ~skolem (Array.map Qvar.to_term qv) in
            (* ADR-0012 L3 auto-trigger inference, applied at the SMT-LIB front end: a
               lemma the file gave NO [:pattern] (the common case — the public quantified
               sets rarely ship patterns) gets one inferred from the body (smallest
               UF-application subterms covering every qvar). Purely a completeness
               heuristic — every instance is a valid consequence, so it never changes a
               verdict — and an unreachable qvar just yields no trigger (the lemma does
               not fire; a live lemma then degrades to a sound [unknown]). Inference is a
               front-end policy, NOT a change to [assert_lemma]: an explicit empty trigger
               through the programmatic API still means "do not fire". *)
            let triggers =
              if List.is_empty triggers
              then Trigger.infer ~ground_occurrences ~qvars:qv body
              else triggers
            in
            { Session.body; triggers })
        with
        | (_ : Session.lemma) -> ()
        (* Partial assertion (lemmas-climb): a SINGLE lemma OUT OF FRAGMENT (an [exists]
           in the body -> [Unsupported]; an unsupported op; an over-precision literal) is
           DROPPED and counted rather than failing the whole file. Dropping only weakens
           the set (sound for [unsat]); the sentinel below reinstates soundness for the
           [sat] direction. A genuinely ILL-FORMED / ill-typed lemma ([Malformed],
           [Sort_error], or assert_lemma's own [Invalid_argument] well-formedness
           rejection — e.g. the F1 head-shadow wrong-unsat guard) is NOT dropped here: it
           propagates to the outer handler and degrades the WHOLE query to [unknown],
           preserving that contract. *)
        | exception (Parser.Unsupported _ | Term.Unsupported _ | Term.Overflow) ->
          incr dropped)
      parsed.Parser.lemmas;
    (* Skolemize each top-level POSITIVE existential (lemmas-climb chunk 2a): mint one
       fresh ground witness per binder and assert the body over them. This is
       EQUISATISFIABLE with [(assert (exists ...))] (a model gives a witness; a witness
       gives a model), so it is sound in BOTH directions — a real assertion, not a drop.
       Freshness is load-bearing: a witness name colliding with a same-sort user symbol
       would be silently REUSED ([declare_fun] is idempotent at equal rank), constraining
       the witness to that symbol — unsound. So each name is checked against the env and
       bumped until unused, then declared through the ordinary user door (never the
       reserved namespace). A body still outside the fragment (e.g. a nested [forall])
       makes [ex_build] raise [Unsupported] -> drop it (the sentinel then guards [sat]);
       [Malformed] propagates as a hard fail (outer handler), exactly as for lemmas. *)
    let sk_counter = ref 0 in
    let fresh_witness sort =
      let env = Session.env s in
      let rec pick () =
        let name = Printf.sprintf "sk!%d" !sk_counter in
        incr sk_counter;
        if is_declared env name then pick () else name
      in
      let sym = Session.declare_const s (pick ()) sort in
      Context.const (Session.context s) sym
    in
    List.iter
      (fun (ex : Parser.exists_src) ->
        let witnesses =
          Array.map
            (fun (_n, sort) -> fresh_witness sort)
            (Array.of_list ex.Parser.ex_qvars)
        in
        match ex.Parser.ex_build witnesses with
        | body -> Session.assert_term s body
        | exception (Parser.Unsupported _ | Term.Unsupported _ | Term.Overflow) ->
          incr dropped)
      parsed.Parser.existentials;
    (* If ANY assertion content was dropped, arm a trivial always-live universal lemma so
       THE SOUNDNESS RULE degrades a [Sat] to [Unknown]: a dropped conjunct can then never
       yield a wrong [sat], while [Unsat] of the asserted (weaker) subset stays sound.
       Body [forall x. x = x] is trivially valid; the empty trigger means it never
       instantiates (zero effort). Arming the sentinel MUST succeed — if it raises, the
       outer handler's [false] degrades the whole query to a sound [unknown], so a drop is
       never left un-guarded. *)
    if !dropped > 0
    then
      ignore
        (Session.assert_lemma
           s
           ~qvars:[ "x", Sort.int ]
           ~build:(fun qv ->
             let x = Qvar.to_term qv.(0) in
             { Session.body = Context.eq (Session.context s) x x; triggers = [] })
         : Session.lemma);
    true
  with
  (* A hard failure on the GROUND batch (or arming the sentinel) — degrade the whole query
     to a sound [unknown]. *)
  | Parser.Malformed _
  | Parser.Unsupported _
  | Term.Unsupported _
  | Term.Overflow
  | Term.Sort_error _
  | Invalid_argument _ -> false
;;
