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
    List.iter
      (fun (lem : Parser.lemma_src) ->
         ignore
           (Session.assert_lemma s ~qvars:lem.Parser.qvars ~build:(fun qv ->
              let body, triggers = lem.Parser.build (Array.map Qvar.to_term qv) in
              (* ADR-0012 L3 auto-trigger inference, applied at the SMT-LIB front end: a
                lemma the file gave NO [:pattern] (the common case — the public quantified
                sets rarely ship patterns) gets one inferred from the body (smallest
                UF-application subterms covering every qvar). Purely a completeness
                heuristic — every instance is a valid consequence, so it never changes a
                verdict — and an unreachable qvar just yields no trigger (the lemma does
                not fire; a live lemma then degrades to a sound [unknown]). Inference is a
                front-end policy, NOT a change to [assert_lemma]: an explicit empty
                trigger through the programmatic API still means "do not fire". *)
              let triggers =
                if List.is_empty triggers then Trigger.infer ~qvars:qv body else triggers
              in
              { Session.body; triggers })
            : Session.lemma))
      parsed.Parser.lemmas;
    true
  with
  (* A lemma body / trigger outside the reader's subset (an unsupported op, an overflowing
     literal, an ill-sorted or ill-formed lemma) -> a sound degrade, never a dropped
     quantifier. [Invalid_argument] covers assert_lemma's own well-formedness rejections
     (non-Bool body, arith-headed trigger, foreign reserved symbol). *)
  | Parser.Malformed _
  | Parser.Unsupported _
  | Term.Unsupported _
  | Term.Overflow
  | Term.Sort_error _
  | Invalid_argument _ -> false
;;
