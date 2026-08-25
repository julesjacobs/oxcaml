; Pigeonhole over Bool — the finite-sort landmine for the ONE sanctioned finite sort
; (TODO Predicates §2, the Bool-cardinality rule). f a, f b, f c are Bool-sorted predicate
; applications that occur ONLY buried inside the uninterpreted-function argument g(·)
; (g : Bool -> S), so the Boolean skeleton never surfaces them as SAT atoms. The three
; S-sorted disequalities on g(f ·) force f a, f b, f c pairwise-distinct by congruence
; contrapositive (f x = f y => g(f x) = g(f y)); three pairwise-distinct Bools is
; impossible (only two truth values), so this is unsat.
;
; Without the Bool-cardinality rule (register every Bool-sorted e-graph term as a SAT
; atom) each buried f(·) stays a third opaque Boolean class the engine never case-splits:
; the combinator's H2 guard degrades that to a sound `unknown` (never wrong-sat), so this
; case FAILS (unknown, not unsat) if the rule is neutered. With the rule, SAT case-splits
; each f(·) true/false and pigeonhole is discharged by congruence + the true<>false axiom.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun f (S) Bool)
(declare-fun g (Bool) S)
(declare-const a S)
(declare-const b S)
(declare-const c S)
(assert (distinct (g (f a)) (g (f b)) (g (f c))))
(check-sat)
