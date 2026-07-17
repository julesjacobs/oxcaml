; Front-end quantified pipeline POLARITY HONEYPOT (chunk-2b landmine, extended to the IR
; path). A double-negated universal `(not (not (forall x. p x)))` is a POSITIVE universal:
; the pipeline must lower it to a live universal LEMMA, never Skolemize the universal x to a
; fresh CONSTANT (that would weaken `forall x. p x` to `p k`, a wrong `sat`). With `not (p a)`
; the sound reading is unsat (the lemma instantiates `p a`); a buggy const-Skolemization
; would make it `sat` (p k /\ not p a with k != a). OFF drops the term-nested quantifier
; (unknown); ON must be `unsat` and MUST NOT be `sat`. NNF collapses the double negation and
; keeps x universal, so the pipeline is sound here by construction.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(declare-fun a () Int)
(assert (not (not (forall ((x Int)) (p x)))))
(assert (not (p a)))
(check-sat)
