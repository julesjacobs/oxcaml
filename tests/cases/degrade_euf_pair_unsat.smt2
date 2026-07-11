; DEGRADATION HONEYPOT (EUF). x=y and f(x)=f(y) are two independent atoms propositionally,
; so the skeleton (x=y true, f(x)=f(y) false) is SATISFIABLE — but congruence makes the
; theory UNSAT. :status unsat; the wired v1 solver must answer `unknown` (THE SOUNDNESS
; RULE). A regression to `sat` fires a soundness failure. Becomes real `unsat` with EUF (M2).
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun f (S) S)
(declare-const x S)
(declare-const y S)
(assert (= x y))
(assert (not (= (f x) (f y))))
(check-sat)
