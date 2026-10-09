; Fixture: no (set-info :status ...) — exercises the "unspecified label" path,
; where the harness records a golden but performs no soundness label check.
(set-logic QF_UFLIA)
(declare-fun x () Int)
(declare-fun p (Int) Bool)
(assert (=> (p x) (p (+ x 0))))
(check-sat)
