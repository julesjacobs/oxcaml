; QF_UFLIA §10 v2 realization — GAP B REGRESSION GUARD (task #117 landed; was codex MED).
; This file is SAT. Under the v1 realization it degraded to unknown (never wrong-sat — R1
; caught the bad model); #117 (arithmetic-below-UF) makes it sat, and this fixture now
; guards that fix.
;
; Shape: an arithmetic term as a UF argument. p(x+2) keys p's table by the value of x+2;
; but x and x+2 are pure-EUF. v1 realized them as INDEPENDENT classes (fresh value of x+2
; != fresh value of x plus 2), while the R1 checker evaluates p(x+2) STRUCTURALLY (x+2 =
; value(x)+2), so the table key never matched and the assertion (= (p (+ x 2)) 5) failed.
; The #117 fix (cdclt.ml value_of): a pure-EUF Arith term is EVALUATED structurally over
; its operands, exactly as R1 does, so the stored key equals the recomputed key.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-fun p (Int) Int)
(declare-const x Int)
(assert (= (p (+ x 2)) 5))
(assert (not (= (p x) 5)))
(check-sat)
