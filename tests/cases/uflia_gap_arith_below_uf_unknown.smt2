; QF_UFLIA §10 realization — KNOWN COMPLETENESS GAP (codex MED, board #117). HONEST
; MARKER: this file is SAT, but the v1 realization degrades it to unknown (never wrong-sat
; — R1 catches the bad model). It FLIPS to sat when #117 (arithmetic-below-UF) lands.
;
; Gap: an arithmetic term as a UF argument. p(x+2) keys p's table by the realized value of
; x+2; but x and x+2 are pure-EUF and get realized as INDEPENDENT classes (the fresh value
; of x+2 is not the fresh value of x plus 2). The R1 checker, evaluating p(x+2)
; STRUCTURALLY, folds x+2 = realize(x)+2, which does not equal realize(x+2), so the table
; key never matches: p(x+2) reads the default, the assertion (= (p (+ x 2)) 5) fails, and
; the solver emits unknown. The fix (#117) is to realize arithmetic-below-UF arguments
; consistently with their operands (or purify before extraction).
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-fun p (Int) Int)
(declare-const x Int)
(assert (= (p (+ x 2)) 5))
(assert (not (= (p x) 5)))
(check-sat)
