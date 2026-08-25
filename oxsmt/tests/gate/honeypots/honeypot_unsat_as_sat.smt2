; HONEYPOT (b): claims sat with a WRONG model. The query (0 <= x <= 0, x != 0)
; is actually unsat, and the sidecar model (x := 5) does not satisfy it.
; The sat-model `decide` check must FAIL; the gate then proves the query unsat by
; grind and reports REFUTED. It must NOT certify.
(set-logic QF_LIA)
(set-info :status sat)
(declare-const x Int)
(assert (>= x 0))
(assert (<= x 0))
(assert (distinct x 0))
(check-sat)
