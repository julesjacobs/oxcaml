; HONEYPOT (a): claims unsat, but 0 <= x <= 5 is satisfiable.
; The gate must NOT certify. With the witness in the sidecar model
; (honeypot_sat_as_unsat.model, x := 0) it should produce REFUTED — a
; kernel-checked proof the assertions are satisfiable.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (>= x 0))
(assert (<= x 5))
(check-sat)
