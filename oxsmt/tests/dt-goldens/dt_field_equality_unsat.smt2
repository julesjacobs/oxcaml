; Datatype rule 2 — field-equality propagation (injectivity). (succ a) = (succ b)
; forces a = b, contradicting (not (= a b)): unsat. The DT theory decides it via the
; injectivity (field-equality) rule.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const a nat)
(declare-const b nat)
(assert (= (succ a) (succ b)))
(assert (not (= a b)))
(check-sat)
