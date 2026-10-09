; (succ a) = (succ b) with a, b free is satisfiable (any a=b works). Injectivity does
; not force a conflict; the model assigns a, b equal nats and a=succ(...) tree, and the
; self-check confirms the equality. Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const a nat)
(declare-const b nat)
(assert (= (succ a) (succ b)))
(check-sat)
