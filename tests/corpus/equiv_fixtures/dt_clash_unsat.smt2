; Driver-equivalence fixture: a QF_DT constructor clash (unsat). Guards that the headline
; driver (corpus_classify) and the shipped CLI (oxsmt_cli) agree on a datatype file — i.e.
; both thread Session.set_datatypes and install the DT theory. Before the DT theory lane
; both drivers degraded datatypes to unknown; a future driver that drops the datatypes
; threading would diverge (one unsat, one unknown) and fail driver_equiv_test.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const x nat)
(assert (= (succ x) zero))
(check-sat)
