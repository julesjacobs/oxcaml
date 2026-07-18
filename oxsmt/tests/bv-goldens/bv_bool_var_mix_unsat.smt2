; Free Boolean variable in an unsatisfiable Bool+BV mix: b defined as (x = 1), then b and
; (x != 1) both asserted. Regression guard that a free Bool variable does not break the
; unsat path (model read-back never runs on unsat) -- NOT a witness of the model
; read-back fix, which the sat companion file exercises.
(set-logic QF_BV)
(set-info :status unsat)
(declare-const x (_ BitVec 4))
(declare-const b Bool)
(assert (= b (= x (_ bv1 4))))
(assert b)
(assert (not (= x (_ bv1 4))))
(check-sat)
