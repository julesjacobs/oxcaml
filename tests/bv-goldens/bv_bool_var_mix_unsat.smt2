; Free Boolean variable in an unsatisfiable Bool+BV mix: b defined as (x = 1), then b and
; (x != 1) both asserted. Confirms free Bool variables are encoded on the unsat path too.
(set-logic QF_BV)
(set-info :status unsat)
(declare-const x (_ BitVec 4))
(declare-const b Bool)
(assert (= b (= x (_ bv1 4))))
(assert b)
(assert (not (= x (_ bv1 4))))
(check-sat)
