; Word-level simplifier soundness (cross-term cancellation): (x + y) - x = y over all x, y.
; Asserting the negation must be unsat. Exercised end-to-end through the real dispatch
; (which runs the pre-blast simplifier on both sides).
(set-logic QF_BV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(declare-const y (_ BitVec 8))
(assert (not (= (bvsub (bvadd x y) x) y)))
(check-sat)
