; Word-level simplifier soundness (reassociation exposing a shared subterm):
; (x + 3) - y = (x - y) + 3 over all x, y. Asserting the negation must be unsat.
(set-logic QF_BV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(declare-const y (_ BitVec 8))
(assert (not (= (bvsub (bvadd x (_ bv3 8)) y) (bvadd (bvsub x y) (_ bv3 8)))))
(check-sat)
