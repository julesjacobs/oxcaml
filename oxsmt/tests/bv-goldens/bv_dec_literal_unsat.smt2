; decimal literal in an operator arg. x = x + 1 is unsatisfiable at any width.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(assert (= x (bvadd x (_ bv1 8))))
(check-sat)
