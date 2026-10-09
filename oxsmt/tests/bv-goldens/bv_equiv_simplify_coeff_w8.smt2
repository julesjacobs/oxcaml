; Word-level simplifier soundness (coefficient accumulation -> multiply by constant):
; x + x + x = x * 3 over all x. Asserting the negation must be unsat. Guards the
; rebuild path that emits bvmul-by-constant for a coefficient greater than one.
(set-logic QF_BV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(assert (not (= (bvadd (bvadd x x) x) (bvmul x (_ bv3 8)))))
(check-sat)
