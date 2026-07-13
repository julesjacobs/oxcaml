; bvult is irreflexive: x <u x is never true, so asserting it is unsat. Marked :status
; unknown until the bit-blasting engine is wired; flips to unsat then.
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 8))
(assert (bvult x x))
(check-sat)
