; bvult is irreflexive: x <u x is never true, so asserting it is unsat. Bit-blasting
; engine wired: the pure-QF_BV dispatch refutes it.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(assert (bvult x x))
(check-sat)
