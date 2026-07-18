; x & 0 is always 0, so it cannot differ from 0: unsat. Exercises bvand + a constant.
; Bit-blasting engine wired: the pure-QF_BV dispatch refutes it (pure-propositional unsat).
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const x (_ BitVec 8))
(assert (distinct (bvand x #x00) #x00))
(check-sat)
