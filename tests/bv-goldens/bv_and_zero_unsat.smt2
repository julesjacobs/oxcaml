; x & 0 is always 0, so it cannot differ from 0: unsat. Exercises bvand + a constant.
; Marked :status unknown until the bit-blasting engine is wired; flips to unsat then.
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 8))
(assert (distinct (bvand x #x00) #x00))
(check-sat)
