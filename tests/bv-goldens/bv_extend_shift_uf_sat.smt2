; Mixes zero_extend, sign_extend, a shift, and an uninterpreted function over bitvectors
; (QF_UFBV): f applied to equal arguments is equal, so this is sat. Exercises the extends,
; bvshl, and EUF congruence over bitvector terms. Marked :status unknown until the
; bit-blasting engine is wired.
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 8))
(declare-fun f ((_ BitVec 16)) Bool)
(assert (= (bvshl ((_ zero_extend 8) x) #x0004) ((_ sign_extend 8) x)))
(assert (=> (f ((_ zero_extend 8) x)) (f ((_ zero_extend 8) x))))
(check-sat)
