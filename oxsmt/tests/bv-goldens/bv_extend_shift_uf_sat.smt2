; Mixes zero_extend, sign_extend, a shift, and a genuine uninterpreted function f over
; bitvectors (QF_UFBV). True status is sat (f applied to equal arguments is equal). This
; is the DOOR TEST: it is NOT pure QF_BV (f is an uninterpreted application), so the pure-
; BV dispatch's conservative gate rejects it and it stays on the combinator's fail-closed
; path, degrading to a sound unknown. Solving it needs UF+BV combination (a later lane);
; eager bit-blasting alone cannot, and must not fabricate a verdict. :status unknown =
; what our v1 engine returns (never a wrong verdict).
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 8))
(declare-fun f ((_ BitVec 16)) Bool)
(assert (= (bvshl ((_ zero_extend 8) x) #x0004) ((_ sign_extend 8) x)))
(assert (=> (f ((_ zero_extend 8) x)) (f ((_ zero_extend 8) x))))
(check-sat)
