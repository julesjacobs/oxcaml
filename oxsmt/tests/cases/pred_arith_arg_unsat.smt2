; Refinement-shaped with an arithmetic argument: is_pos(x+1) and x=0 force the argument
; (x+1) to equal 1 via LIA, so congruence gives is_pos(x+1) ~ is_pos(1); with ¬is_pos(1)
; this is unsat. Exercises predicate congruence across a purified arith argument (the
; Nelson-Oppen shared term (x+1)=1 flows through the combinator seam).
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-fun is_pos (Int) Bool)
(declare-const x Int)
(assert (is_pos (+ x 1)))
(assert (= x 0))
(assert (not (is_pos 1)))
(check-sat)
