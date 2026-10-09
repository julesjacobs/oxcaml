; Presolve-ACTIVE (QF_UFLIA), unsat: x = y + 1 and y = 4 alias x -> 5, substituted into the
; UF argument (h x). Congruence then forces (h x) = (h 5), contradicting the disequality.
; Presolve eliminates x, y; the verdict is unsat either way, and both drivers must agree.
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-fun h (Int) Int)
(declare-const x Int)
(declare-const y Int)
(assert (= x (+ y 1)))
(assert (= y 4))
(assert (distinct (h x) (h 5)))
(check-sat)
