; QF_UFLIA §10 ℤ-realization (task #110): a single table mixing a TIER-1 cell (LIA
; numerically constrains the result) with a TIER-2 cell (pure-EUF Int class, realized).
; f : Int -> Int. f(a) = 7 surfaces into LIA (positive Int equality routes to Both), so
; f(a)'s result cell is the LIA-authoritative 7. f(b) != f(a) routes to EUF only, so f(b)
; is a pure-EUF Int class LIA never values — realized to a fresh integer distinct from the
; LIA-used set {7, a's value, b's value}. distinct a b keeps the argument keys distinct.
; Exercises the two realization tiers in one model; R1 + tests/eval validate it.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-fun f (Int) Int)
(declare-const a Int)
(declare-const b Int)
(assert (= (f a) 7))
(assert (distinct a b))
(assert (not (= (f b) (f a))))
(check-sat)
