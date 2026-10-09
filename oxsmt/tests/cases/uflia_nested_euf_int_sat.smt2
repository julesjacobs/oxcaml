; QF_UFLIA §10 ℤ-realization (task #110): a NESTED pure-EUF Int chain g(f(a)), the shape
; the §10 stub named as the motivating gap. f : U -> Int, g : Int -> Int, and
; g(f(a)) != f(a). Both f(a) and g(f(a)) are Int-sorted pure-EUF classes (no arithmetic
; atom), realized to DISTINCT integers (distinct EUF classes). The realization must be
; consistent across the nesting: f(a)'s realized integer k is the KEY of g's row, so g's
; argument cell reads back k and the R1 checker evaluates g(f(a)) by looking up g at k —
; the same integer extraction stored. Guards realize-consistency between a table's result
; cell and the containing table's argument cell. R1 + tests/eval validate the model.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-sort U 0)
(declare-fun f (U) Int)
(declare-fun g (Int) Int)
(declare-const a U)
(assert (not (= (g (f a)) (f a))))
(check-sat)
