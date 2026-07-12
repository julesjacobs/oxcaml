; QF_UFLIA §10 ℤ-realization (task #110): an Int-CODOMAIN table over an uninterpreted
; domain, whose result cells are pure-EUF Int classes (no arithmetic atom bears on them).
; f : U -> Int with a,b:U distinct and f(a) != f(b). f(a),f(b) are Int-sorted but LIA
; never values them (the disequality routes to EUF only), so before §10 they were omitted
; from the model and this degraded to unknown. The realization gives f(a),f(b) DISTINCT
; concrete integers (they are distinct EUF classes), so f(a) != f(b) holds. The R1
; in-process checker + the external tests/eval both validate the emitted model.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-sort U 0)
(declare-fun f (U) Int)
(declare-const a U)
(declare-const b U)
(assert (distinct a b))
(assert (not (= (f a) (f b))))
(check-sat)
