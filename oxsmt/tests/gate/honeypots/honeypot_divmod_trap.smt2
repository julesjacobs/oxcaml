; Variable-divisor trap (gate-divmod): div/mod is now certifiable ONLY for a nonzero
; INTEGER-LITERAL divisor (v1 solves linear arithmetic; euclidean elimination needs a
; constant d for the x = d*q + r rewrite). A variable divisor (y) is outside that theory,
; so the gate must fail closed: UNSUPPORTED (quarantined, never CERTIFIED, never a silent
; MALFORMED-green). This matches smt/preprocess, which raises "div/mod by a non-constant
; divisor". Labelled unsat so a regression to CERTIFIED would be caught as a breach.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(declare-const y Int)
(assert (= (mod x y) 0))
(check-sat)
