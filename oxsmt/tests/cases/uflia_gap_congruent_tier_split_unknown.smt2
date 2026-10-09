; QF_UFLIA §10 v2 realization — GAP A REGRESSION GUARD (task #117 landed; was codex MED).
; This file is SAT. Under the v1 realization it degraded to unknown (never wrong-sat — R1
; caught the bad model); #117 (class-value inheritance) makes it sat, and this fixture now
; guards that fix.
;
; Shape: a congruent class split across realization tiers. a = b makes f(a) and f(b) the
; SAME EUF class by congruence. f(a) = 7 surfaces to LIA (tier-1, value 7); f(b) occurs
; only under g, so it is a pure-EUF Int class (tier-2). v1 minted a FRESH integer != 7 for
; that class rather than inheriting its class's LIA value 7 — so f's table got two rows at
; the same argument key with different results and R1 rejected. The #117 fix (combine.ml
; class_int): a pure-EUF class that shares its EUF class with a LIA-valued term inherits
; that integer, so every term of the class agrees.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-fun f (Int) Int)
(declare-fun g (Int) Int)
(declare-const a Int)
(declare-const b Int)
(assert (= a b))
(assert (= (f a) 7))
(assert (= (g (f b)) 0))
(check-sat)
