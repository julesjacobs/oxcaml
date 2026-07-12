; QF_UFLIA §10 realization — KNOWN COMPLETENESS GAP (codex MED, board #117). HONEST
; MARKER: this file is SAT, but the v1 realization degrades it to unknown (never wrong-sat
; — R1 catches the bad model). It FLIPS to sat when #117 (class-value inheritance) lands.
;
; Gap: a congruent class split across realization tiers. a = b makes f(a) and f(b) the
; SAME EUF class by congruence. f(a) = 7 surfaces to LIA (tier-1, value 7); f(b) occurs
; only under g, so it is a pure-EUF Int class (tier-2) and the v1 realization assigns it a
; FRESH integer != 7 rather than inheriting its class's LIA value 7. f's table then gets
; two rows with the same argument key but different results (7 and the fresh value) —
; inconsistent — so the R1 checker rejects the model and the solver emits unknown. The fix
; (#117) is to inherit the LIA value across every term of a class before minting a fresh
; one.
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
