; QF_UFLIA §10 v2 realization — KNOWN RESIDUAL GAP (codex MED, task #117). HONEST MARKER:
; this file is SAT, but the v2 realization degrades it to unknown (never wrong-sat — R1
; catches the bad model). It would flip to sat only if a future v3 solved leaf-aliasing.
;
; Gap (leaf-aliasing collision): x and 2*x are DISTINCT pure-EUF classes (2*x is not
; congruent to x), so p's table should key them separately. v2 realizes the leaf x to the
; least-unused integer 0, then evaluates the arith arg 2*x STRUCTURALLY as 2*value(x) =
; 2*0 = 0 (gap-B fold) — which COLLIDES with x's own key 0. p's table then gets two rows
; at key 0 with different results (p(x) true, p(2x) false), inconsistent, so R1 rejects and
; the solver emits unknown. This is the documented v2 residual (design note §8): the fold
; is arithmetically consistent with R1 by construction, but fresh-leaf distinctness is NOT
; propagated through the arithmetic, so two distinct classes can fold to one value. Sound
; (R1 backstop → unknown, never wrong-sat); a hand-crafted trigger, zero corpus occurrences
; at 2s. A satisfying model exists (e.g. x=1: p(1), ¬p(2)); v2 just does not find one.
(set-logic QF_UFLIA)
(set-info :status sat)
(declare-fun p (Int) Bool)
(declare-const x Int)
(assert (p x))
(assert (not (p (* 2 x))))
(check-sat)
