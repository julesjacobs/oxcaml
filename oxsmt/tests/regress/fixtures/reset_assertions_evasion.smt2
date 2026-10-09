; Regression-oracle discrimination fixture (board #162, scoped review RED).
;
; `reset-assertions` clears the assertion set, so after it the empty problem is SAT — the
; declared status. But oxsmt does not support reset*: the parser silently no-ops it
; (parser.ml), so a driver that does not degrade the file keeps the contradictory
; `(assert (= 0 1))` live and answers UNSAT — a FALSE MISMATCH against the sat label.
;
; The whitespace shape `( reset-assertions )` (space after the paren) is deliberate: it
; evades a naive `/\(reset/` substring scan, so this file is the tripwire for the ROBUST
; backstop (corpus_classify.ml scan_commands, which dispatches on the Sexp command head and
; degrades reset* to unknown-incremental exactly as it does push/pop). Expected token:
; unknown-incremental (NOT solved-unsat / mismatch).
(set-logic QF_LIA)
(set-info :status sat)
(assert (= 0 1))
( reset-assertions )
(check-sat)
