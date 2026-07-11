; G3 family / codex round-3 trap: SMT-LIB execution terminates at (exit); a
; conformant solver never runs a command that follows it, so no query is made here.
; A reader that ignores (exit) and keeps folding commands assembles `(assert false)`
; + `(check-sat)` into a theorem AFTER the query has ended, Lean proves false unsat,
; and the gate CERTIFIES a false unsat the file never actually asked. The fixed
; reader arms an `exited` flag at (exit) and rejects any later command as MALFORMED
; (never a silent truncation of the ignored tail — same laundering class).
; Input is codex's verbatim trigger.
(set-logic QF_LIA) (set-info :status unsat) (exit) (assert false) (check-sat)
