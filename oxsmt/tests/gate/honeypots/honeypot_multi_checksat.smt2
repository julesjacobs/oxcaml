; G3 trap (codex): the gate certifies a SINGLE query. The first query here is
; just `p` (satisfiable), so a claim of unsat is wrong. A reader that treats
; check-sat as a no-op unions every assert in the file — `p` AND `(not p)` — into
; one theorem `p -> ¬p -> False` = CERTIFIED (false unsat). The fixed reader
; rejects the assert after the first check-sat: UNSUPPORTED (single-query model),
; never CERTIFIED.
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-const p Bool)
(assert p)
(check-sat)
(assert (not p))
(check-sat)
