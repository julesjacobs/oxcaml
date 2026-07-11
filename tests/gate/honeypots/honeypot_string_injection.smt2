; G2 trap (codex): the :source string contains "(assert false)". A reader that
; does not lex string literals tokenizes that text as a real command, injecting
; `false` into the theorem -> `p -> false -> False` = CERTIFIED (false unsat).
; The fixed reader lexes "..." as one inert Str token (data, never a command), so
; the real theorem is just `p`, and the claimed unsat is REFUTED by the witness
; model (p := true) in the .model sidecar.
(set-logic QF_UFLIA)
(set-info :source "(assert false)")
(set-info :status unsat)
(declare-const p Bool)
(assert p)
(check-sat)
