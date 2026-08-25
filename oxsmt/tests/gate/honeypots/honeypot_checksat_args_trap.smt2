; G3 family / codex round-3 trap: SMT-LIB (check-sat) takes NO arguments. A reader
; that accepts (check-sat X) via a wildcard tail counts junk-arg check-sat as the
; one real query, so this file — which contains ZERO valid (check-sat) commands —
; would fold `(assert false)` into a theorem, Lean proves false unsat, and the gate
; CERTIFIES a false unsat. The fixed reader matches exactly `(check-sat)` and rejects
; `(check-sat ...)` as MALFORMED (fail closed), so nothing is certified.
; Input is codex's verbatim trigger.
(set-logic QF_LIA) (set-info :status unsat) (assert false) (check-sat X)
