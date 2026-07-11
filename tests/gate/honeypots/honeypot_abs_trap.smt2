; abs trap (codex, same class as div/mod G4): (abs x) = -1 is genuinely unsat
; (abs is >= 0), but the gate can't certify abs — grind has no direct abs theory
; and abs needs ite(x>=0,x,-x) elimination (M4). The old reader had no abs case, so
; it fell through to "undeclared operator" = MALFORMED (not red) — a silent oracle
; bypass on a green gate. The fixed reader recognises abs and classifies it LOUD +
; distinct: UNSUPPORTED (visibly quarantined, never CERTIFIED, never MALFORMED-green).
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (abs x) (- 1)))
(check-sat)
