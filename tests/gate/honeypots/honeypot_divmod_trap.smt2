; G4 trap (codex): (mod x 3) = 5 is genuinely unsat (mod is in [0,3)), but the
; gate cannot certify div/mod (grind has no Euclidean ediv/emod reasoning). The
; old reader had no div/mod case, so `mod` fell through to "undeclared operator"
; = MALFORMED, which is not red — a div/mod query silently bypassed the oracle on
; a green gate. The fixed reader recognises div/mod and classifies it LOUD +
; distinct: UNSUPPORTED (never silently green, never CERTIFIED). Flip to
; KILLED-if-CERTIFIED and expect UNSUPPORTED until euclidean elimination lands (M4).
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (mod x 3) 5))
(check-sat)
