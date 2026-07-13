; Wrong-label refute trap (gate-divmod): (mod x 3) = 1 is SATISFIABLE (x := 1 gives
; mod 1 3 = 1), but this file CLAIMS unsat. Post-elimination the unsat direction cannot
; prove False (grind correctly fails on a satisfiable linear system), so the gate falls
; back to the witness model and the sat direction proves the assertion holds under x := 1
; via decide over Int.emod -> REFUTED (a ship-stopper), never CERTIFIED. This exercises the
; div/mod SAT path (Int.emod on a concrete model) end to end.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (mod x 3) 1))
(check-sat)
