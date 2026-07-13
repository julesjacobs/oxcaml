; Cache-key injectivity exhibit B (paired with qA.smt2 — read that header).
; qB asserts only the quoted const |a\n(not $a)| -> a single free Bool var,
; so it is SATISFIABLE. Its "unsat" status is therefore a WRONG claim; under the
; old non-injective canonical form it collided with qA and was stamped CERTIFIED.
; With the netstring-injective canonical form its key differs from qA's and it is
; certified on its own (grind correctly cannot close it -> INCONCLUSIVE).
(set-logic QF_UFLIA)
(declare-const a Bool)
(declare-const |a
(not $a)| Bool)
(set-info :status unsat)
(assert |a
(not $a)|)
(check-sat)
