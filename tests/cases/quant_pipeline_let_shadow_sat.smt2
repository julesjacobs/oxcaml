; Review fix F2 (WRONG UNSAT, let-shadowing in Bool classification). The `distinct` is over
; INTs: its first arg is `(let ((p 0)) (ite (forall x. true) p 1))` = 0, then 2, 3 ->
; distinct(0,2,3) = true -> SAT (z3). But `definitely_bool` classified the `=`/`distinct`
; by peeking at an argument, recursed into the `let` body WITHOUT applying the binding, saw
; the ite then-branch `p`, and found the GLOBAL `p : Bool` -> treated `distinct` as Boolean
; -> 3-arg Bool distinct collapses to False -> wrong `unsat`. FIX: definitely_bool tracks let
; bindings; the shadowing `(let ((p 0)) ...)` makes `p` not-provably-Bool, so distinct stays
; a theory op (the quantifier under it is then dropped -> sentinel -> unknown, sound). ON
; must NOT be `unsat` (z3 = sat).
(set-logic UFLIA)
(set-info :status sat)
(declare-const p Bool)
(assert (distinct (let ((p 0)) (ite (forall ((x Int)) true) p 1)) 2 3))
(check-sat)
