; Reserved-namespace forge soundness test, now on the sat side (array model construction).
; @arr.ext.0 is a USER symbol that mimics the shape of the theory's reserved extensionality
; witness (.oxsmt.arr.ext.0) but in the old, forgeable @arr. spelling. a != b together with
; the forged index agreeing (select a @arr.ext.0 = select b @arr.ext.0) is SATISFIABLE (a and
; b differ at some OTHER, real index). The forged witness must corrupt neither the refutation
; direction (no false unsat) NOR the sat model: the theory uses its own reserved witness, and
; the array checker validates a genuine model where a and b differ. Checked sat (was unknown
; before model construction). Moved here from tests/arr-goldens because an arrays sat cannot
; go through the harness's DT/array-unaware external eval; the array sat-gate checks it.
(set-logic QF_AX)
(set-info :status sat)
(declare-sort I 0)
(declare-sort E 0)
(declare-fun a () (Array I E))
(declare-fun b () (Array I E))
(declare-fun @arr.ext.0 () I)
(assert (not (= a b)))
(assert (= (select a @arr.ext.0) (select b @arr.ext.0)))
(check-sat)
