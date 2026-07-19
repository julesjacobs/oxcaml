; DT+LIA N-O purification, selector-under-arith (task #63, bugreport 03 residual) — UNSAT
; discrimination. (key t) reduces to the compound field (+ x 1); x = 4 forces key t = 5,
; so (* 2 (key t)) = 8 is inconsistent (10 <> 8). Guards that the purification pass routes
; the DT selector-defining equality into LIA rather than degrading (or wrong-answering).
(set-logic QF_UFDTLIA)
(set-info :status unsat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const x Int)
(assert (= t (Node Empty (+ x 1) Empty)))
(assert (= x 4))
(assert (= (* 2 (key t)) 8))
(check-sat)
