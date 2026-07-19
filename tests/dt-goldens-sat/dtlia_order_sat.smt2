; DT+LIA combination (bugreport 03): mixed-SAT discrimination. Same shape as the unsat
; case but the arithmetic is consistent: key(t) = k and k > 0, and we assert (> (key t) 0),
; which is satisfiable. The fix must return sat with a valid model (a constructor tree for
; t plus an Int witness for k satisfying the order atoms), not a spurious unsat/unknown.
(set-logic QF_UFDTLIA)
(set-info :status sat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const k Int)
(assert (> k 0))
(assert (= t (Node Empty k Empty)))
(assert (> (key t) 0))
(check-sat)
