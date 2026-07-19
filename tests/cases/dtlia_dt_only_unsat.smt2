; DT-only control (bugreport 03): no arithmetic ATOM — the Int key is reasoned about only
; through selector-eval + congruence (key(Node Empty k Empty) = k), which the standalone DT
; theory already handles. Must stay unsat under the combination fix (no regression).
(set-logic QF_UFDTLIA)
(set-info :status unsat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const k Int)
(assert (= t (Node Empty k Empty)))
(assert (not (= (key t) k)))
(check-sat)
