; DT+LIA combination (bugreport 03): a mixed datatype/arithmetic ordering obligation.
; key(Node Empty k Empty) = k by selector-eval (DT), and k > 0 (LIA), so (key t) > 0 holds
; and its negation is unsat. Before the DT+LIA combination fix this poisoned to unknown
; (Invalid_argument): the standalone DT theory rejected the foreign order atom (> k 0).
(set-logic QF_UFDTLIA)
(set-info :status unsat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const k Int)
(assert (> k 0))
(assert (= t (Node Empty k Empty)))
(assert (not (> (key t) 0)))
(check-sat)
