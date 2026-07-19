; DT+LIA N-O purification, selector-under-arith (task #63, bugreport 03 residual).
; The selector output (key t) reduces by selector-evaluation to the COMPOUND field
; (+ x 1), whose leaf x LIA never sees (it occurs only inside the datatype constructor).
; (key t) also appears under an arithmetic operator: (* 2 (key t)) = 10 forces key t = 5,
; hence x = 4. Before the purification pass LIA could not value (key t) and the DT-known
; equality (key t) = (+ x 1) was never routed to LIA, so this decidable query degraded to
; a sound unknown (reproduce with OXSMT_DTLIA_PURIFY=0). Now it decides SAT.
(set-logic QF_UFDTLIA)
(set-info :status sat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const x Int)
(assert (= t (Node Empty (+ x 1) Empty)))
(assert (= (* 2 (key t)) 10))
(check-sat)
