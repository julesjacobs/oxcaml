; DT+LIA combination (bugreport 03): UNSAT because of ACYCLICITY, alongside an arithmetic
; atom. t = Node Empty k t makes t a constructor-descendant of itself — the DT occurs check
; refutes it. The (> k 0) keeps a live LIA atom so the COMBINED stack is exercised (not the
; pure-DT path); this pins that the DT axiom rejection (occurs/acyclicity) survives the
; congruence-child wrap of CombinedDt — the combinator emits Sat only after the DT child's
; genuine axiom-validating Final certifies, and that Final refutes the cycle here.
(set-logic QF_UFDTLIA)
(set-info :status unsat)
(declare-datatypes ((Tree 0))
  (((Empty) (Node (left Tree) (key Int) (right Tree)))))
(declare-const t Tree)
(declare-const k Int)
(assert (> k 0))
(assert (= t (Node Empty k t)))
(check-sat)
