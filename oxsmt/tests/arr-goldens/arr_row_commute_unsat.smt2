(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status unsat)
; Store-commutativity at distinct indices: with i <> j the two nested stores are
; equal arrays, so asserting them distinct is unsat. Exercises extensionality (the
; witness index) + the ROW lazy split.
(declare-sort I 0)
(declare-sort E 0)
(declare-fun a () (Array I E))
(declare-fun i () I)
(declare-fun j () I)
(declare-fun x () E)
(declare-fun y () E)
(assert (not (= i j)))
(assert (not (= (store (store a i x) j y) (store (store a j y) i x))))
(check-sat)
(exit)
