(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status sat)
; Two distinct arrays with no other constraint are satisfiable (they differ at some
; index). v1 deliberately degrades any array sat to unknown rather than emit an
; un-self-checked model. This golden pins that degrade (verdict unknown, NOT sat).
(declare-sort I 0)
(declare-sort E 0)
(declare-fun a () (Array I E))
(declare-fun b () (Array I E))
(assert (not (= a b)))
(check-sat)
(exit)
