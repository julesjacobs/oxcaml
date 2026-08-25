(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status unsat)
; a = store b i v and select a i = select b i force a = b (store-with-current-value),
; so a <> b is unsat. Needs extensionality (the witness index) and ROW over the
; store term a is congruent to (not syntactically), exercising the guarded split.
(declare-sort I 0)
(declare-sort E 0)
(declare-fun a () (Array I E))
(declare-fun b () (Array I E))
(declare-fun i () I)
(declare-fun v () E)
(assert (= a (store b i v)))
(assert (= (select a i) (select b i)))
(assert (not (= a b)))
(check-sat)
(exit)
