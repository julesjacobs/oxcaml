; storeinv shape: store(a,i,select b i) = store(b,i,select a i) with a != b is UNSAT (the
; store-equality forces a and b equal pointwise). The arrays theory does not refute this
; shape, so it reaches a Final "Sat"; the INDEPENDENT array checker then cannot satisfy both
; the store-equality and a != b under any single model, so it rejects -> the verdict is a
; sound unknown, NEVER sat. This file's purpose is the soundness discriminator: the array
; sat-gate proves that with the checker bypassed (accept-all) the session would wrongly
; report sat, and the real checker prevents it. (:status unsat; oxsmt answers unknown.)
(set-logic QF_AX)
(set-info :status unsat)
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a () (Array Index Element))
(declare-fun b () (Array Index Element))
(declare-fun i () Index)
(assert (= (store a i (select b i)) (store b i (select a i))))
(assert (not (= a b)))
(check-sat)
