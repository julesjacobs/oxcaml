; storeinv shape: store(a,i,select b i) = store(b,i,select a i) with a != b is UNSAT (the
; store-equality forces a and b equal pointwise). The soundness assertion here is only that
; oxsmt NEVER reports sat on this unsatisfiable query (run_soundness accepts unsat OR
; unknown, never sat). Since upward read propagation landed (ensure_store_reads in arr.ml)
; the arrays theory refutes this shape directly and answers unsat; before that it saturated
; to a Final "Sat" that the independent checker rejected, giving a sound unknown. Either way
; NEVER sat. The checker-bypass discriminator (a commit that ignored Array_model_check would
; wrongly report sat on a genuinely-sat query) is exercised by run_fault_injection on
; arr_select_over_store_sat.smt2, not here.
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
