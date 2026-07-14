(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status unsat)
; storeinv shape: store(a,i,select b i) = store(b,i,select a i) with a != b is UNSAT (the
; store-equality forces a and b equal pointwise, contradicting a != b). Refuting it needs
; UPWARD read propagation: the disequality's extensionality witness reads select(a,k)/
; select(b,k) must reach the two STORE terms built over a and b, so the store-equality is
; exercised at the witness index (ensure_store_reads in arr.ml). Without that propagation
; the theory saturates without ever reading the stores and returns a (sound) unknown; with
; it, the reads close the refutation. This golden is RED against the pre-fix engine
; (unknown), GREEN after (unsat).
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a () (Array Index Element))
(declare-fun b () (Array Index Element))
(declare-fun i () Index)
(assert (= (store a i (select b i)) (store b i (select a i))))
(assert (not (= a b)))
(check-sat)
(exit)
