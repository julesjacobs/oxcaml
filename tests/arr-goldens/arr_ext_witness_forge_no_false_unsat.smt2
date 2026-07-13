(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status sat)
; Regression for the extensionality-witness forge (fable review, 2026-07-13). The theory's
; Skolem witness is now the reserved `.oxsmt.arr.ext.N`; a user declaring the OLD forgeable
; name `@arr.ext.0` must NOT alias it. a,b differ at some index but agree at @arr.ext.0 => SAT,
; degraded to unknown (v1). Pre-fix (@arr.ext witness) this was a FALSE unsat.
(declare-sort I 0)
(declare-sort E 0)
(declare-fun a () (Array I E))
(declare-fun b () (Array I E))
(declare-fun @arr.ext.0 () I)
(assert (not (= a b)))
(assert (= (select a @arr.ext.0) (select b @arr.ext.0)))
(check-sat)
(exit)
