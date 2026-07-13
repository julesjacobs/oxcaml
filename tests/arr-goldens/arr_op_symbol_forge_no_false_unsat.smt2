(set-logic QF_AX)
(declare-sort I 0)
(declare-sort E 0)
(declare-fun x () (Array I E))
(declare-fun y () I)
(declare-fun z () E)
(declare-fun c () (Array I E))
; a real store keyword over (I,E) registers the op symbol @arr.store.U:I.U:E
(assert (= c (store x y z)))
; forge that op symbol via a QUOTED symbol (bypasses the simple-symbol charset), store rank
(declare-fun |@arr.store.U:I.U:E| ((Array I E) I E) (Array I E))
(declare-fun a () (Array I E))
(declare-fun i () I)
(declare-fun v () E)
; g := the forged op; g(a,i,v) is UNINTERPRETED, so select(g(a,i,v),i) = v is NOT entailed => SAT
(assert (not (= (select (|@arr.store.U:I.U:E| a i v) i) v)))
(check-sat)
