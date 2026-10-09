; Second cyclic-witness fbd-separation golden (reduced from barrett-jsat tests/v1/v1l80063).
; Same class as dt_cyclic_witness_fbd_sat: acyclic witness graph, but shared-memo peer
; materialization leaked an in-progress [Uninterp] placeholder into the model. RED on trunk
; (unknown, spine landed), GREEN with the fbd-separation fix (checked sat).
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () list)
(declare-fun x3 () tree)
(assert (and (and (and (and (and (and (and (not (= (cons (node (cons (leaf zero) (cons (car (cons (node (cdr x2)) (cons (car (cons x3 (cdr null))) x2))) (cdr (children (node (cdr null))))))) null) (cdr (cdr (children (car null)))))) (not (= (pred (succ (pred (pred zero)))) x1))) ((_ is succ) (succ x1))) (not (= (succ x1) x1))) (not (= (succ (pred zero)) zero))) (not ((_ is zero) (succ (data (car x2)))))) (not ((_ is null) (children (car (cdr x2)))))) (= x3 (leaf (pred (succ (pred x1)))))))
(check-sat)
