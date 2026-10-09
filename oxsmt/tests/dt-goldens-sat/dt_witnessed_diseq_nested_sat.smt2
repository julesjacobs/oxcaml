; Barrett-family shape (20172804-Barrett/.../v1/v1l50071) reduced to its datatype core: a
; disequality between two WITNESSED classes that reduces, through same-constructor witness
; chains, to a field-level disequality the solver does not propagate as a unit. Concretely
; (cdr (cdr (cdr x2))) and (cons x3 null) are both forced to be [cons ...] with free
; descendants; the model builder must give them DISTINCT trees. Before the class-pair
; disequality-closure completion the builder base-completed the free descendants and
; reproduced (cons (leaf zero) null) on BOTH sides, so Dt_model_check rejected the model
; and the query degraded to a sound [unknown] (this file was one of the 30 QF_DT Barrett
; structural unknowns). z3 dispatches it as sat in ~10ms. Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () list)
(declare-fun x3 () tree)
(assert (and (and (and (and (not (= (cdr (cdr (cdr x2))) (cons x3 null))) (= (cdr x2) (cons (car null) (children x3)))) (not ((_ is zero) (succ (pred x1))))) (not ((_ is succ) (data (car x2))))) (not (= x3 (node (cons (leaf zero) (cdr (children x3))))))))
(check-sat)
