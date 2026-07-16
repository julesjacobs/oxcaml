; Chunk 2c (demote the inert Skolem trigger head) un-inerts a Skolem universal.
; `forall x. (p x) => (exists y. (and (r x y) (g x)))` Skolemizes (chunk 2b) to
; `forall x. (p x) => (and (r x (f x)) (g x))`. The consequent does not fold, so the
; trigger candidates p(x), g(x), f(x) all cover x; without 2c the Skolem head f(x) wins the
; size/tag tiebreak and can never match (the only source of a ground f-term is this lemma
; firing, which needs one) -> the lemma is inert -> unknown. With 2c the loader marks f
; inert, so inference demotes it and picks a firable head; the lemma fires on p a, forces
; g a, and contradicts not (g a) -> unsat.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(declare-fun g (Int) Bool)
(declare-fun r (Int Int) Bool)
(declare-fun a () Int)
(assert (p a))
(assert (not (g a)))
(assert (forall ((x Int)) (=> (p x) (exists ((y Int)) (and (r x y) (g x))))))
(check-sat)
