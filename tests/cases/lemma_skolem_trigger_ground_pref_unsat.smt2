; Chunk 2c (trigger preference for ground-occurring heads) un-inerts a Skolem universal.
; `forall x. (p x) => (exists y. (and (r x y) (g x)))` Skolemizes (chunk 2b) to
; `forall x. (p x) => (and (r x (f x)) (g x))`. The consequent does not fold, so the
; trigger candidates p(x), g(x), f(x) all cover x; without 2c the Skolem head f(x) wins the
; size/tag tiebreak and never matches (no ground f) -> the lemma is inert -> unknown. With
; 2c, p and g have ground occurrences (p a, not (g a)) and f has none, so a ground-matchable
; head is chosen; the lemma fires on p a, forces g a, and contradicts not (g a) -> unsat.
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
