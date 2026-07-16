; Skolem-function Skolemization of a nested positive existential (lemmas-climb chunk 2b):
; a positive `exists` inside a `forall` body becomes a fresh FUNCTION of the enclosing
; universals. Here `forall x. (p x) => (exists y. y < y)` Skolemizes to the genuine
; universal `forall x. (p x) => (f x) < (f x)`; the consequent `(f x) < (f x)` is
; arithmetically false, so with the ground `p a` the instance `(p a) => f(a) < f(a)`
; forces `not (p a)` -> contradiction -> unsat. Before 2b the `exists` in the body made
; the whole lemma out-of-fragment (dropped -> sat-degrade sentinel -> `unknown`); the
; Skolem function turns it into a live, refuting universal.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(declare-fun a () Int)
(assert (p a))
(assert (forall ((x Int)) (=> (p x) (exists ((y Int)) (< y y)))))
(check-sat)
