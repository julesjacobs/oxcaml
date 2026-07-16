; Exists Skolemization (lemmas-climb chunk 2a): a top-level POSITIVE existential is
; Skolemized to a fresh ground witness, so `forall y. p(y)` together with
; `exists x. not p(x)` refutes: the witness c gives `not p(c)`, and the universal lemma
; instantiates `p(c)` on it -> contradiction -> unsat. Before 2a the exists assertion was
; dropped (a sound `unknown`); now it is a real assertion, closing the goal.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(assert (forall ((y Int)) (p y)))
(assert (exists ((x Int)) (not (p x))))
(check-sat)
