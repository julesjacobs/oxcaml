; Quantified array lemmas match against the standalone array theory's e-graph.
; The trigger matches p(select a i), producing the instance that contradicts the
; ground assertion. Before the array e-graph bridge, matching raised and returned unknown.
(set-logic AUFLIA)
(set-info :status unsat)
(declare-const a (Array Int Int))
(declare-const i Int)
(declare-fun p (Int) Bool)
(assert (forall ((x Int))
  (! (p (select a x)) :pattern ((p (select a x))))))
(assert (not (p (select a i))))
(check-sat)
