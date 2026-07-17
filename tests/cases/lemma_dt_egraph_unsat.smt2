; Quantified datatype lemmas match against the standalone DT theory's e-graph.
; The ground problem is satisfiable until the universal instance p(A) is generated.
; Before the DT e-graph bridge, matching raised and the query degraded to unknown.
(set-logic UFDT)
(set-info :status unsat)
(declare-datatypes ((D 0)) (((A))))
(declare-fun p (D) Bool)
(assert (forall ((x D)) (p x)))
(assert (not (p A)))
(check-sat)
