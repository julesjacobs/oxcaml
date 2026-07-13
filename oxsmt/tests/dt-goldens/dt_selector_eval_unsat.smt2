; Datatype rule 3 — selector evaluation. Once the constructor is known, the selector
; reduces: (pred (succ a)) = a, so its negation is unsat. The DT theory decides it via
; the selector-evaluation rule.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const a nat)
(assert (not (= (pred (succ a)) a)))
(check-sat)
