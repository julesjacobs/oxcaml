; Bool-sorted = is iff: (= p q r) means (p <-> q) and (q <-> r). With p and ¬r
; that forces r via the chain, contradicting ¬r -> unsat.
(set-logic QF_UF)
(set-info :status unsat)
(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)
(assert (= p q r))
(assert p)
(assert (not r))
(check-sat)
