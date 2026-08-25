; distinct over Bool: three pairwise-distinct Booleans are impossible (only two
; truth values), so this is unsat.
(set-logic QF_UF)
(set-info :status unsat)
(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)
(assert (distinct p q r))
(check-sat)
