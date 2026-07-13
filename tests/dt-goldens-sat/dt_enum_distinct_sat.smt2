; Three pairwise-distinct values of a three-constructor enum is satisfiable (exactly
; red/green/blue). The exhaustiveness case split assigns each a distinct constructor
; witness; the constructor-tree model records them and the self-check confirms the
; distinctness. Checked sat (the satisfiable side of the enum criterion).
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((color 0)) (((red) (green) (blue))))
(declare-const x color)
(declare-const y color)
(declare-const z color)
(assert (distinct x y z))
(check-sat)
