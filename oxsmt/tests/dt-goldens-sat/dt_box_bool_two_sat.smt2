; Two distinct boxes of a Bool field IS satisfiable (box false, box true). The
; diseq-respecting bounded Bool completion assigns the two disequal Bool fields distinct
; values, and the constructor-tree self-check confirms the distinctness. Checked sat — the
; control that the B1 finite-Bool fix does not over-refute the genuinely-sat case.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((Box 0)) (((box (val Bool)))))
(declare-const a Bool)
(declare-const b Bool)
(assert (distinct (box a) (box b)))
(check-sat)
