; Incremental (two check-sats): both drivers degrade to unknown WITHOUT running the solver
; (corpus_classify -> unknown-incremental; oxsmt_cli -> one unknown block per check-sat).
; Exercises the "n_checks <> 1" normalization branch of the equivalence guard.
(set-logic QF_LIA)
(declare-const x Int)
(assert (> x 0))
(check-sat)
(assert (< x 0))
(check-sat)
