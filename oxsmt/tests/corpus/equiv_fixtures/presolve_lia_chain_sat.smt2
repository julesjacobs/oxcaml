; Presolve-ACTIVE (QF_LIA): a top-level alias chain x = y + 1, y = z, z = 2 lets the W1b
; equality-elimination presolve drop x, y, z and solve the reduced problem. Both drivers
; must batch through assert_presolved and agree (sat). Flat const model -> renderable, so
; oxsmt_cli emits sat (no downgrade).
(set-logic QF_LIA)
(set-info :status sat)
(declare-const x Int)
(declare-const y Int)
(declare-const z Int)
(assert (= x (+ y 1)))
(assert (= y z))
(assert (= z 2))
(assert (<= x 10))
(check-sat)
