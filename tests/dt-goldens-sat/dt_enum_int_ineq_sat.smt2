; rider #1 (task #62): enum datatype + an unrelated Int scalar bounded only by
; inequalities. The Int scalar occurs solely in LIA-owned atoms, so before the
; scalar-completion fix Dt_model_check could not evaluate them and returned a
; SOUND unknown; it now decides sat (z3-agreeing).
(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))
(declare-const c Color)
(declare-const k Int)
(assert (= c Red))
(assert (> k 0))
(assert (< k 5))
(check-sat)
