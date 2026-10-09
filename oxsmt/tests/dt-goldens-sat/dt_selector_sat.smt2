; head(cons(a, l)) = a is a valid selector evaluation, hence satisfiable. The model builds
; cons(a, l) structurally and projects its head; the self-check evaluates the selector on a
; matching constructor and confirms the equality. Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((lst 0)) (((cons (head Int) (tail lst)) (nil))))
(declare-const a Int)
(declare-const l lst)
(assert (= (head (cons a l)) a))
(check-sat)
