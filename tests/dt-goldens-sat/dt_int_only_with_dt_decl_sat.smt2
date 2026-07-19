; rider #1 (task #62): a datatype is declared but the query's arithmetic is over
; a standalone Int scalar (disjunctive enum constraint + Int inequalities). The
; scalar-completion fix lets the independent DT checker evaluate the arithmetic
; assertion so the mixed problem decides sat (z3-agreeing).
(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))
(declare-const c Color)
(declare-const k Int)
(assert (or (= c Red) (= c Green)))
(assert (> k 0))
(assert (< k 5))
(check-sat)
