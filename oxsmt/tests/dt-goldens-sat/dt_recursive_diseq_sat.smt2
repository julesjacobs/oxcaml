; A disequality over a recursive datatype is trivially satisfiable (the two lists differ
; at the top constructor). This exercises MODEL COMPLETION: the two unconstrained,
; disequal classes must get DISTINCT constructor trees (e.g. nil vs cons(_,nil)) — giving
; them the same base value would spuriously fail the self-check. Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((lst 0)) (((cons (head Int) (tail lst)) (nil))))
(declare-const x lst)
(declare-const y lst)
(assert (not (= x y)))
(check-sat)
