; Two distinct arrays that agree at one index is satisfiable (they differ at some OTHER
; index). Exercises extensionality: the theory introduces a witness index where a and b
; differ, so the model's maps differ there and the checker's extensional equality reports
; a != b. Checked sat.
(set-logic QF_AX)
(set-info :status sat)
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a () (Array Index Element))
(declare-fun b () (Array Index Element))
(declare-fun i () Index)
(assert (not (= a b)))
(assert (= (select a i) (select b i)))
(check-sat)
