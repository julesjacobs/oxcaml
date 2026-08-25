; Barrett-family shape (20172804-Barrett/.../v10/v10l40001) reduced to its used
; declarations. The disequality (succ x10) <> x2, together with the surrounding
; constraints, forces two nat classes witnessed by [succ] at DIFFERENT spine depths to be
; disequal; the solver does not propagate the induced pred-field disequality as a unit.
; Before the class-pair disequality-closure completion the builder gave the two free
; pred-descendants consecutive spine values, so the succ-wrapped parents coincided (both
; succ^k(zero)) and Dt_model_check rejected the model -> a sound [unknown] (one of the 30
; QF_DT Barrett structural unknowns). z3 dispatches it as sat in ~15ms. Checked sat.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () nat)
(declare-fun x6 () nat)
(declare-fun x7 () nat)
(declare-fun x8 () nat)
(declare-fun x10 () nat)
(declare-fun x21 () tree)
(declare-fun x27 () tree)
(assert (and (and (and (not (= (pred (pred x6)) x7)) (= x21 (leaf x1))) (= (data x27) x8)) (not (= (succ x10) x2))))
(check-sat)
