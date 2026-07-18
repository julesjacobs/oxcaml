; Negative gate for definite ROW2 (OXSMT_ARR_ROW2) premise-completeness.
; This instance is SATISFIABLE (z3 4.8.5 = sat): take d=false, then the first assert is
; vacuous, i=j is free, so select(store(base,i,v),j)=v and we pick select(base,j) != v.
; The ONLY carrier of i<>j is the guarded (distinct i j), which becomes entailed only on a
; SAT DECISION (d=true) -- NOT at level 0 (unlike corpus diseqs, which are level-0 and
; redundantly re-derived, leaving a dropped premise coincidentally valid). When the solver
; explores d=true, ROW2 fires select(store(base,i,v),j)=select(base,j), contradicting the
; level-0 read-diseq -> a theory conflict whose learned clause must retain the i<>j premise.
; The shipped code keeps that premise: sound verdict here is [unknown] (oxsmt cannot
; model-complete this crafted SAT instance; the flag-OFF path is unknown too), NEVER unsat.
; A commit that drops the i<>j premise from ROW2's reason makes it wrong-UNSAT (M-storeidx
; class). Gate PASS iff the OXSMT_ARR_ROW2=1 verdict is NOT unsat. See logs/ax-rowsort-log.md.
(set-logic QF_AX)
(declare-sort I 0)(declare-sort E 0)
(declare-fun base () (Array I E))
(declare-fun i () I)(declare-fun j () I)(declare-fun v () E)(declare-fun d () Bool)
(assert (or (not d) (distinct i j)))
(assert (not (= (select (store base i v) j) (select base j))))
(check-sat)
