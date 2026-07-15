; Same-sort (Array T T) ROW2 boundary RED (codex bounce, rung-1a fix round). Here the index
; sort = element sort = T, so a read select(_,_) : T is itself usable AS an index (nested
; selects), and a ROW2 [assert_eq] on read RESULTS merges element = index terms mid-pass --
; which can stale the an_diseqs class-pair index that an_distinct_idx keys on. This is the
; case the guarded an_distinct_idx (verify BOTH orientations live, else None) makes sound BY
; CONSTRUCTION: a staled/non-matching hit degrades to a missed match (completeness-only,
; row_split backstop), NEVER a wrong premise. The unguarded form (assume the orientation)
; is the mutant this RED targets.
;
; SATISFIABLE (z3: sat via d=false): on d=true, i<>j is entailed, ROW2 gives
; select(store a i v) j = select a j, so by congruence select(a,(select(store a i v) j)) =
; select(a,(select a j)) -- contradicting the assertion; the solver falls to d=false where
; i<>j is not entailed, ROW2 does not fire, and the two nested reads may differ -> a model.
; Gate PASS iff OXSMT_ARR_ROW2=1 verdict is NOT unsat (unknown ok -- we cannot model-check
; this crafted instance; wrong-unsat = the mutant misfired on a stale same-sort hit).
(set-logic QF_AX)
(declare-sort T 0)
(declare-fun a () (Array T T))
(declare-fun i () T)
(declare-fun j () T)
(declare-fun v () T)
(declare-fun d () Bool)
(assert (or (not d) (distinct i j)))
(assert (not (= (select a (select (store a i v) j)) (select a (select a j)))))
(check-sat)
