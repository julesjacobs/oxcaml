; Same-sort (Array T T) ROW2 boundary pin (rung-1a). Here the index sort = element sort = T,
; so a read select(_,_) : T is itself usable AS an index (nested selects), and a ROW2
; [assert_eq] on read RESULTS merges element = index terms mid-pass. This exercises the
; an_distinct_idx path where its (ci,cj) class-pair key could in principle stale.
;
; WHAT THIS PINS (per the false-invariant-comment standard -- state what is enforced, not a
; mutant that no longer flips): with today's Euf, this instance is NOT unsat under the
; unguarded OR the guarded form -- codex source-verified that a live index hit FORCES
; co-classing (Euf.find has no path compression; class ids are never recycled; union
; re-parents the absorbed root permanently), so a stale key can only MISS, never mis-orient,
; even in this same-sort construct. So this is a FUTURE-PROOF REGRESSION PIN: it is the
; instance a live-rep-breaking Euf change (id RECYCLING or adding PATH COMPRESSION) would
; make the *unguarded* an_distinct_idx wrong-unsat on. The guard (verify BOTH orientations
; live, else None) keeps it sound regardless; this fixture guards the guard's raison d'etre.
;
; SATISFIABLE (z3: sat via d=false): on d=true, i<>j is entailed, ROW2 gives
; select(store a i v) j = select a j, so by congruence select(a,(select(store a i v) j)) =
; select(a,(select a j)) -- contradicting the assertion; the solver falls to d=false where
; i<>j is not entailed, ROW2 does not fire, and the two nested reads may differ -> a model.
; Gate PASS iff OXSMT_ARR_ROW2=1 verdict is NOT unsat (unknown ok -- we cannot model-check
; this crafted instance).
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
