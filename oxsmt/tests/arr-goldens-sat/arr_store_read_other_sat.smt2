; Reading a stored array at a DIFFERENT index returns the original: with i != j,
; select(store(a,i,v), j) = select(a,j) is satisfiable. The checker computes the store
; overlay and, since the looked-up index j does not match the stored i, falls through to
; a's map. Checked sat.
(set-logic QF_AX)
(set-info :status sat)
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a () (Array Index Element))
(declare-fun i () Index)
(declare-fun j () Index)
(declare-fun v () Element)
(assert (not (= i j)))
(assert (= (select (store a i v) j) (select a j)))
(check-sat)
