; select(store(a,i,v), i) = v is a read-over-write tautology, hence satisfiable. The model
; builds store(a,i,v) as a's map overlaid with i->v; the checker reads it back and confirms.
; Checked sat (was unknown before array model construction).
(set-logic QF_AX)
(set-info :status sat)
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a () (Array Index Element))
(declare-fun i () Index)
(declare-fun v () Element)
(assert (= (select (store a i v) i) v))
(check-sat)
