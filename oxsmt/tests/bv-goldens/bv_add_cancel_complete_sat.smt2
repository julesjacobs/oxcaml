; RED for task #50 (gate-OFF, no OXSMT_BV_REWRITE2): the additive normalizer cancels the
; a-a term and the whole equality collapses to a tautology, so NEITHER a nor b reaches the
; bit-blaster. Without model completion this is `sat` with an EMPTY model (the goldens
; harness's "sat must surface a non-empty self-checked model" check then FAILS). With the
; task-#50 completion (unconditional, gate-OFF too) the surfaced model binds both a and b.
(set-logic QF_BV)
(set-info :status sat)
(declare-const a (_ BitVec 8))
(declare-const b (_ BitVec 8))
(assert (= (bvadd (bvsub a a) b) b))
(check-sat)
