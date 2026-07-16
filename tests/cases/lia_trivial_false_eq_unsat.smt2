; Trivial-false LIA equality (census task #78 follow-up, review H1). [x = x + 1] cancels its
; variable part to the context-independent constant [0 = 1] -> Trivially_false. With the fix
; (OXSMT_LIA_TRIVIAL_EQ default-on) this is recorded as a frame-scoped empty-Farkas check
; conflict -> unsat. With the flag off (=0) it takes the trunk raise -> CONTRACT-POISON
; firewall -> whole-query unknown. The loud-unknown/lia-trivial-eq test drives both env
; states, giving the OFF Unsupported-raise path in-process coverage it otherwise lacks (the
; flag is read once at module load, so a single build cannot see both states).
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= x (+ x 1)))
(check-sat)
