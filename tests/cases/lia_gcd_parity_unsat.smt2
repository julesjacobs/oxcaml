; Multi-row parity infeasibility (task #128): x = 2q and x = 2q'+1 force 2q - 2q' = 1,
; which has no integer solution (gcd(2,2)=2 does not divide 1) — UNSAT. Each equality is
; integer-feasible ALONE; the contradiction only appears after eliminating the shared x, so
; the single-row diophantine gcd test misses it and raw B&B diverges on the two unbounded
; quotients. The multi-row integer-elimination gcd cut (OXSMT_LIA_GCD_CUT) refutes it. With
; presolve on (the harness default) presolve derives the same contradiction, so this file
; is a plain unsat golden here; the lia-gcd-cut-test Makefile target exercises the search
; path (--no-presolve) in both flag states.
(set-logic QF_LIA)
(declare-const x Int)
(declare-const q Int)
(declare-const qp Int)
(assert (= x (* 2 q)))
(assert (= x (+ (* 2 qp) 1)))
(check-sat)
