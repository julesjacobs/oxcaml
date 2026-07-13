; Presolve-INERT (QF_LIA): only inequalities, no top-level unconditional alias, so the W1b
; presolve eliminates nothing and this is byte-identical to the old per-term path. The
; no-op family spot-check: verdict (sat) must be unchanged by the wiring, on both drivers.
(set-logic QF_LIA)
(set-info :status sat)
(declare-const a Int)
(declare-const b Int)
(assert (<= a b))
(assert (>= a 0))
(assert (<= b 5))
(check-sat)
