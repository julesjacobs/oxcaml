; rider #19 / fable MED-1: a repeat whose result width (2000000*1) exceeds the default
; max_bv_width (2^20) degrades to unknown at parse time — the cap fires BEFORE any
; allocation, so this never core-dumps (the pre-cap tip aborted the process here).
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 1))
(assert (= ((_ repeat 2000000) x) ((_ repeat 2000000) x)))
(check-sat)
