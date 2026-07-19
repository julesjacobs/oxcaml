; Int-only control (bugreport 03): no datatypes, so the combined EUF+LIA stack handles it
; already. k > 0 and not (k > -1) is unsat. Must stay unsat under the combination fix.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const k Int)
(assert (> k 0))
(assert (not (> k (- 1))))
(check-sat)
