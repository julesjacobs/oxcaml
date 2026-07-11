; HONEYPOT (c): malformed input (unbalanced parentheses). The reader must reject
; it as MALFORMED; it must never reach certification.
(set-logic QF_LIA)
(declare-const x Int)
(assert (>= x 0)
(check-sat)
