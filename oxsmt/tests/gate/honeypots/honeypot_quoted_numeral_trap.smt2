; G1 trap (codex): |0| is the SYMBOL "0" (here sort S), NOT the numeral 0.
; A reader that drops the quote lexes |0| as the numeral 0, turning
; (distinct |0| 0) into (distinct 0 0) = 0 != 0 = False -> a FALSE unsat CERTIFIED.
; The fixed reader keeps |0| a symbol, so (distinct S-symbol Int-literal) is an
; ill-sorted assertion -> MALFORMED (cleanly rejected, never CERTIFIED).
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-sort S 0)
(declare-const |0| S)
(assert (distinct |0| 0))
(check-sat)
