; Incremental (push/pop): the shipped parser is a whole-document reader with no push/pop
; support, so both drivers degrade to unknown (unknown-incremental). Exercises the
; push/pop-detection normalization branch.
(set-logic QF_LIA)
(declare-const x Int)
(push 1)
(assert (> x 0))
(check-sat)
(pop 1)
