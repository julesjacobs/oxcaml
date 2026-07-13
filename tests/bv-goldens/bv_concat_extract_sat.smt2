; Concatenate two 4-bit vectors then extract the halves back out: the high nibble of
; (concat x y) is x and the low nibble is y, so this is sat for every x, y. Exercises
; concat + (_ extract i j). Marked :status unknown until the bit-blasting engine is wired.
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 4))
(declare-const y (_ BitVec 4))
(assert (= ((_ extract 7 4) (concat x y)) x))
(assert (= ((_ extract 3 0) (concat x y)) y))
(check-sat)
