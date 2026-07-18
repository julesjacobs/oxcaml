; Bitvector add is invertible mod 2^w: x + 1 = 0 has the solution x = 0xff (255) at
; width 8. Bit-blasting engine wired: the pure-QF_BV dispatch solves it (model x = 255).
(set-logic QF_UFBV)
(set-info :status sat)
(declare-const x (_ BitVec 8))
(assert (= (bvadd x #x01) #x00))
(check-sat)
