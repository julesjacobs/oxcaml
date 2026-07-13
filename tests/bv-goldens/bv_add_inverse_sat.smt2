; Bitvector add is invertible mod 2^w: x + 1 = 0 has the solution x = 0xff at width 8.
; Sat (model x = #xff). Marked :status unknown until the bit-blasting engine is wired;
; flips to sat then. Front-half golden: parses, sort-checks, prints round-trip.
(set-logic QF_UFBV)
(set-info :status unknown)
(declare-const x (_ BitVec 8))
(assert (= (bvadd x #x01) #x00))
(check-sat)
