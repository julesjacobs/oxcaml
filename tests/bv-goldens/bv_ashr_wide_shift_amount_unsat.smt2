; Wide variable-shift RED (census task #78, bvashr bucket): a variable shift amount [b]
; with bit 63 set means the amount is >= 2^63 >> the 64-bit width, so a logical/arith shift
; right totals to the fill value (0 for bvlshr) for ANY low bits and ANY [a]. Hence
; (bvlshr a b) = 0 and the negation is unsat. This exercises the barrel-shifter's
; overflow term (var_shift in blast.ml): the buggy version tested [1 lsl k >= w] which
; OVERFLOWS OCaml's native int for k >= 63, silently dropping high shift-amount bits from
; the overflow OR — so bit 63 was neither staged nor counted, the circuit treated the shift
; as small, and the SAT core produced a spurious model its own Bv_eval re-check rejected
; (verdict: unknown). With the position-based overflow term the bit is caught -> result 0 ->
; unsat. This goes RED (unknown != unsat) against the pre-fix circuit.
(set-logic QF_BV)
(set-info :status unsat)
(declare-const a (_ BitVec 64))
(declare-const b (_ BitVec 64))
(assert (= ((_ extract 63 63) b) #b1))
(assert (not (= (bvlshr a b) (_ bv0 64))))
(check-sat)
