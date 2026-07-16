; n-ary RED (census task #78, review H2): [bvor] is SMT-LIB :left-assoc, so the 4-ary
; form (bvor a b c d) must parse identically to the nested-binary (bvor (bvor (bvor a b) c) d).
; Asserted distinct, they cannot differ -> unsat. This DISCRIMINATES the n-ary parse fold:
; with the fold the file solves unsat; reverting the fold makes the parser reject the 4-ary
; bvor (Unsupported) so the golden runner reports unknown != unsat and this test goes RED.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 8))
(declare-const b (_ BitVec 8))
(declare-const c (_ BitVec 8))
(declare-const d (_ BitVec 8))
(assert (distinct (bvor a b c d) (bvor (bvor (bvor a b) c) d)))
(check-sat)
