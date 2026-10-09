(set-info :smt-lib-version 2.6)
(set-logic QF_ALIA)
(set-info :status sat)
; REGRESSION (codex wrong-UNSAT): the extensionality witness index MUST be unforgeable.
; A user declares a constant whose name collides with the OLD witness scheme (@arr.ext.0 —
; @ and . are legal SMT-LIB simple-symbol characters, so this parses) and asserts a and b
; AGREE at that index. This formula is genuinely SAT (a and b can differ at some OTHER
; index). With the pre-fix forgeable witness, the ext axiom minted the SAME symbol
; @arr.ext.0 and asserted select a @arr.ext.0 <> select b @arr.ext.0 — a direct
; contradiction with the user's equality => a FALSE UNSAT. The fix mints the witness in the
; reserved .oxsmt.* namespace (unforgeable), so the ext index is distinct from the user's
; constant and no false conflict arises; the query degrades to unknown (v1 array sat).
; Post-fix verdict: unknown (NOT unsat). Pre-fix: unsat (the bug).
(declare-sort Element 0)
(declare-fun a () (Array Int Element))
(declare-fun b () (Array Int Element))
(declare-fun @arr.ext.0 () Int)
(assert (not (= a b)))
(assert (= (select a @arr.ext.0) (select b @arr.ext.0)))
(check-sat)
(exit)
