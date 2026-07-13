; Datatype rule 1 — constructor clash. succ and zero are distinct constructors of
; nat, so (succ n) can never equal zero: unsat. Marked :status unknown while the
; datatype theory is not yet wired (flips to unsat once the theory lane lands; this
; file is its acceptance test).
(set-logic QF_DT)
(set-info :status unknown)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const n nat)
(assert (= (succ n) zero))
(check-sat)
