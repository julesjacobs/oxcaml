; Datatype rule 1 — constructor clash. succ and zero are distinct constructors of
; nat, so (succ n) can never equal zero: unsat. The DT theory (this file's acceptance
; test) decides it via the constructor-distinctness rule.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const n nat)
(assert (= (succ n) zero))
(check-sat)
