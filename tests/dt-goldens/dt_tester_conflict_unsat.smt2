; Datatype testers. n cannot be both a succ and a zero, so the conjunction of the
; two testers is unsat. Exercises tester parsing AND printing ((_ is C) t) end to
; end (round-trip coverage). Marked :status unknown until the datatype theory wires
; the tester/constructor-exclusivity rule.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const n nat)
(assert (and ((_ is succ) n) ((_ is zero) n)))
(check-sat)
