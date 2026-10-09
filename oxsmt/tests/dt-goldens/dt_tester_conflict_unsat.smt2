; Datatype testers. n cannot be both a succ and a zero, so the conjunction of the
; two testers is unsat. Exercises tester parsing AND printing ((_ is C) t) end to
; end (round-trip coverage). The DT theory decides it via tester/constructor
; exclusivity (a true tester whose class carries a different constructor conflicts).
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const n nat)
(assert (and ((_ is succ) n) ((_ is zero) n)))
(check-sat)
