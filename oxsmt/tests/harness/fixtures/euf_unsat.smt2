; Fixture: a small EUF unsat query (congruence: a=b, f(a)!=f(b)).
; Development fixture for the harness itself — the real corpus lives in
; tests/cases/ (owned by another task). The stub solver reports `unknown`, so
; the declared :status is not contradicted (a completeness gap, not a failure).
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort U 0)
(declare-fun a () U)
(declare-fun b () U)
(declare-fun f (U) U)
(assert (= a b))
(assert (not (= (f a) (f b))))
(check-sat)
