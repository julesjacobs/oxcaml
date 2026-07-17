; Review fix F1 (WRONG SAT, split shared existential). `exists x. (x=0 /\ exists z. (x=1
; /\ z=z))` clausifies (Skolemize x->k, z->kz; flatten the And) into ground clauses `k=0`,
; `k=1`, `kz=kz`. The SAME existential x is referenced by two clauses; each clause's build
; must reuse ONE witness symbol for x. A fresh-per-clause witness gave `k1=0 /\ k2=1` (both
; SAT) -> wrong `sat`; z3 says unsat (`x=0 /\ x=1` is contradictory). FIX: the Skolem minter
; is memoized by binder id, so both clauses share `k` -> `k=0 /\ k=1` -> unsat. OFF drops the
; term-nested exists (unknown); ON must be `unsat`, and MUST NOT be `sat`.
(set-logic UFLIA)
(set-info :status unsat)
(assert (exists ((x Int)) (and (= x 0) (exists ((z Int)) (and (= x 1) (= z z))))))
(check-sat)
