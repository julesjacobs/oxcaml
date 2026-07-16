; Partial-assertion SENTINEL regression guard (lemmas-climb hedge H1). The universal
; assertion has an [exists] in its body, so the reader cannot represent it: the loader
; DROPS it (a sound weakening) and arms the always-live sat-degrade sentinel. The ground
; core [(p a)] alone is SATISFIABLE, so WITHOUT the sentinel this file would report a
; (wrong) `sat` — but dropping the universal weakened the set, and the true status is
; `unsat` (the dropped [forall x. (exists y. y=0 /\ not (p x))] is equivalent to
; [forall x. not (p x)], which contradicts [p a]). The sentinel degrades the ground `Sat`
; to `Unknown`, which is sound. If the sentinel were ever disarmed this golden flips to
; `sat` (a soundness violation), so it stands as the standing guard the review asked for.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(declare-fun a () Int)
(assert (p a))
(assert (forall ((x Int)) (exists ((y Int)) (and (= y 0) (not (p x))))))
(check-sat)
