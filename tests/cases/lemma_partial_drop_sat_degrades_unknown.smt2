; Partial-assertion SENTINEL regression guard (lemmas-climb hedge H1). The universal
; assertion has an [exists] in NEGATIVE position (under a [not]), which is NOT Skolemized
; (Skolemizing a negative existential to a function is the polarity landmine, chunk 2b): the
; reader cannot represent it, so the loader DROPS it (a sound weakening) and arms the
; always-live sat-degrade sentinel. The ground core [(p a a)] alone is SATISFIABLE, so
; WITHOUT the sentinel this file would report a (wrong) `sat` — but the dropped universal
; [forall x. not (exists y. p x y)] is [forall x y. not (p x y)], whose true status with
; [p a a] is `unsat`. The sentinel degrades the ground `Sat` to `Unknown`, which is sound.
; If the sentinel were ever disarmed this golden flips to `sat` (a soundness violation), so
; it stands as the standing guard the review asked for. (The earlier POSITIVE-exists shape
; is now Skolemized+solved by chunks 2b/2c — see lemma_skolem_fun_under_forall_unsat and
; lemma_skolem_trigger_ground_pref_unsat — so this guard uses a genuinely-dropped shape.)
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int Int) Bool)
(declare-fun a () Int)
(assert (p a a))
(assert (forall ((x Int)) (not (exists ((y Int)) (p x y)))))
(check-sat)
