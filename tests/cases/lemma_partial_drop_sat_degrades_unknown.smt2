; Partial-assertion SENTINEL regression guard (lemmas-climb hedge H1). The universal
; assertion has an [exists] in NEGATIVE position (under a [not]), which is NOT Skolemized
; (Skolemizing a negative existential to a function is the polarity landmine, chunk 2b) —
; so the reader cannot represent it, the loader DROPS it (a sound weakening) and arms the
; always-live sat-degrade sentinel. The ground core [(p a a)] alone is SATISFIABLE, so
; WITHOUT the sentinel this file would report a (wrong) `sat` — but the dropped universal
; [forall x. not (exists y. p x y)] is [forall x y. not (p x y)], whose true status with
; [p a a] is `unsat`. The sentinel degrades the ground `Sat` to `Unknown`, which is sound.
; If the sentinel were ever disarmed this golden flips to `sat` (a soundness violation).
; A POSITIVE exists under a forall is now Skolemized (chunk 2b), so it would no longer be
; dropped — this guard deliberately uses a NEGATIVE-position exists, which stays dropped, so
; it keeps exercising the sentinel path the review asked for.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int Int) Bool)
(declare-fun a () Int)
(assert (p a a))
(assert (forall ((x Int)) (not (exists ((y Int)) (p x y)))))
(check-sat)
