; Lemma tier soundness rule (GOALS Lemmas / ADR-0012 §2): with a quantifier present, a
; sat degrades to UNKNOWN — never a model that ignores the quantifier. The ground core
; (f(a)=5) alone is sat; the lemma forall x. f(x)>0 is consistent with it, so the matcher
; finds no refutation. Because a lemma is live, the sat is NOT reported as a model —
; check_sat degrades it to unknown. A build that dropped the quantifier would wrongly
; answer sat with a model that need not satisfy the universal.
(set-logic UFLIA)
(set-info :status unknown)
(declare-fun f (Int) Int)
(declare-const a Int)
(assert (forall ((x Int)) (! (> (f x) 0) :pattern ((f x)))))
(assert (= (f a) 5))
(check-sat)
