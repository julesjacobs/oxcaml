; Auto-trigger inference (ADR-0012 L3): the SAME len goal as lemma_len_unsat, but the
; recursive equation ships NO :pattern — the common shape in the public quantified sets.
; The loader infers the trigger (the smallest UF-application covering the binders, here
; len(cons x l)) from the body, so the two instantiations still fire and close the goal.
; Pairs with lemma_len_unsat (explicit pattern) to show inference reaches the same verdict.
(set-logic UFLIA)
(set-info :status unsat)
(declare-sort Elem 0)
(declare-sort Lst 0)
(declare-fun nil () Lst)
(declare-fun cons (Elem Lst) Lst)
(declare-fun len (Lst) Int)
(declare-const a Elem)
(declare-const b Elem)
(assert (= (len nil) 0))
(assert (forall ((x Elem) (l Lst)) (= (len (cons x l)) (+ (len l) 1))))
(assert (not (= (len (cons a (cons b nil))) 2)))
(check-sat)
