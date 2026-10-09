; Lemma tier (ADR-0012 / GOALS Lemmas): a goal that needs two instantiations of a user
; lemma solves. [len] is the classic case — its defining equations as a base fact plus a
; universally-quantified recursive equation with an explicit :pattern. The matcher must
; fire twice (x|->a then x|->b), chaining len(cons a (cons b nil)) = len(cons b nil)+1 =
; (len nil + 1) + 1 = 2, so asserting it is NOT 2 is unsat. Lists are abstracted in UF
; (nil/cons/len uninterpreted) since datatypes are a separate theory.
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
(assert (forall ((x Elem) (l Lst))
  (! (= (len (cons x l)) (+ (len l) 1))
     :pattern ((len (cons x l))))))
(assert (not (= (len (cons a (cons b nil))) 2)))
(check-sat)
