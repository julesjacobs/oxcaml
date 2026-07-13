; Datatype rule 4 — cycle refutation (occurs check). A datatype value cannot contain
; itself: x = (cons h x) is unsat. Uses two datatypes (nat, lst) to exercise a
; multi-datatype declaration block. The DT theory decides it via the occurs check.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((nat 0) (lst 0))
  (((succ (pred nat)) (zero))
   ((cons (head nat) (tail lst)) (nil))))
(declare-const x lst)
(declare-const h nat)
(assert (= x (cons h x)))
(check-sat)
