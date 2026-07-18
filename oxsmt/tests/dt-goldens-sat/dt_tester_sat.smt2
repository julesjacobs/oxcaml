; A tester asserted true is satisfiable: some nat IS a succ (its argument free).
; The model gives n = succ(<base nat>); the constructor-tree self-check confirms
; (_ is succ) n holds, so the verdict is a CHECKED sat (was unknown before model
; construction).
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))
(declare-const n nat)
(assert ((_ is succ) n))
(check-sat)
