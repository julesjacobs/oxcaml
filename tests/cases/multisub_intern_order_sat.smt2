; F2 (LRA review bounce) flag-OFF byte-identity gate. Multi-subtrahend subtraction
; [(- a b c ...)] whose interned operand order (and therefore the printed model bytes) must
; stay byte-identical to trunk. Trunk interned the subtrahends before the head via OCaml's
; right-to-left evaluation of [::]; the LRA front end must reproduce that on the flag-OFF
; integer path. The trivial [(- x y)] case does not surface the divergence; this
; multi-subtrahend case does. The pinned model below is trunk's exact answer.
(set-logic QF_LIA)
(set-info :status sat)
(declare-const a Int)
(declare-const b Int)
(declare-const c Int)
(declare-const d Int)
(assert (<= (- a b c) 5))
(assert (>= (- a b d) 3))
(assert (<= (- b c d a) (- 10)))
(assert (>= (- c a b d) (- 7)))
(assert (distinct (- a b) (- c d)))
(assert (<= (+ a b c d) 100))
(check-sat)
