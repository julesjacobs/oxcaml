; Free Boolean variable mixed with a bit-vector predicate. The blaster assigns the free
; Bool variable a fresh SAT literal, but pre-fix the sat model read-back recovered only
; bit-vector variables, so the independent re-check could not evaluate the assertion that
; mentions [b] and the query fail-closed to unknown. The blaster now records free Bool
; vars for read-back, completing the re-check. b forced true => x = 0. sat.
(set-logic QF_BV)
(set-info :status sat)
(declare-const x (_ BitVec 8))
(declare-const b Bool)
(assert (= (= x (_ bv0 8)) b))
(assert b)
(check-sat)
