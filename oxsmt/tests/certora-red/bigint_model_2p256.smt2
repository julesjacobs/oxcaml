; RED golden for DESIGN A13 (Model.value -> Int of Bigint): a model value exceeding int63.
; Under the pre-A13 native-int Model.Int, extract_model's Rational.num projection overflowed
; -> Rational.Overflow escaped Sat.solve -> unknown. Under A13 the model carries the 2^256
; value as Bigint, R1 model_check evaluates (= x 2^256) true, and the verdict is sat.
; A UF application keys the huge value through the combinator table + Cdclt value_of too.
(set-logic QF_UFLIA)
(declare-fun x () Int)
(declare-fun f (Int) Int)
(assert (= x 115792089237316195423570985008687907853269984665640564039457584007913129639936))
(assert (= (f x) 115792089237316195423570985008687907853269984665640564039457584007913129639935))
(assert (> (f x) 5))
(check-sat)
