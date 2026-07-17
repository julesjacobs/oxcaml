; The harness must validate this sat model with the independent BV evaluator.  The unique
; solution is x=255: changing the emitted payload to 254 falsifies the original assertion.
(set-logic QF_BV)
(set-info :status sat)
(declare-const x (_ BitVec 8))
(assert (= (bvadd x #x01) #x00))
(check-sat)
