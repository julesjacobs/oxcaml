; (_ bvN W) decimal bitvector-literal syntax (SMT-LIB indexed identifier). sat: x = 35.
(set-logic QF_UFBV)
(set-info :status sat)
(declare-const x (_ BitVec 8))
(assert (= x (_ bv35 8)))
(check-sat)
