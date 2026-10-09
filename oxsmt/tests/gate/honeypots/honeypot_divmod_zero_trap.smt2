; Zero-divisor trap (gate-divmod): SMT-LIB leaves (div x 0)/(mod x 0) UNCONSTRAINED (any
; value), and the solver rejects div/mod by zero, so the gate must NOT certify it — the
; euclidean elimination x = 0*q + r, 0 <= r < |0| is vacuous/ill-defined. The divisor
; preflight fails closed: UNSUPPORTED. Note Lean's Int.emod x 0 = x would otherwise let the
; sat direction "compute" a value the solver never sanctions; rejecting up front in BOTH
; directions keeps the gate to exactly the theory the solver solves.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (div x 0) 0))
(check-sat)
