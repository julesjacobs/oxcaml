; Disequality-aware field completion (task #14). x is forced to be a `leaf` and asserted
; distinct from `leaf zero`; by injectivity that means `data x` must differ from `zero`. It
; IS satisfiable (data x = succ zero). Base completion fills the free field `data x` with the
; base `zero`, reproducing the forbidden `leaf zero`, so the checker rejects -> unknown; the
; disequality-aware completion fills `data x` avoiding `zero` -> checked-sat. RED against the
; pre-fix base completion.
(set-logic QF_DT)
(set-info :status sat)
(declare-datatypes ((nat 0) (tree 0)) (((succ (pred nat)) (zero)) ((node (kids tree)) (leaf (data nat)))))
(declare-const x tree)
(assert (not (= x (leaf zero))))
(assert ((_ is leaf) x))
(check-sat)
