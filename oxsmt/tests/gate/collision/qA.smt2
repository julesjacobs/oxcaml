; Cache-key injectivity exhibit A (paired with qB.smt2). A review found that the
; original space/paren-concatenated canonical form was not injective: the quoted
; const name below contains a newline and parens, forging token boundaries so
; that this genuinely-UNSAT query and the genuinely-SAT qB produced identical
; canonical bytes and shared one cache entry. qB then inherited qA's CERTIFIED
; verdict without Lean ever running.
;
; Regression: `gate selftest` embeds both queries and asserts their canonical
; strings and cache keys DIFFER. See canonical.ml for the injectivity argument.
;
; qA asserts a and (not a) -> UNSAT.
(set-logic QF_UFLIA)
(declare-const a Bool)
(declare-const |a
(not $a)| Bool)
(set-info :status unsat)
(assert a)
(assert (not a))
(check-sat)
