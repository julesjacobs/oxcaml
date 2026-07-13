; codex reachability for session default_value: the W1b presolve eliminates the bare Int
; var x (x = (head d)), and model reconstruction then re-derives x from its def, whose only
; free leaf is the nullary DATATYPE const d. default_value(d : lst) has no sound scalar
; default (a datatype value is a constructor tree), so it must fail closed -> no model ->
; unknown, never fabricate VUninterp 0 for d (the silent wrong-value class). Marked
; :status unknown; stays unknown once the datatype theory lands (this is the plumbing
; fail-closed, not a theory result).
(set-logic QF_UFDT)
(set-info :status unknown)
(declare-datatypes ((lst 0)) (((cons (head Int) (tail lst)) (nil))))
(declare-const d lst)
(declare-const x Int)
(assert (= x (head d)))
(check-sat)
