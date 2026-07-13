; Malformed s-expression (unbalanced paren): corpus_classify -> parse-fail; oxsmt_cli
; catches the same parse failure and emits no block. Both fold to unknown. Exercises the
; parse-fail normalization branch.
(set-logic QF_LIA)
(declare-const x Int)
(assert (> x 0)
(check-sat)
