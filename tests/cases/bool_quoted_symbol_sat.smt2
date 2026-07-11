; Pure-Boolean satisfiable with a |quoted symbol| name (contains a space, so it is NOT a
; simple symbol and must round-trip through the eval bridge as |p q|). Exercises the
; solver -> harness -> eval path for SMT-LIB 2.6 quoted names: the harness must re-quote
; the model binding, and the N-version evaluator must accept it. Model in
; bool_quoted_symbol_sat.model. Gate-certified.
(set-logic QF_UF)
(set-info :status sat)
(declare-const |p q| Bool)
(assert |p q|)
(check-sat)
