; codex B1 regression: Box has a single Bool field, so it has exactly two inhabitants
; (box false, box true). Three pairwise-distinct boxes is therefore UNSAT (pigeonhole on a
; 2-element field). Before the finite-Bool fix the DT model path reported a WRONG sat
; (Dt.leaf_value lumped Bool with the unbounded Uninterp bucket); the constructor-tree
; self-check then closed the wrong-sat hole by degrading to a sound `unknown`.
;
; The Bool-cardinality completeness fix ({!Session.register_bool_terms} +
; {!Cdclt.bind_bool_var_atom}) now DECIDES each bare Bool constructor argument [a]/[b]/[c]:
; they are bound to their propositional SAT vars as EUF [K_bool] atoms, so the SAT core
; case-splits them and congruence + the [true <> false] axiom discharges the pigeonhole.
; The verdict is now the correct `unsat` (matching :status), never a guessed sat.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((Box 0)) (((box (val Bool)))))
(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(assert (distinct (box a) (box b) (box c)))
(check-sat)
