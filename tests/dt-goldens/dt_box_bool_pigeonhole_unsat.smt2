; codex B1 regression: Box has a single Bool field, so it has exactly two inhabitants
; (box false, box true). Three pairwise-distinct boxes is therefore UNSAT (pigeonhole on a
; 2-element field). Before the finite-Bool fix the DT model path reported a WRONG sat
; (Dt.leaf_value lumped Bool with the unbounded Uninterp bucket). The theory does not
; case-split Bool fields so it does not refute; the constructor-tree self-check now REJECTS
; the model (a Bool position may not hold an Uninterp / three distinct Bools cannot exist),
; so the verdict is a sound `unknown` — never sat. Marked :status unsat; oxsmt degrades to
; unknown (sound, completeness gap), and MUST NOT report sat.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((Box 0)) (((box (val Bool)))))
(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(assert (distinct (box a) (box b) (box c)))
(check-sat)
