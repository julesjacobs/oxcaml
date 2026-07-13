; Datatype rule 5 — enum case split / pigeonhole (GOALS: "four pairwise-distinct
; values of a three-constructor type is unsat"). color has exactly three nullary
; constructors, so four distinct color values cannot exist: unsat. Marked :status
; unknown until the datatype theory (case splitting) is wired.
(set-logic QF_DT)
(set-info :status unsat)
(declare-datatypes ((color 0)) (((red) (green) (blue))))
(declare-const a color)
(declare-const b color)
(declare-const c color)
(declare-const d color)
(assert (distinct a b c d))
(check-sat)
