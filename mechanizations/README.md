# PIFO tree scheduler equivalence — Lean 4 mechanization

Theorem: for stateless PIFO-tree schedulers (arbitrary finite topologies),
flush-sequence equivalence (push* pop*) implies full interleaved equivalence
((push+pop)*). Statement in PifoStatement.lean; proof in Answer.lean
(sole import: PifoStatement; axioms: propext, Classical.choice, Quot.sound).

Check (Lean 4.31, no mathlib):

    lean PifoStatement.lean -o PifoStatement.olean
    LEAN_PATH=. lean Answer.lean
