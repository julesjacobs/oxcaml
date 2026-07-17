# PIFO tree scheduler equivalence — Lean 4 mechanization

Theorem: for stateless PIFO-tree schedulers (arbitrary finite topologies),
flush-sequence equivalence (push* pop*) implies full interleaved equivalence
((push+pop)*). Statement in PifoStatement.lean; proof in Answer.lean
(sole import: PifoStatement; axioms: propext, Classical.choice, Quot.sound).

Check (Lean 4.31, no mathlib):

    lean PifoStatement.lean -o PifoStatement.olean
    LEAN_PATH=. lean Answer.lean

The English prose proof (refereed; the Lean development follows its structure)
is in pifo-theorem.md.

## Stateful counterexample

For STATEFUL schedulers the two equivalences differ: stateful-counterexample/
contains a machine-checked pair that agrees on every flush word but diverges
on an interleaved word (Pifo.lean; standard axioms). Extended.lean adds a
bounded n<=30 sweep via native_decide as supporting evidence.

    cd stateful-counterexample
    lean Pifo.lean -o Pifo.olean
    LEAN_PATH=. lean Extended.lean
