# Welcome to the vox2 IDE

This is a small, local, single-user editor for **vox2** — OCaml extended with
refinement types whose proof obligations are discharged by the compiler as you
type. Checks run on your own machine; nothing leaves the box.

## The three panes

- **Explorer** (left): the curated `Examples` and these read-only `Docs`. Use
  the `☰` button in the header to hide or show it. Files are colour-coded by
  kind, and an example that is *expected* not to verify is prefixed with `✗`.
- **Editor** (middle): a refinement-aware OCaml buffer. Type errors get red
  squiggles; verification failures get amber ones. Zero-width diagnostics show
  a `^` (type) or `!` (verify) marker.
- **Output** (right): the buffer-wide STATUS zone, the cursor-following PROOF
  zone, and the CURSOR zone with the type and inferred module signature.

## Working in a buffer

- Editing automatically typechecks and verifies after a short pause. From
  left to right, the header shows the backend dropdown, the current status,
  then the latest completed round as `(N ms)`. While a newer round is pending,
  it explicitly shows `checking…` and `(—)` instead of stale results.
- Move the cursor onto a marked obligation to read its goal and hypotheses in
  the Proof pane.

## Header controls

- **compact** — show only the goal and hypotheses; uncheck for the full proof
  state.
- Hypotheses the proof did not rely on are always dimmed.
- **light / dark** — flip the palette; the choice is applied before first paint,
  so there is no flash on reload.

## Reading the docs

These `.md` docs open read-only and are never compiled — switching to a doc
clears the diagnostics and proof state from whatever file you were editing, and
switching back to an example restores the editor.

To start writing, open **Sixty seconds** under `Examples`, or read the
[refinement guide](docs/refinements.md).
