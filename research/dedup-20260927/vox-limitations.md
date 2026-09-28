# Vox limitations met while removing duplicated demo code (27 Sep 2026)

Readiness item 7: one DFA pipeline, a shared spin-lock functor, fewer SAT
solver variants.

1. **`include` of a functor instance loses value identity in refinements.**
   After `module L = Spin_lock.Make (Cell)  include L`, the lemma
   `L.owned_def a h` does not prove a goal stated with the included `owned`
   and `cell`: the checker treats `owned` and `L.owned` as unrelated symbols.
   A plain alias `let owned = L.owned` does keep the identity inside the
   module. Workaround: aliases instead of `include`. Suggested fix: record
   included values as aliases of the functor instance's values.

2. **Signature matching compares refinements syntactically.** With
   `let owned = L.owned`, `let try_acquire = L.try_acquire` still does not
   match `val try_acquire : ... owned a ...` in the `.mli`, because the
   inferred type says `L.owned`. Each re-exported operation needs an eta
   wrapper that restates its refinement (`reference_lock.ml`,
   `unique_lock.ml`: 4 lines each for `try_acquire` and `release`), and a
   lemma whose statement is specialised (`owned_def`) needs a proof wrapper.
   This is why the shared functor saves fewer lines than the duplicated
   protocol. Suggested fix: resolve value aliases (and `[@def]` definitions
   that are plain aliases) when comparing refinements at signature matching,
   or discharge the inclusion with the solver.

3. **`let` is not generalised inside `ghost_`.** In the body of a ghost
   function, `let empty = [] in f empty empty'` fails when the two uses need
   different element types ("The value empty has type int list"). The same
   code outside `ghost_` type-checks. Workaround: write `[]` at each use.

4. **No way to write a search once and get both a runtime certificate and an
   erased one** (already noted in the DFA review). The DFA demo now keeps
   only the erased search; proofs that need a witness word read the ghost
   decision of `comparison_proved` from ghost functions (`compare_states`,
   `distinguish_classes`, `minimization_certificate`). This works well: a
   ghost function (`... : t @ ghost = ghost_ (...)`) can call ordinary total
   functions and pass ghost closures to them. The price is that a runtime
   certificate API (`diagnose_*`) is gone; tests check hand-written
   certificates instead.

5. **Warning 225 (redundant `ghost_`) inside ghost bodies** fires for every
   `ghost_ (lemma ...);` statement once a function becomes ghost, so turning
   a function ghost means stripping its inner `ghost_` wrappers. Harmless,
   but it makes such a change noisier than it needs to be.
