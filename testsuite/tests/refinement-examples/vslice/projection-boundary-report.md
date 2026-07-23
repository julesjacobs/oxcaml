# Future projection boundary

`final`, `prophecy_value`, and `final_frame_values` denote values that do not
exist yet.  They are sound as specification symbols, but cannot implement the
current executable `int list` interface before the corresponding loan closes.

`future_projection_runtime_repro.ml` captures a final frame and attempts to
evaluate its projection before mutating and closing the loan.  The
specification-only boundary now rejects that value-level call.  This also
shows why an executable immutable list cannot represent the future: a client
could otherwise retain the early result across later mutations.

Logical result and arrow modes alone do not form this boundary.  The enforced
split is:

- keep `contents`, `current`, and `snapshot_values` executable because they
  describe materialized values;
- make `final`, `prophecy_value`, and `final_frame_values` specification-only;
- reject direct, aliased, partial, nested, and module-qualified value-level
  calls to specification-only symbols;
- if an executable future is needed, expose a separate typestate API whose
  value can only be read after resolution.

The compiler tests should cover positive refinement use, all value-level
rejection forms, absence of runtime primitive dependencies, and continued
execution of current/snapshot projections.  The quicksort example uses the
three future projections only in refinements, so it should not need a source
rewrite.
