# Vox limitations met while making borrowing partial (28 September 2026)

Branch `jujacobs/vox/partial-borrows-20260928`.

1. **`Owned_array.append` could not be total without a new assumption.**
   The copying path raises above `Sys.max_array_length` elements, and the
   verifier knows no upper bound on an owner's length (it knows `0 <=
   extent` only). Workaround: `Borrow.max_length ()` (`%max_wosize`), a
   bound `n <= max_length ()` assumed on the `Raw.owned_length` external,
   and a precondition on `append`. Suggested fix: a built-in extent bound,
   like the iarray length bound in `vox_vc.ml`, or a join that only accepts
   adjacent pieces of one array, tracked by a ghost origin and offset.

2. **Dropping `total` also drops `stateless`.** `total` implies `stateless`
   in the surface syntax, so replacing `@@ total` with nothing would make the
   loan operations stateful. They were written `@@ stateless` to keep
   everything else unchanged. Easy to miss in review.

3. **Accepted controls in interface-only expect tests.** An accepted
   definition that mentions a module whose implementation is not loaded is
   type-checked and verified, then fails at run time with "Reference to
   undefined compilation unit". Workaround: check it inside
   `module type T = module type of struct ... end`, which is not run.

4. **Empty expected blocks with `-principal`.** Promoting a new phrase whose
   expected block was empty wrote the output into a separate `Principal{|...|}`
   block and left the main block empty; the main block had to be filled by
   hand.

5. **Unannotated functions are never total.** A top-level function without
   `@ total` gets no call congruence, even when its body is total. Tests of
   congruence must annotate the control explicitly.

6. **`[@@decreases]` is checked on partial functions.** The borrowing
   `sort_sized` keeps a checked measure although it is not total. This is
   useful, but nothing in its type says that the recursion is well founded;
   a termination claim for borrowing code needs the finer effect modes
   listed as future work in the readiness checklist.
