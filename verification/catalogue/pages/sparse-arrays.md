title: Sparse array overlays
blurb: A persistent array made of an immutable base and a list of overriding writes, with laws that give the value read at every index after any sequence of writes and clears.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/sparse_overlay.mli — Public interface
  - testsuite/tests/vox/sparse_overlay.ml — Implementation: base array, list of overrides, proofs of the laws
  - verification/library/vox_iarray.mli — `Vox_iarray.at`, the optional array read used by the laws
  - testsuite/tests/vox/sparse_overlay_client.ml — Client: read after write, last write wins, independent writes commute, clearing restores the base
  - testsuite/tests/vox/collections_boundary.ml — Public-only compile of the collection clients, rejected clients and erasure checks
---
`Sparse_overlay` is a persistent overlay on an immutable base array. `empty base` has no overrides, `set i v a` overrides index `i`, `clear i a` removes the override at `i`, and `lookup` and `get` read. For elements of any `immutable_data` type, the laws in `Laws` give the result of `lookup` at every integer index. After `set i v a`, index `q` reads `Some v` if `q = i` and `q` is in bounds, and otherwise what it read in `a`. After `clear i a`, index `i` reads the base value `Vox_iarray.at (base a) i`, and every other index reads what it read in `a`. Outside `0 <= q < length a`, `lookup` returns `None`; inside, `get` returns the value that `lookup` wraps in `Some`. `length` is the length of the base, which `set` and `clear` keep.

The laws, and the observer `base`, are available only for `immutable_data` elements. The other operations accept other element types, including records with mutable fields, but for those the interface does not say what `get` or `lookup` returns. Reads search the list of overrides, so they take time linear in its length in the worst case; `set` and `clear` always rebuild the whole list without the old entry for the index. `set` does not check its index, so a write out of bounds is stored until cleared or overwritten, is never read, and lengthens the list that later operations traverse. No running-time bound is stated.

## Interface

`Laws` gives the defining equations: first the read observations, then
`empty`, `set` and `clear`. They use only the abstract overlay, its immutable
base and indexed lookup; the override representation stays private.

@code testsuite/tests/vox/sparse_overlay.mli

`value mod separable` and `immutable_data` are OxCaml kinds; the first admits records with mutable fields, the second does not. `@ total` and `@@ total` are the totality annotations described on the shared page. `Vox_iarray.at values i` is `Some` of the element at `i` when `0 <= i < Iarray.length values`, and `None` otherwise; `verification/library/vox_iarray.mli` states this with two laws, `at_get` and `at_outside`, which are proved.

## Trusted base

- The checker's built-in meaning of `Iarray.length` (`%array_length`) and of the bounds-checked read `Iarray.Refined.get` (`%array_safe_get`), which the implementation uses to read the base array.

## Scope

- Operations: `empty`, `base`, `length`, `lookup`, `get`, `set` and `clear`. There is no resizing, iteration, conversion back to an array, or removal of all overrides at once.
- The length is the length of the base array and never changes.
- `get` requires `0 <= i < length a`; `lookup` accepts any index and returns `None` outside the bounds, including after a write out of bounds.
- The laws cover `immutable_data` elements only. For other element types, such as the mutable `cell` in the client, only the types of the operations are checked.
- Reads and updates traverse the list of overrides, and out-of-bounds writes are kept (see above).
- `sparse_iarrays.ml`, in the same test directory, is an older and separate fixture built on `Map.MakeTotal`. It does not implement this interface.
- With `-principal`, `sparse_overlay.ml` does not type-check: in the statement of `find_remove`, an immutable list is passed to `remove`, and the type checker reports that a writable value is expected. The test compiles without `-principal`.

## Client example

From the client, inside a functor over any `Element : sig type t : immutable_data end`, with `S = Sparse_overlay` and `L = S.Laws (Element)`. `@ total` marks a function that terminates without raising or touching mutable state. `(x : a) -> b` names the argument so that `b` can mention it. `{i : int | p}` is `int` refined by the predicate `p`, and `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased; here it calls laws, which are lemmas returning `unit` refined by their conclusion. `get` needs an index proved in bounds for the overlay it reads, so the index is given the type `{i : int | 0 <= i && i < S.length result}` after the laws show that `result` has the same length as `source`.

@code testsuite/tests/vox/sparse_overlay_client.ml "let (read_after_write @ total)" "    actual"

The same functor proves, as equality of `lookup` at every index, that the last of two writes to an index wins, that writes to different indices commute, and that clearing an index restores its base value. The client instantiates it at `int` and at a record type.

## A rejected program

Reading index 0 of an overlay built on the empty array is a type error. `[: :]` is the empty immutable array. The client calls two laws, from `L = Sparse_overlay.Laws (struct type t = int end)`, so that the checker knows the overlay has length 0, then calls `get`. It is checked against the public interfaces only.

@code testsuite/tests/vox/collections_boundary.ml "(* sparse_bounds *)" "|}]"

## Reproduce

```
./dev test vox/sparse_overlay_client.ml vox/collections_boundary.ml
```

The test compiles `Vox_sequence`, `Vox_int_sequence`, `Vox_iarray`, the overlay and the client, and runs the client as bytecode and native code. `collections_boundary.ml` compiles the client against the public interfaces only, links and runs it with both compilers, requires the rejected programs (this one among them) to fail with their exact errors, and checks in the emitted Lambda that the bodies of `empty`, `set`, `clear`, `lookup` and `get` call no `_def` equation and mention neither `find_remove` nor `Vox_iarray`. The check is textual and does not follow the helpers `get_raw` and `length_known` that `lookup` and `get` call; by inspection, they call no proof code either.
