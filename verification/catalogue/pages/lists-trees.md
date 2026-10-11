title: Mutable lists and trees
blurb: In-place reversal of a linked list and mirroring of a binary tree built from mutable cells, proved against an erased model of the nodes and of the cells they own.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/pref_list.mli — List: public interface
  - testsuite/tests/vox/pref_list.ml — List: implementation and proofs
  - testsuite/tests/vox/pref_tree.mli — Tree: public interface
  - testsuite/tests/vox/pref_tree.ml — Tree: implementation and proofs
  - verification/library/pref.mli — Mutable cells, typed heaps and ownership tokens
  - testsuite/tests/vox/pref_owned_client.ml — Client of the `Owned` interfaces
  - testsuite/tests/vox/pref_list_client.ml — Client: reversal with a frame, reversed values proved, node identities checked at run time
  - testsuite/tests/vox/pref_tree_client.ml — Client: mirroring with a frame
  - testsuite/tests/vox/pref_list_rejected.ml — Rejected list programs
  - testsuite/tests/vox/pref_tree_rejected.ml — Rejected tree programs
  - testsuite/tests/vox/pref_tree_observe_rejected.ml — Rejected claim about the token `Pref_tree.observe` returns
---
`Pref_list` reverses a mutable linked list in place; `Pref_tree` mirrors a
mutable binary tree by exchanging left and right at every node. The
observable results are familiar pure values: the list's values in reverse
order, and the tree's mirrored values and shape.

The `Owned` interfaces package the links and their ownership. Their
contracts state those observable changes directly and retain the node
records and link-cell identities. Raw operations additionally accept a
frame of other cells and promise to preserve it. Operations that touch
cells are partial; these guarantees describe normal return.

## Interface

Start with the observations: list values in order, and a tree's values and
shape. List reversal is ordinary reversal with an accumulator; tree
mirroring exchanges left and right recursively.

@code testsuite/tests/vox/pref_list.mli "type node =" "[@@inductive]"

@code testsuite/tests/vox/pref_list.mli "val contents :" "  @@ total"

@code testsuite/tests/vox/pref_list.mli "val rev_onto :" "    (match xs with [] -> ys | x :: rest -> rev_onto rest (x :: ys))} @@ total"

@code testsuite/tests/vox/pref_tree.mli "type shape =" "[@@inductive]"

@code testsuite/tests/vox/pref_tree.mli "val shape_of :" "| Branch (n, l, r) -> Fork (n.value, shape_of l, shape_of r))} @@ total"

@code testsuite/tests/vox/pref_tree.mli "val mirror_shape :" "  @@ total"

The `Owned` operations package the root, model and ownership token.
`reverse` directly promises reversed values, and `mirror` directly promises
the mirrored shape. Their model equations additionally preserve the node
records: `rev_append` reverses the linked-list model and `flipped` mirrors
the tree model. The complete interfaces define both recursive functions.
The main operations need no heap premises from a caller:

@code testsuite/tests/vox/pref_list.mli "module Owned : sig" "    {result : node list | result === nodes (model state)}"

@code testsuite/tests/vox/pref_tree.mli "module Owned : sig" "    {result : shape | result === shape_of (model state)}"

`observe` borrows the owned structure to read its nodes or shape.
`@ unique` consumes the caller's handle, `@ ghost` marks erased values,
and `@@ total` declares total functions. `model` retains node records and
link-cell identities, so the same handle also supports clients that need
to reason about nodes.

For clients managing an unrelated heap frame, the raw operations expose
explicit ownership. `heap model` describes the model's link cells, and
`valid model` requires their separation; their recursive definitions are
in the complete [list interface](src:testsuite/tests/vox/pref_list.mli) and
[tree interface](src:testsuite/tests/vox/pref_tree.mli). These contracts
preserve every cell in `frame` while updating the structure:

@code testsuite/tests/vox/pref_list.mli "val reverse : (pointer :" "      && valid (rev_append xs Nil)} @ unique"

@code testsuite/tests/vox/pref_tree.mli "val mirror_with_frame :" "      @ unique"

`adopt` and `release` cross between explicit tokens and `Owned.t`, preserving
the model and its owned heap. The final observation laws connect model
reversal and mirroring to values and shapes. The complete interfaces link
these observations to the heap through `root`, `heap` and `valid`; this
ownership detail is needed when crossing that boundary.

## Trusted base

- Nothing beyond the shared base, which covers the `Pref` cell operations and the heap laws. The list proofs use the stated heap laws `partition_law`, `put_law`, `put_union_law` and `union_law`; the tree proofs also use `commute_law`, `domain_law` and `union_domain_law`.

## Scope

- List: `reverse` (with a frame), `empty`, `cons`, `of_list`, `observe_read` and `observe`; `Owned` has `empty`, `of_list`, `reverse`, `observe`, `adopt` and `release`. Tree: `mirror_with_frame`, `empty`, `leaf`, `branch`, `observe_read` and `observe`; `Owned` has `empty`, `leaf`, `branch`, `mirror`, `observe`, `adopt` and `release`. There is no insertion, deletion, search or length.
- Values are `int`.
- All operations that touch cells are partial; contracts describe normal return.
- List reversal is tail-recursive. The list observers and `of_list` are not: their stack depth grows linearly with the list's length. The tree operations recurse to the depth of the tree.
- A token owns cells of one payload type. The frame passed through `reverse` or `mirror_with_frame` must consist of `node option` cells; a cell of another type needs its own token, as in the clients.
- `observe` in both modules takes the token and returns it with the same heap; `observe_read` and both `Owned.observe` functions borrow it.
- Under `-principal`, neither interface type-checks, nor does client code that passes `node option` cells or tokens to `Pref` functions: the compiler cannot show that `node option` has kind `immutable_data`. The rejection tests record this error for their `-principal` variant.
- The list client reverses lists of up to 1,000 nodes, proves the reversed values and node records, and checks with `==` at run time that the node records are the original ones.

## Client example

From the client of the `Owned` interfaces. `borrow_ x` lends `x` for a read without consuming it, and `ghost_ (...)` is proof code, checked and then erased; here it takes a snapshot of the model and proves what the observers return. `{v : t | p}` is the type `t` refined by the predicate `p`, and `===` is logical equality: the annotations on `reversed`, `mirrored` and the two `raw` values are checked, and so are the facts about `actual`, from the exported lemmas and the definitions.

@code testsuite/tests/vox/pref_owned_client.ml "let () =" "  ()"

`release` turns an `Owned.t` back into the raw record `built` with the invariant stated, and `adopt` does the reverse.

## A rejected program

A function that returns the list unchanged cannot claim to have reversed it:

@code testsuite/tests/vox/pref_list_rejected.ml "module No_reversal = struct" "end;;"

Compiled as a file after `open Pref_list`, the installed compiler prints:

```
File "no_reversal.ml", line 11, characters 4-5:
11 |     r
         ^
Error: Refinement could not be proved (counterexample)
File "no_reversal.ml", line 7, characters 20-58:
7 |       {r : result | r.pointer === root (rev_append xs Nil)
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
```

The rejection tests also reject a list function that loses a node, a tree function that claims to mirror without doing so, a tree whose two subtrees share a node, uses of private helpers, and reuse of a consumed `Owned.t` or of a released raw record. Against the lemmas, they reject the claims that reversal leaves a list's values unchanged, that a list's nodes hold its values in reverse order, that mirroring leaves a tree's shape unchanged, and that the token `Pref_tree.observe` returns owns nothing.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/pref_list_client.ml vox/pref_tree_client.ml vox/pref_owned_client.ml vox/pref_list_rejected.ml vox/pref_tree_rejected.ml vox/pref_tree_observe_rejected.ml vox/structures_erasure.ml
```

The client tests check `Pref_list` and `Pref_tree` while compiling them and run as bytecode and native code. `structures_erasure.ml` checks in the native `-dlambda` output that `reverse`, `mirror`, `mirror_with_frame` and the observers, and every function of the same module that they call by name, call only functions on a fixed list of runtime functions (so no model function, lemma, `_def` equation or heap law) and refer to no model module, and that the observers neither split nor join tokens. Calls into other modules are not followed.
