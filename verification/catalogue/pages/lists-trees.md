title: Mutable lists and trees
blurb: In-place reversal of a linked list and mirroring of a binary tree built from mutable cells, proved against an erased model of the nodes and of the cells they own.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/pref_list.mli — List: public interface
  - testsuite/tests/vox/pref_list.ml — List: implementation and proofs
  - testsuite/tests/vox/pref_tree.mli — Tree: public interface
  - testsuite/tests/vox/pref_tree.ml — Tree: implementation and proofs
  - verification/library/pref.mli — Mutable cells, typed heaps and ownership tokens
  - testsuite/tests/vox/pref_owned_client.ml — Client of the `Owned` interfaces
  - testsuite/tests/vox/pref_list_client.ml — Client: reversal with a frame, node identities checked at run time
  - testsuite/tests/vox/pref_tree_client.ml — Client: mirroring with a frame
  - testsuite/tests/vox/pref_list_rejected.ml — Rejected list programs
  - testsuite/tests/vox/pref_tree_rejected.ml — Rejected tree programs
---
`Pref_list` is a singly linked list and `Pref_tree` a binary tree whose links are mutable cells of type `node option Pref.t`. `Pref_list.reverse` reverses a list in place and `Pref_tree.mirror_with_frame` swaps the children of every tree node in place. Each structure is described by an erased model: a list or tree of the node records themselves. A node record holds its value and the identities of its link cells, so two nodes are logically equal exactly when they have the same value and the same cells. On normal return:

- `reverse` returns the root of `rev_append xs Nil`, where `xs` is the input model, and a token that owns exactly the link cells of the reversed model, with their new contents, plus an unchanged frame of other cells. The reversed model's nodes are logically equal to the input's (same values, same link cells); that the node records are reused rather than copied is checked only at run time, by the list client.
- `mirror_with_frame` returns a token that owns exactly the cells of `flipped model`, the tree with every node's children exchanged, plus the unchanged frame.
- The constructors and observers are specified exactly: `empty`, `cons` and `branch` give the new model, `of_list` the values it holds, and `observe_read` returns the model's node list or the tree's shape.

Each module also has an `Owned` interface. An `Owned.t` is an unboxed value holding the root pointer, the erased model and the erased token, with the representation invariant hidden. `Owned.reverse` gives a list whose model is `rev_append (model state) Nil`, and `Owned.mirror` a tree whose model is `flipped (model state)`.

Termination of the operations is not proved: every operation that reads or writes a cell is partial. (The model functions are `total`.) The interface also leaves out some lemmas a client would want. `of_list` is specified through `contents` (the values) but `observe` through `nodes` (the node records), and no exported lemma relates the two or says that the values of a reversed list are `List.rev` of the original values; the clients check such facts with run-time `assert`s. For trees, `leaf` fixes only the shape of the new model, and `Pref_tree.observe` consumes the tree's token, so the tree cannot be used after it.

## Client example

From the client of the `Owned` interfaces. `borrow_ x` lends `x` for a read without consuming it, and `ghost_ (...)` is proof code, checked and then erased; here it takes a snapshot of the model. `{v : t | p}` is the type `t` refined by the predicate `p`, and `===` is logical equality: the annotations on `reversed`, `mirrored` and the two `raw` values are checked. The `assert`s are run-time tests, not proofs.

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
File "no_reversal.ml", lines 7-8, characters 20-56:
7 | ....................r.pointer === root (rev_append xs Nil)
8 |         && Pref.own r.state === heap (rev_append xs Nil)............
  The refinement is stated here.
```

The rejection tests also reject a list function that loses a node, a tree function that claims to mirror without doing so, a tree whose two subtrees share a node, uses of private helpers, and reuse of a consumed `Owned.t` or of a released raw record.

## Interface

`[@@inductive]` marks a model type that the checker may reason about by induction. Each model function `f` outside `Owned` comes with an equation `f_def` that gives its definition; `Owned.model` keeps its definition private. `Pref.own t` is the heap owned by token `t`, a finite map from cells to contents, and `H` is `Pref.Heap`. `valid` states that the link cells of different nodes are distinct. `@ unique` means the caller passes its only reference, `@ ghost` marks an erased value, and `@@ total` a function declared total.

@code testsuite/tests/vox/pref_list.mli

The tree interface defines `root`, `links`, `heap` and `valid` in the same way (`valid` also requires each node's two cells to differ), then:

@code testsuite/tests/vox/pref_tree.mli "val flipped :" "@ unique -> {result : shape | result === shape_of model}"

@code testsuite/tests/vox/pref_tree.mli "module Owned : sig" "end"

## Trusted base

- The cell operations `Pref.empty`, `alloc`, `read`, `write`, `split` and `join` are `external`s whose contracts are assumed, like the `Ghost_pref` operations on the shared page.
- The list proofs use the stated heap laws `partition_law`, `put_law`, `put_union_law` and `union_law`; the tree proofs also use `commute_law`, `domain_law` and `union_domain_law`.

## Scope

- List: `reverse` (with a frame), `empty`, `cons`, `of_list`, `observe_read` and `observe`; `Owned` has `empty`, `of_list`, `reverse`, `observe`, `adopt` and `release`. Tree: `mirror_with_frame`, `empty`, `leaf`, `branch`, `observe_read` and `observe`; `Owned` has `empty`, `leaf`, `branch`, `mirror`, `observe`, `adopt` and `release`. There is no insertion, deletion, search or length.
- Values are `int`.
- All operations that touch cells are partial; contracts describe normal return.
- A token owns cells of one payload type. The frame passed through `reverse` or `mirror_with_frame` must consist of `node option` cells; a cell of another type needs its own token, as in the clients.
- `Pref_tree.observe` consumes its token. `Pref_tree.observe_read` and both `Owned.observe` functions only borrow it.
- Under `-principal`, neither interface type-checks, nor does client code that passes `node option` cells or tokens to `Pref` functions: the compiler cannot show that `node option` has kind `immutable_data`. The rejection tests record this error for their `-principal` variant.
- The list client reverses lists of up to 1,000 nodes and checks node identities with `==` at run time.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/pref_list_client.ml vox/pref_tree_client.ml vox/pref_owned_client.ml vox/pref_list_rejected.ml vox/pref_tree_rejected.ml vox/structures_erasure.ml
```

The client tests check `Pref_list` and `Pref_tree` while compiling them and run as bytecode and native code. `structures_erasure.ml` checks in the native `-dlambda` output that `reverse`, `mirror` and the observers call no model or proof function, and that the observers neither split nor join tokens.
