title: Doubly linked rings
blurb: Circular doubly linked lists with a sentinel, built from mutable cells: reversal of a ring of any length and moving a range of nodes between two rings, each proved to give the stated rings and the stated final heap.
status: reviewed
date: 27 September 2026
sources:
  - testsuite/tests/vox/pref_ring_general.mli — Reversal of any ring, and the `Owned` interface for one ring
  - testsuite/tests/vox/pref_ring_general.ml — Reversal: implementation and proofs
  - testsuite/tests/vox/pref_ring_splice_general.mli — Moving a range between two rings, and the `Owned` interface for a pair
  - testsuite/tests/vox/pref_ring_splice_general.ml — Range move: implementation and proofs
  - testsuite/tests/vox/pref_ring.mli — Nodes, the ring predicates and the raw operations
  - testsuite/tests/vox/pref_ring.ml — Raw operations: implementation
  - verification/library/pref.mli — Mutable cells, typed heaps and ownership tokens
  - testsuite/tests/vox/pref_ring_general_client.ml — Client of the reversal interface
  - testsuite/tests/vox/pref_ring_splice_general_client.ml — Client of the range-move interface
  - testsuite/tests/vox/pref_ring_general_demo.ml — Builds a three-node ring and reverses it
  - testsuite/tests/vox/pref_ring_splice_general_demo.ml — Moves ranges between two concrete rings
  - testsuite/tests/vox/pref_ring_general_rejected.ml — Rejected programs
---
A ring is a circular doubly linked list with a sentinel node. Each node record holds an `int` value, a sentinel flag and two mutable cells, `prev` and `next`, of type `node option Pref.t`. A ring is described by an erased model: its sentinel and the list of its other nodes in forward order. `ring h s ns` states that in the heap `h` the cells of `s` and `ns` link them into that cycle in both directions, and `separated` states that no cell belongs to two nodes and no node's `prev` and `next` are the same cell. Nodes are compared as values: two node records are equal when they have the same value, flag and cells. Two operations are proved for rings of any length, on normal return:

- `Pref_ring_general.reverse` reverses a separated ring. The result is a separated ring whose model is `reversed ns`, and the owned heap is exactly the old one with each node's `prev` and `next` contents exchanged (`flipped_all`), so every other cell is unchanged.
- `Pref_ring_splice_general.splice` removes a nonempty contiguous range `first :: rest` from one ring and inserts it after `destination_left` in another, where the two rings together are separated. Both new models are stated, both are separated rings, and the owned heap is exactly the old one after six link writes (`spliced`).

Each has an `Owned` interface that hides the ring invariant: `Owned.reverse` for one ring, and `Owned.splice` and `Owned.swap` for a pair of rings. `Pref_ring` also exports raw operations (`connect`, `insert_between`, `remove`, `splice_range`, `make_node`, `traverse`, `detach`, `reverse_nodes`) whose contracts state heap updates, the nodes traversed or a split of ownership, not that the result is a ring.

There is no ring-level insertion or removal. Through the `Owned` interfaces a client can create only an empty ring, and no pair of rings; the tests build nonempty rings with the raw operations and fixed-size helpers for rings of two and three nodes, prove the ring predicates for them by unfolding definitions, and then `adopt` them. Nothing is proved about termination, since every operation that reads or writes a cell is partial. Reversal first copies the ring's nodes into a list, so it uses space linear in the ring's length.

## Client example

From the client of the reversal interface. `{v : t | p}` is the type `t` refined by the predicate `p`, and `===` is logical equality. `@ unique` means the caller passes its only reference. `built` is the raw record of a sentinel, an erased node list and an erased token; `Pref.own t` is the heap owned by token `t`. `adopt` wraps a record that satisfies the ring invariant into an `Owned.t`, and `release` unwraps it. `borrow_ x` lends `x` for a read without consuming it, and `ghost_ (...)` is proof code, checked and then erased. The result type is checked: the `Owned` contracts are enough to recover the raw postcondition of `reverse`.

@code testsuite/tests/vox/pref_ring_general_client.ml "let reverse_owned" "  G.Owned.release owned"

## A rejected program

Reversal requires the ring to be separated. Without that premise the call is rejected:

@code testsuite/tests/vox/pref_ring_general_rejected.ml "module Missing_separation = struct" "end;;"

Compiled as a file after `open Pref_ring` and `open Pref_ring_general`, the installed compiler prints:

```
File "missing_separation.ml", line 7, characters 24-25:
7 |     reverse sentinel ns t
                            ^
Error: Refinement could not be proved (counterexample)
File "pref_ring_general.mli", lines 14-15, characters 39-32:
  The refinement is stated here.
```

The same test rejects a range move whose `final` is not the last node of the range, one whose `destination_left` is not the node before the insertion point, reuse of a consumed `Owned.t` or released raw record, and uses of private proofs.

## Interface

@code testsuite/tests/vox/pref_ring_general.mli

`@ ghost` marks an erased value, and `@@ total` a function declared total. Each model function `f` outside `Owned` comes with an equation `f_def` that gives its definition; the `Owned` observers keep theirs private. `H` is `Pref.Heap`; `H.at h p` is `Some v` when `h` maps cell `p` to `v`. `Owned.t` is an unboxed value holding the sentinel, the erased node list and the erased token. For a pair of rings:

@code testsuite/tests/vox/pref_ring_splice_general.mli "val splice : (s : node)" "heap next === heap state} @ unique"

`head ns s` is the first node of `ns`, or `s` if `ns` is empty, and `last x ns` is the last node of `x :: ns`. `spliced` is three applications of `connected h l r`, which sets `l.next` to `r` and `r.prev` to `l`, and `flipped_all h ns` exchanges the contents of the `prev` and `next` cells of every node in `ns`. The ring predicates, from `pref_ring.mli`:

@code testsuite/tests/vox/pref_ring.mli "val present :" "(not (n.prev === n.next)))))} @@ total"

@code testsuite/tests/vox/pref_ring.mli "val linked :" "separated rest))} @@ total"

## Trusted base

- Nothing beyond the shared base. The ring proofs use none of the heap laws stated in `pref.mli`; they rely on the checker's encoding of heap operations and on the contracts of the `Pref` operations.

## Scope

- Proved for any length: `reverse` of one ring and `splice` of a nonempty range between two separated rings, with `Owned` versions and `Owned.swap`, which exchanges the two rings of a pair. Moving a range within one ring is not covered.
- No ring-level insertion, removal, search or length for rings of any length. The raw `insert_between` and `remove` specify the link writes only, so a caller must prove by hand that the result is a ring.
- The fixed-size demos `Pref_ring_reverse.reverse_demo` (three nodes) and `Pref_ring_splice.splice_demo` (two nodes) and their setup modules predate the general operations. `Pref_ring_insert_remove.insert_remove_demo` inserts two nodes into an empty ring and removes one, proving the ring predicate after each step.
- All operations that touch cells are partial. Two helpers that read a link (`Pref_ring.read_link`, and `read_node` in `pref_ring_splice_general.ml`) call `failwith` in a branch the contracts rule out.
- `reverse` and `Owned.observe` traverse the ring with a recursion that is not tail-recursive and build a list of its nodes.
- A token owns cells of one payload type; an unrelated cell of another type needs its own token.
- Under `-principal`, the ring interfaces fail to type-check with a kind error on `node option`; the rejection tests' `-principal` expectations record the same error in client code.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/pref_ring_general_client.ml vox/pref_ring_splice_general_client.ml vox/pref_ring_general_demo.ml vox/pref_ring_splice_general_demo.ml vox/pref_ring_insert_remove_demo.ml vox/pref_ring_general_rejected.ml
```

The client tests check `Pref_ring`, `Pref_ring_general` and `Pref_ring_splice_general` while compiling them and run as bytecode and native code. At run time the reversal client reverses only an empty ring and the range-move client calls nothing; the demos, which run as bytecode, execute the operations on nonempty rings.
