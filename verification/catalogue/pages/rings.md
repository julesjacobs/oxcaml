title: Doubly linked rings
blurb: Circular doubly linked lists with a sentinel, built from mutable cells: insertion, removal and reversal in a ring of any length and moving a range of nodes between two rings, each proved to give the stated rings and the stated final heap.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/pref_ring_general.mli — Insertion, removal and reversal for rings of any length, and the `Owned` interface for one ring
  - testsuite/tests/vox/pref_ring_general.ml — Insertion, removal and reversal: implementation and proofs
  - testsuite/tests/vox/pref_ring_splice_general.mli — Moving a range between two rings, and the `Owned` interface for a pair
  - testsuite/tests/vox/pref_ring_splice_general.ml — Range move: implementation and proofs
  - testsuite/tests/vox/pref_ring.mli — Nodes, the ring predicates and the raw operations
  - testsuite/tests/vox/pref_ring.ml — Raw operations: implementation
  - testsuite/tests/vox/pref_ring_proofs.ml — Lemmas about single heap writes, used by the proofs above
  - verification/library/pref.mli — Mutable cells, typed heaps and ownership tokens
  - testsuite/tests/vox/pref_ring_general_client.ml — Client of the one-ring interface: builds a ring with `insert`, removes nodes and reverses it
  - testsuite/tests/vox/pref_ring_splice_general_client.ml — Client of the range-move interface
  - testsuite/tests/vox/pref_ring_general_demo.ml — Builds a three-node ring and reverses it
  - testsuite/tests/vox/pref_ring_splice_general_demo.ml — Moves ranges between two concrete rings
  - testsuite/tests/vox/pref_ring_general_rejected.ml — Rejected programs
---
A ring is a circular doubly linked list with a sentinel. Its observable
model is the sentinel and a list of other nodes in forward order. The
`Owned` interfaces package the mutable links and their ownership:

- `insert` puts a new node between a given prefix and suffix.
- `remove` removes the named node between a prefix and suffix.
- `reverse` reverses the node list.
- `splice` moves a nonempty contiguous range from one ring into another.

The contracts give the resulting node lists, retain the sentinels, and
state the exact link updates, preserving every other cell. They apply on
normal return. The raw interfaces expose the ring invariant for clients
that manage ownership explicitly. One owned ring can be built from
`Owned.empty`; the pair interface is entered by adopting two raw rings.
Reversal first copies the nodes into a list, using space linear in the
ring's length. Termination and running time are unspecified.

## Interface

A ring's observable model is its sentinel and a list of nodes in forward
order. `append` concatenates lists, `reversed` reverses them, and
`last sentinel prefix` gives the insertion point before a suffix, using
the sentinel for an empty prefix. Their definitions are short:

@code testsuite/tests/vox/pref_ring_general.mli "(** {1 Model definitions} *)" "(** {1 Operations with explicit ownership} *)"

Read the one-ring `Owned` interface next. It packages the sentinel,
node list and ownership token. Insertion changes `prefix @ suffix` into
`prefix @ node :: suffix`; removal makes the reverse change; reversal
reverses the node list. Here `@` denotes list concatenation. The contracts
also retain the sentinel and state the exact link updates:

@code testsuite/tests/vox/pref_ring_general.mli "module Owned : sig" "  val heap :"

@code testsuite/tests/vox/pref_ring_general.mli "  val empty :" "end (* Owned *)"

The pair interface uses the same list model. Splicing removes
`first :: rest` from the source and inserts it between the destination's
prefix and suffix. `swap` exchanges the two rings' roles.

@code testsuite/tests/vox/pref_ring_splice_general.mli "  val splice :" "end"

The heap equations are supporting guarantees for clients that manage
other cells or cross into the raw interface. A node retains its value,
sentinel flag and `prev`/`next` cell identities.

@code testsuite/tests/vox/pref_ring.mli "type node =" "}"

`ring h sentinel ns` describes those nodes linked into a cycle in both
directions; `separated (sentinel :: ns)` requires distinct link cells.
The complete [ring interface](src:testsuite/tests/vox/pref_ring.mli) gives
their recursive definitions (`head`, `present`, `linked`, `apart` and
`apart_all`). `H` is `Pref.Heap`; `H.at h p` observes cell `p` in heap `h`.

Each update is expressed by a small pure heap transformation. Insertion
connects the new node on both sides, removal reconnects its neighbours and
links the removed node to itself, and reversal exchanges the links of each
node. These definitions also preserve every other cell:

@code testsuite/tests/vox/pref_ring.mli "val connected :" "    ghost_ (connected (connected h left right) n n)} @@ total"

The [remaining heap definitions](src:testsuite/tests/vox/pref_ring.mli)
give `flipped_all`; the [splice interface](src:testsuite/tests/vox/pref_ring_splice_general.mli)
defines `spliced` as three connections, or six cell writes.
The raw contracts explicitly require and return the ring invariant.
`adopt` and `release` preserve the model and heap when crossing between raw
records and `Owned.t`. `@ ghost` marks erased values and `@ unique`
consumes an ownership handle. `Pref_ring_general.Proofs` is support for the
range-move proof and can be skipped when reading the operation contracts.

## Trusted base

- Nothing beyond the shared base. The proof of `insert` uses the heap laws `put_law` and `commute_law` from `pref.mli`, to show that the cells `make_node` allocates and `insert` then overwrites leave exactly the heap `inserted` states. The other ring proofs use no heap law; they rely on the checker's encoding of heap operations and on the contracts of the `Pref` operations.

## Scope

- Proved for any length: `insert`, `remove` and `reverse` in one ring and `splice` of a nonempty range between two separated rings, with `Owned` versions and `Owned.swap`, which exchanges the two rings of a pair. Moving a range within one ring is not covered.
- A caller names the insertion point or the removed node and splits the model around it; there is no search, indexing or length. `remove` keeps the removed node's cells in the token, linked to itself.
- The fixed-size demos `Pref_ring_reverse.reverse_demo` (three nodes) and `Pref_ring_splice.splice_demo` (two nodes) and their setup modules predate the general operations. `Pref_ring_insert_remove.insert_remove_demo` inserts two nodes into an empty ring and removes one, proving the ring predicate after each step.
- All operations that touch cells are partial. The helpers that read a link (`Pref_ring.read_link`, and `read_node` in `pref_ring_general.ml` and `pref_ring_splice_general.ml`) end in `unreachable_ ()` for an empty link, a branch the checker proves dead from their preconditions.
- `reverse` and `Owned.observe` traverse the ring with a recursion that is not tail-recursive and build a list of its nodes.
- A token owns cells of one payload type; an unrelated cell of another type needs its own token.
- Under `-principal`, the ring interfaces fail to type-check with a kind error on `node option`; the rejection tests' `-principal` expectations record the same error in client code.

## Client example

From the client of the reversal interface. `{v : t | p}` is the type `t` refined by the predicate `p`, and `===` is logical equality. `@ unique` means the caller passes its only reference. `built` is the raw record of a sentinel, an erased node list and an erased token; `Pref.own t` is the heap owned by token `t`. `adopt` wraps a record that satisfies the ring invariant into an `Owned.t`, and `release` unwraps it. `borrow_ x` lends `x` for a read without consuming it, and `ghost_ (...)` is proof code, checked and then erased. The result type is checked: the `Owned` contracts are enough to recover the raw postcondition of `reverse`.

@code testsuite/tests/vox/pref_ring_general_client.ml "let reverse_owned" "  G.Owned.release owned"

Insertion and removal at the front, from the same client. `#{node; ring}` is an unboxed record of the new node and the new ring. Each function states its result's model. The test's main program then builds the ring 1, 2, 3 with `push_front`, removes the middle node, inserts 4 after the last one, reverses the ring and empties it with `pop_front`, proving the model after each step:

@code testsuite/tests/vox/pref_ring_general_client.ml "let push_front" "    G.Owned.remove [] n rest state"

## A rejected program

Reversal requires the ring to be separated. Without that premise the call is rejected:

@code testsuite/tests/vox/pref_ring_general_rejected.ml "module Missing_separation = struct" "end;;"

Compiled as a file after `open Pref_ring` and `open Pref_ring_general`, the installed compiler prints:

```
File "missing_separation.ml", line 7, characters 24-25:
7 |     reverse sentinel ns t
                            ^
Error: Refinement could not be proved (counterexample)
File "pref_ring_general.mli", line 24, characters 6-32:
  The refinement is stated here.
```

The same test rejects a range move whose `final` is not the last node of the range, one whose `destination_left` is not the node before the insertion point, reuse of a consumed `Owned.t` or released raw record, and uses of private proofs. For insertion and removal it rejects an insertion whose `left` is not the node before the insertion point, the claim that insertion always puts the node at the front, removal of a node that is not in the model, and the claim that removal leaves the model unchanged.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/pref_ring_general_client.ml vox/pref_ring_splice_general_client.ml vox/pref_ring_general_demo.ml vox/pref_ring_splice_general_demo.ml vox/pref_ring_insert_remove_demo.ml vox/pref_ring_general_rejected.ml vox/structures_erasure.ml
```

The client tests check `Pref_ring`, `Pref_ring_general` and `Pref_ring_splice_general` while compiling them and run as bytecode and native code. At run time the one-ring client builds, edits, reverses and empties a ring of up to three nodes and checks the observed values; the range-move client calls nothing. The demos run as bytecode: `pref_ring_general_demo.ml` reverses a three-node ring, `pref_ring_insert_remove_demo.ml` inserts two nodes and removes one, and `pref_ring_splice_general_demo.ml` moves single nodes and then a whole range between two rings, emptying one ring and filling an empty one. `structures_erasure.ml` checks in the native `-dlambda` output that the ring operations (`insert`, `remove`, `reverse`, `splice`, `adopt`, `release`, `swap`, `observe`, the observers of a pair, `sentinel_node`, the raw operations and the fixed-size demos' entry points), and every function of the same module that they call by name, call only functions on a fixed list of runtime functions (so no model function, lemma, `_def` equation or heap law) and refer to no model module. Calls into other modules are not followed.
