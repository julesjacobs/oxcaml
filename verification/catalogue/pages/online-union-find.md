title: Online union–find
blurb: A pure partition model specifies online union–find; imperative operations implement its updates, and exact charged work stays within inverse-Ackermann amortized fees.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_partition_classes.ml — Public class-list snapshots and relational definitions
  - verification/library/vox_partition_classes_proof.ml — Derived pure laws
  - verification/library/vox_partition_classes_group.ml — Private conversion from bindings to classes
  - verification/library/vox_partition_classes_bridge.ml — Proof that classes and bindings have the same observations and transitions
  - verification/library/vox_partition_transport_proof.ml — Proof that binding relations respect observation equality
  - verification/library/vox_partition.ml — Private member/representative bindings
  - verification/library/vox_union_find_partition_proof.ml — Private projection from paths to the pure partition
  - verification/library/vox_connectivity.mli — Public interface
  - verification/library/vox_connectivity.ml — Implementation of the public interface over the online layer
  - verification/library/vox_union_find_online.mli — Online layer, with the forest exposed
  - verification/library/vox_union_find_online.ml — Ghost capacity doubling and saved credits
  - verification/library/vox_union_find.mli — Core structure: fees, event history, potential invariant
  - verification/library/vox_union_find.ml — Core operations; records events and charges
  - verification/library/vox_union_find_worker.ml — The run-time code and its hand-placed charges
  - verification/library/vox_union_find_events.mli — Event weights
  - verification/library/vox_union_find_bank.ml — The potential function
  - verification/library/vox_big_credits.mli — Ghost credit tokens
  - verification/library/vox_ackermann.ml — Clamped Ackermann function and its inverse
  - verification/library/vox_union_find_online_cost.ml — Summing fees over a sequence of operations
  - testsuite/tests/vox/connectivity.ml — Public client
  - testsuite/tests/vox/connectivity_rejected.ml — Rejected programs
---
A pure snapshot is a list of `(representative, others)` pairs with distinct members. `Vox_connectivity.Partition` exposes the snapshot definitions; `Vox_connectivity.Make` relates the mutable state to a snapshot through `model` and specifies each operation with a pure relation.

The relations leave class order and member order unconstrained. A union may choose any member of either merged class as representative. Other classes keep their representatives, and an already-connected union preserves every pure observation. Heap ownership, paths, ranks and proof helpers stay outside the pure spec.

## Interface

The complete pure snapshot definitions. For example, `[(a, [b; c]); (d, [e])]` has two classes represented by a and d. The refinement on `t` makes the classes disjoint. `same` compares observations, so list order is irrelevant:

@code verification/library/vox_partition_classes.ml

`model` gives the current pure snapshot. `create` establishes `empty`; `make_set` establishes `added`; `find` returns the current representative and establishes `same`; `union` establishes `joined`. Accounting observations live under `Cost`. `observation` names the shared type of ghost readers of borrowed state.

@code verification/library/vox_connectivity.mli

`t`'s layout is a product of `void`s: a state has no run-time representation. `@ unique` marks a value the call consumes, and `ghost` a value that is erased. The event type and weights:

@code verification/library/vox_union_find_events.mli

Credits (`Vox_big_credits.S`) are ghost tokens with a nonnegative count. `split` and `merge` conserve credits and `tick` removes one; only `Make ()`'s `Budget.create`, which the client calls, creates them.

@code verification/library/vox_big_credits.mli "module type S = sig" "end"

The cost definitions are grouped at the start of `Vox_ackermann`, before
its proofs and inverse computation. `iter cap k 1 1` is A_k(1), with every
value clamped at `cap`; `below cap a` holds when A_j(1) < `cap` for every
1 ≤ j < a:

@code verification/library/vox_ackermann.ml "let[@def] minimum" "if level > 0Z then level else 0Z]"

## Trusted base

- `Ghost_pref.alloc`, which allocates each node, is an external (`caml_pref_alloc_step`), although `ghost_pref.mli` declares it with `val`.
- `Pref.equal` is physical equality (`%eq`), trusted to agree with `===` on elements; `link` uses it to detect that both roots are the same node.
- The cost weights: which steps call `C.tick` in `vox_union_find_worker.ml`, and how many times, is a modelling choice that no check relates to the compiled code.

## Scope

- Operations: `create`, `make_set`, `find`, `union`. There is no run-time equality or connectivity test, deletion, iteration or listing of a class.
- `make_set` requires fewer than `max_int` elements. The ghost capacity doubles as elements are added, so there is no fixed population limit below that.
- Payments are exact: overpaying is rejected as well as underpaying. Credits are ghost and erased.
- `Cost.depth` is abstract beyond being nonnegative and 0 exactly at a root, so the interface fixes which events each operation records but not how many links a find follows.
- `Vox_union_find_online.create` checks `max_int >= 1` at run time and raises `Invalid_argument` otherwise, because `max_int` carries no refinement.
- Only normal return is specified. An exception such as `Out_of_memory` during allocation loses the state.
- The tests run in bytecode only.

## Implementation and cost proof

The private proof projects the forest to `(member, representative)` bindings, then groups those bindings into classes. The grouping proof preserves membership, representatives and size and produces distinct members. The bridge proves that the binding relations and class-list relations agree. Allocation, compression and linking therefore establish the public class-list contracts. Heap ownership, path validity and rank-based representative selection remain inside the implementation.

Each operation also takes a ghost credit token of fixed size: 1 for `create`, 11 for `make_set`, `Cost.find_fee s` for `find` and `Cost.union_fee s` for `union`. `Cost.account s` is the total paid, and the interface proves `Cost.ticks s <= Cost.account s`. It also proves `Cost.find_fee s <= 4a + 12` and `Cost.union_fee s <= 12a + 36` whenever the structure has at most N elements, where a is the least k ≥ 1 with A_k(1) ≥ N, for A_0(x) = x + 1 and A_k(x) = A_(k−1) applied x + 1 times to x. So a = 3 for 8 ≤ N ≤ 2,047 and a = 4 for any larger N the structure can hold.

`Cost.ticks` counts charges placed by hand in the run-time code (`vox_union_find_worker.ml`). A find that follows d parent links costs 4d + 2: two per node visited and two per pointer rewritten. An allocation costs 3 and a link 7. `vox_union_find.ml` adds 1 per union, which performs two finds and a link, and 1 for `create`. The implementation proves that the fixed fees always cover these charges. It keeps 4 credits in reserve per unit of a potential over node ranks, in the style of Tarjan's analysis (`vox_union_find_bank.ml`), and saves 8 of each `make_set`'s 11 credits to pay for the potential's increase when the ghost capacity doubles; `Cost.account` is `Cost.ticks` plus these reserves. Summed over m `make_set`, f `find` and u `union` calls on at most N elements, the charges are therefore at most 1 + 11m + (4a + 12)f + (12a + 36)u. The weights are a model; nothing checks them against what the compiled code executes.

The separate `Cost` submodule retains abstract forest snapshots and exact event accounting. Each operation's contract states the events it appends to `Cost.events`, newest first. `create` starts from `[Initialize]` and `make_set` appends `Allocate`. With p the cost snapshot before the call, `find x` appends `Find (Cost.depth p x)` and leaves the cost snapshot `Cost.compressed p x`; `union x y` appends `Find (Cost.depth p x)`, `Find (Cost.depth (Cost.compressed p x) y)`, `Link` and `Union`. `Cost.event_cost` equates ticks with the total event weight, and `Cost.account_bounds` bounds them by the credits paid. Compression can change a cost snapshot while preserving all pure partition observations.

`Cost.depth` is nonnegative, and `Cost.root_depth` says a member's depth is 0 exactly when its pure representative is itself. The implementation proves that depth counts the parent links the run-time loop follows; the cost interface keeps the forest abstract. `Vox_partition_classes_proof.representative_law` gives the separate pure fact that a member's representative is a member and its own representative. The interface has no run-time operation that compares two elements; `Partition.connected` is ghost. Only normal return is specified.

## Client example

From the public client. `funded` pairs a state with a ghost wallet of credits. The wrapper splits exactly `Cost.find_fee` credits off the wallet with `C.split` and pays them to `U.find`; the client's `fee_bounds` shows the fee is at most 44 while the structure has at most 8 elements. `{r : t | p}` is `t` restricted to values satisfying `p`, `===` is logical equality, `8Z` is a `Bigint` literal and `s.#state` reads a field of an unboxed record. `ghost_ (...)` is proof code, checked and then erased; the `_def` calls give the solver the definitions of the named functions. A result refinement that begins `let owned = owned in` refers to the argument `owned`. The wrapper's last conjunct, a cost of 4·depth + 2 ticks, follows from the event `U.find` appends: `Cost.event_cost` before and after the call, and the definitions of `total` and `weight`.

@code testsuite/tests/vox/connectivity.ml "let find : (x : U.elem) @ immutable ->" "let r = #{value; owned} in r"

After five insertions, four unions and two finds, of `x0` and of `x3`, the client calls `find` on `root0`, the root returned by the first find. This is allowed because the pure `representative_law` shows that `root0` is a member; the find returns `root0` and costs exactly 2 ticks, because `Cost.root_depth` gives a root depth 0. From the events the client then proves the run's exact cost. `work6` to `work9` are the links followed by each union's two finds and `work10` and `work11` those of the first two finds, named before each call; `U.Cost.depth_law` makes each nonnegative.

@code testsuite/tests/vox/connectivity.ml "let work = ghost_ (Bigint.add" "work >= 0Z} = () in"

It then proves:

@code testsuite/tests/vox/connectivity.ml "let proof : {u : unit | root0 === root3 &&" "Cost.ticks owned.#state <= initial} = () in"

`initial` is the 1,000 credits the client minted. Last, it sums the fees over the recorded accounts to prove that 70 + 4·`work` is at most 1 + 11·5 + 44·3 + 132·4 = 716, so all the finds together, including the two inside each union, follow at most 161 parent links. For these fees it bounds a by 8, using the conjunct `k <= capacity` of `Vox_ackermann.inverse`'s contract with N = 8. The rest of that contract determines a = 3, but the client does not unfold `iter` and `below` to show it.

## A rejected program

Paying 10 credits for `make_set`, which requires exactly 11, is a type error. The `[%%expect]` block holds the compiler's message as the test records it.

@code testsuite/tests/vox/connectivity_rejected.ml "module Underpay_insertion = struct" "Error: Refinement could not be proved"

The same test rejects overpayment, reuse of a consumed state, access to private implementation fields, unproved membership, insertion at the machine limit, an unproved claim that a model is empty, an unrecorded find, an unjustified claim that every find costs 2 ticks, and splitting 11 credits from a wallet of 10.

## Reproduce

```
./dev test vox/partition.ml vox/partition_classes.ml vox/connectivity.ml vox/connectivity_rejected.ml vox/union_find_online.ml vox/union_find_online_rejected.ml vox/union_find.ml vox/union_find_rejected.ml vox/ackermann.ml
```

`connectivity.ml` and `connectivity_rejected.ml` use the public interface; the other tests exercise the online and core layers and the Ackermann lemmas.
