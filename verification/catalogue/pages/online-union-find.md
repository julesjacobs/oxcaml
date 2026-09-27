title: Online union–find
blurb: Union by rank with path compression, proved to maintain a partition; each operation's contract states the steps it is charged for, and their total under a hand-placed cost model is proved to stay within inverse-Ackermann amortized fees.
status: owner-review
date: 27 September 2026
sources:
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
`Vox_connectivity.Make` is a union–find structure with union by rank and full path compression. An element is a pointer to a mutable node. The state `t` is ghost, so at run time `find` and `union` take only elements. The interface describes a state by an abstract snapshot with `contains` and `root`, and its laws give every element's membership and root after each operation. `make_set` adds a fresh element that is its own root and changes no other root. `find x` returns `root x` and changes no membership or root. `union x y` returns one of the two old roots, which becomes the root of every element of both classes; every other root is unchanged.

Each operation also takes a ghost credit token of fixed size: 1 for `create`, 11 for `make_set`, `find_fee s` for `find` and `union_fee s` for `union`. `account s` is the total paid, and the interface proves `ticks s <= account s`. It also proves `find_fee s <= 4a + 12` and `union_fee s <= 12a + 36` whenever the structure has at most N elements, where a is the least k ≥ 1 with A_k(1) ≥ N, for A_0(x) = x + 1 and A_k(x) = A_(k−1) applied x + 1 times to x. So a = 3 for 8 ≤ N ≤ 2,047 and a = 4 for any larger N the structure can hold.

`ticks` counts charges placed by hand in the run-time code (`vox_union_find_worker.ml`). A find that follows d parent links costs 4d + 2: two per node visited and two per pointer rewritten. An allocation costs 3 and a link 7. `vox_union_find.ml` adds 1 per union, which performs two finds and a link, and 1 for `create`. The implementation proves that the fixed fees always cover these charges. It keeps 4 credits in reserve per unit of a potential over node ranks, in the style of Tarjan's analysis (`vox_union_find_bank.ml`), and saves 8 of each `make_set`'s 11 credits to pay for the potential's increase when the ghost capacity doubles; `account` is `ticks` plus these reserves. Summed over m `make_set`, f `find` and u `union` calls on at most N elements, the charges are therefore at most 1 + 11m + (4a + 12)f + (12a + 36)u. The weights are a model; nothing checks them against what the compiled code executes.

Each operation's contract states the events it appends to `events`, newest first. `create` starts from `[Initialize]` and `make_set` appends `Allocate`. With p the snapshot before the call, `find x` appends `Find (depth p x)` and leaves the snapshot `compressed p x`, and `union x y` appends `Find (depth p x)`, `Find (depth (compressed p x) y)`, `Link` and `Union`. Since `event_cost` makes `ticks s` the total weight of `events s`, a client can compute the ticks of any sequence of operations from the interface alone, and `account_bounds` bounds them by the credits paid. `depth` is abstract: the interface says only that it is nonnegative and that a member's depth is 0 exactly when it is its own root (`root_law`). The implementation proves that it counts the parent links the run-time loop follows, but the interface cannot say so without exposing the forest. `root_law` also states that a member's root is a member and is its own root, so a client can pass a returned root back to `find`. The interface has no run-time operation that compares two elements; `connected` is ghost. Only normal return is specified.

## Client example

From the public client. `funded` pairs a state with a ghost wallet of credits. The wrapper splits exactly `find_fee` credits off the wallet with `C.split` and pays them to `U.find`; the client's `fee_bounds` shows the fee is at most 44 while the structure has at most 8 elements. `{r : t | p}` is `t` restricted to values satisfying `p`, `===` is logical equality, `8Z` is a `Bigint` literal and `s.#state` reads a field of an unboxed record. `ghost_ (...)` is proof code, checked and then erased; the `_def` calls give the solver the definitions of the named functions. A result refinement that begins `let owned = owned in` refers to the argument `owned`. The wrapper's last conjunct, a cost of 4·depth + 2 ticks, follows from the event `U.find` appends: `event_cost` before and after the call, and the definitions of `total` and `weight`.

@code testsuite/tests/vox/connectivity.ml "let find : (x : U.elem) @ immutable ->" "let r = #{value; owned} in r"

After five insertions, four unions and two finds, of `x0` and of `x3`, the client calls `find` on `root0`, the root returned by the first find. This is allowed because `root_law` shows that `root0` is a member; the find returns `root0` and costs exactly 2 ticks, because a root has depth 0. From the events the client then proves the run's exact cost. `work6` to `work9` are the links followed by each union's two finds and `work10` and `work11` those of the first two finds, named before each call; `U.depth_law` makes each nonnegative.

@code testsuite/tests/vox/connectivity.ml "(* The whole run:" "work >= 0Z} = () in"

It then proves:

@code testsuite/tests/vox/connectivity.ml "let proof : {u : unit | root0 === root3 &&" "U.ticks owned.#state <= initial} = () in"

`initial` is the 1,000 credits the client minted. Last, it sums the fees over the recorded accounts to prove that 70 + 4·`work` is at most 1 + 11·5 + 44·3 + 132·4 = 716, so all the finds together, including the two inside each union, follow at most 161 parent links. For these fees it bounds a by 8, using the conjunct `k <= capacity` of `Vox_ackermann.inverse`'s contract with N = 8. The rest of that contract determines a = 3, but the client does not unfold `iter` and `below` to show it.

## A rejected program

Paying 10 credits for `make_set`, which requires exactly 11, is a type error. The `[%%expect]` block holds the compiler's message as the test records it.

@code testsuite/tests/vox/connectivity_rejected.ml "module Underpay_insertion = struct" "Error: Refinement could not be proved"

The same test rejects ten more programs against the public interface: paying 12 credits, using a state after `find` consumed it, naming the online layer's `savings` field (not in scope here), asserting without proof that an element is a member, inserting into a state of `max_int` elements, reaching the hidden `contents`, asserting a `found` transition without calling `find`, a find that returns its state unchanged while claiming the event a find records, a wrapper around `find` that claims every find costs 2 ticks, and splitting 11 credits from a wallet of 10.

## Interface

@code verification/library/vox_connectivity.mli

`t`'s layout is a product of `void`s: a state has no run-time representation. `@ unique` marks a value the call consumes, and `ghost` a value that is erased. The event type and weights:

@code verification/library/vox_union_find_events.mli

Credits (`Vox_big_credits.S`) are ghost tokens with a nonnegative count. `split` and `merge` conserve credits and `tick` removes one; only `Make ()`'s `Budget.create`, which the client calls, creates them.

@code verification/library/vox_big_credits.mli "module type S = sig" "end"

`Vox_ackermann` has no interface file. `iter cap k 1 1` is A_k(1), with every value clamped at `cap`, and `below cap a` holds when A_j(1) < `cap` for every 1 ≤ j < a:

@code verification/library/vox_ackermann.ml "let[@def] rec iter" "iter cap level (Bigint.sub count 1Z) next"

@code verification/library/vox_ackermann.ml "let[@def] rec below" "iter cap (Bigint.sub level 1Z) 1Z 1Z < cap"

## Trusted base

- `Ghost_pref.alloc`, which allocates each node, is an external (`caml_pref_alloc_step`), although `ghost_pref.mli` declares it with `val`.
- `Pref.equal` is physical equality (`%eq`), trusted to agree with `===` on elements; `link` uses it to detect that both roots are the same node.
- The cost weights: which steps call `C.tick` in `vox_union_find_worker.ml`, and how many times, is a modelling choice that no check relates to the compiled code.

## Scope

- Operations: `create`, `make_set`, `find`, `union`. There is no run-time equality or connectivity test, deletion, iteration or listing of a class.
- `make_set` requires fewer than `max_int` elements. The ghost capacity doubles as elements are added, so there is no fixed population limit below that.
- Payments are exact: overpaying is rejected as well as underpaying. Credits are ghost and erased.
- `depth` is abstract beyond being nonnegative and 0 exactly at a root (see above), so the interface fixes which events each operation records but not how many links a find follows.
- `Vox_union_find_online.create` checks `max_int >= 1` at run time and raises `Invalid_argument` otherwise, because `max_int` carries no refinement.
- Only normal return is specified. An exception such as `Out_of_memory` during allocation loses the state.
- The tests run in bytecode only.

## Reproduce

```
./dev test vox/connectivity.ml vox/connectivity_rejected.ml vox/union_find_online.ml vox/union_find_online_rejected.ml vox/union_find.ml vox/union_find_rejected.ml vox/ackermann.ml
```

`connectivity.ml` and `connectivity_rejected.ml` use the public interface; the other tests exercise the online and core layers and the Ackermann lemmas.
