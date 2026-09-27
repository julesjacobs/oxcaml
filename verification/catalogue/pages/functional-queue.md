title: Functional queue
blurb: A persistent two-list queue proved to act on the list of its elements: `enqueue` appends, and `dequeue` of a nonempty queue returns the first element and the rest.
status: review-pending
date: 27 September 2026
sources:
  - testsuite/tests/vox/functional_queue.mli — Public interface
  - testsuite/tests/vox/functional_queue.ml — Implementation: front and rear lists, reversal lemmas
  - verification/library/vox_sequence.mli — `append` and its defining equation
  - testsuite/tests/vox/queue_client.ml — Client: two values come out in the order they went in
  - testsuite/tests/vox/queue_rejected.ml — Rejected client: dequeue from the empty queue
  - testsuite/tests/vox/queue_rejected.compilers.reference — The expected error
---
`Functional_queue` is a persistent queue stored as a front list and a reversed rear list. The interface describes a queue `q` by `contents q`, the list of its elements from first to last. `contents empty` is `[]`; `contents (enqueue q x)` is `append (contents q) [x]`; and `dequeue q`, which requires `contents q` to be nonempty, returns a pair `(head, tail)` with `contents q === head :: contents tail`. Since `t` is abstract and these are its only operations, the equations determine `contents` of every queue a client can build, so a client reasons about a queue as a list. `===` is logical equality.

Not proved: running time. When each version of a queue is used at most once, `enqueue` and `dequeue` take amortized constant time; dequeuing repeatedly from the same older version can repeat the reversal of its rear list. No cost theorem is stated. There is no emptiness test. `dequeue` needs a proof that the queue is nonempty, and a caller who does not know this statically can only decide it by computing `contents q`, which takes time linear in the length of the queue; a loop that tests the queue this way before each `dequeue` takes quadratic time. Elements must be `immutable_data`, which excludes closures and mutable records.

## Client example

From the client, which uses only the public interface. It enqueues two values into an empty queue, dequeues twice, and proves that the values come out in order and the queue is empty again. `('a : immutable_data)` restricts the element type. `{r : t | p}` is the type `t` refined by the predicate `p`. `let refine_ x = e` binds `x` and keeps the refinement of `e`'s result as a fact about `x`, and `refine_ e` checks `e` against the refinement expected at that point. `ghost_ (...)` is proof code, checked and then erased. The checker does not evaluate recursive functions by itself: `append_def` unfolds `append` once, so that `append [] [first]` is known to be `[first]`.

@code testsuite/tests/vox/queue_client.ml "let (fifo @ total) :" "refine_ result"

## A rejected program

Dequeuing from the empty queue is a type error. This client claims that `empty` is nonempty in order to pass it to `dequeue`.

@code testsuite/tests/vox/queue_rejected.ml "let () =" "  ()"

@text testsuite/tests/vox/queue_rejected.compilers.reference

## Interface

@code testsuite/tests/vox/functional_queue.mli

`Vox_sequence.t` is `'a list`, and `Vox_sequence.append` is list concatenation, exported with its defining equation `append_def`. `(q : a) -> b` names the argument so that `b` can mention it. The precondition of `dequeue`, `(contents q === []) === false`, says that the queue is nonempty. `@@ total` declares a function total: it terminates without raising or touching mutable state, which lets it appear in refinements. `@ immutable` is an OxCaml mode that forbids access to mutable fields; the queue's `immutable_data` values have none, so it does not restrict them.

## Trusted base

- Nothing beyond the shared base. `dequeue` contains one `unreachable_ ()`, on the branch where reversing the rear list of a nonempty queue with an empty front gives `[]`. It is not an assumption: the checker must prove that the branch cannot be reached, and a runtime trap remains in the compiled code.

## Scope

- Operations: `empty`, `enqueue`, `dequeue` and `contents`. There is no `is_empty`, `length`, `peek`, iteration or conversion from a list.
- `dequeue` requires a nonempty queue; `contents` is the only way to test emptiness at runtime, and it copies the queue into a list.
- Elements are `immutable_data`.
- Every function is `total`. No bound on running time or allocation is stated.

## Reproduce

```
./dev test vox/queue_client.ml vox/queue_rejected.ml
```

`queue_client.ml` compiles the queue, `Vox_sequence` and the client, and runs the client as bytecode and native code. `queue_rejected.ml` compiles the rejected client against the two interfaces and compares the error with the expected output.
