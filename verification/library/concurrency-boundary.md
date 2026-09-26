# Channels and reference locks

The checked claims are normal-return ownership safety and preservation of
payload refinements. They are conditional on the trusted primitive contracts
below and the language's affine/ghost discipline. This is not a mechanized
concurrent soundness theorem for the compiler or runtime.

## Ordered review surfaces

Read this document first. Paths below are relative to the repository root.
Each list is transitive; implementations and proof annotations are excluded.

One-shot channels:

1. `verification/library/pref.mli`
2. `verification/library/ghost_pref.mli`
3. `verification/library/verified_atomic.mli`
4. `verification/library/unique_cell.mli`
5. `verification/library/one_shot.mli`

Channel buffer protocol additionally requires, after item 2,
`verification/library/raw_memory.mli`, and finally `verification/library/channel_buffer.mli`. The executable `run` demonstrates
normal-return reclamation after joining the transferred byte permissions and
the retained deallocation permission. It is not a theorem of exception-safe
reclamation.

Unique payload locks: items 1–4 above, followed by
`verification/library/unique_lock.mli` and the chosen `Unique_cell.Payload`
argument, including its complete `snapshot` definition. The model need not
identify values injectively; it records exactly what the caller's snapshot
observes. Ownership transfer still applies when that observation is constant.

Nonnegative reference locks: items 1–3 above, followed by
`verification/library/reference_lock.mli`.

`owned_def` completely characterizes ownership: the heap is a singleton at the
lock's location, containing a payload snapshot (unique lock) or a nonnegative
integer (reference lock). No invariant predicate or atomic implementation
module is exported. Acquisition returns that authority on success and empty
authority on failure. Release requires and consumes that same full authority.
A unique lock can be released only after a taken payload has been put back.
The integer increment implementation saturates at max_int and reports acquisition;
its exported `t -> bool` type does not prove either behavior to public clients.
The `make` interfaces likewise export no relation between the initial argument
and a later observation. Runtime fixtures check initial values and increment
counts. The checked public claims are the acquisition/release authority
contracts, the reference read's nonnegative value and agreement with the owned
snapshot, and the unique-cell take/put snapshot equations. Payload type
refinements remain enforced, including when the snapshot discards information.

## Primitive meanings and assumptions

`Pref.Heap` is a finite partial map from allocation locations to values.
`at h p` is None exactly outside its domain; `mem h p` is membership.
`empty` has no members. `put h p x` overrides p with x. `union a b` is
left-biased union. `restrict h s` retains keys in s's domain; `exclude h s`
retains keys outside it. `disjoint` means domain intersection is empty;
`same_domain` means equal domains. Map equality compares domains and values.
These are trusted compiler semantics; the exported laws are additional
trusted characterizations, not executable map operations. Token observations
are immutable snapshots. Distinct simultaneous live authority occurrences own
disjoint locations. Observations do not authorize memory access. Allocation
creates fresh identity; consuming writes update only owned locations.

An atomic operation opens its invariant at one strong sequentially consistent
event. CAS succeeds exactly when the observed integer equals expected; only
success replaces it with desired. The erased total transition must restore
the invariant and return disjoint caller authority. Allocation/identity can
fail. Native scalar atomic results allocate no carrier; bytecode allocates its
product adapter before the event. The logic assumes these C contracts and the
runtime's atomic memory ordering; it does not prove them from a memory model.

Unique cells move payloads rather than copy usable ownership. Taking empties
the cell; putting requires emptiness. Managed handles/cells are garbage
collected, but losing affine authority never promises to reclaim raw payload
allocations. `Raw_memory` requires explicit free and the deallocation marker.
Neither library catches exceptions to manufacture replacement authority.

No fairness, starvation freedom, delivery, bounded waiting, cancellation
recovery, exception safety, or general leak freedom is proved. Channel receive
spins until publication; lock acquisition performs one CAS. Sending and lock
release perform one CAS without waiting. Wrapper/handle allocation and GC
costs are outside the logical cost model; no asymptotic performance theorem
is claimed. There are no runtime proof certificates or final proof checkers.

## Excluded prototype

Permission capsules and capsule locks are not delivered by this change. The
historical prototype documents an exception-constructor provenance hole through
first-class modules and unauthorized traversal through Marshal. Those holes
remain unresolved. They do not acquire a completed isolation claim here.
Safe ownership claims exclude unsafe primitives, generic serialization of
abstract representations, and known compiler mode-soundness escapes.

## Provenance

`concurrency-provenance.json` records the five authorized agent-2 snapshots
and SHA-256 hashes. Originals were read twice and compared while copying;
no source-owner files were changed. The lock code was ported to typed heaps,
void invariant keys, and unboxed atomic results on commit d143961f17.

## Reproduction

Configure with `--prefix="$PWD/_install" --enable-multidomain
--enable-poll-insertion`, then `./dev init`. Run
`verification/concurrency/check.sh`. It compiles interfaces and implementations
separately, copies only public `.cmi` files into a separate client directory,
and compiles/runs the clients there with bytecode and native compilers. Source
and implementation `.cmx` files are absent during client compilation.
It then checks expected rejection categories and emitted Lambda for runtime
heap/authority observations. Multidomain tests perform 4,000 successful updates
and force collections; these executions support, but do not prove, concurrent
progress or runtime correctness.

The constant-snapshot raw-buffer payload deliberately makes its ownership
refinement, rather than its observation function, responsible for access.
Its client mutates and reads the transferred byte and explicitly frees the
allocation. Its constant snapshot does not prove preservation of byte contents
across release/reacquisition; the payload refinement proves access and
reclamation authority. Reusing that payload after transfer is rejected. The scalar channel
client derives `roundtrip n = n` through a singleton payload refinement, and the
buffer protocol client derives the exact requested byte and retained location.
The public channel interface supplies payload-type preservation, not a separate
history/linearizability theorem. No theorem equates arbitrary unrefined send
and receive values via a public trace model.

Fresh evidence is recorded in `verification/concurrency/evidence.json`.
All eight public-only clients and 15 intended rejection cases pass in both
backends on the multidomain/poll-insertion build. Library modules also compile
with `-principal`; clients retain their existing non-principal configuration
because the historical channel tuple fixture hits an unrelated principal-kind
inference limitation. The existing `one_shot_rejected.ml` expect test passes in
both backends. Native Cmm and arm64 assembly retain CAS/cell moves/reference
access while heap observations, invariant keys, and permission fields erase.
Primitive library definitions remain trusted; their exported external symbols
are not themselves evidence of runtime observations in client code.
