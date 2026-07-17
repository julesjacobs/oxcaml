/-
  STATEMENT (no proofs) of the open conjecture:

    For STATELESS packet schedulers over a finite packet alphabet, built on
    PIFO trees of arbitrary finite topology (Mohan/Liu/Foster/Kappé/Kozen,
    "Formal Abstractions for Packet Scheduling", OOPSLA'23, arXiv 2211.11659),
    flush-equivalence implies interleaved-equivalence.

  This file contains ONLY definitions — zero theorems, zero proofs, zero
  `sorry`.  The conjecture is the final definition,

    def statelessFlushImpliesInterleaved : Prop

  A future PROOF is a separate file importing this one and providing
    theorem answer : PifoStatement.statelessFlushImpliesInterleaved
  and a DISPROOF provides
    theorem answer : ¬ PifoStatement.statelessFlushImpliesInterleaved

  Model recap (paper Defs 3.4–3.6):
    * A PIFO is a priority queue: pop removes the LOWEST-rank entry,
      ties broken FIFO (earliest arrival first).
    * A PIFO tree: every node holds a PIFO.  Leaf PIFOs hold packets;
      internal-node PIFOs hold ranked child indices.
    * push(packet, path): the path gives (child index, rank) for each internal
      node on a root-to-leaf walk plus a final leaf rank; each node on the walk
      enqueues its entry.
    * pop(): pop the root PIFO to get a child index, recurse into that child,
      until a leaf pops an actual packet — the observable output.

  Conventions in this file (each flagged where defined):
    * Arrival order is explicit: every entry carries a global arrival index,
      so FIFO tie-breaking is unambiguous.
    * pop is ATOMIC: it either fully succeeds or leaves the state unchanged
      and emits `none`.  Definedness of pop is observable in run outputs
      (paper Def 4.1, condition 1).
    * Ill-formed paths make push a no-op on the mismatched subtree; the
      conjecture quantifies only over schedulers whose paths are well-formed
      (`Scheduler.Valid`), so this branch is dead code in the statement.
    * All recursions are structural (mutual recursion on the tree and on its
      list of children); there are no `partial` definitions and no fuel.
-/

namespace PifoStatement

/-! ## PIFOs (priority queues with FIFO tie-breaking) -/

/-- One entry of a PIFO.  `val` is a child index (at internal nodes) or a
    packet (at leaves).  `rank` is the priority (LOWER pops first).  `arr` is
    the global arrival index of the push that created the entry, used to break
    rank ties FIFO.  Within any single queue all `arr` are distinct (one push
    adds at most one entry per node, and arrival indices strictly increase),
    so tie-breaking is unambiguous. -/
structure Entry (α : Type) where
  val  : α
  rank : Nat
  arr  : Nat
deriving Repr, DecidableEq

/-- A PIFO's contents, in arrival order (we always append at the tail). -/
abbrev Queue (α : Type) := List (Entry α)

variable {α : Type}

/-- `true` when `a` must pop before `b`: lower rank, ties by earlier arrival. -/
def better (a b : Entry α) : Bool :=
  a.rank < b.rank || (a.rank == b.rank && a.arr < b.arr)

/-- Pop the least (rank, arrival) entry, returning it and the rest of the
    queue (with the relative order of the remaining entries preserved).
    This is the PIFO `pop` of paper Def 3.1/3.4. -/
def qpop : Queue α → Option (Entry α × Queue α)
  | []      => none
  | e :: es =>
    match qpop es with
    | none              => some (e, [])
    | some (best, rest) =>
      if better e best then some (e, es) else some (best, e :: rest)

/-! ## PIFO trees: topology, state, push, pop -/

/-- The shape of a PIFO tree: a leaf, or an internal node with an ordered
    (finite, possibly empty) list of subtrees.  Paper Def 3.2's topologies. -/
inductive Topology where
  | leaf : Topology
  | node : List Topology → Topology
deriving Repr

/-- The state of a PIFO tree: every node holds a queue.  Leaves queue packets
    (of type `α`); internal nodes queue child indices (positions into
    `children`).  Paper Def 3.4's states. -/
inductive Tree (α : Type) where
  | leaf (q : Queue α)
  | node (q : Queue Nat) (children : List (Tree α))
deriving Repr

mutual
  /-- The all-queues-empty state of a given topology. -/
  def emptyTree : Topology → Tree α
    | .leaf    => .leaf []
    | .node ts => .node [] (emptyForest ts)
  def emptyForest : List Topology → List (Tree α)
    | []      => []
    | t :: ts => emptyTree t :: emptyForest ts
end

/-- A root-to-leaf path annotated with a rank at every node: at each internal
    node, which child to descend into and the rank of the reference enqueued
    there; at the leaf, the rank of the packet itself.  Paper Def 3.5's
    `push` argument. -/
inductive Path where
  | leaf (rank : Nat)
  | node (child : Nat) (rank : Nat) (rest : Path)
deriving Repr, DecidableEq

mutual
  /-- Does the path fit the topology (right depth, all child indices in
      range)?  Boolean, hence decidable by computation. -/
  def pathOk : Topology → Path → Bool
    | .leaf,    .leaf _        => true
    | .node ts, .node c _ rest => pathOkAt ts c rest
    | _,        _              => false
  def pathOkAt : List Topology → Nat → Path → Bool
    | [],      _,     _ => false
    | t :: _,  0,     p => pathOk t p
    | _ :: ts, c + 1, p => pathOkAt ts c p
end

mutual
  /-- push (paper Def 3.5): walk the path from the root; each internal node
      appends the reference entry (child index, rank, arrival) to its queue;
      the leaf appends the packet entry (packet, rank, arrival).
      If the path does not fit the tree (wrong depth or child index out of
      range) the mismatched subtree is left unchanged; the conjecture below
      only quantifies over `Valid` schedulers, for which this never happens. -/
  def treePush (pkt : α) (arr : Nat) : Tree α → Path → Tree α
    | .leaf q,    .leaf r        => .leaf (q ++ [⟨pkt, r, arr⟩])
    | .node q cs, .node c r rest => .node (q ++ [⟨c, r, arr⟩]) (treePushAt pkt arr cs c rest)
    | t,          _              => t
  def treePushAt (pkt : α) (arr : Nat) : List (Tree α) → Nat → Path → List (Tree α)
    | [],      _,     _ => []
    | t :: ts, 0,     p => treePush pkt arr t p :: ts
    | t :: ts, c + 1, p => t :: treePushAt pkt arr ts c p
end

mutual
  /-- pop (paper Def 3.6): pop the node's own queue; at a leaf that yields the
      emitted packet; at an internal node it yields a child index, into which
      we recurse.  ATOMICITY CONVENTION: pop either fully succeeds, returning
      the packet and the new state, or returns `none` (state to be kept
      unchanged by the caller).  A partial failure — the node's queue pops a
      child index but the recursive pop of that child fails — therefore
      consumes nothing.  For states reachable by `Valid` schedulers this
      branch is unreachable, by the counting invariant: at every internal
      node, the number of queued references to child `c` equals the total
      packet count in child `c`'s subtree (a push down a valid path adds one
      of each, a successful pop removes one of each).  So a popped reference
      always points to a subtree holding at least one packet, recursively
      down to a nonempty leaf; hence on reachable states `none` occurs
      exactly when the tree holds no packets at all. -/
  def treePop : Tree α → Option (α × Tree α)
    | .leaf q =>
      match qpop q with
      | none         => none
      | some (e, q') => some (e.val, .leaf q')
    | .node q cs =>
      match qpop q with
      | none         => none
      | some (e, q') =>
        match treePopAt cs e.val with
        | none            => none
        | some (pkt, cs') => some (pkt, .node q' cs')
  def treePopAt : List (Tree α) → Nat → Option (α × List (Tree α))
    | [],      _     => none
    | t :: ts, 0     =>
      match treePop t with
      | none           => none
      | some (pkt, t') => some (pkt, t' :: ts)
    | t :: ts, c + 1 =>
      match treePopAt ts c with
      | none            => none
      | some (pkt, ts') => some (pkt, t :: ts')
end

/-! ## Stateless schedulers -/

/-- A stateless scheduler over the packet alphabet `Fin k`: a tree topology
    together with one fixed annotated path per packet type — EVERY push of
    type `i` uses the SAME annotated path `assign i`.  (No state, no
    dependence on arrival number: this is exactly what "stateless" means.) -/
structure Scheduler (k : Nat) where
  topo   : Topology
  assign : Fin k → Path

/-- Every assigned path fits the topology.  The conjecture quantifies only
    over valid schedulers. -/
def Scheduler.Valid {k : Nat} (S : Scheduler k) : Prop :=
  ∀ i : Fin k, pathOk S.topo (S.assign i) = true

/-! ## Running words of operations

OBSERVABLE CONVENTION: each pop records the emitted packet's TYPE, not its
identity (arrival number).  For the STATELESS schedulers quantified over here
the two observables carry the same information: all packets of one type are
pushed with the same annotated path, hence carry the same rank in every queue
they enter, so any two same-type packets in a queue tie on rank and FIFO
tie-breaking releases them in arrival order.  Therefore the i-th emitted
packet of type `t` is always the i-th pushed packet of type `t`: a word's
type-level output determines its identity-level output (the converse is
trivial), and type-level equivalence coincides with identity-level
equivalence.  Statelessness is load-bearing exactly here — a STATEFUL
scheduler can give same-type packets different paths or ranks, letting them
overtake each other, so for stateful schedulers the two observables genuinely
differ (which is why the known stateful counterexample,
/tmp/pifo-lean/Pifo.lean, outputs arrival numbers instead). -/

/-- One operation: push a packet of type `i`, or pop. -/
inductive Op (k : Nat) where
  | push (i : Fin k)
  | pop
deriving Repr, DecidableEq

/-- Run a word of operations on state `t`, where `cnt` pushes happened
    already (`cnt` is the arrival counter: the n-th push overall stamps its
    entries with arrival index n).  The result is the list of pop results, in
    order: each pop contributes `some i` if it emitted a packet of type `i`,
    or `none` if pop was undefined (nothing to emit; state unchanged).
    Definedness is thus observable, matching paper Def 4.1 condition 1. -/
def runFrom {k : Nat} (S : Scheduler k) (cnt : Nat) (t : Tree (Fin k)) :
    List (Op k) → List (Option (Fin k))
  | []             => []
  | .push i :: ops => runFrom S (cnt + 1) (treePush i (cnt + 1) t (S.assign i)) ops
  | .pop    :: ops =>
    match treePop t with
    | none           => none     :: runFrom S cnt t  ops
    | some (pkt, t') => some pkt :: runFrom S cnt t' ops

/-- Run a word of operations from the empty state; the observation is the
    list of pop results (one `Option (Fin k)` per pop, in order). -/
def run {k : Nat} (S : Scheduler k) (ops : List (Op k)) : List (Option (Fin k)) :=
  runFrom S 0 (emptyTree S.topo) ops

/-! ## The two equivalences and the conjecture -/

/-- The flush word of `w`: push the packets of `w` in order, then pop `|w|`
    times ("push a word, then pop it dry").  Popping exactly `|w|` times is
    WLOG among `push* pop^m` words: for `m ≤ |w|` the output is a prefix of
    the full drain (pops are deterministic), and `m > |w|` only appends
    `none`s — a case covered by `interEquiv` in any event. -/
def flushOps {k : Nat} (w : List (Fin k)) : List (Op k) :=
  w.map Op.push ++ List.replicate w.length Op.pop

/-- Flush-equivalence: identical outputs on EVERY flush word. -/
def flushEquiv {k : Nat} (S1 S2 : Scheduler k) : Prop :=
  ∀ w : List (Fin k), run S1 (flushOps w) = run S2 (flushOps w)

/-- Interleaved-equivalence: identical outputs on EVERY word of operations
    (pushes and pops arbitrarily interleaved; over-popping is allowed and
    observable as `none`, identically so for any two schedulers whenever
    their trees are simultaneously packet-free). -/
def interEquiv {k : Nat} (S1 S2 : Scheduler k) : Prop :=
  ∀ ops : List (Op k), run S1 ops = run S2 ops

/-- THE CONJECTURE.  For any finite packet alphabet and any two valid
    stateless schedulers over it (arbitrary, possibly different topologies):
    if they agree on all flush words then they agree on all interleaved
    words.  (The converse is trivial, flush words being op words.) -/
def statelessFlushImpliesInterleaved : Prop :=
  ∀ (k : Nat) (S1 S2 : Scheduler k),
    S1.Valid → S2.Valid → flushEquiv S1 S2 → interEquiv S1 S2

end PifoStatement
