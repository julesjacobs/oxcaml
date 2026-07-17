/-
  Machine-checked STATEFUL counterexample for PIFO-tree
  flush-equivalence vs interleaved-equivalence.

  Model (see /tmp/pifo-context.md, "Established results" item 1):
    * A PIFO is a list of entries carrying (value, rank, arrival).
      pop removes the entry with least rank, ties broken by least arrival
      (arrival = a global monotone counter, so within any single PIFO this is
      exactly FIFO tie-breaking).
    * A flat PIFO tree has a root PIFO holding leaf-indices and one PIFO per leaf
      holding packets.  push installs a root entry (leaf-index, rootRank) and a
      leaf entry (packet, leafRank).  pop pops the root to get a leaf index, then
      pops that leaf to emit a packet.
    * Packets are represented by their arrival number, which is the strongest
      possible observation; the |Pkt|=2 version of the counterexample follows by
      relabeling arrivals to their packet type.

  Semantics validated against /tmp/pifo-logic.js (node) before proving.
-/

namespace Pifo

/-- One entry of a PIFO. `val` is a child index (root) or packet (leaf). -/
structure Entry where
  val  : Nat
  rank : Nat
  arr  : Nat
deriving Repr, DecidableEq

abbrev Queue := List Entry

/-- `true` when `a` should be popped before `b`: lower rank, ties by lower arrival. -/
def better (a b : Entry) : Bool :=
  a.rank < b.rank || (a.rank == b.rank && a.arr < b.arr)

/-- Pop the least (rank, arr) entry, returning it and the remaining queue.
    Within a single PIFO all `arr` are distinct, so the choice is unambiguous. -/
def qpop : Queue → Option (Entry × Queue)
  | []      => none
  | e :: es =>
    match qpop es with
    | none            => some (e, [])
    | some (best, rest) =>
      if better e best then some (e, best :: rest)
      else some (best, e :: rest)

/-- Flat tree state: a root queue plus one queue per leaf. -/
structure State where
  root   : Queue
  leaves : List Queue
deriving Repr, DecidableEq

def emptyState (nLeaves : Nat) : State :=
  { root := [], leaves := List.replicate nLeaves [] }

/-- push: install root entry (leafIdx, rootRank) and leaf entry (packet, leafRank).
    `arr` is used both as the entry arrival tie-breaker and, in the leaf, as the
    packet identity. -/
def pushState (st : State) (leafIdx leafRank rootRank arr : Nat) : State :=
  let rootE : Entry := { val := leafIdx, rank := rootRank, arr := arr }
  let leafE : Entry := { val := arr,     rank := leafRank, arr := arr }
  let leaf  := (st.leaves.getD leafIdx []) ++ [leafE]
  { root := st.root ++ [rootE], leaves := st.leaves.set leafIdx leaf }

/-- pop: pop root to a leaf index, then pop that leaf to emit a packet (its arr). -/
def popState (st : State) : Option (Nat × State) :=
  match qpop st.root with
  | none => none
  | some (rootE, root') =>
    let idx := rootE.val
    match qpop (st.leaves.getD idx []) with
    | none => none
    | some (leafE, leaf') =>
      some (leafE.val, { root := root', leaves := st.leaves.set idx leaf' })

/-- A scheduler's transaction: arrival k ↦ (leafIdx, leafRank, rootRank). -/
abbrev Tx := Nat → Nat × Nat × Nat

inductive Op | push | pop
deriving Repr, DecidableEq

/-- Running configuration: arrival counter, tree state, emitted-packet list. -/
structure Config where
  cnt : Nat
  st  : State
  out : List Nat

def step (tx : Tx) (c : Config) : Op → Config
  | Op.push =>
    let k := c.cnt + 1
    let (li, lr, rr) := tx k
    { cnt := k, st := pushState c.st li lr rr k, out := c.out }
  | Op.pop =>
    match popState c.st with
    | none          => c
    | some (pkt, st') => { cnt := c.cnt, st := st', out := c.out ++ [pkt] }

def run (tx : Tx) (nLeaves : Nat) (ops : List Op) : List Nat :=
  (ops.foldl (step tx) { cnt := 0, st := emptyState nLeaves, out := [] }).out

/-- Flush word: n pushes followed by n pops. -/
def flushOps (n : Nat) : List Op :=
  List.replicate n Op.push ++ List.replicate n Op.pop

/-! ## The two counterexample schedulers (both flat, 3 leaves: 0,1,2). -/

/-- S1: 1↦C(1,1) 2↦B(1,3) 3↦C(2,2) k≥4↦G(k,100+k).  (B=0, C=1, G=2). -/
def txS1 : Tx := fun k =>
  if k == 1 then (1, 1, 1)
  else if k == 2 then (0, 1, 3)
  else if k == 3 then (1, 2, 2)
  else (2, k, 100 + k)

/-- S2: 1↦E(1,3) 2↦E(2,1) 3↦F(1,2) k≥4↦G(k,100+k).  (E=0, F=1, G=2). -/
def txS2 : Tx := fun k =>
  if k == 1 then (0, 1, 3)
  else if k == 2 then (0, 2, 1)
  else if k == 3 then (1, 1, 2)
  else (2, k, 100 + k)

def drainS1 (n : Nat) : List Nat := run txS1 3 (flushOps n)
def drainS2 (n : Nat) : List Nat := run txS2 3 (flushOps n)

/-- Expected flush drain: n=1↦[1], n=2↦[1,2], n≥3↦[1,3,2,4,5,…,n]. -/
def expectedDrain (n : Nat) : List Nat :=
  match n with
  | 0 => []
  | 1 => [1]
  | 2 => [1, 2]
  | _ => 1 :: 3 :: 2 :: (List.range' 4 (n - 3))

/-! ## CHECKED (a): flush agreement for every n ≤ 8, both equal to expectedDrain. -/

theorem flush_agree_le_8 :
    ∀ n, n ≤ 8 →
      drainS1 n = drainS2 n ∧ drainS1 n = expectedDrain n := by
  decide

/-- Same fact stated as an explicit finite conjunction (independent cross-check). -/
theorem flush_agree_explicit :
    (drainS1 0 = drainS2 0) ∧ (drainS1 1 = drainS2 1) ∧
    (drainS1 2 = drainS2 2) ∧ (drainS1 3 = drainS2 3) ∧
    (drainS1 4 = drainS2 4) ∧ (drainS1 5 = drainS2 5) ∧
    (drainS1 6 = drainS2 6) ∧ (drainS1 7 = drainS2 7) ∧
    (drainS1 8 = drainS2 8) ∧
    drainS1 8 = [1,3,2,4,5,6,7,8] := by
  decide

/-! ## CHECKED (b): divergence on the interleaved word push,pop,push,push,pop. -/

def divergeOps : List Op :=
  [Op.push, Op.pop, Op.push, Op.push, Op.pop]

theorem diverge_S1 : run txS1 3 divergeOps = [1, 3] := by decide
theorem diverge_S2 : run txS2 3 divergeOps = [1, 2] := by decide

theorem schedulers_diverge_interleaved :
    run txS1 3 divergeOps ≠ run txS2 3 divergeOps := by decide

/-! The two schedulers agree on every flush word (arrivals ignore packet identity,
    so the drain permutation determines the output on any alphabet) yet differ on
    an interleaved word — hence flush-equivalence does NOT imply
    interleaved-equivalence for stateful assigners. -/

end Pifo
