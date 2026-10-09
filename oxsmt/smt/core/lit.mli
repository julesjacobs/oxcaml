(** A signed theory literal — an {!Atom.t} plus a polarity, packed into a [private int]
    (MiniSat-style low bit: [0] positive, [1] negative), mirroring {!Oxsmt_solver.Sat}'s
    literal encoding. [equal]/[compare]/[hash] are O(1) (INVARIANTS.md I6). This is the
    polarity-carrying currency the engine asserts into a theory ([assert_lit]) and that
    theories return as propagations, conflict premises, and explanation premises (ADR-0005
    D2/D3/D7).

    Frozen at the M1 THEORY freeze (ADR-0005 Tranche A). *)

type t = private int

(** [make a positive] is the literal for atom [a] with the given polarity
    ([positive = true] is the positive literal). *)
val make : Atom.t -> bool -> t

(** The underlying atom. *)
val atom : t -> Atom.t

(** [true] for a positive literal. *)
val sign : t -> bool

(** [negate l] flips the polarity, keeping the atom. *)
val negate : t -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
