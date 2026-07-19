(** Engine-assigned theory-atom id — the per-assertion currency across the THEORY seam
    (ADR-0005 Decision 2). A dense [private int]; identity is the id, so
    [equal]/[compare]/[hash] are O(1) and deterministic (INVARIANTS.md I6). A theory
    reasons in terms of [Atom.t]/[Lit.t] and receives the underlying [Term.t] only once,
    at {!Oxsmt_core.Theory.THEORY.register_atom} — this keeps per-assertion traffic a
    packed int and designs the single-[Context] hazard (core-review R3) off the hot path.

    {b Allocation goes through a safe minter (ADR-0005 CONTRACT-ATOM), never a public
      [of_int].}
    There is deliberately no id-forging constructor on this surface: a forged id would
    miss the engine's atom⇄var map (or alias another atom's slot), a forged premise
    literal would malform 1UIP, and a hand-chosen id would break the dense/monotonic
    invariant [fresh] guarantees (I6). Ids are minted only by {!fresh} from an
    {!allocator}; the engine holds one allocator and mints one id per theory atom, 1:1
    with its SAT variable. (A core-private no-copy cast, [Atom_unsafe.of_int], lets [Lit]
    unpack a packed literal inside [core]; it is a dune [private_modules] and a compile
    error outside [core] — the [Iarr_unsafe] pattern, ADR-0003 B1.) Frozen at the M1
    THEORY freeze (ADR-0005 Tranche A). *)

type t = private int

(** A monotonic id source. The engine holds exactly one per session. *)
type allocator

(** [create_allocator ()] is a fresh source whose first {!fresh} is the least id. *)
val create_allocator : unit -> allocator

(** [fresh a] returns the {e next} id (dense, strictly increasing, deterministic —
    CONTRACT-ATOM / I6); it is the sole way to obtain an [Atom.t]. The engine calls it
    once per theory atom, pairing the result 1:1 with the atom's SAT variable. A theory
    plugin never calls it: it receives its atoms through
    {!Oxsmt_core.Theory.THEORY.register_atom}. *)
val fresh : allocator -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = t
