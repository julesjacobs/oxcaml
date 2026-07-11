(** Engine-assigned theory-atom id — the per-assertion currency across the THEORY seam
    (ADR-0005 Decision 2). A dense [private int], 1:1 with the SAT variable the clausifier
    gives the atom; identity is the id, so [equal]/[compare]/[hash] are O(1) and
    deterministic (INVARIANTS.md I6). A theory reasons in terms of [Atom.t]/[Lit.t] and
    receives the underlying [Term.t] only once, at
    {!Oxsmt_core.Theory.THEORY.register_atom} — this keeps per-assertion traffic a packed
    int and designs the single-[Context] hazard (core-review R3) off the hot path.

    {b Allocation is the engine's (ADR-0005 CONTRACT-ATOM).} The engine allocates
    [Atom.t]s 1:1 with SAT variables via a deterministic monotonic counter (I6); this
    module only provides the typed wrapper {!of_int} and the id-keyed containers. Frozen
    at the M1 THEORY freeze (ADR-0005 Tranche A). *)

type t = private int

(** [of_int v] tags SAT-variable id [v] as an atom id. The engine's atom allocator is the
    sole intended caller (CONTRACT-ATOM: called once per theory atom, 1:1 with the SAT
    var, in monotonic allocation order — I6). A theory plugin never calls this: it
    receives its atoms through {!Oxsmt_core.Theory.THEORY.register_atom}. *)
val of_int : int -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = t
