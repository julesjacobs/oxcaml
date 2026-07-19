(** Incremental difference-logic constraint graph (Cotton–Maler 2006 incremental
    consistency), polymorphic over the premise token ['p]. The Dartagnan charter's
    census-matched propagator: 82% of that class's atoms are difference ([x - y <= c]) or
    var-const ([x <= c]) relations, which this graph decides with negative-cycle detection
    at a fraction of the simplex's per-decision cost.

    {b Semantics.} A constraint is [hd - tl <= w] over integer node ids, stored as a
    directed edge [tl -(w)-> hd]. The graph is CONSISTENT iff it has no negative cycle; a
    feasible integer potential [pi] with [pi(hd) - pi(tl) <= w] for every edge witnesses
    consistency (single-source shortest-path feasibility). Var-const bounds use a caller-
    supplied zero node ([x <= c] is [x - z0 <= c]).

    {b Trail.} Edges assert in call order and retract newest-first. Because removing an
    edge only RELAXES the system, a potential feasible for the larger edge set stays
    feasible for any subset — so {!pop}/{!rewind} never restore potentials, they only drop
    the edge suffix (INVARIANT: [pi] is a feasible potential for the live edge set at all
    times). This is the soundness centre the charter names.

    {b Determinism.} All iteration is over integer-id order or call-order trails; no
    polymorphic hashing of ['p]. *)

type 'p t

(** A negative-cycle conflict: the premises of the edges forming the cycle (the
    conjunction of these asserted constraints is difference-infeasible). Never empty. *)
type 'p conflict = { premises : 'p list }

val create : unit -> 'p t

(** Ensure node id [i] exists (grows internal arrays; potential 0). Idempotent. *)
val ensure_node : 'p t -> int -> unit

(** [assert_edge t ~tl ~hd ~weight prem] adds [hd - tl <= weight] attributed to [prem].
    Returns [Some conflict] if it closes a negative cycle (the edge is NOT added in that
    case — the graph is left consistent for continued use after the SAT core backtracks),
    else [None] (edge added, potential restored to feasibility). *)
val assert_edge : 'p t -> tl:int -> hd:int -> weight:int -> 'p -> 'p conflict option

(** [implied t ~tl ~hd ~weight] is [Some premises] when the current graph ENTAILS
    [hd - tl <= weight] — i.e. the shortest path [tl ⇝ hd] has length [<= weight] — with
    [premises] the edges of that path (a valid, precedence-respecting reason). [None] when
    not currently entailed. Read-only; does not mutate the graph. *)
val implied : 'p t -> tl:int -> hd:int -> weight:int -> 'p list option

(** {2 Trail} *)

val push : 'p t -> unit
val pop : 'p t -> int -> unit

type checkpoint

val checkpoint : 'p t -> checkpoint
val rewind_to_checkpoint : 'p t -> checkpoint -> unit

(** {2 Test hooks} *)

(** Current number of live (asserted, un-popped) edges. *)
val edge_count : 'p t -> int

(** [consistent_naive t] recomputes feasibility from scratch (Bellman–Ford over the live
    edge set) — the differential-test reference oracle, never used in the solve path. *)
val consistent_naive : 'p t -> bool
