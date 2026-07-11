# SPINE.md — frozen core interfaces (GENERATED — regenerate with `make spine`)

The master's working-set view of the frozen core data types (DESIGN.md §11).
Do NOT edit by hand. Frozen per ADR-0003; hash-checked via FROZEN.sha256
(`make check-frozen`). Changing any of these requires the unfreeze ritual.

=== smt/core/sort.mli ===

(** Sorts (ADR-0003 Decision 6). [private] variant: deep matching is allowed, construction
    goes through the smart constructors so [equal]/[hash] stay O(1). [int_kind] is the §1
    width hook; v1 has only [Mathematical] (unbounded ℤ). Uninterpreted sorts are 0-arity
    in v1. *)

type t = private
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t

and int_kind = Mathematical

val bool : t
val int : t
val uninterpreted : Symbol.t -> t
val equal : t -> t -> bool
val hash : t -> int

=== smt/core/symbol.mli ===

(** Interned symbols (ADR-0003 Decision 4). A symbol is a small [int] id; identity is the
    id, so [equal]/[hash] are O(1). Interning is by name and {b idempotent}: the same name
    always maps to the same id, so a fixed sequence of declarations yields identical ids
    across runs (INVARIANTS.md I6).

    Names live in a process-global table keyed by id (this is why [name] needs no
    environment); ids are handed out in first-encounter order. Ranks live in {!Env}, not
    here.

    {b Deviation from ADR-0003 Decision 4:} the ADR pictured interning living in [Env]; we
    moved it to this process-global table so [name : t -> string] can be environment-free
    (the frozen signature takes no [Env]). I6 is unaffected — term identity is the
    per-[Context] tag stream, not the symbol id, and the cross-run cache key is computed
    gate-side (ADR-0003 Decision 4); global interning is idempotent-by-name, so a fixed
    declaration sequence still yields identical ids across runs. *)

type t = private int

(** [intern name] returns the id for [name], allocating a fresh one the first time and
    returning the same id on every later call (idempotent). This is the sole
    symbol-creation path; {!Env} and {!Context} call it. *)
val intern : string -> t

val equal : t -> t -> bool
val hash : t -> int
val name : t -> string

=== smt/core/term.mli ===

(** Hash-consed terms (ADR-0003 Decision 5). The 9-node set is frozen. The type is
    [private]: deep matching and field access are allowed, construction is not —
    {!Context}'s smart constructors are the sole build path, so every [Term.t] in
    existence is well-sorted and hash-consed (INVARIANTS.md I1/I2).
    [equal]/[compare]/[hash] are O(1) via the hash-cons [tag]; equal terms are physically
    equal.

    {b WARNING — single-Context contract.} [equal]/[compare]/[hash] use the [tag], which
    is unique only {e within} the {!Context} that built the term. Comparing or combining
    terms from two different contexts is {b undefined behavior} (cross-context tags
    collide). v1 has no per-context brand (flagged for the M1 THEORY-freeze checkpoint);
    use one [Context] per session — see {!Context}.

    Booleans are terms: connectives ([And]/[Or]/[Not]/[Ite]/Bool-sorted [Eq]) are
    Bool-sorted nodes (Decision 2). Arithmetic is a single normalized linear form
    ([Arith]); order comparisons lower to one [Le] atom ([arg <= 0]), gcd-tightened
    (Decision 1). Nullary constants and program variables are [App(sym, [])]. *)

type t = Node.t = private
  { node : node
  ; sort : Sort.t
  ; tag : int (* hash-cons identity *)
  }

and node = Node.node = private
  | Bool_const of bool
  | Int_const of int
  | App of Symbol.t * t Iarr.t (* EUF-congruent; nullary = constant *)
  | Arith of linear (* Int; LIA leaf to EUF *)
  | Le of t (* (arg <= 0), arg : Int; only order atom; gcd-normalized *)
  | Eq of t * t (* same sort, tag-ordered; Bool = iff/connective *)
  | Not of t
  | And of t Iarr.t (* n-ary, >= 2 *)
  | Or of t Iarr.t (* n-ary, >= 2 *)
  | Ite of t * t * t (* cond Bool; branches share result sort *)

and linear = Node.linear = private
  { coeffs : (t * int) Iarr.t (* tag-sorted, coeff <> 0, no Arith child *)
  ; const : int
  }

(** Raised by an arithmetic constructor that would exceed native [int] range, {b before}
    any intern-table mutation; caught at the session layer and turned into verdict
    [unknown] (ADR-0003 Overflow contract). *)
exception Overflow

(** Raised by a constructor given an ill-sorted operand. *)
exception Sort_error of string

(** Raised for an out-of-fragment input (e.g. a non-constant or zero divisor); same
    pre-mutation / session-caught contract as {!Overflow}. *)
exception Unsupported of string

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = t

(** Deep validator for the ADR-0003 checked invariants (run in tests / debug builds;
    DESIGN §4). *)
module Debug : sig
  (** [Construction] permits Int-sorted [Ite] (it is removed by a later pass); [Pipeline]
      additionally forbids any Int-[Ite] and any residual reserved [div]/[mod] application
      (ADR-0003 invariant 10, checked on the preprocessed formula). *)
  type mode =
    | Construction
    | Pipeline

  (** [check ?mode ?env t] raises [Failure] on the first violated invariant. [env], when
      supplied, enables the [App]-rank check (arity + argument sorts
      + codomain); without it that check is skipped (rank matching is otherwise a
        construction-time guarantee of {!Context.app}). *)
  val check : ?mode:mode -> ?env:Env.t -> t -> unit
end

=== smt/core/context.mli ===

(** The session context (ADR-0003 Decisions 4 & 6): bundles an {!Env.t} with the strong
    monotonic intern table and tag counter. {b All} term construction threads one
    [Context] — the parser, clausifier, preprocessing, and the theory/combination layer
    mid-solve — so every term shares the tag stream (I6) and hash-consing. These smart
    constructors are the sole public construction path (I2): each sort-checks, normalizes,
    and hash-conses; ill-sorted operands raise {!Term.Sort_error}, overflowing arithmetic
    raises {!Term.Overflow} before any interning.

    {b WARNING — single-Context contract.} A [Term.t] belongs to the [Context] that built
    it: identity (tag) is assigned by that context's counter. Mixing terms from two
    different contexts in one construction, comparison, or {!Term.equal} is
    {b undefined behavior} — tags collide across contexts, so {!Term.equal} and the
    hash-cons table silently misbehave. v1 has no per-context brand enforcing this (a
    brand needs an interface unfreeze — flagged for the M1 THEORY-freeze checkpoint);
    until then it is a caller contract. Use one [Context] per session. *)

type t

val create : Env.t -> t

(** Number of distinct terms interned so far (introspection for tests / metrics; deviation
    from the ADR sketch). *)
val term_count : t -> int

(** [const t sym] is the nullary application [sym] (a constant or program variable); [sym]
    must have arity 0 in the env. *)
val const : t -> Symbol.t -> Term.t

(** [app t sym args] sort-checks [args] against [sym]'s rank and returns the application;
    raises {!Term.Sort_error} on arity/sort mismatch or unknown symbol. *)
val app : t -> Symbol.t -> Term.t list -> Term.t

val int_const : t -> int -> Term.t
val bool_const : t -> bool -> Term.t
val add : t -> Term.t -> Term.t -> Term.t
val sub : t -> Term.t -> Term.t -> Term.t
val neg : t -> Term.t -> Term.t
val mul_const : t -> int -> Term.t -> Term.t

(** [linear_combination t pairs const] is [Σ (cᵢ · termᵢ) + const] (additive convenience
    for LIA). *)
val linear_combination : t -> (int * Term.t) list -> int -> Term.t

(** [div t x d] / [mod_ t x d]: [d] must be a nonzero [Int_const], else
    {!Term.Unsupported} (a documented v1 limitation). Built on the reserved [div]/[mod]
    symbols, eliminated by a later pass. *)
val div : t -> Term.t -> Term.t -> Term.t

val mod_ : t -> Term.t -> Term.t -> Term.t

(** [abs t x] desugars to [Ite(x >= 0, x, -x)] at construction. *)
val abs : t -> Term.t -> Term.t

val eq : t -> Term.t -> Term.t -> Term.t

(** [le]/[lt]/[ge]/[gt] all lower to a single gcd-normalized [Le] atom. *)
val le : t -> Term.t -> Term.t -> Term.t

val lt : t -> Term.t -> Term.t -> Term.t
val ge : t -> Term.t -> Term.t -> Term.t
val gt : t -> Term.t -> Term.t -> Term.t

(** [distinct t xs] desugars to pairwise [Not(Eq)] at construction. *)
val distinct : t -> Term.t list -> Term.t

val not_ : t -> Term.t -> Term.t
val and_ : t -> Term.t list -> Term.t
val or_ : t -> Term.t list -> Term.t
val implies : t -> Term.t -> Term.t -> Term.t
val iff : t -> Term.t -> Term.t -> Term.t
val ite : t -> Term.t -> Term.t -> Term.t -> Term.t

=== smt/core/iarr.mli ===

(** Portable immutable array (ADR-0003 Decision on [Iarr]; ADR-0002 pins stock OCaml 5.4,
    so this is not OxCaml [iarray]).

    The type is {b abstract} and covariant: values can only be built by the two copying
    constructors below, and there is no mutator and no [to_array]. No read path hands back
    the backing array and no write path exists, so a term's hash-consed [Iarr] payload
    cannot be corrupted in place (INVARIANTS.md I7; upholds I1/I2). Covariance is sound
    precisely because the value is immutable.

    There is deliberately {b no} aliasing / unsafe constructor on this public surface. A
    no-copy cast lives in the library-private [Iarr_unsafe] module (dune
    [private_modules]) and is invisible to every consumer. *)

type +'a t

val of_list : 'a list -> 'a t

(** [of_array a] copies [a] ([Array.copy]); the caller keeps ownership of its array and
    may mutate it afterwards without affecting the result. *)
val of_array : 'a array -> 'a t

val to_list : 'a t -> 'a list
val length : 'a t -> int

(** [get t i] is O(1). Raises [Invalid_argument] if out of bounds. *)
val get : 'a t -> int -> 'a

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val map : ('a -> 'b) -> 'a t -> 'b t
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** [hash_fold f acc t] folds [f] over {e every} element (order and count included), so
    distinct arrays hash distinctly (ADR-0003 required #8). *)
val hash_fold : (int -> 'a -> int) -> int -> 'a t -> int

=== smt/core/env.mli ===

(** Symbol environment (ADR-0003 Decision 6): maps function symbols to their {!Rank.t} and
    tracks declared uninterpreted sorts. One [Env.t] backs a session's {!Context.t}.
    [create] pre-declares the reserved [div]/[mod] built-in symbols (ADR-0003 Decision 5),
    reachable via {!div_sym}/{!mod_sym} for the div/mod-elimination pass.

    Symbol ids themselves are process-global (see {!Symbol}); an [Env] only owns the
    ranks.

    {b Shared name/symbol namespace (parser-layer obligation).} Because symbols are
    interned by name in one global table, sort names and function names share a namespace:
    [declare_sort t "S"] and [declare_fun t "S" r] return the {e same} symbol id. v1
    uninterpreted sorts are rare and 0-arity, so this is accepted; a front end (e.g. the
    SMT-LIB parser) that needs SMT-LIB's separate sort/function namespaces must
    disambiguate names before calling here. *)

type t

(** Raised by {!declare_sort}/{!declare_fun} when asked to (re)declare a reserved built-in
    name ([div] or [mod]); protects the pre-declared reserved ranks from being clobbered
    (R2). *)
exception Reserved_symbol of string

(** [create ()] builds a fresh environment with [div]/[mod] pre-declared. *)
val create : unit -> t

(** [declare_sort t name] interns [name] as a 0-arity uninterpreted sort symbol (v1:
    uninterpreted sorts are 0-arity). Raises {!Reserved_symbol} if [name] is [div]/[mod]. *)
val declare_sort : t -> string -> Symbol.t

(** [declare_fun t name rank] interns [name] and records its rank. Re-declaring a
    (non-reserved) name overwrites the rank. Raises {!Reserved_symbol} if [name] is
    [div]/[mod]. *)
val declare_fun : t -> string -> Rank.t -> Symbol.t

(** [rank t sym] is the recorded rank of [sym]. Raises [Not_found] if [sym] has no rank in
    [t] (e.g. an undeclared symbol or a sort symbol). {!Context.app} turns this into a
    [Term.Sort_error]. *)
val rank : t -> Symbol.t -> Rank.t

(** The reserved [div]/[mod] symbols, ranks [(Int, Int) -> Int]. Deviation from the ADR's
    four-function [Env] sketch: exposed so {!Context} can build [div]/[mod] applications
    without re-interning by name. *)
val div_sym : t -> Symbol.t

val mod_sym : t -> Symbol.t

=== smt/core/rank.mli ===

(** The signature of an uninterpreted function symbol (ADR-0003 Decision 6): argument
    sorts and result sort. A predicate is a symbol whose [codomain] is [Sort.bool]; a
    nullary constant or program variable has an empty [domain]. Ranks live in {!Env},
    keyed by symbol. *)

type t =
  { domain : Sort.t Iarr.t
  ; codomain : Sort.t
  }

val create : Sort.t list -> Sort.t -> t
val arity : t -> int

=== smt/core/theory_view.mli ===

(** How the solver reads a term for theory dispatch (ADR-0003 Decision 2). The [App] vs
    [Arith]/[Le] split is the load-bearing signal: EUF congruence-closes only [App];
    [Arith]/[Le] are opaque leaves owned by LIA. *)

type atom =
  | Equality of Term.t * Term.t (* non-Bool Eq: uninterpreted / shared equality *)
  | Le_zero of Term.t (* LIA: [term <= 0] *)
  | Predicate of Symbol.t * Term.t Iarr.t (* Bool-codomain App *)
  | Bool_lit of bool

(** [is_atom t] is the frozen Decision-2 predicate: for a Bool-sorted [t], true unless
    [top(t)] is [And]/[Or]/[Not], a result-Bool [Ite], or an [Eq] whose {e arguments} are
    Bool-sorted (a disguised iff — a connective the clausifier descends into, never an
    opaque EUF atom). Non-Bool terms are not atoms. *)
val is_atom : Term.t -> bool

(** [atom t] classifies an atom; requires [is_atom t]. *)
val atom : Term.t -> atom

(** [is_app t] holds for [App] nodes — the terms EUF congruence applies to. *)
val is_app : Term.t -> bool

(** [linear t] is the linear form when [t] is an [Arith] node, else [None]. *)
val linear : Term.t -> Term.linear option

=== smt/core/atom.mli ===

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

=== smt/core/lit.mli ===

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

=== smt/core/explanation.mli ===

(** The uniform reason currency (DESIGN.md §7; INVARIANTS.md I4): every derived fact — a
    theory propagation or a theory conflict — is justified by a
    {b premise set + rule tag}. The engine turns a conflict's premises into the learned
    clause and resolves them against the trail for 1UIP and selector-based unsat cores
    (§7).

    Frozen at the M1 THEORY freeze (ADR-0005 Tranche A). Pure signature — no
    implementation module. *)

(** A certificate-shaped classifier of a derived fact. {b Payload-free, permanently}
    (ADR-0006): the M5 certificate witnesses (Farkas vectors, congruence chains) live in
    the off-core [smt/certificate/] module, never as a tag payload — this keeps LIA's
    rational type off the frozen core on the hot 1UIP path (I3). A future theory may add
    {e new constructors} (e.g. datatype rules); that is an additive enum unfreeze,
    orthogonal to the no-payload rule. *)
module Rule_tag : sig
  type t =
    | Trivial (** a tautology / constant-folded fact *)
    | Euf_congruence (** EUF: a proof-forest transitivity + congruence chain *)
    | Lia_bound (** LIA: a simplex bound propagation *)
    | Lia_farkas (** LIA: an infeasible row, Farkas-certified *)
    | Lia_branch (** LIA: a branch-and-bound case split *)
    | Shared_eq
    (** Nelson–Oppen: an equality entailed in one theory, replayed in another *)
end

(** The premises are asserted theory literals currently true on the trail; their
    conjunction T-entails the explained fact (is T-unsat, for a conflict). For a
    propagated literal they are {b precedence-valid} (ADR-0005 CONTRACT-EX: each assigned
    strictly before the propagated literal) and in deterministic order (C2). *)
type t =
  { premises : Lit.t list
  ; rule : Rule_tag.t
  }

=== smt/core/theory.mli ===

(** The frozen THEORY plugin signature (ADR-0005) — the seam the CDCL(T) engine (M1/M4)
    drives and that EUF (M2) and LIA (M3) implement, through a thin adapter over their
    [Term]/[Context]-facing engines. Nelson–Oppen combination (M4) is itself a THEORY:
    [Combine (A) (B)] presents one THEORY to the engine (functor packaging à la Alt-Ergo
    [CC(X)]; engine-observable semantics are Z3's model-based combination).

    All shared vocabulary ([Atom]/[Lit]/[Explanation]/[Model]) lives in [core] because the
    module DAG forbids [theories → solver]. Frozen at the M1 THEORY freeze (ADR-0005
    Tranche A). Pure signature — no implementation module.

    {b Determinism (INVARIANTS.md I6, ADR-0005 C1–C8)} and the soundness contracts
    CONTRACT-EX (precedence-valid explanations), CONTRACT-SPLIT (a [Split] is a clausified
    disjunction over ≥2 distinct atoms), CONTRACT-MODEL, and CONTRACT-POISON (an
    {e engine} obligation: any exception escaping a THEORY op bricks the instance and
    degrades the query to [unknown], I8) are stated in the ADR; they are discipline on the
    caller/implementer, not encoded in this signature. *)

(** The effort of a {!THEORY.check}. *)
type effort =
  | Propagate
  (** cheap, in-search: theory propagation + fast inconsistency; never returns [Sat] or
      [Split]. *)
  | Final
  (** the SAT core has a full boolean model: the theory must be complete (LIA
      branch-and-bound for integrality; model-based Nelson–Oppen). *)

(** The result of a {!THEORY.check}. *)
type check_result =
  | Sat (** [Final] only: the theory certifies this assignment T-satisfiable. *)
  | Propagations of Lit.t list
  (** consistent so far; these literals are T-implied (lazy explanation via
      {!THEORY.explain}), in deterministic order (C1). *)
  | Conflict of Explanation.t (** the asserted set is T-inconsistent. *)
  | Split of Term.t list
  (** [Final] only: clausify each term to a literal and assert their {b disjunction} as
      one clause (ADR-0005 CONTRACT-SPLIT) — a B&B branch, an N-O ℤ-trichotomy, or an
      E-matching lemma. Must force a choice among ≥2 distinct atoms. *)

module type THEORY = sig
  type t

  (** [create ctx env] is an empty theory state bound to the session [Context] (ADR-0003
      D6): every term the theory builds mid-solve (a [Split] disjunct) goes through [ctx],
      sharing its tag stream and hash-consing (I6). *)
  val create : Context.t -> Env.t -> t

  (** The sole point a theory receives a [Term.t] (ADR-0005 CONTRACT-REG-1/2). Called as
      the clausifier internalizes each theory atom and for atoms minted from a {!Split}.
      The theory walks the term for subterms, indexes them by [Term] tag, and builds its
      structure (EUF: e-graph; LIA: bound/row). Idempotent (C7). *)
  val register_atom : t -> Atom.t -> Term.t -> unit

  (** Assert a signed literal (its atom is registered). Cheap incremental state update, no
      output; consistency/propagation are deferred to {!check}. Asserted in the current
      frame (see {!push}/{!pop}). *)
  val assert_lit : t -> Lit.t -> unit

  (** Theory reasoning over the currently-asserted literals; see {!check_result}.
      Propagations are returned in deterministic order (C1). [Sat]/[Split] are legal only
      at [Final]. *)
  val check : t -> effort -> check_result

  (** The premises + rule tag justifying a literal THIS theory propagated (a
      {!Propagations} element). Lazy but always available (§7), and precedence-valid
      (ADR-0005 CONTRACT-EX: every premise was assigned strictly before [lit] on the
      trail). Deterministic (C2). *)
  val explain : t -> Lit.t -> Explanation.t

  (** [push t] opens a backtrack frame; [pop t n] discards the last [n], restoring state
      to that checkpoint. A frame is opened at each SAT decision level and each user
      assertion frame, so assert-after-check and incremental push/pop are first-class
      (ADR-0005 D6). *)
  val push : t -> unit

  val pop : t -> int -> unit

  (** A candidate model, valid whenever the last {!check} was consistent — a complete,
      integer-valued, N-O-agreed model after [Final]→[Sat] (ADR-0005 CONTRACT-MODEL). Used
      by [Combine] for model-based combination (§6) and by the §8 sat evaluator. *)
  val model : t -> Model.t
end

