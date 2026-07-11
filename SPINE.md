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

