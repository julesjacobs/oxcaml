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

(** Arbitrary-precision entry points (core-bignum W2): identical to {!int_const} /
    {!mul_const} / {!linear_combination} but taking {!Bigint.t} coefficients/constants, so
    a literal or coefficient exceeding int63 is admitted without loss (the [int] variants
    above are exactly these with a [Bigint.of_int] widen). Used by the parser for >2^63
    numerals and by presolve when rebuilding a substituted linear term. *)
val int_const_big : t -> Bigint.t -> Term.t

val real_const_big : t -> num:Bigint.t -> den:Bigint.t -> Term.t

val mul_const_big : t -> Bigint.t -> Term.t -> Term.t

val mul_real_const_big :
  t -> num:Bigint.t -> den:Bigint.t -> Term.t -> Term.t

val linear_combination_big : t -> (Bigint.t * Term.t) list -> Bigint.t -> Term.t

val real_linear_combination_big :
  t -> (Term.rational * Term.t) list -> Term.rational -> Term.t

(** [div t x d] / [mod_ t x d]: [d] must be a nonzero [Int_const], else
    {!Term.Unsupported} (a documented v1 limitation). Built on the reserved [div]/[mod]
    symbols, eliminated by a later pass. *)
val div : t -> Term.t -> Term.t -> Term.t

val mod_ : t -> Term.t -> Term.t -> Term.t

(** [abs t x] desugars to [Ite(x >= 0, x, -x)] at construction. *)
val abs : t -> Term.t -> Term.t

val eq : t -> Term.t -> Term.t -> Term.t

(** [le]/[lt]/[ge]/[gt] lower to a single [Le] atom. Integer comparisons retain their
    gcd/ceil normalization; Real comparisons preserve their exact rational bound. *)
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
