(** Hash-consed terms (ADR-0003 Decision 5). The node set is frozen. The type is
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
    Bool-sorted nodes (Decision 2). Integer and Real arithmetic use normalized [Arith]
    and [Real_arith] linear forms. Order comparisons lower to one [Le] atom
    ([arg <= 0]); only the integer form is gcd/ceil-tightened (Decision 1). Nullary
    constants and program variables are [App(sym, [])].

    {b Arbitrary-precision integers (core-bignum W2, GOALS solve-rate b2).} Integer
    literals and linear coefficients/constants are {!Bigint.t}, not native [int], so a
    coefficient of any size (e.g. the >2^63 literals in the QF_LIA convert family) is
    representable and term construction never overflows. The native-[int] precision
    boundary that remains is downstream, at the model / branch-and-bound int-projection
    sinks, which degrade to [unknown] (never wrap). *)

type rational = Node.rational = private
  { num : Bigint.t
  ; den : Bigint.t
  }

type t = Node.t = private
  { node : node
  ; sort : Sort.t
  ; tag : int (* hash-cons identity *)
  }

and node = Node.node = private
  | Bool_const of bool
  | Int_const of Bigint.t (* arbitrary precision *)
  | App of Symbol.t * t Iarr.t (* EUF-congruent; nullary = constant *)
  | Arith of linear (* Int; LIA leaf to EUF *)
  | Le of t (* (arg <= 0), arg : Int or Real; Int is gcd-normalized *)
  | Eq of t * t (* same sort, tag-ordered; Bool = iff/connective *)
  | Not of t
  | And of t Iarr.t (* n-ary, >= 2 *)
  | Or of t Iarr.t (* n-ary, >= 2 *)
  | Ite of t * t * t (* cond Bool; branches share result sort *)
  | Real_const of rational
  | Real_arith of real_linear (* Real; LRA leaf to EUF *)

and linear = Node.linear = private
  { coeffs : (t * Bigint.t) Iarr.t (* tag-sorted, coeff <> 0, no Arith child *)
  ; const : Bigint.t
  }

and real_linear = Node.real_linear = private
  { coeffs : (t * rational) Iarr.t
        (* tag-sorted, coeff <> 0, Real children, no Real_arith child *)
  ; const : rational
  }

(** [rational_of_frac_big ~num ~den] is the unique reduced representation of [num/den].
    The denominator is positive, and zero is represented as [0/1].  Raises
    [Invalid_argument] when [den] is zero. *)
val rational_of_frac_big : num:Bigint.t -> den:Bigint.t -> rational

(** Retained from the ADR-0003 Overflow contract and still caught at the session layer
    (turned into verdict [unknown]), but
    {b no longer raised by any arithmetic constructor}: arithmetic is arbitrary-precision
    ({!Bigint}), so construction cannot overflow. The residual native-[int] precision
    boundary moved downstream to the model / branch-and-bound int-projection sinks
    (core-bignum W2 R1), which degrade to [unknown] and never wrap. *)
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
  (** [Construction] permits arithmetic-sorted [Ite] (it is removed by a later pass);
      [Pipeline] additionally forbids any Int/Real [Ite] and any residual reserved
      [div]/[mod] application (ADR-0003 invariant 10, checked on the preprocessed
      formula). *)
  type mode =
    | Construction
    | Pipeline

  (** [check ?mode ?env t] raises [Failure] on the first violated invariant. [env], when
      supplied, enables the [App]-rank check (arity + argument sorts
      + codomain); without it that check is skipped (rank matching is otherwise a
        construction-time guarantee of {!Context.app}). *)
  val check : ?mode:mode -> ?env:Env.t -> t -> unit
end
