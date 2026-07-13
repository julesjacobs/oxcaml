(** Sorts (ADR-0003 Decision 6). [private] variant: deep matching is allowed, construction
    goes through the smart constructors so [equal]/[hash] stay O(1). [int_kind] is the §1
    width hook; v1 has only [Mathematical] (unbounded ℤ). Uninterpreted sorts are 0-arity
    in v1.

    [Datatype] is an algebraic-datatype sort (GOALS: Datatypes), identified by the sort
    symbol it was declared under. It is a {e distinct} variant, not a flavour of
    [Uninterpreted], on purpose: every [Sort.t] match becomes non-exhaustive until it
    handles datatypes, so the compiler surfaces every consumer that must route a datatype
    term to the datatype theory. Treating a datatype as an uninterpreted sort would let
    the combinator hand it an opaque [Model.Uninterp] value and report [Sat] without the
    datatype axioms ever firing — a wrong-[Sat]. The sort carries only the identity; the
    datatype's shape (constructors, selectors, testers) lives in the non-frozen
    {!Datatype_defs} registry, keyed by this same symbol. *)

type t = private
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t
  | Datatype of Symbol.t

and int_kind = Mathematical

val bool : t
val int : t
val uninterpreted : Symbol.t -> t

(** [datatype_ sym] is the datatype sort declared under [sym]. Identity is [sym], so
    [equal]/[hash] stay O(1). The shape is registered separately in {!Datatype_defs}. *)
val datatype_ : Symbol.t -> t

val equal : t -> t -> bool
val hash : t -> int
