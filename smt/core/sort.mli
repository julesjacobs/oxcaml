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
    {!Datatype_defs} registry, keyed by this same symbol.

    [Array (index, element)] is a (functional, extensional) array sort (GOALS-adjacent
    arrays lane): a total map from [index] to [element]. Like [Datatype] it is a
    {e distinct} variant so every [Sort.t] match goes non-exhaustive until it routes array
    terms to the arrays theory (treating an array as uninterpreted would let the
    combinator report [Sat] with the select/store/extensionality axioms never firing — a
    wrong-[Sat]). It is the sole {e recursive} sort, so [equal]/[hash] here recurse over
    the (small, parse-bounded) sort tree rather than being strictly O(1); array sorts are
    shallow in practice. The [select]/[store] operator symbols are minted per (index,
    element) instantiation and recorded in the non-frozen {!Array_defs} registry. *)

type t = private
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t
  | Datatype of Symbol.t
  | Array of t * t

and int_kind = Mathematical

val bool : t
val int : t
val uninterpreted : Symbol.t -> t

(** [datatype_ sym] is the datatype sort declared under [sym]. Identity is [sym], so
    [equal]/[hash] stay O(1). The shape is registered separately in {!Datatype_defs}. *)
val datatype_ : Symbol.t -> t

(** [array_ ~index ~element] is the array sort mapping [index] to [element]. Structural
    identity; the [select]/[store] symbols over it are recorded in {!Array_defs}. *)
val array_ : index:t -> element:t -> t

val equal : t -> t -> bool
val hash : t -> int
