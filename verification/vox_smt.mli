type datatype

type sort =
  | Bool
  | Bv63
  | Int
  | Opaque of int
  | Datatype of datatype

module Datatype : sig
  type t = datatype

  val create : label:string -> t

  val label : t -> string
end

type constructor

module Constructor : sig
  type t = constructor

  val create : datatype:Datatype.t -> label:string -> (string * sort) list -> t

  val label : t -> string

  val datatype : t -> Datatype.t

  val fields : t -> (string * sort) list
end

type datatype_declaration =
  { datatype : Datatype.t;
    constructors : Constructor.t list
  }

val datatypes_well_founded : datatype_declaration list -> bool

module Symbol : sig
  type t

  (** Fresh identity; [label] is diagnostic metadata, never SMT-LIB syntax. The
      caller maps resolved source identities to these symbols. *)
  val create : label:string -> sort -> t

  val label : t -> string

  val sort : t -> sort
end

module Function : sig
  type t

  val create : label:string -> arguments:sort list -> result:sort -> t

  val label : t -> string

  val arguments : t -> sort list

  val result : t -> sort
end

(** Unprefixed arithmetic wraps modulo [2^63] and uses signed order. [Int_*]
    operations use unbounded integers. *)
type op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Not
  | And
  | Or
  | Implies
  | Ite
  | Int_add
  | Int_sub
  | Int_mul
  | Int_div
  | Int_mod
  | Int_neg
  | Int_lt
  | Int_le
  | Int_gt
  | Int_ge
  | Int_of_bv63

type term =
  | Boolean of bool
  | Integer of int64
  | Big_integer of string
  | Var of Symbol.t
  | App of op * term list
  | Call of Function.t * term list
  | Construct of Constructor.t * term list
  | Is of Constructor.t * term
  | Select of Constructor.t * int * term

type labelled_term =
  { label : string;
    term : term
  }

type query =
  { datatypes : datatype_declaration list;
    symbols : Symbol.t list;
    functions : Function.t list;
    facts : labelled_term list;
    goal : labelled_term
  }

(** A compiler invariant failure, not an unproved goal. *)
exception Sort_error of string

exception Unsupported_target of int

(** Canonical signed decimal text: no leading zeroes, plus sign, or [-0]. *)
val decimal_integer : string -> bool

(** Result sort of a well-sorted term; does not validate operands or
    declarations. Only conditional result branches need traversal. Use [check]
    for validation. *)
val term_sort : term -> sort

(** [int_width] is the target's OCaml integer width, not the host width. Only 63
    is supported. Operators have fixed arity. [Integer] constants must be signed
    63-bit integers; [Big_integer] constants use canonical decimal text.
    Undeclared and duplicate symbols are errors. *)
val check : int_width:int -> query -> unit

(** Always checks sorts first. Names [v0], [v1], ... follow declaration order.
    Includes options, declarations, assertions and [check-sat], but not [exit].
    Queries using [Int], opaque sorts, or datatypes select ALL; other queries
    use QF_UFBV for uninterpreted functions and QF_BV otherwise. No quantifiers
    can be represented. [Div]/[Rem] use signed bitvector semantics; callers must
    exclude zero divisors when modeling OCaml normal returns.
    [Int_div]/[Int_mod] use Euclidean semantics; callers must supply the
    zero-divisor behavior. *)
val to_smtlib : int_width:int -> timeout_ms:int -> query -> string

(** Integer model values are signed, including on a narrower host. *)
type value =
  | Bool_value of bool
  | Int_value of int64
  | Bigint_value of string

type validity =
  | Valid
  | Invalid of (Symbol.t * value) list option
  | Unknown of string option
  | Timeout
  | Failure of string
