type sort =
  | Bool
  | Int63

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

(** Arithmetic wraps modulo [2^63]; comparisons use signed order. General
    multiplication is uninterpreted. *)
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

type term =
  | Boolean of bool
  | Integer of int64
  | Var of Symbol.t
  | App of op * term list
  | Call of Function.t * term list

type labelled_term =
  { label : string;
    term : term
  }

type query =
  { symbols : Symbol.t list;
    functions : Function.t list;
    facts : labelled_term list;
    goal : labelled_term
  }

(** A compiler invariant failure, not an unproved goal. *)
exception Sort_error of string

exception Unsupported_target of int

(** Result sort of a well-sorted term; does not validate operands or
    declarations. Only conditional result branches need traversal. Use [check]
    for validation. *)
val term_sort : term -> sort

(** [int_width] is the target's OCaml integer width, not the host width. Only 63
    is supported. All operators have fixed arity: one for [Neg] and [Not], three
    for [Ite], and two otherwise. Constants must be signed 63-bit integers.
    Undeclared and duplicate symbols are errors. *)
val check : int_width:int -> query -> unit

(** Always checks sorts first. Names [v0], [v1], ... follow declaration order.
    Includes options, declarations, assertions and [check-sat], but not [exit].
    No quantifiers can be represented. Machine integers are bounded SMT
    integers. Addition, subtraction, negation, division, and remainder have
    exact signed 63-bit semantics; multiplication is a shared uninterpreted
    function. Callers must exclude zero divisors when modeling OCaml normal
    returns. *)
val to_smtlib : int_width:int -> timeout_ms:int -> query -> string

(** Integer model values are signed, including on a narrower host. *)
type value =
  | Bool_value of bool
  | Int_value of int64

type validity =
  | Valid
  | Invalid of (Symbol.t * value) list option
  | Unknown of string option
  | Timeout
  | Failure of string
