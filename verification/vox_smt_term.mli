module Z = Vox_smt_integer.Z

type sort = Bool | Int | Bitvec of int | Named of int
type symbol = int
type term = int
type node =
  | Boolean of bool
  | Integer of Z.t
  | Bits of int * Z.t
  | Apply of symbol * term array

type symbol_kind =
  | Uninterpreted
  | Builtin of string
  | Constructor of int
  | Selector of symbol * int
  | Tester of symbol

type signature =
  { name : string;
    arguments : sort array;
    result : sort;
    kind : symbol_kind
  }

type t
val create : unit -> t

(** Poll while constructing and simplifying terms during [f]. Restore the
    previous callback on return or exception. Callbacks must not mutate [t]. *)
val with_poll : t -> (unit -> unit) -> (unit -> 'a) -> 'a

(** Discard newly allocated terms and symbols after [f] returns or raises.
    Their IDs are valid only while [f] runs. *)
val with_temporary : t -> (unit -> 'a) -> 'a

val symbol : t -> signature -> symbol
val signature : t -> symbol -> signature
val has_datatypes : t -> bool
val constructors : t -> int -> symbol list
val selector : t -> symbol -> int -> symbol
val tester : t -> symbol -> symbol
val node : t -> term -> node
val sort : t -> term -> sort
val count : t -> int
val boolean : t -> bool -> term
val integer : t -> Z.t -> term
val bits : t -> int -> Z.t -> term
val apply : t -> symbol -> term array -> term
val builtin : t -> string -> sort -> term array -> term
val equal : t -> term -> term -> term

(** Equality for a theory explanation or model repair. Preserve
    non-reflexive equalities as atoms to link independently encoded terms. *)
val equal_atom : t -> term -> term -> term
val not_ : t -> term -> term
val and_ : t -> term array -> term
val or_ : t -> term array -> term
val ite : t -> term -> term -> term -> term
val to_string : t -> term -> string

(** Borrowed internal arrays. Mutating them invalidates the term arena and
    every solver using it. The ordinary accessors return independent arrays. *)
module Unsafe : sig
  val node : t -> term -> node
  val signature : t -> symbol -> signature
end
