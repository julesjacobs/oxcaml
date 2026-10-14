module Term = Vox_smt_term

type t

exception Unsupported of string

(** [int_bridge source bits] relates an integer source to its low bits. [bits]
    uses signed SAT literals, least significant bit first. The bridge must
    enforce integer equality modulo [2 ** Array.length bits]. *)
val create :
  ?int_bridge:(Term.term -> int array -> unit) ->
  ?poll:(unit -> unit) ->
  terms:Term.t ->
  fresh:(unit -> int) ->
  clause:(int array -> unit) ->
  boolean:(Term.term -> int) ->
  unit ->
  t

(** Returned arrays are shared and must not be mutated. Unknown builtins raise
    [Unsupported]; uninterpreted applications receive fresh bits and require
    congruence constraints from the caller. Indexed operators use names such as
    [extract:7:0], [zero_extend:8], and [rotate_left:3]. *)
val bits : t -> Term.term -> int array

(** Bitvector equality and signed/unsigned comparisons. Unsupported predicate
    names return [None]; malformed operands raise [Invalid_argument]. *)
val predicate : t -> string -> Term.term array -> int option
