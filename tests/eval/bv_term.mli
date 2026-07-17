open Oxsmt_core

(** The independent evaluator's private bit-vector term vocabulary.  These descriptors
    are intentionally separate from {!Oxsmt_core.Bv}: the evaluator reads the original
    SMT-LIB formula into its own reserved symbols and derives their meaning here, without
    consulting the solver's bit-vector classifier or evaluator. *)

type op =
  | Not
  | And
  | Or
  | Xor
  | Neg
  | Add
  | Sub
  | Mul
  | Udiv
  | Urem
  | Shl
  | Lshr
  | Ashr
  | Ult
  | Ule
  | Ugt
  | Uge
  | Slt
  | Sle
  | Sgt
  | Sge
  | Sdiv
  | Srem
  | Smod
  | Comp
  | Nand
  | Nor
  | Xnor
  | Concat
  | Extract of int * int
  | Zero_extend of int
  | Sign_extend of int
  | Rotate_left of int
  | Rotate_right of int
  | Repeat of int

type view =
  | Const of
      { bits : Bigint.t
      ; width : int
      }
  | Op of
      { op : op
      ; args : Term.t list
      }

(** A narrowed reserved-symbol constructor, normally
    [Env.declare_reserved cap env]. *)
type minter = string -> Rank.t -> Symbol.t

(** The evaluator-private reserved namespace. *)
val is_name : string -> bool

(** Decode an evaluator bit-vector term.  Every decoded field is checked against the
    term's actual arity, operand sorts, and result sort.  A malformed or mis-ranked marker
    returns [None], never a partial interpretation. *)
val view : Term.t -> view option

val op_name : op -> string

(** Source literals are reduced modulo [2^width], as SMT-LIB bit-vector literals are.
    [width] must be positive. *)
val const : Context.t -> minter -> bits:Bigint.t -> width:int -> Term.t

(** [unop] accepts [Not] and [Neg]. *)
val unop : Context.t -> minter -> op -> Term.t -> Term.t

(** [binop] accepts every equal-width binary operator above except [Concat] and the
    indexed unary operators.  Comparisons return [Bool], [Comp] returns [BitVec 1], and
    all other accepted operators return the operands' width. *)
val binop : Context.t -> minter -> op -> Term.t -> Term.t -> Term.t

val concat : Context.t -> minter -> Term.t -> Term.t -> Term.t
val extract : Context.t -> minter -> i:int -> j:int -> Term.t -> Term.t
val zero_extend : Context.t -> minter -> n:int -> Term.t -> Term.t
val sign_extend : Context.t -> minter -> n:int -> Term.t -> Term.t
val rotate_left : Context.t -> minter -> n:int -> Term.t -> Term.t
val rotate_right : Context.t -> minter -> n:int -> Term.t -> Term.t
val repeat : Context.t -> minter -> n:int -> Term.t -> Term.t
