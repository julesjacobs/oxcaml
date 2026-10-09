(* The bit-blaster's canonical operator vocabulary. This is the blaster's OWN internal
   form: the SAT-facing circuit library ({!Blast}) matches on it, and it is what
   {!Blast.defs} classifies an [App] symbol into.

   bv-front's parser/registry ({!Bitvec_defs}, landing in oxsmt_core) has its own [bv_op]
   variant; on rebase a one-function adapter maps that variant onto this one, so this
   module and the whole circuit library are unaffected by the exact registry
   representation. Keeping the variant here (not in the frozen core) is what lets the two
   builders develop against separate copies and reconcile with a single flat match. *)

type t =
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
  | Concat
  | Extract of
      { hi : int
      ; lo : int
      }
  | Zero_extend of int (* number of zero bits added at the high end *)
  | Sign_extend of int (* number of sign-copy bits added at the high end *)
  | Ult
  | Ule
  | Ugt
  | Uge
  | Slt
  | Sle
  | Sgt
  | Sge

let to_string = function
  | Not -> "bvnot"
  | And -> "bvand"
  | Or -> "bvor"
  | Xor -> "bvxor"
  | Neg -> "bvneg"
  | Add -> "bvadd"
  | Sub -> "bvsub"
  | Mul -> "bvmul"
  | Udiv -> "bvudiv"
  | Urem -> "bvurem"
  | Shl -> "bvshl"
  | Lshr -> "bvlshr"
  | Ashr -> "bvashr"
  | Concat -> "concat"
  | Extract { hi; lo } -> Printf.sprintf "extract[%d:%d]" hi lo
  | Zero_extend k -> Printf.sprintf "zero_extend[%d]" k
  | Sign_extend k -> Printf.sprintf "sign_extend[%d]" k
  | Ult -> "bvult"
  | Ule -> "bvule"
  | Ugt -> "bvugt"
  | Uge -> "bvuge"
  | Slt -> "bvslt"
  | Sle -> "bvsle"
  | Sgt -> "bvsgt"
  | Sge -> "bvsge"
;;
