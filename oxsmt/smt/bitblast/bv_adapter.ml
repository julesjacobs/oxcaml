open Oxsmt_core

let op_of_bv : Bv.op -> Bv_op.t = function
  | Bv.Bvnot -> Bv_op.Not
  | Bv.Bvand -> Bv_op.And
  | Bv.Bvor -> Bv_op.Or
  | Bv.Bvxor -> Bv_op.Xor
  | Bv.Bvneg -> Bv_op.Neg
  | Bv.Bvadd -> Bv_op.Add
  | Bv.Bvsub -> Bv_op.Sub
  | Bv.Bvmul -> Bv_op.Mul
  | Bv.Bvudiv -> Bv_op.Udiv
  | Bv.Bvurem -> Bv_op.Urem
  | Bv.Bvshl -> Bv_op.Shl
  | Bv.Bvlshr -> Bv_op.Lshr
  | Bv.Bvashr -> Bv_op.Ashr
  | Bv.Bvult -> Bv_op.Ult
  | Bv.Bvule -> Bv_op.Ule
  | Bv.Bvslt -> Bv_op.Slt
  | Bv.Bvsle -> Bv_op.Sle
  | Bv.Concat -> Bv_op.Concat
  | Bv.Extract (i, j) -> Bv_op.Extract { hi = i; lo = j }
  | Bv.Zero_extend n -> Bv_op.Zero_extend n
  | Bv.Sign_extend n -> Bv_op.Sign_extend n
;;

let defs : Blast.defs =
  { Blast.classify =
      (fun term ->
        match Bv.view term with
        | None -> None
        | Some (Bv.Const { value; width }) -> Some (Blast.Const (value, width))
        | Some (Bv.Op { op; args; result_width }) ->
          Some (Blast.Op (op_of_bv op, args, result_width)))
  ; width_of_sort = Bv.width_of_sort
  }
;;
