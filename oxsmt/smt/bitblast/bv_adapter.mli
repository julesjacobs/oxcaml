(** The bridge from bv-front's term vocabulary ({!Oxsmt_core.Bv}) to the bit-blaster's
    circuit-library classifier ({!Blast.defs}). A flat map from [Bv.op] to {!Bv_op.t} plus
    passthroughs of [Bv.view] and [Bv.width_of_sort] — no reshaping, because the blaster's
    [classify] view was defined to mirror [Bv.view]. This is the single point of contact
    between the two halves of the bit-vector lane. *)

open Oxsmt_core

val op_of_bv : Bv.op -> Bv_op.t

(** The classifier the QF_BV solve driver passes to {!Blast}/{!Bv_solve}. *)
val defs : Blast.defs
