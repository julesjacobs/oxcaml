(* Refinement-stripped twin of set_group/set_intf.ml.

   Every refinement type is reduced to its carrier and every mode annotation is dropped.
   Nothing else changes: the value names, their arities and their labels are those of the
   original, so a module that satisfies the original signature satisfies this one after
   the same stripping. *)
module type SET = sig
  type t

  val empty : t
  val insert : int -> t -> t
  val member : int -> t -> bool
  val equal : t -> t -> bool
  val empty_law : query:int -> unit
  val insert_law : inserted:int -> tree:t -> query:int -> unit
  val equal_forward_law : t1:t -> t2:t -> equal_trees:unit -> query:int -> unit
  val equal_backward_law : t1:t -> t2:t -> pointwise:(query:int -> unit) -> unit
end
