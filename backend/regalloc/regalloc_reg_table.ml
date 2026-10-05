[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Misc.Or_null
module Array = ArrayLabels

(* Capped storage bounds memory; [Reg.same] distinguishes same-stamp aliases. *)
type 'a entry =
  { reg : Reg.t;
    mutable value : 'a;
    next : 'a entry Misc.Or_null.t
  }

type 'a t =
  { slots : 'a entry Misc.Or_null.t array;
    sparse : 'a Reg.Tbl.t
  }

let create size =
  let max_dense_capacity = 65536 in
  { slots = Array.make (max 1 (min size max_dense_capacity)) Null;
    sparse = Reg.Tbl.create 0
  }

let clear t =
  Array.fill t.slots ~pos:0 ~len:(Array.length t.slots) Null;
  Reg.Tbl.clear t.sparse

let[@inline] find t reg =
  let slot = Reg.Stamp.to_int reg.Reg.stamp in
  if slot < 0 || slot >= Array.length t.slots
  then Reg.Tbl.find t.sparse reg
  else
    let rec loop = function
      | Null -> raise Not_found
      | This entry ->
        if Reg.same reg entry.reg then entry.value else loop entry.next
    in
    loop t.slots.(slot)

let replace t reg value =
  let slot = Reg.Stamp.to_int reg.Reg.stamp in
  if slot < 0 || slot >= Array.length t.slots
  then Reg.Tbl.replace t.sparse reg value
  else
    let rec loop = function
      | Null -> t.slots.(slot) <- This { reg; value; next = t.slots.(slot) }
      | This entry ->
        if Reg.same reg entry.reg then entry.value <- value else loop entry.next
    in
    loop t.slots.(slot)

let fold f t init =
  let rec fold_entry acc = function
    | Null -> acc
    | This entry -> fold_entry (f entry.reg entry.value acc) entry.next
  in
  Reg.Tbl.fold f t.sparse (Array.fold_left t.slots ~init ~f:fold_entry)
