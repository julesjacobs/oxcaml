(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int = Numbers.Int

module Id = struct
  include Int

  let flags_size_in_bits = 3

  let flags_shift = Sys.int_size - flags_size_in_bits

  let mask_selecting_bottom_bits = -1 lsr flags_size_in_bits

  let create t flags =
    if flags < 0 || flags >= 1 lsl (flags_size_in_bits + 1)
    then Misc.fatal_errorf "Flags value 0x%x out of range" flags;
    t land mask_selecting_bottom_bits lor (flags lsl flags_shift)

  let flags t = t lsr flags_shift
end

module Make (E : sig
  type t

  val flags : int

  val print : Format.formatter -> t -> unit

  val hash : t -> int

  val equal : t -> t -> bool
end) =
struct
  module HT = Hashtbl.Make (struct
    type t = int

    (* Keep this hash unchanged: serialized tables contain buckets populated by
       it, including tables produced before ids were allocated densely. *)
    let hash (t : t) =
      let mixed = t lxor (t lsr 31) in
      let h = mixed * 0x27d4eb2f in
      h lxor (h lsr 29)

    let equal t1 t2 = t1 == t2
  end)

  let () = assert (E.flags lsr Id.flags_size_in_bits = 0)

  type t =
    { mutable slots : int array;
          (* Slots contain a value index plus one; zero marks an empty slot. *)
      mutable values : Obj.t array;
      mutable length : int
    }

  let create () = { slots = [| 0 |]; values = [||]; length = 0 }

  (* [values] is never a flat-float array, and [index] is in its initialized
     prefix. The option-array view selects an address-array load; its result
     remains an arbitrary OCaml value. *)
  external address_array_get : Obj.t option array -> int -> Obj.t
    = "%array_unsafe_get"

  let[@inline always] get_value (values : Obj.t array) index : E.t =
    Obj.obj (address_array_get (Obj.magic values) index)

  let find_slot t elt hash =
    let mask = Array.length t.slots - 1 in
    let rec loop slot =
      let index = Array.unsafe_get t.slots slot in
      if index = 0 || E.equal (get_value t.values (index - 1)) elt
      then slot
      else loop ((slot + 1) land mask)
    in
    loop (hash land mask)

  let empty_slot slots hash =
    let mask = Array.length slots - 1 in
    let rec loop slot =
      if Array.unsafe_get slots slot = 0
      then slot
      else loop ((slot + 1) land mask)
    in
    loop (hash land mask)

  let grow t elt =
    if t.length > Sys.max_array_length / 4
    then Misc.fatal_errorf "No ids left for@ %a" E.print elt;
    let capacity = if t.length = 0 then 16 else 2 * t.length in
    (* Initializing with an integer keeps floats boxed and preserves physical
       identity. Only the first [length] entries contain values of type
       [E.t]. *)
    let values = Array.make capacity (Obj.repr 0) in
    Array.blit t.values 0 values 0 t.length;
    let slots = Array.make (2 * capacity) 0 in
    for index = 0 to t.length - 1 do
      let slot = empty_slot slots (E.hash (get_value values index)) in
      slots.(slot) <- index + 1
    done;
    t.values <- values;
    t.slots <- slots

  let add t elt =
    let hash = E.hash elt in
    let slot = find_slot t elt hash in
    let index = Array.unsafe_get t.slots slot in
    if index <> 0
    then Id.create (index - 1) E.flags
    else
      let index = t.length in
      let slot =
        if index = Array.length t.values
        then (
          grow t elt;
          empty_slot t.slots hash)
        else slot
      in
      t.values.(index) <- Obj.repr elt;
      t.slots.(slot) <- index + 1;
      t.length <- index + 1;
      Id.create index E.flags

  let find t id =
    assert (Id.flags id = E.flags);
    let index = id land Id.mask_selecting_bottom_bits in
    if index >= t.length then raise Not_found;
    get_value t.values index

  (* Keep the serialized representation independent of the dense in-memory
     table, since imported ids belong to the exporting compilation unit. *)
  type serializable = E.t HT.t

  let export t ~iter =
    let exported = HT.create 0 in
    iter (fun id -> HT.replace exported id (find t id));
    exported

  let import t id =
    assert (Id.flags id = E.flags);
    try HT.find t id
    with Not_found ->
      Misc.fatal_error "Id was not exported from this compilation unit."
end
