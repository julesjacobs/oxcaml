module S = Vox_sequence
module L = Vox_table_model_proofs

module Make (Key : Vox_table_map.Key)
    (I : module type of Vox_table_invariant.Make (Key)) = struct
  let[@def] (occupied @ total) (entry : (Key.t * 'a) option @ immutable) (byte
    : int) =
    match entry with None -> byte = 254 | Some _ -> byte <> 254

  let (index_step @ total) (count : int) (index : int) :
      {u : unit | not (0 < count && count <= 1073741824 &&
        0 <= index && index < count - 1) ||
        0 <= index + 1 && index + 1 < count &&
        Bigint.of_int (index + 1) = Bigint.add (Bigint.of_int index) 1Z} @
          ghost =
    ghost_ ()

  let rec (full_count @ total) : ('a : immutable_data).
      (slots : 'a I.Map.slots) @ immutable -> (controls : int list) ->
      (count : {n : int | 0 <= n && n <= 1073741824 &&
        S.length slots = Bigint.of_int n}) ->
      ((index : int) -> {u : unit | not (0 <= index && index < count) ||
        (match S.at slots (Bigint.of_int index), S.at controls (Bigint.of_int
          index) with
         | Some entry, Some byte -> occupied entry byte | _ -> false)} @
           ghost) @ total ->
      {u : unit | Bigint.add (I.live_count slots) (I.deleted_count controls
        count) =
        Bigint.of_int count} @ ghost = fun slots controls count proof ->
          ghost_ (
    S.length_def slots;
    I.live_count_def slots; I.deleted_count_def controls count;
    proof 0; S.at_def slots 0Z; S.at_def controls 0Z;
    match slots, controls with
    | entry :: tail, byte :: rest ->
      L.length_nonnegative tail;
      occupied_def entry byte;
      full_count tail rest (count - 1) (fun index -> ghost_ (
        if 0 <= index && index < count - 1 then begin
          index_step count index;
          proof (index + 1);
          S.at_def slots (Bigint.of_int (index + 1));
          S.at_def controls (Bigint.of_int (index + 1));
          ()
        end else ()));
      ()
    | _ -> ())
  let (reserve_positive @ total) (capacity : int) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824) ||
        0 < I.reserve capacity && I.reserve capacity <= capacity} @ ghost =
    ghost_ (I.reserve_def capacity; ())

  let (not_full @ total) (capacity : int) (size : int) (deleted : int)
    (reserve : int) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        0 <= size && size <= capacity && 0 <= deleted && deleted <= capacity -
          size &&
        0 < reserve && reserve <= capacity && size + deleted <= capacity -
          reserve) ||
        Bigint.add (Bigint.of_int size) (Bigint.of_int deleted) <>
          Bigint.of_int capacity}
      @ ghost = ghost_ ()

end
