module M = Vox_table_model
module S = Vox_sequence
module L = Vox_table_model_proofs

module Make (Key : Vox_table_map.Key)
    (Invariant : module type of Vox_table_invariant.Make (Key)) = struct
  module Update = Vox_table_update_proofs.Make (Key) (Invariant)

  let[@def] (fingerprint @ total) (key : Key.t @ immutable) = Key.hash key
    land 127

  let (fingerprint_bounds @ total) (key : Key.t @ immutable) :
      {u : unit | 0 <= fingerprint key && fingerprint key <= 127} @ ghost =
    ghost_ (fingerprint_def key; ())

  let[@def] (insert_model @ total)
      (before : (Key.t, 'a) M.state @ immutable) (index : int)
      (key : Key.t @ immutable) (value : 'a @ immutable) (old_byte : int) =
    M.set_counts
      (M.set_byte (M.set_slot before index (Some (key, value))) index
        (fingerprint key))
      (before.size + 1) (before.deleted - (if old_byte = 254 then 1 else 0))

  let (fields @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable -> (old_byte :
        int) ->
      {u : unit | (insert_model before index key value old_byte).capacity =
          before.capacity &&
        (insert_model before index key value old_byte).size = before.size + 1 &&
        (insert_model before index key value old_byte).deleted =
          before.deleted - (if old_byte = 254 then 1 else 0) &&
        (insert_model before index key value old_byte).slots ===
          S.set before.slots (Bigint.of_int index) (Some (key, value)) &&
        (insert_model before index key value old_byte).controls ===
          (M.set_byte before index (fingerprint key)).controls} @ ghost =
    fun before index key value old_byte -> ghost_ (
      insert_model_def before index key value old_byte;
      M.set_slot_def before index (Some (key, value));
      let slots = M.set_slot before index (Some (key, value)) in
      let byte = fingerprint key in
      M.set_byte_def slots index byte; M.set_byte_def before index byte;
      M.set_control_def slots index byte; M.set_control_def before index byte;
      if index < 15 then begin
        M.set_control_def (M.set_control slots index byte) (before.capacity +
          index)
          byte;
        M.set_control_def (M.set_control before index byte) (before.capacity +
          index)
          byte;
        ()
      end;
      M.set_counts_def (M.set_byte slots index byte) (before.size + 1)
        (before.deleted - (if old_byte = 254 then 1 else 0));
      ())

  let rec (routes_set @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (paths : (int * int) list) -> (start : Bigint.t) -> (index : Bigint.t) ->
      (entry : (Key.t * 'a) option) @ immutable -> (path : (int * int)) ->
      ((index : int) -> {u : unit |
        not (M.control after index === Some 128) ||
        M.control before index === Some 128} @ ghost) @ total ->
      {u : unit | not (before.capacity = after.capacity &&
        Invariant.routes_valid before slots paths start &&
        Invariant.route after (Bigint.add start index) entry path) ||
        Invariant.routes_valid after (S.set slots index entry)
          (S.set paths index path) start} @ ghost =
    fun before after slots paths start index entry path proof -> ghost_ (
      S.set_def slots index entry; S.set_def paths index path;
      Invariant.routes_valid_def before slots paths start;
      Invariant.routes_valid_def after (S.set slots index entry)
        (S.set paths index path) start;
      match slots, paths with
      | old :: tail, old_path :: rest ->
        if index = 0Z then begin
          Update.routes_preserved before after tail rest (Bigint.add start 1Z)
            proof;
          ()
        end else begin
          Update.route_preserved before after start old old_path proof;
          routes_set before after tail rest (Bigint.add start 1Z)
            (Bigint.sub index 1Z) entry path proof;
          ()
        end
      | _ -> ())
  let rec (cells_set @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a Invariant.Map.slots) @ immutable ->
      (start : Bigint.t) -> (target : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      ((index : Bigint.t) -> {u : unit |
        not (0Z <= index && index < Bigint.of_int before.capacity) ||
        S.at after.controls index ===
          (if index = target then Some (fingerprint key)
           else S.at before.controls index)} @ ghost) @ total ->
      {u : unit | not (Invariant.cells_valid before slots start &&
        0Z <= start && Bigint.add start (S.length slots) <=
          Bigint.of_int before.capacity) ||
        Invariant.cells_valid after
          (S.set slots (Bigint.sub target start) (Some (key, value))) start}
      @ ghost = fun before after slots start target key value proof -> ghost_ (
    fingerprint_def key;
    S.length_def slots;
    S.set_def slots (Bigint.sub target start) (Some (key, value));
    Invariant.cells_valid_def before slots start;
    Invariant.cells_valid_def after
      (S.set slots (Bigint.sub target start) (Some (key, value))) start;
    match slots with
    | [] -> ()
    | _ :: tail ->
      L.length_nonnegative tail;
      proof start;
      L.set_negative tail (Bigint.sub target (Bigint.add start 1Z))
        (Some (key, value));
      cells_set before after tail (Bigint.add start 1Z) target key value proof;
      ())

  let (reachability @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable -> (index : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (old_byte : int) -> (path : (int * int)) ->
      (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (Invariant.valid before &&
        0 <= index && index < before.model.capacity &&
        Invariant.route before.model (Bigint.of_int index) (Some (key, value))
          path &&
        after.model === insert_model before.model index key value old_byte &&
        after.routes === S.set before.routes (Bigint.of_int index) path) ||
        Invariant.cells_valid after.model after.model.slots 0Z &&
        Invariant.clones after.model 15 &&
        Invariant.routes_valid after.model after.model.slots after.routes 0Z}
      @ ghost = fun before index key value old_byte path after -> ghost_ (
    if Invariant.valid before && 0 <= index && index < before.model.capacity &&
       Invariant.route before.model (Bigint.of_int index) (Some (key, value))
         path &&
       after.model === insert_model before.model index key value old_byte &&
       after.routes === S.set before.routes (Bigint.of_int index) path then
         begin
      Invariant.valid_def before; Invariant.shape_def before.model;
      fields before.model index key value old_byte;
      let byte = fingerprint key in
      fingerprint_def key;
      let no_new_empty : (query : int) ->
          {u : unit | not (M.control after.model query === Some 128) ||
            M.control before.model query === Some 128} @ ghost =
        fun query -> ghost_ (
          L.byte_write before.model index byte query;
          M.control_def after.model query;
          M.control_def (M.set_byte before.model index byte) query;
          ()) in
      cells_set before.model after.model before.model.slots 0Z
        (Bigint.of_int index) key value (fun query -> ghost_ (
          L.byte_at_primary before.model index byte query; ()));
      Update.clones_byte before.model after.model index byte 15;
      Update.route_preserved before.model after.model (Bigint.of_int index)
        (Some (key, value)) path no_new_empty;
      routes_set before.model after.model before.model.slots before.routes 0Z
        (Bigint.of_int index) (Some (key, value)) path no_new_empty;
      ()
    end else ())

  let rec (deleted_nonnegative @ total) : (bytes : int list) -> (count : int) ->
      {u : unit | 0Z <= Invariant.deleted_count bytes count} @ ghost =
    fun bytes count -> ghost_ (
    Invariant.deleted_count_def bytes count;
    match bytes with
    | [] -> ()
    | _ :: tail -> if count > 0 then deleted_nonnegative tail (count - 1); ())

  let (deleted_count @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      (byte : int) -> (old_byte : int) ->
      {u : unit | not (16 <= model.capacity && model.capacity <= 1073741824 &&
        0 <= index && index < model.capacity && byte <> 254 &&
        M.control model index === Some old_byte) ||
        Invariant.deleted_count (M.set_byte model index byte).controls
          model.capacity =
          Bigint.sub (Invariant.deleted_count model.controls model.capacity)
            (if old_byte = 254 then 1Z else 0Z)} @ ghost =
    fun model index byte old_byte -> ghost_ (
      M.set_byte_def model index byte;
      M.set_control_def model index byte;
      M.control_def model index;
      Update.deleted_weight_def byte; Update.deleted_weight_def old_byte;
      Update.deleted_set model.controls model.capacity (Bigint.of_int index)
        byte;
      if index < 15 then begin
        let first = M.set_control model index byte in
        M.set_control_def first (model.capacity + index) byte;
        Update.deleted_set first.controls model.capacity
          (Bigint.of_int (model.capacity + index)) byte;
        ()
      end else ())

  let (arithmetic @ total) (capacity : int) (size : int) (deleted : int)
      (reserve : int) (used_deleted : bool) (dead_after : Bigint.t) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824 &&
        0 <= size && size <= capacity && 0 <= deleted && deleted <= capacity -
          size &&
        1 <= reserve && reserve <= capacity &&
        size + deleted + (if used_deleted then 0 else 1) <= capacity - reserve
          &&
        0Z <= dead_after && dead_after = Bigint.sub (Bigint.of_int deleted)
          (if used_deleted then 1Z else 0Z)) ||
        0 <= size + 1 && size + 1 <= capacity &&
        0 <= deleted - (if used_deleted then 1 else 0) &&
        deleted - (if used_deleted then 1 else 0) <= capacity - (size + 1) &&
        size + 1 + (deleted - (if used_deleted then 1 else 0)) <= capacity -
          reserve &&
        Bigint.of_int (size + 1) = Bigint.add (Bigint.of_int size) 1Z &&
        Bigint.of_int (deleted - (if used_deleted then 1 else 0)) = dead_after}
      @ ghost = ghost_ ()

  let (reserve_bounds @ total) (capacity : int) :
      {u : unit | not (16 <= capacity && capacity <= 1073741824) ||
        1 <= Invariant.reserve capacity && Invariant.reserve capacity <=
          capacity}
      @ ghost = ghost_ (Invariant.reserve_def capacity; ())

  let (shape @ total) : ('a : immutable_data).
      (before : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable -> (old_byte :
        int) ->
      (after : (Key.t, 'a) M.state) @ immutable ->
      {u : unit | not (Invariant.shape before &&
        0 <= index && index < before.capacity && M.slot before index === Some
          None &&
        M.control before index === Some old_byte &&
        before.size + before.deleted + (if old_byte = 254 then 0 else 1) <=
          before.capacity - Invariant.reserve before.capacity &&
        after === insert_model before index key value old_byte) ||
        Invariant.shape after} @ ghost = fun before index key value old_byte
          after ->
    ghost_ (
      if Invariant.shape before && 0 <= index && index < before.capacity &&
         M.slot before index === Some None && M.control before index === Some
           old_byte &&
         before.size + before.deleted + (if old_byte = 254 then 0 else 1) <=
           before.capacity - Invariant.reserve before.capacity &&
         after === insert_model before index key value old_byte then begin
        Invariant.shape_def before;
        fields before index key value old_byte;
        M.slot_def before index;
        S.set_length before.slots (Bigint.of_int index) (Some (key, value));
        fingerprint_bounds key;
        Update.byte_length before index (fingerprint key);
        deleted_count before index (fingerprint key) old_byte;
        deleted_nonnegative after.controls after.capacity;
        Update.live_set before.slots (Bigint.of_int index) (Some (key, value));
        Update.weight_def (Update.empty_entry before);
        Update.weight_def (Some (key, value));
        reserve_bounds before.capacity;
        arithmetic before.capacity before.size before.deleted
          (Invariant.reserve before.capacity) (old_byte = 254)
          (Invariant.deleted_count after.controls after.capacity);
        Invariant.shape_def after;
        ()
      end else ())

  let (insert_valid @ total) : ('a : immutable_data).
      (before : 'a Invariant.view) @ immutable -> (index : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (old_byte : int) -> (path : (int * int)) ->
      (after : 'a Invariant.view) @ immutable ->
      {u : unit | not (Invariant.valid before &&
        0 <= index && index < before.model.capacity &&
        M.slot before.model index === Some None &&
        M.control before.model index === Some old_byte &&
        Invariant.Map.absent before.model.slots key &&
        before.model.size + before.model.deleted + (if old_byte = 254 then 0
          else 1) <=
          before.model.capacity - Invariant.reserve before.model.capacity &&
        Invariant.route before.model (Bigint.of_int index) (Some (key, value))
          path &&
        after.model === insert_model before.model index key value old_byte &&
        after.routes === S.set before.routes (Bigint.of_int index) path &&
        after.plan === before.plan) ||
        Invariant.valid after && Invariant.Map.same after.model.slots
          (Invariant.Map.put before.model.slots key value)} @ ghost =
    fun before index key value old_byte path after -> ghost_ (
      if Invariant.valid before &&
         0 <= index && index < before.model.capacity &&
         M.slot before.model index === Some None &&
         M.control before.model index === Some old_byte &&
         Invariant.Map.absent before.model.slots key &&
         before.model.size + before.model.deleted + (if old_byte = 254 then 0
           else 1) <=
           before.model.capacity - Invariant.reserve before.model.capacity &&
         Invariant.route before.model (Bigint.of_int index) (Some (key,
           value)) path &&
         after.model === insert_model before.model index key value old_byte &&
         after.routes === S.set before.routes (Bigint.of_int index) path &&
         after.plan === before.plan then begin
        Invariant.valid_def before;
        fields before.model index key value old_byte;
        shape before.model index key value old_byte after.model;
        reachability before index key value old_byte path after;
        M.slot_def before.model index;
        Invariant.Map.distinct_insert before.model.slots (Bigint.of_int index)
          key value;
        Invariant.Map.same_insert before.model.slots (Bigint.of_int index) key
          value;
        Invariant.valid_def after;
        ()
      end else ())

end
