module M = Vox_table_model
module L = Vox_table_model_proofs
module S = Vox_sequence

module Make (Key : Vox_table_map.Key)
    (Invariant : module type of Vox_table_invariant.Make (Key)) = struct

  let rec (empty_distinct @ total) : ('a : immutable_data).
      (count : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None) ||
        Invariant.Map.distinct (M.repeat count entry)} @ ghost =
    fun count entry -> ghost_ (
      M.repeat_def count entry;
      Invariant.Map.distinct_def (M.repeat count entry);
      if count > 0 then empty_distinct (count - 1) entry;
      ())
    [@@decreases if count > 0 then count else 0]

  let rec (empty_count @ total) : ('a : immutable_data).
      (count : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None) ||
        Invariant.live_count (M.repeat count entry) = 0Z} @ ghost =
    fun count entry -> ghost_ (
      M.repeat_def count entry;
      Invariant.live_count_def (M.repeat count entry);
      if count > 0 then empty_count (count - 1) entry;
      ())
    [@@decreases if count > 0 then count else 0]

  let rec (no_deleted @ total) : (count : int) -> (remaining : int) ->
      {u : unit | Invariant.deleted_count (M.repeat count 128) remaining = 0Z}
      @ ghost = fun count remaining -> ghost_ (
    M.repeat_def count 128;
    Invariant.deleted_count_def (M.repeat count 128) remaining;
    if count > 0 && remaining > 0 then
      no_deleted (count - 1) (remaining - 1);
    ())
    [@@decreases if count > 0 then count else 0]

  let (shape @ total) : ('a : immutable_data).
      (capacity : {c : int | 16 <= c && c <= 1073741824 &&
        c land (c - 1) = 0}) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None) || Invariant.shape (M.initial capacity
        entry)}
      @ ghost = fun capacity entry -> ghost_ (
    M.initial_def capacity entry;
    Invariant.shape_def (M.initial capacity entry);
    Invariant.power_of_two_def capacity; Invariant.reserve_def capacity;
    L.repeat_length capacity entry;
    L.repeat_length (capacity + 15) 128;
    empty_count capacity entry;
    no_deleted (capacity + 15) capacity;
    ())

  let rec (clones @ total) : ('a : immutable_data).
      (capacity : {c : int | 16 <= c && c <= 1073741824}) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (lanes : {n : int | 0 <= n && n <= 15}) ->
      {u : unit | Invariant.clones (M.initial capacity entry) lanes} @ ghost =
    fun capacity entry lanes -> ghost_ (
      let model = M.initial capacity entry in
      M.initial_def capacity entry;
      Invariant.clones_def model lanes;
      if lanes > 0 then begin
        clones capacity entry (lanes - 1);
        L.initial_control model capacity entry (capacity + lanes - 1);
        L.initial_control model capacity entry (lanes - 1);
        ()
      end else ())
    [@@decreases lanes]

  let (index_step @ total)
      (capacity : {c : int | 16 <= c && c <= 1073741824})
      (index : {i : int | 0 <= i && i < capacity}) :
      {u : unit | capacity - (index + 1) = (capacity - index) - 1 &&
        Bigint.of_int (index + 1) = Bigint.add (Bigint.of_int index) 1Z} = ()

  let (index_bounds @ total)
      (capacity : {c : int | 16 <= c && c <= 1073741824})
      (index : {i : int | 0 <= i && i <= capacity}) :
      {u : unit | 0Z <= Bigint.of_int index &&
        Bigint.of_int index < Bigint.of_int (capacity + 15)} = ()

  let (cells_step @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (tail : 'a Invariant.Map.slots) @ immutable -> (index : Bigint.t) ->
      {u : unit | not (entry === None &&
        S.at model.controls index === Some 128 &&
        Invariant.cells_valid model tail (Bigint.add index 1Z)) ||
        Invariant.cells_valid model (entry :: tail) index} @ ghost =
    fun model entry tail index -> ghost_ (
      Invariant.cells_valid_def model (entry :: tail) index;
      ())

  let (cells_empty @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : Bigint.t) ->
      {u : unit | Invariant.cells_valid model [] index} @ ghost =
    fun model index -> ghost_ (Invariant.cells_valid_def model [] index; ())

  let rec (cells @ total) : ('a : immutable_data).
      (capacity : {c : int | 16 <= c && c <= 1073741824}) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (index : {i : int | 0 <= i && i <= capacity}) ->
      {u : unit | not (entry === None) ||
        Invariant.cells_valid (M.initial capacity entry)
          (M.repeat (capacity - index) entry) (Bigint.of_int index)} @ ghost =
    fun capacity entry index -> ghost_ (
      let model = M.initial capacity entry in
      M.initial_def capacity entry;
      M.repeat_def (capacity - index) entry;
      index_bounds capacity index;
      L.repeat_at (capacity + 15) 128 (Bigint.of_int index);
      if index < capacity then begin
        index_step capacity index;
        cells capacity entry (index + 1);
        cells_step model entry (M.repeat (capacity - index - 1) entry)
          (Bigint.of_int index)
      end else cells_empty model (Bigint.of_int index);
      ())
    [@@decreases capacity - index]

  let rec (routes @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (count : int) -> (index : Bigint.t) ->
      {u : unit | not (entry === None) ||
        Invariant.routes_valid model (M.repeat count entry)
          (M.repeat count (0, 0)) index} @ ghost =
    fun model entry count index -> ghost_ (
      M.repeat_def count entry; M.repeat_def count (0, 0);
      Invariant.routes_valid_def model (M.repeat count entry)
        (M.repeat count (0, 0)) index;
      Invariant.route_def model index entry (0, 0);
      if count > 0 then routes model entry (count - 1) (Bigint.add index 1Z);
      ())
    [@@decreases if count > 0 then count else 0]

  let (initial @ total) : ('a : immutable_data).
      (capacity : {c : int | 16 <= c && c <= 1073741824 &&
        c land (c - 1) = 0}) ->
      (entry : (Key.t * 'a) option) @ immutable ->
      (view : 'a Invariant.view) @ immutable ->
      {u : unit | not (entry === None &&
        view.model === M.initial capacity entry &&
        view.routes === M.repeat capacity (0, 0) &&
        Vox_table_probe.valid view.plan &&
        capacity = Vox_table_wrap.scale16 (Vox_table_probe.groups view.plan))
        || Invariant.valid view} @ ghost =
    fun capacity entry view -> ghost_ (
      M.initial_def capacity entry;
      Invariant.valid_def view;
      shape capacity entry;
      clones capacity entry 15;
      cells capacity entry 0;
      routes (M.initial capacity entry) entry capacity 0Z;
      empty_distinct capacity entry;
      ())
end
