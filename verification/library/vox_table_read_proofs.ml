module M = Vox_table_model
module S = Vox_sequence
module W = Vox_table_wrap
module Mask = Vox_table_mask

module Make (Key : Vox_table_map.Key) = struct
  module I = Vox_table_invariant.Make (Key)

  let rec (cell_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a I.Map.slots) @ immutable -> (start : Bigint.t) ->
      (index : Bigint.t) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (I.cells_valid model slots start &&
        S.at slots index === Some entry) ||
        (match entry, S.at model.controls (Bigint.add start index) with
         | Some (key, _), Some byte -> byte = (Key.hash key land 127)
         | None, Some byte -> byte = 128 || byte = 254
         | _ -> false)} @ ghost = fun model slots start index entry -> ghost_ (
    I.cells_valid_def model slots start;
    S.at_def slots index;
    match slots with
    | [] -> ()
    | _ :: tail ->
      if index <> 0Z then
        cell_at model tail (Bigint.add start 1Z) (Bigint.sub index 1Z) entry;
      ())

  let rec (route_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (slots : 'a I.Map.slots) @ immutable ->
      (paths : (int * int) list) @ immutable -> (start : Bigint.t) ->
      (index : Bigint.t) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (I.routes_valid model slots paths start &&
        S.at slots index === Some entry) ||
        (match S.at paths index with
         | Some path -> I.route model (Bigint.add start index) entry path
         | None -> false)} @ ghost =
    fun model slots paths start index entry -> ghost_ (
      I.routes_valid_def model slots paths start;
      S.at_def slots index; S.at_def paths index;
      match slots, paths with
      | _ :: tail, _ :: rest ->
        if index <> 0Z then
          route_at model tail rest (Bigint.add start 1Z)
            (Bigint.sub index 1Z) entry;
        ()
      | _ -> ())

  let rec (clone_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (count : {n : int | 0 <= n && n <= 15}) ->
      (lane : {i : int | 0 <= i && i < count}) ->
      {u : unit | not (I.clones model count) ||
        M.control model (model.capacity + lane) === M.control model lane}
      @ ghost = fun model count lane -> ghost_ (
    I.clones_def model count;
    if lane < count - 1 then clone_at model (count - 1) lane;
    ())
    [@@decreases count]

  let (wrapped_control @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (group : int) -> (lane : Mask.lane) ->
      {u : unit | not (I.shape model && I.clones model 15 &&
        0 <= group && group < model.capacity) ||
        M.control model (group + lane) ===
          M.control model (I.wrap model.capacity (group + lane))} @ ghost =
    fun model group lane -> ghost_ (
      I.shape_def model; I.power_of_two_def model.capacity;
      if I.shape model && I.clones model 15 &&
          0 <= group && group < model.capacity then begin
        W.wrap_add model.capacity group lane;
        W.P.addmod_def model.capacity group lane;
        I.wrap_def model.capacity (group + lane);
        if group + lane >= model.capacity then
          clone_at model 15 (group + lane - model.capacity);
        ()
      end else ())

  let (matching_control @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (group : int) -> (needle : int) -> (lane : Mask.lane) ->
      {u : unit | not (I.shape model && I.clones model 15 &&
        0 <= group && group < model.capacity) ||
        (M.matching model group needle 16 land M.lane_bit lane <> 0) =
        (M.control model (I.wrap model.capacity (group + lane)) ===
          Some needle)} @ ghost = fun model group needle lane -> ghost_ (
    wrapped_control model group lane;
    Mask.get model group needle 16 lane;
    ())
  let (slot_present @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) ->
      {u : unit | not (I.shape model && 0 <= index && index < model.capacity)
        || (match M.slot model index with Some _ -> true | None -> false)}
      @ ghost = fun model index -> ghost_ (
    I.shape_def model; I.power_of_two_def model.capacity; M.slot_def model
      index;
    if I.shape model && 0 <= index && index < model.capacity then begin
      Vox_table_model_proofs.index_bounds model.capacity index;
      Vox_table_model_proofs.at_present model.slots (Bigint.of_int index);
      ()
    end else ())

  let (initialized @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (group : int) -> (needle : {n : int | 0 <= n && n <= 127}) ->
      (lane : Mask.lane) ->
      {u : unit | not (I.shape model && I.cells_valid model model.slots 0Z &&
        I.clones model 15 && 0 <= group && group < model.capacity &&
        M.matching model group needle 16 land M.lane_bit lane <> 0) ||
        (match M.slot model (I.wrap model.capacity (group + lane)) with
         | Some (Some _) -> true | _ -> false)} @ ghost =
    fun model group needle lane -> ghost_ (
      I.shape_def model; I.power_of_two_def model.capacity;
      if I.shape model && I.cells_valid model model.slots 0Z &&
          I.clones model 15 && 0 <= group && group < model.capacity then begin
        matching_control model group needle lane;
        let index = I.wrap model.capacity (group + lane) in
        I.wrap_def model.capacity (group + lane);
        W.wrap_range model.capacity (group + lane);
        slot_present model index;
        M.slot_def model index; M.control_def model index;
        (match M.slot model index with
         | Some entry -> cell_at model model.slots 0Z (Bigint.of_int index)
           entry
         | None -> ());
        ()
      end else ())

  let (group_range @ total) (capacity : W.capacity) (hash : int) (rank : int) :
      {u : unit | 0 <= I.group capacity hash rank &&
        I.group capacity hash rank < capacity} =
    I.group_def capacity hash rank;
    I.probe_def capacity hash rank;
    if rank <= 0 then begin
      I.wrap_def capacity (hash lsr 7);
      W.wrap_range capacity (hash lsr 7);
      ()
    end else begin
      let group, step = I.probe capacity hash (rank - 1) in
      I.wrap_def capacity (group + step);
      W.wrap_range capacity (group + step);
      ()
    end

  let (group_in_shape @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (hash : int) ->
      (rank : int) -> {u : unit | not (I.shape model) ||
        0 <= I.group model.capacity hash rank &&
        I.group model.capacity hash rank < model.capacity} @ ghost =
    fun model hash rank -> ghost_ (
      I.shape_def model; I.power_of_two_def model.capacity;
      if I.shape model then group_range model.capacity hash rank;
      ())

  let (route_info @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (rank : int) -> (lane : int) ->
      {u : unit | not (I.route model index (Some (key, value)) (rank, lane)) ||
        0 <= rank && 0 <= lane && lane < 16 &&
        index = Bigint.of_int (I.wrap model.capacity
          (I.group model.capacity (Key.hash key) rank + lane)) &&
        I.empty_free model (Key.hash key) rank} @ ghost =
    fun model index key value rank lane -> ghost_ (
      I.route_def model index (Some (key, value)) (rank, lane);
      I.route_position_def model.capacity (Key.hash key) index (rank, lane);
      ())

  let (slot_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : Bigint.t) ->
      (physical : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (index = Bigint.of_int physical &&
        S.at model.slots index === Some entry) ||
        M.slot model physical === Some entry} @ ghost =
    fun model index physical entry -> ghost_ (M.slot_def model physical; ())

  let (stored_control @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (physical : int) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      {u : unit | not (I.cells_valid model model.slots 0Z &&
        M.slot model physical === Some (Some (key, value))) ||
        M.control model physical === Some (Key.hash key land 127)} @ ghost =
    fun model physical key value -> ghost_ (
      M.slot_def model physical; M.control_def model physical;
      cell_at model model.slots 0Z (Bigint.of_int physical) (Some (key, value));
      ())

  let (route_equal_key @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (query : Key.t) @ immutable ->
      (value : 'a) @ immutable -> (rank : int) -> (lane : int) ->
      {u : unit | not (I.route model index (Some (key, value)) (rank, lane) &&
        Key.equal key query) ||
        index = Bigint.of_int (I.wrap model.capacity
          (I.group model.capacity (Key.hash query) rank + lane))}
      @ ghost = fun model index key query value rank lane -> ghost_ (
    route_info model index key value rank lane;
    Key.hash_equal key query;
    ())

  let (capacity_bounds @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      {u : unit | not (I.shape model) ||
        16 <= model.capacity && model.capacity <= 1073741824 &&
        model.capacity land (model.capacity - 1) = 0 &&
        0 < model.capacity lsr 4 && model.capacity lsr 4 <= 67108864}
      @ ghost = fun model -> ghost_ (I.shape_def model; I.power_of_two_def
        model.capacity; ())

  let (next_probe @ total) (capacity : int) (hash : int)
      (rank : {r : int | 0 <= r && r < 1073741824})
      (group : int) (step : int) :
      {u : unit | not (I.probe capacity hash rank === (group, step)) ||
        I.probe capacity hash (rank + 1) ===
          ((group + step) land (capacity - 1), step + 16)} =
    I.probe_def capacity hash (rank + 1);
    I.wrap_def capacity (group + step);
    ()

end
