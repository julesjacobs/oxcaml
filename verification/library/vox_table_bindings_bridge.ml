module Make (Key : Vox_table_map.Key)
    (Slots : module type of Vox_table_map.Make (Key)) = struct
  module Model = Map.MakeLogical (Key)

  let[@def] rec (model @ total) (slots : 'a Slots.slots @ immutable) = ghost_ (
    match slots with
    | [] -> Model.empty ()
    | None :: tail -> model tail
    | Some (key, value) :: tail -> Model.add key value (model tail))

  let rec (lookup @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | Model.find_opt key (model slots) === Slots.lookup slots key}
      @ ghost = fun slots key -> ghost_ (
    model_def slots; Slots.lookup_def slots key;
    match slots with | [] -> () | _ :: tail -> lookup tail key)

  let (same @ total) : ('a : immutable_data).
      (left : 'a Slots.slots) @ immutable -> (right : 'a Slots.slots) @ immutable ->
      {u : unit | not (Slots.same left right) || model left === model right}
      @ ghost = fun left right -> ghost_ (
    match Model.Proof.difference (model left) (model right) with
    | None -> ()
    | Some key ->
      lookup left key; lookup right key; Slots.same_get left right key)

  let (erase @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | model (Slots.erase slots key) ===
        Model.remove key (model slots)}
      @ ghost = fun slots key -> ghost_ (
    match Model.Proof.difference (model (Slots.erase slots key))
        (Model.remove key (model slots)) with
    | None -> ()
    | Some query ->
      lookup (Slots.erase slots key) query; lookup slots query;
      Slots.erase_get slots key query)

  let (put @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      {u : unit | model (Slots.put slots key value) ===
        Model.add key value (model slots)} @ ghost =
    fun slots key value -> ghost_ (
      match Model.Proof.difference (model (Slots.put slots key value))
          (Model.add key value (model slots)) with
      | None -> ()
      | Some query ->
        lookup (Slots.put slots key value) query; lookup slots query;
        Slots.put_get slots key value query)

  let rec (empty @ total) : ('a : immutable_data).
      (capacity : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None) ||
        model (Vox_table_model.repeat capacity entry) === Model.empty ()}
      @ ghost =
    fun capacity entry -> ghost_ (
      Vox_table_model.repeat_def capacity entry;
      model_def (Vox_table_model.repeat capacity entry);
      if capacity > 0 then empty (capacity - 1) entry; ())
    [@@decreases if capacity > 0 then capacity else 0]
end
