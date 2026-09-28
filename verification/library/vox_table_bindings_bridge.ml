module Make (Key : Vox_table_map.Key)
    (Slots : module type of Vox_table_map.Make (Key)) = struct
  module Map = Vox_table_bindings.Make (Key)
  module Assoc = Map.Assoc

  let[@def] rec (compact @ total) (slots : 'a Slots.slots @ immutable) =
    match slots with
    | [] -> []
    | None :: tail -> compact tail
    | Some binding :: tail -> binding :: compact tail

  let rec (lookup @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | Assoc.lookup (compact slots) key === Slots.lookup slots key}
      @ ghost = fun slots key -> ghost_ (
    compact_def slots; Slots.lookup_def slots key;
    Assoc.lookup_def (compact slots) key;
    match slots with | [] -> () | _ :: tail -> lookup tail key)

  let rec (distinct @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable ->
      {u : unit | Assoc.distinct (compact slots) = Slots.distinct slots}
      @ ghost = fun slots -> ghost_ (
    compact_def slots; Slots.distinct_def slots;
    Assoc.distinct_def (compact slots);
    match slots with
    | [] -> ()
    | None :: tail -> distinct tail
    | Some (key, _) :: tail ->
      Slots.absent_lookup tail key; lookup tail key; distinct tail)

  let rec (agrees @ total) : ('a : immutable_data).
      (left : 'a Slots.slots) @ immutable -> (right : 'a Slots.slots) @ immutable ->
      {u : unit | Assoc.agrees (compact left) (compact right) = Slots.agrees left right}
      @ ghost = fun left right -> ghost_ (
    compact_def left; Slots.agrees_def left right;
    Assoc.agrees_def (compact left) (compact right);
    match left with
    | [] -> ()
    | None :: tail -> agrees tail right
    | Some (key, _) :: tail -> lookup right key; agrees tail right)

  let (same @ total) : ('a : immutable_data).
      (left : 'a Slots.slots) @ immutable -> (right : 'a Slots.slots) @ immutable ->
      {u : unit | Assoc.same (compact left) (compact right) = Slots.same left right}
      @ ghost = fun left right -> ghost_ (
    Slots.same_def left right; Assoc.same_def (compact left) (compact right);
    agrees left right; agrees right left)

  let rec (erase @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      {u : unit | compact (Slots.erase slots key) === Assoc.erase (compact slots) key}
      @ ghost = fun slots key -> ghost_ (
    compact_def slots; Slots.erase_def slots key;
    Assoc.erase_def (compact slots) key;
    compact_def (Slots.erase slots key);
    match slots with | [] -> () | _ :: tail -> erase tail key)

  let (put @ total) : ('a : immutable_data).
      (slots : 'a Slots.slots) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      {u : unit | compact (Slots.put slots key value) ===
        Assoc.put (compact slots) key value} @ ghost =
    fun slots key value -> ghost_ (
      Slots.put_def slots key value; compact_def (Slots.put slots key value);
      Assoc.put_def (compact slots) key value; erase slots key)
  let rec (empty @ total) : ('a : immutable_data).
      (capacity : int) -> (entry : (Key.t * 'a) option) @ immutable ->
      {u : unit | not (entry === None) ||
        compact (Vox_table_model.repeat capacity entry) === []} @ ghost =
    fun capacity entry -> ghost_ (
      Vox_table_model.repeat_def capacity entry;
      compact_def (Vox_table_model.repeat capacity entry);
      if capacity > 0 then empty (capacity - 1) entry; ())
    [@@decreases if capacity > 0 then capacity else 0]
end
