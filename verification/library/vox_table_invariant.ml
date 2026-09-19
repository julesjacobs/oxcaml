module S = Vox_sequence
module M = Vox_table_model

module Make (Key : Vox_table_map.Key) = struct
  module Map = Vox_table_map.Make (Key)

  type ('a : immutable_data) view = {
    model : (Key.t, 'a) M.state @@ ghost;
    routes : (int * int) list @@ ghost;
    plan : Vox_table_probe.plan @@ ghost;
  }

  let[@def] (wrap @ total) (capacity : int) (index : int) =
    index land (capacity - 1)

  let[@def] rec (probe @ total) (capacity : int) (hash : int) (depth : int) =
    if depth <= 0 then (wrap capacity (hash lsr 7), 16)
    else
      let group, step = probe capacity hash (depth - 1) in
      (wrap capacity (group + step), step + 16)
    [@@decreases if depth > 0 then depth else 0]

  let[@def] (group @ total) (capacity : int) (hash : int) (depth : int) =
    let group, _ = probe capacity hash depth in group

  let[@def] rec (empty_free @ total)
      (s : (Key.t, 'a) M.state @ immutable) (hash : int) (depth : int) =
    if depth <= 0 then true else
      M.matching s (group s.capacity hash (depth - 1)) 128 16 = 0
      && empty_free s hash (depth - 1)
    [@@decreases if depth > 0 then depth else 0]

  let[@def] (route_position @ total) (capacity : int) (hash : int)
      (index : Bigint.t) (path : int * int) =
    let depth, lane = path in
    0 <= depth && depth < (capacity lsr 4) && 0 <= lane && lane < 16
    && index = Bigint.of_int
      (wrap capacity (group capacity hash depth + lane))

  let[@def] (route @ total) (s : (Key.t, 'a) M.state @ immutable)
      (index : Bigint.t) (entry : (Key.t * 'a) option @ immutable)
      (path : int * int) =
    match entry with
    | None -> true
    | Some (key, _) ->
      let depth, _ = path in
      route_position s.capacity (Key.hash key) index path
      && empty_free s (Key.hash key) depth

  let[@def] rec (routes_valid @ total)
      (s : (Key.t, 'a) M.state @ immutable)
      (slots : 'a Map.slots @ immutable) (paths : (int * int) list)
      (index : Bigint.t) =
    match slots, paths with
    | [], [] -> true
    | entry :: tail, path :: rest ->
      route s index entry path && routes_valid s tail rest (Bigint.add index 1Z)
    | _ -> false

  let[@def] rec (cells_valid @ total)
      (s : (Key.t, 'a) M.state @ immutable)
      (slots : 'a Map.slots @ immutable) (index : Bigint.t) =
    match slots with
    | [] -> true
    | entry :: tail ->
      (match entry, S.at s.controls index with
       | Some (key, _), Some byte -> byte = (Key.hash key land 127)
       | None, Some byte -> byte = 128 || byte = 254
       | _ -> false)
      && cells_valid s tail (Bigint.add index 1Z)

  let[@def] rec (clones @ total)
      (s : (Key.t, 'a) M.state @ immutable) (lanes : int) = ghost_ (
    if lanes <= 0 then true else
      M.control s (s.capacity + lanes - 1) === M.control s (lanes - 1)
      && clones s (lanes - 1))
    [@@decreases if lanes > 0 then lanes else 0]

  let[@def] rec (live_count @ total) (slots : 'a Map.slots @ immutable) =
    match slots with
    | [] -> 0Z
    | None :: tail -> live_count tail
    | Some _ :: tail -> Bigint.add 1Z (live_count tail)

  let[@def] rec (deleted_count @ total) (bytes : int list) (remaining : int) =
    if remaining <= 0 then 0Z else
      match bytes with
      | [] -> 0Z
      | byte :: tail -> Bigint.add (if byte = 254 then 1Z else 0Z)
          (deleted_count tail (remaining - 1))

  let[@def] (power_of_two @ total) (capacity : int) =
    capacity land (capacity - 1) = 0

  let[@def] (reserve @ total) (capacity : int) = capacity lsr 3

  let[@def] (shape @ total) (s : (Key.t, 'a) M.state @ immutable) =
    16 <= s.capacity && s.capacity <= 1073741824
    && power_of_two s.capacity
    && S.length s.slots = Bigint.of_int s.capacity
    && S.length s.controls = Bigint.of_int (s.capacity + 15)
    && 0 <= s.size && 0 <= s.deleted
    && s.size <= s.capacity && s.deleted <= s.capacity - s.size
    && s.size + s.deleted <= s.capacity - reserve s.capacity
    && Bigint.of_int s.size = live_count s.slots
    && Bigint.of_int s.deleted = deleted_count s.controls s.capacity

  let[@def] (valid @ total) (view : 'a view @ immutable) = ghost_ (
    Vox_table_probe.valid view.plan
    && view.model.capacity = Vox_table_wrap.scale16
      (Vox_table_probe.groups view.plan)
    && shape view.model && cells_valid view.model view.model.slots 0Z
    && clones view.model 15 && Map.distinct view.model.slots
    && routes_valid view.model view.model.slots view.routes 0Z)
end
