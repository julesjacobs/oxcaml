module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module T = Vox_table_storage

module type Key = Vox_table_map.Key
module Make (Key : Key) = struct
  module Impl = Vox_table_implementation.Make (Key)
  module I = Impl.Spec
  module Bridge = Vox_table_bindings_bridge.Make (Key) (I.Map)
  module Map = Bridge.Map
  module Assoc = Bridge.Assoc
  type ('a : immutable_data) t = 'a Impl.t
  type ('a : immutable_data) state = (Key.t, 'a) M.state

  (* A view pairs a storage snapshot with the map it represents. The map
     has the same bindings as the live slots, but not necessarily in slot
     order, so updates can state it exactly. *)
  type ('a : immutable_data) snapshot = {
    storage : 'a I.view @@ ghost;
    map : 'a Map.t @@ ghost;
  }
  type ('a : immutable_data) view = {v : 'a snapshot |
    I.valid v.storage && Assoc.same v.map (Bridge.compact v.storage.model.slots)}

  let location = T.location
  let[@def] (version @ total) (view : 'a view @ immutable) = ghost_ view.storage.model
  let[@def] (bindings @ total) (view : 'a view @ immutable) : 'a Map.t @ ghost =
    ghost_ view.map
  let[@def] (capacity @ total) (view : 'a view @ immutable) :
      {n : int | 16 <= n && n <= 1073741824} @ ghost = ghost_ (
    I.valid_def view.storage; I.shape_def view.storage.model;
    view.storage.model.capacity)
  type ('a : immutable_data) created = #{
    table : 'a t @@ aliased;
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }
  type ('a : immutable_data) updated = #{
    view : 'a view @@ aliased immutable;
    token : 'a state P.token @@ ghost;
  }

  (* Storage views are records of ghost fields, so the implementation's
     zero-size view can be rebuilt from the copy kept in [view]. *)
  let storage (view : 'a view @ immutable) :
      {v : 'a I.view | v === view.storage} @ immutable =
    {I.model = ghost_ view.storage.model; routes = ghost_ view.storage.routes;
     plan = ghost_ view.storage.plan}

  let rec (count_bridge @ total) (slots : 'a I.Map.slots) :
      {u : unit | Assoc.count (Bridge.compact slots) = I.live_count slots}
      @ ghost = ghost_ (
    Bridge.compact_def slots; Assoc.count_def (Bridge.compact slots);
    I.live_count_def slots;
    match slots with | [] -> () | _ :: tail -> count_bridge tail)

  (* Reads: a view's map answers lookups and [count] like its live slots. *)
  let (lookup_agrees @ total) (view : 'a view @ immutable) (key : Key.t) :
      {u : unit | Map.lookup (bindings view) key ===
        I.Map.lookup view.storage.model.slots key} @ ghost = ghost_ (
    bindings_def view; Map.lookup_def view.map key;
    Assoc.same_get view.map (Bridge.compact view.storage.model.slots) key;
    Bridge.lookup view.storage.model.slots key)

  let (count_agrees @ total) (view : 'a view @ immutable) :
      {u : unit | Map.count (bindings view) =
        I.live_count view.storage.model.slots} @ ghost = ghost_ (
    bindings_def view; Map.count_def view.map; I.valid_def view.storage;
    Bridge.distinct view.storage.model.slots;
    Assoc.same_count view.map (Bridge.compact view.storage.model.slots);
    count_bridge view.storage.model.slots)

  (* Writes: storage produced by a mutation represents the updated map. *)
  let (empty_view @ total) (storage : 'a I.view @ immutable) (capacity : int) :
      {u : unit | not (storage.model === M.initial capacity None) ||
        Assoc.same Map.empty (Bridge.compact storage.model.slots)} @ ghost =
    ghost_ (
      M.initial_def capacity (None : (Key.t * 'a) option);
      Bridge.empty capacity (None : (Key.t * 'a) option);
      Map.empty_same Map.empty (Bridge.compact storage.model.slots))

  let (put_view @ total) (before : 'a view @ immutable) (key : Key.t)
      (value : 'a) (storage : 'a I.view @ immutable) :
      {u : unit | not (I.Map.same storage.model.slots
          (I.Map.put before.storage.model.slots key value)) ||
        Assoc.same (Map.put (bindings before) key value)
          (Bridge.compact storage.model.slots)} @ ghost = ghost_ (
    let old_slots = Bridge.compact before.storage.model.slots in
    let new_slots = Bridge.compact storage.model.slots in
    bindings_def before; Map.put_def before.map key value;
    Bridge.put before.storage.model.slots key value;
    Bridge.same storage.model.slots (I.Map.put before.storage.model.slots key value);
    Assoc.same_put before.map old_slots key value;
    Assoc.same_def new_slots (Assoc.put old_slots key value);
    Assoc.same_def (Assoc.put old_slots key value) new_slots;
    Assoc.same_trans (Assoc.put before.map key value)
      (Assoc.put old_slots key value) new_slots)

  let (erase_view @ total) (before : 'a view @ immutable) (key : Key.t)
      (storage : 'a I.view @ immutable) :
      {u : unit | not (I.Map.same storage.model.slots
          (I.Map.erase before.storage.model.slots key)) ||
        Assoc.same (Map.erase (bindings before) key)
          (Bridge.compact storage.model.slots)} @ ghost = ghost_ (
    let old_slots = Bridge.compact before.storage.model.slots in
    let new_slots = Bridge.compact storage.model.slots in
    bindings_def before; Map.erase_def before.map key;
    Bridge.erase before.storage.model.slots key;
    Bridge.same storage.model.slots (I.Map.erase before.storage.model.slots key);
    Assoc.same_erase before.map old_slots key;
    Assoc.same_def new_slots (Assoc.erase old_slots key);
    Assoc.same_def (Assoc.erase old_slots key) new_slots;
    Assoc.same_trans (Assoc.erase before.map key)
      (Assoc.erase old_slots key) new_slots)

  let create : ('a : immutable_data).
    (token : 'a state P.token) @ unique ghost ->
    {r : 'a created | bindings r.#view === Map.empty && capacity r.#view = 16 &&
      not (H.mem (P.own token) (location r.#table)) &&
      P.own r.#token === H.put (P.own token) (location r.#table) (version r.#view)} @ unique = fun token ->
    let r = Impl.create token in
    ghost_ (empty_view r.view 16;
      M.initial_def 16 (Impl.Mutation.empty_entry r.table));
    let view : 'a view = {storage = r.view; map = Map.empty} in
    ghost_ (version_def view; bindings_def view; capacity_def view);
    #{table = r.table; view; token = r.state}

  let length : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {n : int | 0 <= n && Bigint.of_int n = Map.count (bindings view)} = fun table view token ->
    ghost_ (version_def view; count_agrees view;
      I.valid_def view.storage; I.shape_def view.storage.model);
    Impl.length table (storage view) token

  let find_opt : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {value : 'a option | value === Map.lookup (bindings view) key} = fun table view key token ->
    ghost_ (version_def view; lookup_agrees view key);
    Impl.find_opt table (storage view) key token

  let find : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {value : 'a | Map.lookup (bindings view) key === Some value} = fun table view key token ->
    ghost_ (version_def view; lookup_agrees view key);
    Impl.find table (storage view) key token

  let mem : ('a : immutable_data).
    (table : 'a t) -> (view : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version view)}) @ local read ghost ->
    {present : bool | present = (match Map.lookup (bindings view) key with
      | None -> false | Some _ -> true)} = fun table view key token ->
    ghost_ (version_def view; lookup_agrees view key);
    Impl.mem table (storage view) key token

  let replace : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (value : 'a) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.put (bindings before) key value &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique = fun table before key value token ->
    ghost_ (version_def before);
    let r = Impl.replace table (storage before) key value token in
    ghost_ (put_view before key value r.#view);
    let view : 'a view =
      {storage = r.#view; map = ghost_ (Map.put (bindings before) key value)} in
    ghost_ (version_def view; bindings_def view);
    #{view; token = r.#state}

  let remove : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable -> (key : Key.t) ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.erase (bindings before) key &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique = fun table before key token ->
    ghost_ (version_def before);
    let r = Impl.remove table (storage before) key token in
    ghost_ (erase_view before key r.#view);
    let view : 'a view =
      {storage = r.#view; map = ghost_ (Map.erase (bindings before) key)} in
    ghost_ (version_def view; bindings_def view);
    #{view; token = r.#state}

  let clear : ('a : immutable_data).
    (table : 'a t) -> (before : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (version before)}) @ unique read_write ghost ->
    {r : 'a updated | bindings r.#view === Map.empty &&
      capacity r.#view = capacity before &&
      P.own r.#token === H.put (P.own token) (location table) (version r.#view)} @ unique = fun table before token ->
    ghost_ (version_def before);
    let r = Impl.clear table (storage before) token in
    ghost_ (empty_view r.#view before.storage.model.capacity;
      M.initial_def before.storage.model.capacity
        (Impl.Mutation.empty_entry table));
    let view : 'a view = {storage = r.#view; map = Map.empty} in
    ghost_ (version_def view; bindings_def view; capacity_def before;
      capacity_def view);
    #{view; token = r.#state}
end
