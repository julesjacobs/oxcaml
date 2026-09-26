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
  let[@def] rec (count @ total) (bindings : 'a Map.t @ immutable) =
    match bindings with
    | [] -> 0Z
    | _ :: tail -> Bigint.add 1Z (count tail)
  type ('a : immutable_data) t = 'a Impl.t
  type ('a : immutable_data) state = (Key.t, 'a) M.state
  type ('a : immutable_data) view = {v : 'a I.view | I.valid v}
  let location = T.location
  let[@def] (model @ total) (view : 'a view @ immutable) = ghost_ view.model
  let[@def] (bindings @ total) (view : 'a view @ immutable) :
      {entries : 'a Map.t | Map.distinct entries} @ immutable ghost = ghost_ (
    I.valid_def view; Bridge.distinct view.model.slots;
    Bridge.compact view.model.slots)
  let[@def] (capacity @ total) (view : 'a view @ immutable) :
      {n : int | 16 <= n && n <= 1073741824} @ ghost = ghost_ (
    I.valid_def view; I.shape_def view.model; view.model.capacity)
  type ('a : immutable_data) created = {
    table : 'a t @@ aliased;
    view : 'a view @@ aliased immutable;
    state : 'a state P.token @@ ghost;
  }
  type ('a : immutable_data) result = #{
    view : 'a view @@ aliased immutable;
    state : 'a state P.token @@ ghost;
  }
  let rec (count_bridge @ total) (slots : 'a I.Map.slots @ immutable) :
      {u : unit | count (Bridge.compact slots) = I.live_count slots} @ ghost = ghost_ (
    Bridge.compact_def slots; count_def (Bridge.compact slots);
    I.live_count_def slots;
    match slots with | [] -> () | _ :: tail -> count_bridge tail)
  let (empty_map @ total) (view : 'a view @ immutable)
      (capacity : int) :
      {u : unit | not (view.model === M.initial capacity None) ||
        Bridge.compact view.model.slots === []} @ ghost = ghost_ (
    M.initial_def capacity (None : (Key.t * 'a) option);
    Bridge.empty capacity (None : (Key.t * 'a) option); ())

  let create : ('a : immutable_data).
    (token : 'a state P.token) @ unique ghost ->
    {r : 'a created | bindings r.view === [] && capacity r.view = 16 &&
      not (H.mem (P.own token) (location r.table)) &&
      P.own r.state === H.put (P.own token) (location r.table) (model r.view)} @ unique = fun token ->
    let r = Impl.create token in
    ghost_ (empty_map r.view 16; bindings_def r.view; model_def r.view;
      capacity_def r.view; M.initial_def 16 (Impl.Mutation.empty_entry r.table));
    {table = r.table; view = r.view; state = r.state}

  let length : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {n : int | 0 <= n && Bigint.of_int n = count (bindings view)} = fun table view token ->
    ghost_ (model_def view; bindings_def view; I.valid_def view;
      I.shape_def view.model; count_bridge view.model.slots);
    Impl.length table view token

  let find_opt : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {value : 'a option | value === Map.lookup (bindings view) key} @ immutable = fun table view key token ->
    ghost_ (model_def view; bindings_def view;
      Bridge.lookup view.model.slots key);
    Impl.find_opt table view key token

  let find : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {value : 'a | Map.lookup (bindings view) key === Some value} @ immutable = fun table view key token ->
    ghost_ (model_def view; bindings_def view;
      Bridge.lookup view.model.slots key);
    Impl.find table view key token

  let mem : ('a : immutable_data).
    (table : 'a t) @ immutable -> (view : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model view)}) @ local read ghost ->
    {present : bool | present = (match Map.lookup (bindings view) key with
      | None -> false | Some _ -> true)} = fun table view key token ->
    ghost_ (model_def view; bindings_def view;
      Bridge.lookup view.model.slots key);
    Impl.mem table view key token

  let replace : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | Map.same (bindings r.#view) (Map.put (bindings before) key value) &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique = fun table before key value token ->
    ghost_ (model_def before; bindings_def before);
    let r = Impl.replace table before key value token in
    ghost_ (model_def r.#view; bindings_def r.#view;
      Bridge.same r.#view.model.slots (I.Map.put before.model.slots key value);
      Bridge.put before.model.slots key value);
    #{view = r.#view; state = r.#state}

  let remove : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (key : Key.t) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | Map.same (bindings r.#view) (Map.erase (bindings before) key) &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique = fun table before key token ->
    ghost_ (model_def before; bindings_def before);
    let r = Impl.remove table before key token in
    ghost_ (model_def r.#view; bindings_def r.#view;
      Bridge.same r.#view.model.slots (I.Map.erase before.model.slots key);
      Bridge.erase before.model.slots key);
    #{view = r.#view; state = r.#state}

  let clear : ('a : immutable_data).
    (table : 'a t) @ immutable -> (before : 'a view) @ immutable ->
    (token : {t : 'a state P.token |
      H.at (P.own t) (location table) === Some (model before)}) @ unique read_write ghost ->
    {r : 'a result | bindings r.#view === [] &&
      capacity r.#view = capacity before &&
      P.own r.#state === H.put (P.own token) (location table) (model r.#view)} @ unique = fun table before token ->
    ghost_ (model_def before; bindings_def before);
    let r = Impl.clear table before token in
    ghost_ (empty_map r.#view before.model.capacity;
      model_def r.#view; bindings_def r.#view;
      capacity_def before; capacity_def r.#view;
      M.initial_def before.model.capacity (Impl.Mutation.empty_entry table));
    #{view = r.#view; state = r.#state}
end
