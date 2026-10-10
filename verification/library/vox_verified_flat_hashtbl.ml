module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module T = Vox_table_storage

module type Key = Vox_table_map.Key
module Make (Key : Key) = struct
  module Impl = Vox_table_implementation.Make (Key)
  module I = Impl.Spec
  module Bridge = Vox_table_bindings_bridge.Make (Key) (I.Map)
  module Model = Bridge.Model
  type 'a t = 'a Impl.t
  type 'a resource = {
    table : 'a t @@ ghost global;
    storage : 'a I.view @@ ghost global;
    token : (Key.t, 'a) M.state P.token @@ ghost;
  }
  type 'a permission = {p : 'a resource |
    I.valid p.storage &&
    H.at (P.own p.token) (T.location p.table) === Some p.storage.model}

  let[@def transparent] (owner @ total)
      (p : 'a permission @ local immutable) = ghost_ p.table
  let[@def transparent] (bindings @ total)
      (p : 'a permission @ local immutable) =
    ghost_ (Bridge.model p.storage.model.slots)

  type 'a created = #{
    table : 'a t @@ aliased;
    permission : 'a permission;
  }

  let (storage @ total) (p : 'a permission @ local immutable) :
      {v : 'a I.view | v === ghost_ p.storage} @ immutable =
    {I.model = ghost_ p.storage.model; routes = ghost_ p.storage.routes;
     plan = ghost_ p.storage.plan}

  let rec (count_bridge @ total) (slots : 'a I.Map.slots) :
      {u : unit | not (I.Map.distinct slots) ||
        Model.cardinal (Bridge.model slots) = I.live_count slots}
      @ ghost = ghost_ (
    Bridge.model_def slots; I.Map.distinct_def slots; I.live_count_def slots;
    match slots with
    | [] -> ()
    | None :: tail -> count_bridge tail
    | Some (key, _) :: tail ->
      I.Map.absent_lookup tail key; Bridge.lookup tail key; count_bridge tail)

  let (lookup_agrees @ total) (view : 'a I.view @ immutable) (key : Key.t) :
      {u : unit | Model.find_opt key (Bridge.model view.model.slots) ===
        I.Map.lookup view.model.slots key} @ ghost = ghost_ (
    Bridge.lookup view.model.slots key)

  let (count_agrees @ total) (view : {v : 'a I.view | I.valid v} @ immutable) :
      {u : unit | Model.cardinal (Bridge.model view.model.slots) =
        I.live_count view.model.slots} @ ghost = ghost_ (
    I.valid_def view;
    count_bridge view.model.slots)

  let (empty_view @ total) (storage : 'a I.view @ immutable) (capacity : int) :
      {u : unit | not (storage.model === M.initial capacity None) ||
        Bridge.model storage.model.slots === Model.empty ()} @ ghost = ghost_ (
    M.initial_def capacity (None : (Key.t * 'a) option);
    Bridge.empty capacity (None : (Key.t * 'a) option))

  let (put_view @ total) (before : 'a I.view @ immutable) (key : Key.t)
      (value : 'a) (storage : 'a I.view @ immutable) :
      {u : unit | not (I.Map.same storage.model.slots
          (I.Map.put before.model.slots key value)) ||
        Bridge.model storage.model.slots ===
          Model.add key value (Bridge.model before.model.slots)}
      @ ghost = ghost_ (
    Bridge.put before.model.slots key value;
    Bridge.same storage.model.slots (I.Map.put before.model.slots key value))

  let (erase_view @ total) (before : 'a I.view @ immutable) (key : Key.t)
      (storage : 'a I.view @ immutable) :
      {u : unit | not (I.Map.same storage.model.slots
          (I.Map.erase before.model.slots key)) ||
        Bridge.model storage.model.slots ===
          Model.remove key (Bridge.model before.model.slots)}
      @ ghost = ghost_ (
    Bridge.erase before.model.slots key;
    Bridge.same storage.model.slots (I.Map.erase before.model.slots key))

  let create : unit ->
    {r : 'a created | owner r.#permission === r.#table &&
      bindings r.#permission === Model.empty ()} @ unique = fun () ->
    let r = Impl.create (P.empty ()) in
    ghost_ (empty_view r.view 16;
      M.initial_def 16 (Impl.Mutation.empty_entry r.table));
    let p : 'a permission =
      {table = ghost_ r.table; storage = r.view; token = r.state} in
    #{table = r.table; permission = p}

  let length :
    (table : 'a t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {n : int | Bigint.of_int n = Model.cardinal (bindings p)} = fun table p ->
    ghost_ (count_agrees p.storage;
      I.valid_def p.storage; I.shape_def p.storage.model);
    Impl.length table (storage p) p.token

  let find_opt :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {value : 'a option | value === Model.find_opt key (bindings p)} =
    fun table key p ->
    ghost_ (lookup_agrees p.storage key);
    Impl.find_opt table (storage p) key p.token

  let find :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {value : 'a | Model.find_opt key (bindings p) === Some value} =
    fun table key p ->
    ghost_ (lookup_agrees p.storage key);
    Impl.find table (storage p) key p.token

  let mem :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ local read ->
    {present : bool | present = Model.mem key (bindings p)} = fun table key p ->
    ghost_ (lookup_agrees p.storage key);
    Impl.mem table (storage p) key p.token

  let replace :
    (table : 'a t) -> (key : Key.t) -> (value : 'a) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.add key value (bindings p)} @ unique =
    fun table key value p ->
    let before = storage (borrow_ p) in
    let r = Impl.replace table before key value p.token in
    ghost_ (put_view before key value r.#view);
    let q : 'a permission =
      {table = ghost_ table; storage = r.#view; token = r.#state} in
    q

  let remove :
    (table : 'a t) -> (key : Key.t) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.remove key (bindings p)} @ unique =
    fun table key p ->
    let before = storage (borrow_ p) in
    let r = Impl.remove table before key p.token in
    ghost_ (erase_view before key r.#view);
    let q : 'a permission =
      {table = ghost_ table; storage = r.#view; token = r.#state} in
    q

  let clear :
    (table : 'a t) ->
    (p : {p : 'a permission | owner p === table}) @ unique ->
    {q : 'a permission | owner q === table &&
      bindings q === Model.empty ()} @ unique = fun table p ->
    let before = storage (borrow_ p) in
    let r = Impl.clear table before p.token in
    ghost_ (empty_view r.#view before.model.capacity;
      M.initial_def before.model.capacity (Impl.Mutation.empty_entry table));
    let q : 'a permission =
      {table = ghost_ table; storage = r.#view; token = r.#state} in
    q

end
