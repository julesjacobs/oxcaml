module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model

module Make (Key : Vox_table_map.Key) = struct
  module Search = Vox_table_search.Make (Key)
  module I = Search.I
  module Proof = Vox_table_update_proofs.Make (Key) (I)

  type ('a : immutable_data) result = #{
    view : 'a I.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }

  module Initial = Vox_table_initial.Make (Key) (I)

  type ('a : immutable_data) created = {
    table : (Key.t, 'a) T.t @@ aliased;
    view : 'a I.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }

  let (empty_entry @ total) : ('a : immutable_data).
      (Key.t, 'a) T.t @ immutable ->
      {entry : (Key.t * 'a) option | entry === None} @ immutable ghost =
    fun _ -> ghost_ None

  let create_capacity : ('a : immutable_data).
      (capacity : {c : int | 16 <= c && c <= 1073741824 && c land (c - 1) =
        0}) ->
      (plan : {p : Vox_table_probe.plan | Vox_table_probe.valid p &&
        capacity = Vox_table_wrap.scale16 (Vox_table_probe.groups p)}) @
          immutable ghost ->
      (token : (Key.t, 'a) M.state P.token) @ unique ghost ->
      {r : 'a created | I.valid r.view &&
        r.view.model === M.initial capacity None &&
        not (H.mem (P.own token) (T.location r.table)) &&
        P.own r.state === H.put (P.own token) (T.location r.table) r.view.model}
      @ unique = fun capacity plan token ->
    let allocated = T.create capacity token in
    let entry = empty_entry allocated.value in
    let view = {I.model = ghost_ (M.initial capacity entry);
      routes = ghost_ (M.repeat capacity (0, 0)); plan} in
    ghost_ (Initial.initial capacity entry view);
    {table = allocated.value; view; state = allocated.state}

  let create : ('a : immutable_data).
      (token : (Key.t, 'a) M.state P.token) @ unique ghost ->
      {r : 'a created | I.valid r.view &&
        r.view.model === M.initial 16 None &&
        not (H.mem (P.own token) (T.location r.table)) &&
        P.own r.state === H.put (P.own token) (T.location r.table) r.view.model}
      @ unique = fun token ->
    let allocated = T.create 16 token in
    let entry = empty_entry allocated.value in
    let view = {I.model = ghost_ (M.initial 16 entry);
      routes = ghost_ (M.repeat 16 (0, 0)); plan = Vox_table_probe.One} in
    ghost_ (
      Vox_table_probe.valid_def Vox_table_probe.One;
      Vox_table_probe.groups_def Vox_table_probe.One;
      Vox_table_wrap.scale16_def 1;
      Initial.initial 16 entry view);
    {table = allocated.value; view; state = allocated.state}

  let clear : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model}) @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        r.#view.model === M.initial before.model.capacity None &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before token ->
    let state = T.clear table {T.model = before.model} token in
    let entry = empty_entry table in
    let view = {I.model = ghost_ (M.initial before.model.capacity entry);
      routes = ghost_ (M.repeat before.model.capacity (0, 0)); plan =
        before.plan} in
    ghost_ (
      I.valid_def before; I.shape_def before.model; I.power_of_two_def
        before.model.capacity;
      Initial.initial before.model.capacity entry view);
    (#{view; state} : 'a result)

  let write_byte : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : (Key.t, 'a) T.view |
        16 <= v.model.capacity && v.model.capacity <= 1073741824}) @ immutable
          ->
      (index : {i : int | 0 <= i && i < before.model.capacity}) ->
      (byte : {b : int | 0 <= b && b <= 255}) ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model}) @ unique read_write ghost ->
      {t : (Key.t, 'a) M.state P.token | P.own t === H.put (P.own token)
        (T.location table)
        (M.set_byte before.model index byte)} @ unique ghost =
    fun table before index byte token ->
      let old_heap = ghost_ (P.own (borrow_ token)) in
      let state = T.write_control table before index byte token in
      ghost_ (M.set_byte_def before.model index byte);
      if index < 15 then begin
        let capacity = T.capacity table
          {T.model = ghost_ (M.set_control before.model index byte)}
          (borrow_ state) in
        ghost_ (M.set_control_def before.model index byte);
        let changed = ghost_ (M.set_control before.model index byte) in
        let state = T.write_control table {T.model = changed}
          (capacity + index) byte state in
        ghost_ (H.put_law old_heap (T.location table) changed
          (M.set_control changed (capacity + index) byte));
        state
      end else state

  let erase_existing : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (index : {i : int | 0 <= i && i < before.model.capacity}) ->
      (key : Key.t) @ immutable ghost -> (value : 'a) @ immutable ghost ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model && M.slot before.model index === Some (Some (key,
          value))})
        @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.erase before.model.slots key) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before index key value token ->
    let old_heap = ghost_ (P.own (borrow_ token)) in
    let size = T.size table {T.model = before.model} (borrow_ token) in
    let deleted = T.deleted table {T.model = before.model} (borrow_ token) in
    let state = T.clear_slot table {T.model = before.model} index token in
    let vacant = ghost_ (M.set_slot before.model index None) in
    ghost_ (
      I.valid_def before; I.shape_def before.model;
      M.set_slot_def before.model index None);
    let state = write_byte table {T.model = vacant} index 254 state in
    let marked = ghost_ (M.set_byte vacant index 254) in
    let state = T.set_counts table {T.model = marked} (size - 1) (deleted + 1)
      state in
    let model = ghost_ (M.remove_slot before.model index) in
    let view = {I.model; routes = before.routes; plan = before.plan} in
    ghost_ (
      M.remove_slot_def before.model index;
      H.put_law old_heap (T.location table) vacant marked;
      H.put_law old_heap (T.location table) marked model;
      Proof.remove_valid before index key value view);
    (#{view; state} : 'a result)

  let remove : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model}) @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.erase before.model.slots query) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before query token ->
    let index = Search.find_index table before query (borrow_ token) in
    if index = -1 then begin
      ghost_ (
        I.valid_def before;
        I.Map.same_absent_remove before.model.slots query;
        H.put_law (P.own (borrow_ token)) (T.location table) before.model
          before.model);
      (#{view = before; state = token} : 'a result)
    end else begin
      let entry = ghost_ (M.slot before.model index) in
      let key = ghost_ (match entry with Some (Some (key, _)) -> key | _ ->
        query) in
      let value = ghost_ (match entry with
        | Some (Some (_, value)) -> value
        | _ -> unreachable_ ()) in
      let result = erase_existing table before index key value token in
      ghost_ (
        I.valid_def before; I.valid_def result.#view;
        M.slot_def before.model index;
        I.Map.absent_at before.model.slots (Bigint.of_int index) query;
        I.Map.erase_congruent before.model.slots key query;
        I.Map.erase_distinct before.model.slots query;
        I.Map.same_transitive result.#view.model.slots
          (I.Map.erase before.model.slots key) (I.Map.erase before.model.slots
            query));
      (#{view = result.#view; state = result.#state} : 'a result)
    end

  let write_existing : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (index : {i : int | 0 <= i && i < before.model.capacity}) ->
      (key : {k : Key.t | match M.slot before.model index with
        | Some (Some (stored, _)) -> stored === k | _ -> false})
        @ immutable ghost ->
      (value : 'a) @ immutable ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model}) @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.put before.model.slots key
          value) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before index key value token ->
    let state = T.write_value table {T.model = before.model} index {T.key} value
      token in
    let view = {I.model = ghost_ (M.set_slot before.model index (Some (key,
      value)));
      routes = before.routes; plan = before.plan} in
    ghost_ (Proof.replace_value before index key value view);
    (#{view; state} : 'a result)
end
