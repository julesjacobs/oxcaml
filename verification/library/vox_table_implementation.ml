module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model

module Make (Key : Vox_table_map.Key) = struct
  module Resize = Vox_table_resize.Make (Key)
  module Insert = Resize.Insert
  module Mutation = Resize.Mutation
  module I = Resize.I
  module Spec = I

  type ('a : immutable_data) t = (Key.t, 'a) T.t
  type ('a : immutable_data) created = 'a Mutation.created = {
    table : 'a t @@ aliased;
    view : 'a I.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }
  type ('a : immutable_data) result = 'a Mutation.result = #{
    view : 'a I.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }

  let create = Mutation.create
  let find_opt = Mutation.Search.find_opt
  let remove = Mutation.remove
  let clear = Mutation.clear

  module Proof = struct
    (* The slot that [find_index] reports holds the binding for [key]. *)
    let (lookup_found @ total) : ('a : immutable_data).
        (view : 'a I.view) @ immutable -> (index : int) -> (key : Key.t) ->
        {u : unit | not (I.valid view && 0 <= index &&
            index < view.model.capacity) ||
          (match M.slot view.model index with
           | Some (Some (stored, value)) ->
             not (Key.equal stored key) ||
             I.Map.lookup view.model.slots key === Some value
           | _ -> true)} @ ghost =
      fun view index key -> ghost_ (
        I.valid_def view; M.slot_def view.model index;
        match M.slot view.model index with
        | Some (Some (stored, value)) ->
          I.Map.lookup_at view.model.slots (Bigint.of_int index) stored value
            key
        | _ -> ())

    let (absent_after_rebuild @ total) : ('a : immutable_data).
        (before : 'a I.view) @ immutable -> (rebuilt : 'a I.view) @ immutable ->
        (key : Key.t) ->
        {u : unit | not (I.Map.absent before.model.slots key &&
          I.Map.same rebuilt.model.slots before.model.slots) ||
          I.Map.absent rebuilt.model.slots key} @ ghost =
      fun before rebuilt key -> ghost_ (
        I.Map.absent_lookup before.model.slots key;
        I.Map.same_get rebuilt.model.slots before.model.slots key;
        I.Map.absent_lookup rebuilt.model.slots key)

    let (put_after_rebuild @ total) : ('a : immutable_data).
        (before : 'a I.view) @ immutable -> (rebuilt : 'a I.view) @ immutable ->
        (after : 'a I.view) @ immutable -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (I.valid before && I.valid rebuilt && I.valid after &&
          I.Map.same rebuilt.model.slots before.model.slots &&
          I.Map.same after.model.slots (I.Map.put rebuilt.model.slots key value)) ||
          I.Map.same after.model.slots (I.Map.put before.model.slots key value)}
        @ ghost = fun before rebuilt after key value -> ghost_ (
      I.valid_def before; I.valid_def rebuilt; I.valid_def after;
      I.Map.put_same rebuilt.model.slots before.model.slots key value;
      I.Map.put_distinct before.model.slots key value;
      I.Map.same_transitive after.model.slots
        (I.Map.put rebuilt.model.slots key value)
        (I.Map.put before.model.slots key value))

    let (put_equal_key @ total) : ('a : immutable_data).
        (before : 'a I.view) @ immutable -> (after : 'a I.view) @ immutable ->
        (stored : Key.t) -> (key : Key.t) -> (value : 'a) ->
        {u : unit | not (I.valid before && I.valid after && Key.equal stored key &&
          I.Map.same after.model.slots (I.Map.put before.model.slots stored value)) ||
          I.Map.same after.model.slots (I.Map.put before.model.slots key value)}
        @ ghost = fun before after stored key value -> ghost_ (
      I.valid_def before; I.valid_def after;
      I.Map.put_congruent before.model.slots stored key value;
      I.Map.put_distinct before.model.slots key value;
      I.Map.same_transitive after.model.slots
        (I.Map.put before.model.slots stored value)
        (I.Map.put before.model.slots key value))
  end

  let length : ('a : immutable_data).
      (table : 'a t) -> (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (token : {t : (Key.t, 'a) M.state P.token |
        H.at (P.own t) (T.location table) === Some view.model})
        @ local read ghost ->
      {size : int | size = view.model.size} = fun table view token ->
    T.size table {T.model = view.model} token

  let find : ('a : immutable_data).
      (table : 'a t) -> (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : Key.t) ->
      (token : {t : (Key.t, 'a) M.state P.token |
        H.at (P.own t) (T.location table) === Some view.model})
        @ local read ghost ->
      {value : 'a | I.Map.lookup view.model.slots key === Some value} =
    fun table view key token ->
    let index = Mutation.Search.find_index table view key token in
    if index = -1 then raise Not_found;
    let value = T.read_value table {T.model = view.model} index token in
    ghost_ (Proof.lookup_found view index key);
    value

  let mem : ('a : immutable_data).
      (table : 'a t) -> (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : Key.t) ->
      (token : {t : (Key.t, 'a) M.state P.token |
        H.at (P.own t) (T.location table) === Some view.model})
        @ local read ghost ->
      {present : bool | present = (match I.Map.lookup view.model.slots key with
        | None -> false | Some _ -> true)} = fun table view key token ->
    let index = Mutation.Search.find_index table view key token in
    ghost_ (if index <> -1 then Proof.lookup_found view index key);
    index <> -1

  (* Insert a key known to be absent, rebuilding until a vacancy is found.
     The key stays absent across a rebuild, so the retry skips the search. *)
  let rec insert_absent_hashed : ('a : immutable_data).
      (table : 'a t) -> (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : {k : Key.t | I.Map.absent before.model.slots k}) -> (value : 'a) ->
      (hash : {h : int | h = Key.hash key}) ->
      (token : {t : (Key.t, 'a) M.state P.token |
        H.at (P.own t) (T.location table) === Some before.model})
        @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.put before.model.slots key value) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before key value hash token ->
    let old_heap = ghost_ (P.own (borrow_ token)) in
    let attempted = Insert.try_insert_hashed table before key value hash token in
    if attempted.#inserted then #{view = attempted.#view; state = attempted.#state}
    else begin
      let rebuilt = Resize.rebuild table before attempted.#state in
      ghost_ (Proof.absent_after_rebuild before rebuilt.#view key);
      let result = insert_absent_hashed table rebuilt.#view key value hash
        rebuilt.#state in
      ghost_ (
        Proof.put_after_rebuild before rebuilt.#view result.#view key value;
        H.put_law old_heap (T.location table) rebuilt.#view.model
          result.#view.model);
      #{view = result.#view; state = result.#state}
    end

  (* Overwrite the value of an existing equal key, or insert a new binding. *)
  let replace : ('a : immutable_data).
      (table : 'a t) -> (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : Key.t) -> (value : 'a) ->
      (token : {t : (Key.t, 'a) M.state P.token |
        H.at (P.own t) (T.location table) === Some before.model})
        @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.put before.model.slots key value) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before key value token ->
    let hash = Key.hash key in
    let index = Mutation.Search.find_index_hashed table before key hash
      (borrow_ token) in
    if index = -1 then insert_absent_hashed table before key value hash token
    else begin
      let stored = ghost_ (match M.slot before.model index with
        | Some (Some (stored, _)) -> stored
        | _ -> unreachable_ ()) in
      let result = Mutation.write_existing table before index stored value
        token in
      ghost_ (Proof.put_equal_key before result.#view stored key value);
      #{view = result.#view; state = result.#state}
    end
end
