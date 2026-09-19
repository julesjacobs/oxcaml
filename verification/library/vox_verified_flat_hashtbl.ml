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
    view : 'a Spec.view @@ aliased;
    state : P.token @@ ghost;
  }
  type ('a : immutable_data) result = 'a Mutation.result = #{
    view : 'a Spec.view @@ aliased;
    state : P.token @@ ghost;
  }

  let create = Mutation.create
  let find_opt = Mutation.Search.find_opt
  let remove = Mutation.remove
  let clear = Mutation.clear

  let length : ('a : immutable_data).
      (table : 'a t) @ immutable -> (view : {v : 'a Spec.view | Spec.valid v})
        @ immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        view.model})
        @ local read ghost ->
      {size : int | size = view.model.size} = fun table view token ->
    T.size table {T.model = view.model} token

  let find : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable -> (key : Key.t) @
        immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        view.model})
        @ local read ghost ->
      {value : 'a | I.Map.lookup view.model.slots key === Some value} @
        immutable =
    fun table view key token ->
      let index = Mutation.Search.find_index table view key token in
      if index = -1 then raise Not_found else begin
        let value = T.read_value table {T.model = view.model} index token in
        ghost_ (
          I.valid_def view; Vox_table_model.slot_def view.model index;
          match Vox_table_model.slot view.model index with
          | Some (Some (stored, v)) ->
            I.Map.lookup_at view.model.slots (Bigint.of_int index) stored v key
          | _ -> ());
        value
      end

  let mem : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable -> (key : Key.t) @
        immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        view.model})
        @ local read ghost ->
      {present : bool | present = (match I.Map.lookup view.model.slots key with
        | None -> false | Some _ -> true)} = fun table view key token ->
    let index = Mutation.Search.find_index table view key token in
    ghost_ (
      I.valid_def view; Vox_table_model.slot_def view.model index;
      if index <> -1 then
        match Vox_table_model.slot view.model index with
        | Some (Some (stored, value)) ->
          I.Map.lookup_at view.model.slots (Bigint.of_int index) stored value
            key
        | _ -> ());
    index <> -1

  let rec replace : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) === Some
        before.model})
        @ unique read_write ghost ->
      {r : 'a Mutation.result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.put before.model.slots key
          value) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before key value token ->
    let index = Mutation.Search.find_index table before key (borrow_ token) in
    if index <> -1 then begin
      let stored = T.read_key table {T.model = before.model} index (borrow_
        token) in
      let result = Mutation.write_existing table before index stored value
        token in
      ghost_ (
        I.valid_def before; I.valid_def result.#view;
        I.Map.put_congruent before.model.slots stored key value;
        I.Map.put_distinct before.model.slots key value;
        I.Map.same_transitive result.#view.model.slots
          (I.Map.put before.model.slots stored value) (I.Map.put
            before.model.slots key value));
      (#{Mutation.view = result.#view; state = result.#state} : 'a
        Mutation.result)
    end else begin
      let old_heap = ghost_ (P.own (borrow_ token)) in
      let attempted = Insert.try_insert table before (refine_ key) value token
        in
      if attempted.#inserted then
        (#{Mutation.view = attempted.#view; state = attempted.#state} : 'a
          Mutation.result)
      else begin
        let rebuilt = Resize.rebuild table before attempted.#state in
        let result = replace table rebuilt.#view key value rebuilt.#state in
        ghost_ (
          I.valid_def before; I.valid_def rebuilt.#view; I.valid_def
            result.#view;
          I.Map.put_same rebuilt.#view.model.slots before.model.slots key value;
          I.Map.put_distinct before.model.slots key value;
          I.Map.same_transitive result.#view.model.slots
            (I.Map.put rebuilt.#view.model.slots key value) (I.Map.put
              before.model.slots key value);
          H.put_law old_heap (T.location table) rebuilt.#view.model
            result.#view.model);
        (#{Mutation.view = result.#view; state = result.#state} : 'a
          Mutation.result)
      end
    end
end
