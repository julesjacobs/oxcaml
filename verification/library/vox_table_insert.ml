module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module S = Vox_sequence

module Make (Key : Vox_table_map.Key) = struct
  module Mutation = Vox_table_mutation.Make (Key)
  module I = Mutation.I
  module Proof = Vox_table_insert_proofs.Make (Key) (I)

  let write_new : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (index : {i : int | 0 <= i && i < before.model.capacity}) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (old_byte : int) -> (path : (int * int)) @ ghost ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) ===
        Some before.model && M.slot before.model index === Some None &&
        M.control before.model index === Some old_byte &&
        I.Map.absent before.model.slots key &&
        before.model.size + before.model.deleted + (if old_byte = 254 then 0
          else 1) <=
          before.model.capacity - I.reserve before.model.capacity &&
        I.route before.model (Bigint.of_int index) (Some (key, value)) path})
        @ unique read_write ghost ->
      {r : 'a Mutation.result | I.valid r.#view &&
        I.Map.same r.#view.model.slots (I.Map.put before.model.slots key
          value) &&
        P.own r.#state === H.put (P.own token) (T.location table) r.#view.model}
      @ unique = fun table before index key value old_byte path token ->
    let old_heap = ghost_ (P.own (borrow_ token)) in
    let size = T.size table {T.model = before.model} (borrow_ token) in
    let deleted = T.deleted table {T.model = before.model} (borrow_ token) in
    let state = T.write_slot table {T.model = before.model} index key value
      token in
    let slots = ghost_ (M.set_slot before.model index (Some (key, value))) in
    let byte = Key.hash key land 127 in
    ghost_ (
      I.valid_def before; I.shape_def before.model;
      M.set_slot_def before.model index (Some (key, value)));
    let state = Mutation.write_byte table {T.model = slots} index byte state in
    let marked = ghost_ (M.set_byte slots index byte) in
    let state = T.set_counts table {T.model = marked} (size + 1)
      (deleted - (if old_byte = 254 then 1 else 0)) state in
    let model = ghost_ (Proof.insert_model before.model index key value
      old_byte) in
    let view = {I.model;
      routes = ghost_ (S.set before.routes (Bigint.of_int index) path);
      plan = before.plan} in
    ghost_ (
      Proof.fingerprint_def key;
      Proof.insert_model_def before.model index key value old_byte;
      H.put_law old_heap (T.location table) slots marked;
      H.put_law old_heap (T.location table) marked model;
      Proof.insert_valid before index key value old_byte path view);
    (#{Mutation.view; state} : 'a Mutation.result)
  module Vacancy = Vox_table_vacancy.Make (Key) (Mutation.Search.R)

  type ('a : immutable_data) attempt = #{
    inserted : bool;
    view : 'a I.view @@ aliased;
    state : (Key.t, 'a) M.state P.token @@ ghost;
  }

  let try_insert : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (key : {k : Key.t | I.Map.absent before.model.slots k}) @ immutable ->
      (value : 'a) @ immutable ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location table) ===
        Some before.model}) @ unique read_write ghost ->
      {r : 'a attempt | I.valid r.#view &&
        (if r.#inserted then
          I.Map.same r.#view.model.slots (I.Map.put before.model.slots key
            value) &&
          P.own r.#state === H.put (P.own token) (T.location table)
            r.#view.model
         else r.#view.model === before.model && P.own r.#state === P.own token)}
      @ unique = fun table before key value token ->
    let found = Vacancy.find table before key (ghost_ value) (borrow_ token) in
    begin
      let snapshot = {T.model = before.model} in
      let size = T.size table snapshot (borrow_ token) in
      let deleted = T.deleted table snapshot (borrow_ token) in
      let capacity = T.capacity table snapshot (borrow_ token) in
      ghost_ (I.reserve_def capacity);
      if size + deleted + (if found.#byte = 254 then 0 else 1) >
          capacity - (capacity lsr 3) then
        #{inserted = false; view = before; state = token}
      else begin
        let result = write_new table before found.#index key value found.#byte
          found.#path token in
        #{inserted = true; view = result.#view; state = result.#state}
      end
    end

end
