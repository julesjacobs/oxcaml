module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module S = Vox_sequence

module Make (Key : Vox_table_map.Key) = struct
  module Insert = Vox_table_insert.Make (Key)
  module Mutation = Insert.Mutation
  module I = Insert.I
  module Proof = Vox_table_migration_proofs.Make (Key) (I.Map)

  type ('a : immutable_data) result = #{
    complete : bool;
    view : 'a I.view @@ aliased;
    state : P.token @@ ghost;
  }

  let (classify @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (index : int) -> (byte :
        int) ->
      {u : unit | not (I.shape model && I.cells_valid model model.slots 0Z &&
        0 <= index && index < model.capacity && M.control model index === Some
          byte) ||
        (if byte = 128 || byte = 254 then M.slot model index === Some None
         else match M.slot model index with Some (Some _) -> true | _ -> false)}
      @ ghost = fun model index byte -> ghost_ (
    Mutation.Search.R.slot_present model index;
    M.slot_def model index; M.control_def model index;
    match M.slot model index with
    | Some entry ->
      Mutation.Search.R.cell_at model model.slots 0Z (Bigint.of_int index)
        entry;
      (match entry with
       | Some (key, _) ->
         let (_ : {u : unit | 0 <= (Key.hash key land 127) &&
           (Key.hash key land 127) <= 127}) = refine_ () in ()
       | None -> ())
    | None -> ())

  let (next_index @ total) (capacity : int) (index : int) :
      {u : unit | not (0 <= index && index < capacity && capacity <=
        1073741824) ||
        0 <= index + 1 && index + 1 <= capacity &&
        Bigint.of_int (index + 1) = Bigint.add (Bigint.of_int index) 1Z} @
          ghost =
    ghost_ ()

  let rec copy_loop : ('a : immutable_data).
      (source : (Key.t, 'a) T.t) @ immutable ->
      (source_view : {v : 'a I.view | I.valid v}) @ immutable ->
      (destination : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (index : {i : int | 0 <= i && i <= source_view.model.capacity}) ->
      (heap : P.heap) @ immutable ghost ->
      (source_token : {t : P.token |
        H.at (P.own t) (T.location source) === Some source_view.model}) @
          local read ghost ->
      (token : {t : P.token |
        H.at (P.own t) (T.location destination) === Some before.model &&
        P.own t === H.put heap (T.location destination) before.model &&
        I.Map.same before.model.slots (S.take (Bigint.of_int index)
          source_view.model.slots)})
        @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        P.own r.#state === H.put heap (T.location destination)
          r.#view.model &&
        (not r.#complete || I.Map.same r.#view.model.slots
          source_view.model.slots)}
      @ unique = fun source source_view destination before index heap
        source_token token ->
    let capacity = T.capacity source {T.model = source_view.model}
      source_token in
    ghost_ (I.valid_def source_view; I.shape_def source_view.model;
      I.valid_def before);
    if index = capacity then begin
      ghost_ (
        S.take_all source_view.model.slots;
        H.put_law (P.own (borrow_ token)) (T.location destination)
          before.model before.model);
      #{complete = true; view = before; state = token}
    end else begin
      let snapshot = {T.model = source_view.model} in
      let byte = T.read_control source snapshot index source_token in
      ghost_ (
        classify source_view.model index byte;
        M.slot_def source_view.model index;
        next_index capacity index);
      if byte = 128 || byte = 254 then begin
        ghost_ (
          Proof.skip_step source_view.model.slots (Bigint.of_int index)
            before.model.slots);
        copy_loop source source_view destination before (index + 1) heap
          source_token token
      end else begin
        let key = T.read_key source snapshot index source_token in
        let value = T.read_value source snapshot index source_token in
        ghost_ (Proof.destination_absent source_view.model.slots
          (Bigint.of_int index)
          before.model.slots key value);
        let inserted = Insert.try_insert destination before (refine_ key)
          value token in
        if inserted.#inserted then begin
          ghost_ (
            I.valid_def inserted.#view;
            Proof.copy_step source_view.model.slots (Bigint.of_int index)
              before.model.slots inserted.#view.model.slots key value);
          ghost_ (H.put_law heap (T.location destination)
            before.model inserted.#view.model);
          copy_loop source source_view destination inserted.#view
            (index + 1) heap source_token inserted.#state
        end else begin
          #{complete = false; view = inserted.#view; state = inserted.#state}
        end
      end
    end

  let copy : ('a : immutable_data).
      (source : (Key.t, 'a) T.t) @ immutable ->
      (source_view : {v : 'a I.view | I.valid v}) @ immutable ->
      (destination : (Key.t, 'a) T.t) @ immutable ->
      (before : {v : 'a I.view | I.valid v}) @ immutable ->
      (index : {i : int | 0 <= i && i <= source_view.model.capacity}) ->
      (source_token : {t : P.token |
        H.at (P.own t) (T.location source) === Some source_view.model}) @
          local read ghost ->
      (token : {t : P.token |
        H.at (P.own t) (T.location destination) === Some before.model &&
        I.Map.same before.model.slots (S.take (Bigint.of_int index)
          source_view.model.slots)})
        @ unique read_write ghost ->
      {r : 'a result | I.valid r.#view &&
        P.own r.#state === H.put (P.own token) (T.location destination)
          r.#view.model &&
        (not r.#complete || I.Map.same r.#view.model.slots
          source_view.model.slots)}
      @ unique =
    fun source source_view destination before index source_token token ->
      let heap = ghost_ (P.own (borrow_ token)) in
      ghost_ (H.put_law heap (T.location destination)
        before.model before.model);
      copy_loop source source_view destination before index heap source_token
        token
end
