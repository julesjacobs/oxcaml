module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module B = Vox_table_bits
module W = Vox_table_wrap

module Make (Key : Vox_table_map.Key) = struct
  module Proof = Vox_table_stop_proof.Make (Key)
  module Spec = Proof.Spec
  module R = Spec.R
  module I = R.I

  let rec candidates : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (capacity : {c : int | c = view.model.capacity}) ->
      (group : {g : int | 0 <= g && g < view.model.capacity}) ->
      (needle : {n : int | 0 <= n && n <= 127}) ->
      (mask : {m : B.mask | m land M.matching view.model group needle 16 = m})
        ->
      (token : {t : P.token | H.at (P.own t) (T.location table) ===
        Some view.model}) @ local read ghost ->
      {index : int | if index = -1 then
        Spec.candidates_absent view.model query group mask
        else 0 <= index && index < view.model.capacity &&
          (match M.slot view.model index with
           | Some (Some (stored, _)) -> Key.equal stored query
           | _ -> false)} =
    fun table view query capacity group needle mask token ->
    ghost_ (
      I.valid_def view;
      R.capacity_bounds view.model;
      Spec.candidates_absent_def view.model query group mask);
    if mask = 0 then -1 else
      let lane = B.first (refine_ mask) in
      let index = (group + lane) land (capacity - 1) in
      ghost_ (
        I.wrap_def capacity (group + lane);
        W.wrap_range capacity (group + lane);
        let (_ : {u : unit | M.matching view.model group needle 16 land
          M.lane_bit lane <> 0}) = refine_ () in
        R.initialized view.model group needle lane;
        let (_ : {u : unit | match M.slot view.model index with
          | Some (Some _) -> true | _ -> false}) = refine_ () in
        ());
      let snapshot = {T.model = view.model} in
      let stored = T.read_key table snapshot index token in
      if Key.equal stored query then index else begin
        ghost_ (Spec.misses_def view.model query index);
        let rest = B.clear mask in
        candidates table view query capacity group needle (refine_ rest) token
      end
  (* Keep this specialization boundary so small comparators inline in the loop. *)
  let[@inline never] rec groups : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (capacity : {c : int | c = view.model.capacity}) ->
      (hash : {h : int | h = Key.hash query}) ->
      (rank : {r : int | 0 <= r && r <= (view.model.capacity lsr 4)}) ->
      (group : int) -> (step : int) ->
      (token : {t : P.token | H.at (P.own t) (T.location table) ===
        Some view.model && Spec.prefix_absent view.model query rank &&
        I.probe view.model.capacity (Key.hash query) rank === (group, step)})
        @ local read ghost ->
      {index : int | if index = -1 then
        I.Map.absent view.model.slots query &&
        I.Map.lookup view.model.slots query === None
        else 0 <= index && index < view.model.capacity &&
          (match M.slot view.model index with
           | Some (Some (stored, _)) -> Key.equal stored query
           | _ -> false)} =
    fun table view query capacity hash rank group step token ->
    ghost_ (I.valid_def view; R.capacity_bounds view.model);
    if rank = capacity lsr 4 then begin
      ghost_ (Proof.exhausted view query);
      -1
    end else begin
      ghost_ (
        I.group_def capacity hash rank;
        R.group_in_shape view.model hash rank);
      let snapshot = {T.model = view.model} in
      let needle = hash land 127 in
      let scanned = T.match16_empty table snapshot group needle token in
      let mask = scanned land 65535 in
      let found = candidates table view query capacity group needle
        (refine_ mask) token in
      if found >= 0 then found else begin
        let empty = scanned land 65536 in
        ghost_ (Spec.prefix_absent_def view.model query (rank + 1));
        if empty <> 0 then begin
          ghost_ (Proof.empty_stop view query rank);
          -1
        end else begin
          let next_group = (group + step) land (capacity - 1) in
          ghost_ (
            R.next_probe capacity hash rank group step);
          groups table view query capacity hash (rank + 1)
            next_group (step + 16) token
        end
      end
    end

  let find_index_hashed : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (hash : {h : int | h = Key.hash query}) ->
      (token : {t : P.token | H.at (P.own t) (T.location table) ===
        Some view.model}) @ local read ghost ->
      {index : int | if index = -1 then
        I.Map.absent view.model.slots query &&
        I.Map.lookup view.model.slots query === None
        else 0 <= index && index < view.model.capacity &&
          (match M.slot view.model index with
           | Some (Some (stored, _)) -> Key.equal stored query
           | _ -> false)} = fun table view query hash token ->
    let snapshot = {T.model = view.model} in
    let capacity = T.capacity table snapshot token in
    let group = (hash lsr 7) land (capacity - 1) in
    ghost_ (
      I.valid_def view; R.capacity_bounds view.model;
      Spec.prefix_absent_def view.model query 0;
      I.probe_def capacity hash 0; I.wrap_def capacity (hash lsr 7));
    if capacity = 16 then begin
      ghost_ (I.group_def capacity hash 0; R.group_in_shape view.model hash 0);
      let needle = hash land 127 in
      let mask = T.match16 table snapshot group needle token in
      let found = candidates table view query capacity group needle
        (refine_ mask) token in
      if found >= 0 then found else begin
        ghost_ (
          Spec.prefix_absent_def view.model query 1;
          Proof.exhausted view query);
        -1
      end
    end else
      groups table view query capacity hash 0 group 16 token


  let find_index : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) ===
        Some view.model}) @ local read ghost ->
      {index : int | if index = -1 then
        I.Map.absent view.model.slots query &&
        I.Map.lookup view.model.slots query === None
        else 0 <= index && index < view.model.capacity &&
          (match M.slot view.model index with
           | Some (Some (stored, _)) -> Key.equal stored query
           | _ -> false)} = fun table view query token ->
    find_index_hashed table view query (Key.hash query) token

  let find_opt : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a I.view | I.valid v}) @ immutable ->
      (query : Key.t) @ immutable ->
      (token : {t : P.token | H.at (P.own t) (T.location table) ===
        Some view.model}) @ local read ghost ->
      {value : 'a option | value === I.Map.lookup view.model.slots query}
      @ immutable = fun table view query token ->
    let index = find_index table view query token in
    if index = -1 then None else begin
      let snapshot = {T.model = view.model} in
      let value = T.read_value table snapshot index token in
      ghost_ (
        I.valid_def view;
        M.slot_def view.model index;
        match M.slot view.model index with
        | Some (Some (key, stored)) ->
          I.Map.lookup_at view.model.slots (Bigint.of_int index) key stored
            query
        | _ -> ());
      Some value
    end

end
