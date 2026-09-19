module T = Vox_table_storage
module P = Ghost_pref
module H = P.Heap
module M = Vox_table_model
module B = Vox_table_bits
module W = Vox_table_wrap

module Make (Key : Vox_table_map.Key)
    (Read : module type of Vox_table_read_proofs.Make (Key)) = struct
  module Progress = Vox_table_vacancy_progress.Make (Key) (Read)
  type found = #{ index : int; byte : int; path : (int * int) @@ ghost }

  let (vacant @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (index : int) -> (byte : int) ->
      {u : unit | not (Read.I.shape model &&
        Read.I.cells_valid model model.slots 0Z &&
        0 <= index && index < model.capacity && (byte = 128 || byte = 254) &&
        M.control model index === Some byte) || M.slot model index === Some
          None}
      @ ghost = fun model index byte -> ghost_ (
    Read.slot_present model index;
    M.slot_def model index; M.control_def model index;
    match M.slot model index with
    | Some entry ->
      Read.cell_at model model.slots 0Z (Bigint.of_int index) entry;
      (match entry with
       | Some (key, _) ->
         let (_ : {u : unit | 0 <= (Key.hash key land 127) &&
           (Key.hash key land 127) <= 127}) = refine_ () in ()
       | None -> ())
    | None -> ())

  let (route_here @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable -> (rank : int) -> (group : int) ->
      (lane : int) -> (index : int) ->
      {u : unit | not (0 <= rank && rank < (model.capacity lsr 4) &&
        0 <= lane && lane < 16 && Read.I.group model.capacity (Key.hash key)
          rank = group &&
        index = Read.I.wrap model.capacity (group + lane) &&
        Read.I.empty_free model (Key.hash key) rank) ||
        Read.I.route model (Bigint.of_int index) (Some (key, value)) (rank,
          lane)}
      @ ghost = fun model key value rank group lane index -> ghost_ (
    Read.I.route_def model (Bigint.of_int index) (Some (key, value)) (rank,
      lane);
    Read.I.route_position_def model.capacity (Key.hash key) (Bigint.of_int
      index)
      (rank, lane);
    ())

  let rec scan : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a Read.I.view | Read.I.valid v}) @ immutable ->
      (query : Key.t) @ immutable -> (value : 'a) @ immutable ghost ->
      (capacity : {c : int | c = view.model.capacity}) ->
      (hash : {h : int | h = Key.hash query}) ->
      (rank : {r : int | 0 <= r && r <= (view.model.capacity lsr 4)}) ->
      (group : int) -> (step : int) ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location
        table) === Some
        view.model &&
        Read.I.empty_free view.model hash rank &&
        Read.I.probe capacity hash rank === (group, step)}) @ local read ghost
          ->
      {r : found | 0 <= r.#index && r.#index < view.model.capacity &&
         (r.#byte = 128 || r.#byte = 254) &&
         M.control view.model r.#index === Some r.#byte &&
         M.slot view.model r.#index === Some None &&
         Read.I.route view.model (Bigint.of_int r.#index)
           (Some (query, value)) r.#path} =
    fun table view query value capacity hash rank group step token ->
      ghost_ (Read.I.valid_def view; Read.capacity_bounds view.model);
      if rank = capacity lsr 4 then begin
        ghost_ (Progress.not_exhausted view hash);
        unreachable_ ()
      end else begin
        ghost_ (
          Read.I.group_def capacity hash rank;
          Read.group_in_shape view.model hash rank);
        let snapshot = {T.model = view.model} in
        let deleted = T.match16 table snapshot group 254 token in
        let empty = T.match16 table snapshot group 128 token in
        if deleted <> 0 || empty <> 0 then begin
          let mask = if deleted <> 0 then deleted else empty in
          let byte = if deleted <> 0 then 254 else 128 in
          let lane = B.first (refine_ mask) in
          let index = (group + lane) land (capacity - 1) in
          ghost_ (
            Read.I.wrap_def capacity (group + lane);
            W.wrap_range capacity (group + lane);
            Read.matching_control view.model group byte lane;
            vacant view.model index byte;
            route_here view.model query value rank group lane index);
          #{index; byte; path = (rank, lane)}
        end else begin
          ghost_ (
            Read.I.empty_free_def view.model hash (rank + 1);
            Read.next_probe capacity hash rank group step);
          scan table view query value capacity hash (rank + 1)
            ((group + step) land (capacity - 1)) (step + 16) token
        end
      end

  let find : ('a : immutable_data).
      (table : (Key.t, 'a) T.t) @ immutable ->
      (view : {v : 'a Read.I.view | Read.I.valid v}) @ immutable ->
      (query : Key.t) @ immutable -> (value : 'a) @ immutable ghost ->
      (token : {t : (Key.t, 'a) M.state P.token | H.at (P.own t) (T.location table) === Some
        view.model})
        @ local read ghost ->
      {r : found | 0 <= r.#index && r.#index < view.model.capacity &&
         (r.#byte = 128 || r.#byte = 254) &&
         M.control view.model r.#index === Some r.#byte &&
         M.slot view.model r.#index === Some None &&
         Read.I.route view.model (Bigint.of_int r.#index)
           (Some (query, value)) r.#path} = fun table view query value token ->
    let capacity = T.capacity table {T.model = view.model} token in
    let hash = Key.hash query in
    let group = (hash lsr 7) land (capacity - 1) in
    ghost_ (
      Read.I.valid_def view; Read.capacity_bounds view.model;
      Read.I.empty_free_def view.model hash 0;
      Read.I.probe_def capacity hash 0; Read.I.wrap_def capacity (hash lsr 7));
    scan table view query value capacity hash 0 group 16 token
end
