module M = Vox_table_model
module S = Vox_sequence
module Mask = Vox_table_mask

module Make (Key : Vox_table_map.Key) = struct
  module Spec = Vox_table_search_spec.Make (Key)
  module R = Spec.R
  module I = R.I
  module Map = I.Map

  let (fingerprinted_misses @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (query : Key.t) @ immutable -> (group : int) -> (lane : Mask.lane) ->
      {u : unit | not (I.shape model && I.clones model 15 &&
        0 <= group && group < model.capacity &&
        Spec.candidates_absent model query group
          (M.matching model group (Key.hash query land 127) 16) &&
        M.control model (I.wrap model.capacity (group + lane)) ===
          Some (Key.hash query land 127)) ||
        Spec.misses model query (I.wrap model.capacity (group + lane))}
      @ ghost = fun model query group lane -> ghost_ (
    let needle = Key.hash query land 127 in
    R.matching_control model group needle lane;
    Mask.bounded model group needle 16;
    Spec.candidate_at model query group
      (refine_ (M.matching model group needle 16)) lane;
    ())

  let (visited_misses @ total) : ('a : immutable_data).
      (view : 'a I.view) @ immutable -> (query : Key.t) @ immutable ->
      (count : int) -> (index : Bigint.t) ->
      (key : Key.t) @ immutable -> (value : 'a) @ immutable ->
      (rank : int) -> (lane : int) ->
      {u : unit | not (I.valid view && Spec.prefix_absent view.model query
        count &&
        0 <= rank && rank < count &&
        I.route view.model index (Some (key, value)) (rank, lane) &&
        S.at view.model.slots index === Some (Some (key, value))) ||
        not (Key.equal key query)} @ ghost =
    fun view query count index key value rank lane -> ghost_ (
      let model = view.model in
      I.valid_def view;
      if I.valid view && Spec.prefix_absent model query count &&
          0 <= rank && rank < count &&
          I.route model index (Some (key, value)) (rank, lane) &&
          S.at model.slots index === Some (Some (key, value)) &&
          Key.equal key query then begin
        Key.hash_equal key query;
        R.route_info model index key value rank lane;
        R.route_equal_key model index key query value rank lane;
        let group = I.group model.capacity (Key.hash query) rank in
        let position = I.wrap model.capacity (group + lane) in
        R.slot_at model index position (Some (key, value));
        R.stored_control model position key value;
        R.group_in_shape model (Key.hash query) rank;
        Spec.prefix_at model query count rank;
        fingerprinted_misses model query group lane;
        Spec.misses_def model query position;
        ()
      end else ())

  let (stored_misses @ total) : ('a : immutable_data).
      (view : 'a I.view) @ immutable -> (query : Key.t) @ immutable ->
      (stop : {i : int | 0 <= i && i < 1073741824}) ->
      (index : Bigint.t) -> (key : Key.t) @ immutable ->
      (value : 'a) @ immutable ->
      {u : unit | not (I.valid view &&
        Spec.prefix_absent view.model query (stop + 1) &&
        M.matching view.model
          (I.group view.model.capacity (Key.hash query) stop) 128 16 <> 0 &&
        S.at view.model.slots index === Some (Some (key, value))) ||
        not (Key.equal key query)} @ ghost =
    fun view query stop index key value -> ghost_ (
      let model = view.model in
      I.valid_def view;
      if I.valid view && Spec.prefix_absent model query (stop + 1) &&
          M.matching model (I.group model.capacity (Key.hash query) stop)
            128 16 <> 0 &&
          S.at model.slots index === Some (Some (key, value)) &&
          Key.equal key query then begin
        R.route_at model model.slots view.routes 0Z index (Some (key, value));
        Key.hash_equal key query;
        let (_ : {u : unit | Key.hash key = Key.hash query}) = refine_ () in
        match S.at view.routes index with
        | None -> ()
        | Some (rank, lane) ->
          let (_ : {u : unit | I.route model index (Some (key, value))
            (rank, lane)}) = refine_ () in
          R.route_info model index key value rank lane;
          R.route_equal_key model index key query value rank lane;
          if rank <= stop then begin
            visited_misses view query (stop + 1) index key value rank lane;
            ()
          end else begin
            Spec.empty_free_at model (Key.hash key) rank stop;
            ()
          end
      end else ())
  let (empty_stop @ total) : ('a : immutable_data).
      (view : 'a I.view) @ immutable -> (query : Key.t) @ immutable ->
      (stop : {i : int | 0 <= i && i < 1073741824}) ->
      {u : unit | not (I.valid view &&
        Spec.prefix_absent view.model query (stop + 1) &&
        M.matching view.model
          (I.group view.model.capacity (Key.hash query) stop) 128 16 <> 0) ||
        Map.absent view.model.slots query &&
        Map.lookup view.model.slots query === None} @ ghost =
    fun view query stop -> ghost_ (
      if I.valid view && Spec.prefix_absent view.model query (stop + 1) &&
          M.matching view.model (I.group view.model.capacity (Key.hash query)
            stop)
            128 16 <> 0 then begin
        Map.absent_intro view.model.slots query (fun index -> ghost_ (
          match S.at view.model.slots index with
          | Some (Some (key, value)) -> stored_misses view query stop index
            key value
          | _ -> ()));
        Map.absent_lookup view.model.slots query;
        ()
      end else ())

  let (exhausted @ total) : ('a : immutable_data).
      (view : 'a I.view) @ immutable -> (query : Key.t) @ immutable ->
      {u : unit | not (I.valid view &&
        Spec.prefix_absent view.model query (view.model.capacity lsr 4)) ||
        Map.absent view.model.slots query &&
        Map.lookup view.model.slots query === None} @ ghost =
    fun view query -> ghost_ (
      I.valid_def view;
      if I.valid view && Spec.prefix_absent view.model query
          (view.model.capacity lsr 4) then begin
        Map.absent_intro view.model.slots query (fun index -> ghost_ (
          match S.at view.model.slots index with
          | Some (Some (key, value)) ->
            R.route_at view.model view.model.slots view.routes 0Z index
              (Some (key, value));
            (match S.at view.routes index with
             | None -> ()
             | Some ((rank, lane) as path) ->
               I.route_def view.model index (Some (key, value)) path;
               I.route_position_def view.model.capacity (Key.hash key) index
                 path;
               visited_misses view query (view.model.capacity lsr 4)
                 index key value rank lane;
               ())
          | _ -> ()));
        Map.absent_lookup view.model.slots query;
        ()
      end else ())

end
