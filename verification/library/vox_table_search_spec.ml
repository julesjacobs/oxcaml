module M = Vox_table_model
module B = Vox_table_bits

module Make (Key : Vox_table_map.Key) = struct
  module R = Vox_table_read_proofs.Make (Key)
  module I = R.I

  let[@def] (misses @ total) (model : (Key.t, 'a) M.state @ immutable)
      (query : Key.t @ immutable) (index : int) =
    match M.slot model index with
    | Some (Some (stored, _)) -> not (Key.equal stored query)
    | _ -> true

  let[@def] rec (candidates_absent @ total)
      (model : (Key.t, 'a) M.state @ immutable)
      (query : Key.t @ immutable) (group : int) (mask : int) =
    if mask <= 0 || mask > 65535 then true else
      let lane = B.first mask in
      let index = I.wrap model.capacity (group + lane) in
      let rest = B.clear mask in
      misses model query index &&
        candidates_absent model query group rest
    [@@decreases if mask > 0 && mask <= 65535 then mask else 0]

  let rec (candidate_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (query : Key.t) @ immutable -> (group : int) -> (mask : B.mask) ->
      (lane : Vox_table_mask.lane) ->
      {u : unit | not (candidates_absent model query group mask &&
        mask land M.lane_bit lane <> 0) ||
        misses model query (I.wrap model.capacity (group + lane))} @ ghost =
    fun model query group mask lane -> ghost_ (
      candidates_absent_def model query group mask;
      if mask <> 0 then begin
        let chosen = B.first mask in
        B.clear_lane mask lane;
        if lane <> chosen then begin
          let rest = B.clear mask in
          candidate_at model query group rest lane
        end;
        ()
      end else ())
    [@@decreases mask]
  let[@def] rec (prefix_absent @ total)
      (model : (Key.t, 'a) M.state @ immutable)
      (query : Key.t @ immutable) (count : int) =
    if count <= 0 then true else
      let group = I.group model.capacity (Key.hash query) (count - 1) in
      candidates_absent model query group
        (M.matching model group (Key.hash query land 127) 16)
      && prefix_absent model query (count - 1)
    [@@decreases if count > 0 then count else 0]

  let rec (prefix_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable ->
      (query : Key.t) @ immutable -> (count : int) -> (rank : int) ->
      {u : unit | not (prefix_absent model query count &&
        0 <= rank && rank < count) ||
        candidates_absent model query
          (I.group model.capacity (Key.hash query) rank)
          (M.matching model (I.group model.capacity (Key.hash query) rank)
            (Key.hash query land 127) 16)} @ ghost =
    fun model query count rank -> ghost_ (
      prefix_absent_def model query count;
      if count > 0 && rank < count - 1 then prefix_at model query (count - 1)
        rank;
      ())
    [@@decreases if count > 0 then count else 0]

  let rec (empty_free_at @ total) : ('a : immutable_data).
      (model : (Key.t, 'a) M.state) @ immutable -> (hash : int) ->
      (count : int) -> (rank : int) ->
      {u : unit | not (I.empty_free model hash count &&
        0 <= rank && rank < count) ||
        M.matching model (I.group model.capacity hash rank) 128 16 = 0}
      @ ghost = fun model hash count rank -> ghost_ (
    I.empty_free_def model hash count;
    if count > 0 && rank < count - 1 then empty_free_at model hash (count - 1)
      rank;
    ())
    [@@decreases if count > 0 then count else 0]

end
