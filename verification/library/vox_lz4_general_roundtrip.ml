module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module Z = Vox_lz4_general_sized
module C = Vox_lz4_general_cost
module W = Vox_lz4_general_wire
module F = Vox_lz4_general_bridge
module S = Vox_lz4_snapshot
module D = Vox_lz4_packed
module R = Vox_lz4_roundtrip
module EB = Vox_lz4_encode_buffer
module DB = Vox_lz4_buffer
module M = Raw_memory
module G = Ghost_pref

let roundtrip_plan_capacity :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan source 0 p}) ->
    (capacity : {c : int |
      Iarray.length source <= c && c <= 4194304}) ->
    {r : (D.status * DB.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = capacity
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source plan capacity ->
    ghost_ (Vox_lz4_spec_plan.valid_plan_def source 0 plan);
    match Z.encode source plan with
    | None -> None
    | Some encoded ->
      let snapshot = S.snapshot_prefix encoded in
      let { S.values = wire; buffer = encoded } = snapshot in
      let { EB.block = encoded_block;
            permission = encoded_permission; used = encoded_used } =
        encoded in
      match D.decode wire capacity with
      | None ->
        EB.release { EB.block = encoded_block;
                     permission = encoded_permission; used = encoded_used };
        None
      | Some (status, decoded) ->
        let { DB.block = decoded_block;
              permission = decoded_permission; used = decoded_used } =
          decoded in
        ghost_ (
          C.encoded_size_capacity source 0 plan;
          C.encode_model_size source 0 plan encoded_block 0
            (M.footprint encoded_block);
          F.model_wire source wire 0 plan encoded_block 0
            (M.footprint encoded_block);
          W.decode_wire_matches_source source wire plan capacity
            decoded_block);
        EB.release { EB.block = encoded_block;
                     permission = encoded_permission; used = encoded_used };
        Some (status,
              { DB.block = decoded_block;
                permission = decoded_permission; used = decoded_used })

let roundtrip_plan :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan source 0 p}) ->
    {r : (D.status * DB.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = Iarray.length source
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source plan ->
    roundtrip_plan_capacity source plan (Iarray.length source)

let roundtrip_hints :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (hints : {h : int iarray |
      Iarray.length h = Iarray.length source}) ->
    {r : (D.status * DB.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = Iarray.length source
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source hints ->
    let plan = P.from_hints source hints in
    roundtrip_plan source plan
