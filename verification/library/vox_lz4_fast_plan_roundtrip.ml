module F = Vox_lz4_fast_plan_model
module MS = Vox_lz4_mutable_scan
module T = Vox_lz4_general_roundtrip
module D = Vox_lz4_packed
module B = Vox_lz4_buffer
module M = Raw_memory
module R = Vox_lz4_roundtrip
module G = Ghost_pref

let roundtrip_capacity :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (capacity : {c : int |
      Iarray.length source <= c && c <= 4194304}) ->
    {r : (D.status * B.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = capacity
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source capacity ->
    let plan = F.from_source source in
    T.roundtrip_plan_capacity source plan capacity

let roundtrip :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    {r : (D.status * B.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = Iarray.length source
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source -> roundtrip_capacity source (Iarray.length source)

let mutable_scan_roundtrip_capacity :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (capacity : {c : int |
      Iarray.length source <= c && c <= 4194304}) ->
    {r : (D.status * B.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        status === D.Done
        && M.length decoded.block = capacity
        && decoded.used = Iarray.length source
        && R.output_matches (G.own decoded.permission) decoded.block
             source decoded.used} @ unique =
  fun source capacity ->
    let plan = MS.from_source source in
    T.roundtrip_plan_capacity source plan capacity
