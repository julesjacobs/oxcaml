module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module C = Vox_lz4_general_cost
module X = Vox_lz4_packed_encode
module B = Vox_lz4_encode_buffer
module M = Raw_memory
module G = Ghost_pref

let rec emit :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (plan : {p : P.plan | P.valid_plan source anchor p}) ->
    (buffer : {b : B.t |
      C.encoded_size source anchor plan <= M.length b.block - b.used})
      @ unique ->
    {after : B.t |
      let refine_ model =
        E.encode_model source anchor plan buffer.block buffer.used
          (G.own buffer.permission) in
      after.block === buffer.block
      && after.used = model.E.count
      && G.own after.permission === model.E.state} @ unique =
  fun source anchor plan buffer ->
    let { B.block; permission; used } = buffer in
    ghost_ (P.valid_plan_def source anchor plan);
    ghost_ (C.encoded_size_def source anchor plan);
    ghost_ (E.encode_model_def source anchor plan block used
      (G.own (borrow_ permission)));
    let buffer : B.t = { B.block; permission; used } in
    match plan with
    | P.End ->
      let literals = Iarray.length source - anchor in
      let extensions =
        if literals >= 15 then X.extension_count (literals - 15)
        else 0 in
      let needed = 1 + extensions + literals in
      ghost_ (Vox_lz4_general_wire.extra_count_def literals);
      let _ : {u : unit | needed <= M.length block - used} =
        ghost_ (refine_ ()) in
      let token = X.literal_token literals in
      let buffer = B.append buffer token in
      let buffer =
        if literals >= 15 then X.emit_extensions (literals - 15) buffer
        else buffer in
      X.copy_literals source anchor literals buffer
    | P.Sequence (step, rest) ->
      let literals = step.position - anchor in
      let match_code = step.length - 4 in
      let literal_extensions =
        if literals >= 15 then X.extension_count (literals - 15)
        else 0 in
      let match_extensions =
        if match_code >= 15 then X.extension_count (match_code - 15)
        else 0 in
      let needed =
        1 + literal_extensions + literals + 2 + match_extensions in
      ghost_ (
        Vox_lz4_general_wire.extra_count_def literals;
        Vox_lz4_general_wire.extra_count_def match_code;
        C.encoded_size_loose_bound source
          (step.position + step.length) rest);
      let _ : {u : unit | needed <= M.length block - used} =
        ghost_ (refine_ ()) in
      let token = E.match_token literals match_code in
      let buffer = B.append buffer token in
      let buffer =
        if literals >= 15 then X.emit_extensions (literals - 15) buffer
        else buffer in
      let buffer = X.copy_literals source anchor literals buffer in
      let distance = E.split_distance step.distance in
      let buffer = B.append buffer distance.low in
      let buffer = B.append buffer distance.high in
      let buffer =
        if match_code >= 15 then
          X.emit_extensions (match_code - 15) buffer
        else buffer in
      emit source (step.position + step.length) rest buffer

let encode :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (plan : {p : P.plan | P.valid_plan source 0 p}) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let refine_ model =
          E.encode_model source 0 plan buffer.block 0
            (M.footprint buffer.block) in
        buffer.used = model.E.count
        && G.own buffer.permission === model.E.state} @ unique =
  fun source plan ->
    let length = Iarray.length source in
    let capacity = length + X.extension_count length + 15 in
    match B.create capacity with
    | None -> None
    | Some buffer ->
      ghost_ (
        C.encoded_size_capacity source 0 plan;
        C.extension_budget_def length);
      Some (emit source 0 plan buffer)
