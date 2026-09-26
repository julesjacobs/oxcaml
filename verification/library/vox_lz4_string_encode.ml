module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module C = Vox_lz4_general_cost
module X = Vox_lz4_packed_encode
module B = Vox_lz4_encode_buffer
module M = Raw_memory
module G = Ghost_pref

module V = Vox_string_view

let rec copy_literals_into :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (first : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length model - first}) ->
    (block : {b : M.t | M.length b <= 4210768}) ->
    (used : {n : int | 0 <= n && n <= M.length block
      && remaining <= M.length block - n}) ->
    (permission : {p : G.token |
      M.covers (G.own p) block 0 (M.length block)
      && G.Heap.mem (G.own p) (M.location block (-1))
      && Vox_lz4_spec_storage.initialized (G.own p) block used}) @ unique ghost ->
    {after : B.t | after.block === block
      && after.used = used + remaining
      && G.own after.permission ===
           X.literal_heap (G.own permission) block used
             model first remaining} @ unique =
  fun model source first remaining block used permission ->
    ghost_ (X.literal_heap_def (G.own (borrow_ permission))
      block used model first remaining);
    if remaining = 0 then { B.block; permission; used }
    else
      let value = Vox_lz4_spec_parse.byte_of_char (V.get source first) in
      ghost_ (Vox_iarray.at_get model first);
      let buffer = B.append { B.block; permission; used } value in
      let { B.block; permission; used } = buffer in
      copy_literals_into model source (first + 1) (remaining - 1)
        block used permission
[@@decreases remaining]

let copy_literals :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (first : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length model - first}) ->
    (buffer : {b : B.t | remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + remaining
      && G.own after.permission ===
           X.literal_heap (G.own buffer.permission) buffer.block buffer.used
             model first remaining} @ unique =
  fun model source first remaining buffer ->
    let { B.block; permission; used } = buffer in
    copy_literals_into model source first remaining block used permission

let rec emit :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length model}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan model anchor p}) ->
    (buffer : {b : B.t |
      C.encoded_size model anchor plan <= M.length b.block - b.used})
      @ unique ->
    {after : B.t |
      let encoded_model =
        E.encode_model model anchor plan buffer.block buffer.used
          (G.own buffer.permission) in
      after.block === buffer.block
      && after.used = encoded_model.E.count
      && G.own after.permission === encoded_model.E.state} @ unique =
  fun model source anchor plan buffer ->
    let { B.block; permission; used } = buffer in
    ghost_ (Vox_lz4_spec_plan.valid_plan_def model anchor plan);
    ghost_ (C.encoded_size_def model anchor plan);
    ghost_ (E.encode_model_def model anchor plan block used
      (G.own (borrow_ permission)));
    let buffer : B.t = { B.block; permission; used } in
    match plan with
    | P.End ->
      let literals = V.length source - anchor in
      let extensions =
        if literals >= 15 then Vox_lz4_spec_bytes.extension_count (literals - 15)
        else 0 in
      let needed = 1 + extensions + literals in
      ghost_ (Vox_lz4_spec_wire.extra_count_def literals);
      let _ : {u : unit | needed <= M.length block - used} =
        ghost_ (()) in
      let token = Vox_lz4_spec_bytes.literal_token literals in
      let buffer = B.append buffer token in
      let buffer =
        if literals >= 15 then X.emit_extensions (literals - 15) buffer
        else buffer in
      copy_literals model source anchor literals buffer
    | P.Sequence (step, rest) ->
      let literals = step.position - anchor in
      let match_code = step.length - 4 in
      let literal_extensions =
        if literals >= 15 then Vox_lz4_spec_bytes.extension_count (literals - 15)
        else 0 in
      let match_extensions =
        if match_code >= 15 then Vox_lz4_spec_bytes.extension_count (match_code - 15)
        else 0 in
      let needed =
        1 + literal_extensions + literals + 2 + match_extensions in
      ghost_ (
        Vox_lz4_spec_wire.extra_count_def literals;
        Vox_lz4_spec_wire.extra_count_def match_code;
        C.encoded_size_loose_bound model
          (step.position + step.length) rest);
      let _ : {u : unit | needed <= M.length block - used} =
        ghost_ (()) in
      let token = Vox_lz4_spec_token.match_token literals match_code in
      let buffer = B.append buffer token in
      let buffer =
        if literals >= 15 then X.emit_extensions (literals - 15) buffer
        else buffer in
      let buffer = copy_literals model source anchor literals buffer in
      let distance = Vox_lz4_spec_token.split_distance step.distance in
      let buffer = B.append buffer distance.low in
      let buffer = B.append buffer distance.high in
      let buffer =
        if match_code >= 15 then
          X.emit_extensions (match_code - 15) buffer
        else buffer in
      emit model source (step.position + step.length) rest buffer

let encode :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan model 0 p}) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let encoded_model =
          E.encode_model model 0 plan buffer.block 0
            (M.footprint buffer.block) in
        buffer.used = encoded_model.E.count
        && G.own buffer.permission === encoded_model.E.state} @ unique =
  fun model source plan ->
    let length = V.length source in
    let capacity = length + Vox_lz4_spec_bytes.extension_count length + 15 in
    match B.create capacity with
    | None -> None
    | Some buffer ->
      ghost_ (
        C.encoded_size_capacity model 0 plan;
        C.extension_budget_def length);
      Some (emit model source 0 plan buffer)
