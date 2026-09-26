module P = Vox_lz4_general_plan
module E = Vox_lz4_packed_encode
module B = Vox_lz4_encode_buffer
module M = Raw_memory
module G = Ghost_pref
module H = G.Heap

include Vox_lz4_spec_token

type model_result = {
  count : int;
  state : G.heap @@ ghost;
}

let[@def] rec (encode_model @ total)
    (source : char iarray @ immutable)
    (anchor : int)
    (plan : P.plan @ immutable)
    (block : M.t @ immutable) (used : int)
    (heap : G.heap @ immutable) : model_result @ ghost = ghost_ (
  Vox_lz4_spec_plan.valid_plan_def source anchor plan;
  if Iarray.length source > 4194304
     || not (Vox_lz4_spec_plan.valid_plan source anchor plan) then
    { count = used; state = heap }
  else match plan with
  | P.End ->
    let literals = Iarray.length source - anchor in
    let extensions =
      if literals >= 15 then Vox_lz4_spec_bytes.extension_count (literals - 15)
      else 0 in
    let after_token =
      H.put heap (M.location block used) (Some (Vox_lz4_spec_bytes.literal_token literals)) in
    let after_extensions =
      if literals >= 15 then
        E.extension_heap after_token block (used + 1) (literals - 15)
      else after_token in
    { count = used + 1 + extensions + literals;
      state = E.literal_heap after_extensions block
                (used + 1 + extensions) source anchor literals }
  | P.Sequence (step, rest) ->
    let literals = step.position - anchor in
    let match_code = step.length - 4 in
    let literal_extensions =
      if literals >= 15 then Vox_lz4_spec_bytes.extension_count (literals - 15)
      else 0 in
    let match_extensions =
      if match_code >= 15 then Vox_lz4_spec_bytes.extension_count (match_code - 15)
      else 0 in
    let after_token =
      H.put heap (M.location block used)
        (Some (Vox_lz4_spec_token.match_token literals match_code)) in
    let after_literal_extensions =
      if literals >= 15 then
        E.extension_heap after_token block (used + 1) (literals - 15)
      else after_token in
    let literal_pos = used + 1 + literal_extensions in
    let after_literals =
      E.literal_heap after_literal_extensions block literal_pos
        source anchor literals in
    let distance_pos = literal_pos + literals in
    let distance = Vox_lz4_spec_token.split_distance step.distance in
    let after_low =
      H.put after_literals (M.location block distance_pos)
        (Some distance.low) in
    let after_high =
      H.put after_low (M.location block (distance_pos + 1))
        (Some distance.high) in
    let after_match_extensions =
      if match_code >= 15 then
        E.extension_heap after_high block (distance_pos + 2)
          (match_code - 15)
      else after_high in
    encode_model source (step.position + step.length) rest block
      (distance_pos + 2 + match_extensions) after_match_extensions)

let rec emit :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan source anchor p}) ->
    (buffer : B.t) @ unique ->
    {r : B.t option | match r with
      | None -> true
      | Some after ->
        let model =
          encode_model source anchor plan buffer.block buffer.used
            (G.own buffer.permission) in
        after.block === buffer.block
        && after.used = model.count
        && G.own after.permission === model.state} @ unique =
  fun source anchor plan buffer ->
    let { B.block; permission; used } = buffer in
    ghost_ (Vox_lz4_spec_plan.valid_plan_def source anchor plan);
    ghost_ (encode_model_def source anchor plan block used
      (G.own (borrow_ permission)));
    let buffer : B.t = { B.block; permission; used } in
    match plan with
    | P.End ->
      let literals = Iarray.length source - anchor in
      let extensions =
        if literals >= 15 then Vox_lz4_spec_bytes.extension_count (literals - 15)
        else 0 in
      let needed = 1 + extensions + literals in
      if needed > B.capacity (borrow_ buffer) - B.used (borrow_ buffer)
      then begin B.release buffer; None end
      else
        let token = Vox_lz4_spec_bytes.literal_token literals in
        let buffer = B.append buffer token in
        let buffer =
          if literals >= 15 then E.emit_extensions (literals - 15) buffer
          else buffer in
        Some (E.copy_literals source anchor literals buffer)
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
      if needed > B.capacity (borrow_ buffer) - B.used (borrow_ buffer)
      then begin B.release buffer; None end
      else
        let token = Vox_lz4_spec_token.match_token literals match_code in
        let buffer = B.append buffer token in
        let buffer =
          if literals >= 15 then E.emit_extensions (literals - 15) buffer
          else buffer in
        let buffer = E.copy_literals source anchor literals buffer in
        let distance = Vox_lz4_spec_token.split_distance step.distance in
        let buffer = B.append buffer distance.low in
        let buffer = B.append buffer distance.high in
        let buffer =
          if match_code >= 15 then
            E.emit_extensions (match_code - 15) buffer
          else buffer in
        emit source (step.position + step.length) rest buffer

let encode :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (plan : {p : P.plan | Vox_lz4_spec_plan.valid_plan source 0 p}) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let model =
          encode_model source 0 plan buffer.block 0
            (M.footprint buffer.block) in
        buffer.used = model.count
        && G.own buffer.permission === model.state} @ unique =
  fun source plan ->
    let length = Iarray.length source in
    let capacity = length + Vox_lz4_spec_bytes.extension_count length + 15 in
    match B.create capacity with
    | None -> None
    | Some buffer -> emit source 0 plan buffer
