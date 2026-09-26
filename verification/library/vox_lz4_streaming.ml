module P = Vox_lz4_general_plan
module F = Vox_lz4_forward_model
module Old = Vox_lz4_fast_plan_model
module Match = Vox_lz4_string_match
module T = Vox_lz4_mutable_scan
module A = Vox_iarray
module Table = Borrow_iarray.Owned_array
module S = Vox_lz4_string_encode
module E = Vox_lz4_general_encode
module X = Vox_lz4_packed_encode
module C = Vox_lz4_general_cost
module B = Vox_lz4_encode_buffer
module M = Raw_memory
module G = Ghost_pref
module V = Vox_string_view

let[@inline always] split_distance :
    (distance : {d : int | 0 <= d && d <= 65535}) ->
    {r : E.distance_bytes | r === Vox_lz4_spec_token.split_distance distance} =
  fun distance ->
    let high = distance / 256 in
    let low = distance - 256 * high in
    let _ = ghost_ (Vox_lz4_spec_token.split_distance distance) in
    { E.low = low; high = high }

let (emit_head_capacity @ total) :
    (model : {m : char iarray | Iarray.length m <= 4194304}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length model}) ->
    (step : {s : P.sequence | anchor <= s.position
      && s.position <= Iarray.length model && 4 <= s.length
      && s.length <= 4194304}) ->
    (rest : {p : P.plan |
      Vox_lz4_spec_plan.valid_plan model anchor (P.Sequence (step, p))}) ->
    (remaining : {n : int |
      C.encoded_size model anchor (P.Sequence (step, rest)) <= n}) ->
    {u : unit |
      let literals = step.position - anchor in
      let match_code = step.length - 4 in
      3 + Vox_lz4_spec_wire.extra_count literals + literals
        + Vox_lz4_spec_wire.extra_count match_code <= remaining
      && 0 <= C.encoded_size model (step.position + step.length) rest
      && C.encoded_size model (step.position + step.length) rest
        <= 2 * (Iarray.length model - (step.position + step.length)) + 16}
      @ ghost =
  fun model anchor step rest remaining -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def model anchor (P.Sequence (step, rest));
    C.encoded_size_def model anchor (P.Sequence (step, rest));
    Vox_lz4_spec_wire.extra_count_def (step.position - anchor);
    Vox_lz4_spec_wire.extra_count_def (step.length - 4);
    C.encoded_size_loose_bound model (step.position + step.length) rest;
    ())

let[@inline always] emit_head :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length model}) ->
    (step : P.sequence) ->
    (rest : {p : P.plan | Vox_lz4_spec_plan.valid_plan model anchor (P.Sequence (step, p))}) @ ghost ->
    (buffer : {b : B.t |
      C.encoded_size model anchor (P.Sequence (step, rest)) <=
        M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && C.encoded_size model (step.position + step.length) rest <=
           M.length after.block - after.used
      && let before_model = E.encode_model model anchor
           (P.Sequence (step, rest)) buffer.block buffer.used (G.own buffer.permission) in
         let after_model = E.encode_model model (step.position + step.length)
           rest after.block after.used (G.own after.permission) in
         before_model.E.count = after_model.E.count
         && before_model.E.state === after_model.E.state} @ unique =
  fun model source anchor step rest buffer ->
    let { B.block; permission; used } = buffer in
    ghost_ (
      Vox_lz4_spec_plan.valid_plan_def model anchor (P.Sequence (step, rest));
      C.encoded_size_def model anchor (P.Sequence (step, rest));
      emit_head_capacity model anchor step rest (M.length block - used);
      Vox_lz4_spec_wire.extra_count_def (step.position - anchor);
      Vox_lz4_spec_wire.extra_count_def (step.length - 4);
      E.encode_model_def model anchor (P.Sequence (step, rest)) block used
        (G.own (borrow_ permission)));
    let buffer : B.t = { B.block; permission; used } in
      let literals = step.position - anchor in
      let match_code = step.length - 4 in
      let token = Vox_lz4_spec_token.match_token literals match_code in
      let buffer = B.append buffer token in
      let buffer =
        if literals >= 15 then X.emit_extensions (literals - 15) buffer
        else buffer in
      let buffer = S.copy_literals model source anchor literals buffer in
      let distance = split_distance step.distance in
      let buffer = B.append buffer distance.low in
      let buffer = B.append buffer distance.high in
      let buffer =
        if match_code >= 15 then
          X.emit_extensions (match_code - 15) buffer
        else buffer in
      buffer

let rec scan :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (entries : Old.hashes) @ ghost ->
    (table : {t : int Table.t | Iarray.length (Table.contents t) = 65536
      && T.agrees (Table.contents t) entries 65536}) @ unique ->
    (position : {p : int | 0 <= p && p <= Iarray.length model}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length model - position + 1}) @ ghost ->
    (buffer : {b : B.t |
      C.encoded_size model anchor (Vox_lz4_spec_scan.scan model entries position anchor fuel)
        <= M.length b.block - b.used}) @ unique ->
    {after : B.t |
      let encoded = E.encode_model model anchor
          (Vox_lz4_spec_scan.scan model entries position anchor fuel)
          buffer.block buffer.used (G.own buffer.permission) in
      after.block === buffer.block && after.used = encoded.E.count
      && G.own after.permission === encoded.E.state} @ unique =
  fun model source entries table position anchor fuel buffer ->
    ghost_ (Vox_lz4_spec_scan.scan_def model entries position anchor fuel);
    if position > V.length source - 12 then begin
      let _ = Table.into_iarray table in
      S.emit model source anchor P.End buffer
    end else
      let hash = Match.hash4 source model position in
      let before = ghost_ (Table.contents (borrow_ table)) in
      ghost_ (T.agrees_get before entries 65536 hash);
      let candidate = Table.get_int (borrow_ table) hash in
      let table = Table.set_int table hash position in
      ghost_ (
        A.updated_length before hash position;
        T.agrees_set before entries 65536 hash position);
      let entries = ghost_ ((hash, position) :: entries) in
      let limit = V.length source - 5 - position in
      match Match.choose_match source model position limit candidate with
      | None ->
        scan model source entries table (position + 1) anchor (ghost_ (fuel - 1)) buffer
      | Some choice ->
        let next = position + choice.length in
        let step = { P.position; distance = choice.distance; length = choice.length } in
        let rest = ghost_ (Vox_lz4_spec_scan.scan model entries next next (fuel - choice.length)) in
        ghost_ (Vox_lz4_spec_plan.valid_plan_def model anchor (P.Sequence (step, rest)));
        let buffer = emit_head model source anchor step rest buffer in
        scan model source entries table next next (ghost_ (fuel - choice.length)) buffer
[@@decreases fuel]

let encode :
    (model : {m : char iarray | Iarray.length m <= 4194304}) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let encoded = E.encode_model model 0 (Vox_lz4_spec_scan.from_source model)
            buffer.block 0 (M.footprint buffer.block) in
        buffer.used = encoded.E.count
        && G.own buffer.permission === encoded.E.state} @ unique =
  fun model source ->
    let length = V.length source in
    let capacity = length + Vox_lz4_spec_bytes.extension_count length + 15 in
    let table = Table.of_iarray T.empty_table in
    match B.create capacity with
    | None -> let _ = Table.into_iarray table in None
    | Some buffer ->
      ghost_ (
        Vox_lz4_spec_scan.from_source_def model;
        C.encoded_size_capacity model 0 (Vox_lz4_spec_scan.from_source model);
        C.extension_budget_def length);
      (* Exceptions consume authority; unreachable storage is reclaimed by
         the raw carrier finalizer without restoring the input token. *)
      Some (scan model source (ghost_ []) table 0 0 (ghost_ (length + 1)) buffer)
