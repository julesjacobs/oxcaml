module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module X = Vox_lz4_packed_encode
module D = Vox_lz4_packed
module R = Vox_lz4_roundtrip
module M = Raw_memory
module G = Ghost_pref

let (match_token_fields @ total) :
    (literals : {n : int | 0 <= n && n <= 4194304}) ->
    (match_code : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit |
      Vox_lz4_spec_parse.high4 (Vox_lz4_spec_token.match_token literals match_code) =
        (if literals >= 15 then 15 else literals)
      && Vox_lz4_spec_parse.low15 (Vox_lz4_spec_token.match_token literals match_code) =
        (if match_code >= 15 then 15 else match_code)} @ ghost =
  fun literals match_code -> ghost_ (
    let token = Vox_lz4_spec_token.match_token literals match_code in
    let high = Vox_lz4_spec_parse.high4 token in
    let low = Vox_lz4_spec_parse.low15 token in
    let _ = high, low in
    ())

include Vox_lz4_spec_wire

let (decode_end @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (block : M.t) -> (heap : G.heap) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor P.End
      && (last_match_start < 0 ||
          (anchor <= Iarray.length source - 5
           && last_match_start <= Iarray.length source - 12)))
      || let result =
           Vox_lz4_spec_decode.decode_model wire cursor last_match_start fuel
             capacity block anchor heap in
         result.D.kind === D.Done
         && result.D.count = Iarray.length source
         && result.D.state ===
              Vox_lz4_spec_decode.literal_heap heap block anchor source anchor
                (Iarray.length source - anchor)} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity block heap ->
  ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor cursor P.End;
    Vox_lz4_spec_plan.valid_plan_def source anchor P.End;
    if Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor P.End
       && (last_match_start < 0 ||
           (anchor <= Iarray.length source - 5
            && last_match_start <= Iarray.length source - 12)) then begin
      let literals = Iarray.length source - anchor in
      let extensions = Vox_lz4_spec_wire.extra_count literals in
      Vox_lz4_spec_wire.extra_count_def literals;
      R.extra_count_def literals;
      R.wire_byte_get wire cursor (Vox_lz4_spec_bytes.literal_token literals);
      R.literal_token_fields literals;
      let token = Vox_lz4_spec_parse.byte_of_char
        (Vox_sequence.iarray_get wire cursor) in
      let initial = Vox_lz4_spec_parse.high4 token in
      R.read_literal_length wire (cursor + 1) literals initial;
      R.literal_heap_substitute heap block anchor wire
        (cursor + 1 + extensions) source anchor literals;
      Vox_lz4_spec_decode.decode_model_def wire cursor last_match_start fuel
        capacity block anchor heap
    end;
    ())

let (decode_sequence_step @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (block : M.t) -> (heap : G.heap) ->
    (step : P.sequence) -> (rest : P.plan) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor
      (P.Sequence (step, rest)))
      || let literals = step.position - anchor in
         let match_code = step.length - 4 in
         let next_cursor = cursor + 1 + Vox_lz4_spec_wire.extra_count literals
           + literals + 2 + Vox_lz4_spec_wire.extra_count match_code in
         let after_literals =
           Vox_lz4_spec_decode.literal_heap heap block anchor source anchor literals in
         let after_match =
           Vox_lz4_spec_decode.copy_heap after_literals block step.position
             step.distance step.length in
         Vox_lz4_spec_decode.decode_model wire cursor last_match_start fuel
           capacity block anchor heap ===
         Vox_lz4_spec_decode.decode_model wire next_cursor step.position (fuel - 1)
           capacity block
           (step.position + step.length) after_match} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity block heap
    step rest ->
  ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor cursor
      (P.Sequence (step, rest));
    Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, rest));
    if Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor
         (P.Sequence (step, rest)) then begin
      let literals : {n : int | 0 <= n && n <= 4194304} =
        (step.position - anchor) in
      let match_code : {n : int | 0 <= n && n <= 4194304} =
        (step.length - 4) in
      let literal_extensions = Vox_lz4_spec_wire.extra_count literals in
      let match_extensions = Vox_lz4_spec_wire.extra_count match_code in
      let literal_pos = cursor + 1 + literal_extensions in
      let distance_pos = literal_pos + literals in
      Vox_lz4_spec_wire.extra_count_def literals;
      Vox_lz4_spec_wire.extra_count_def match_code;
      R.extra_count_def literals;
      R.extra_count_def match_code;
      R.wire_byte_get wire cursor
        (Vox_lz4_spec_token.match_token literals match_code);
      match_token_fields literals match_code;
      R.literal_token_fields literals;
      R.match_token_fields match_code;
      let token = Vox_lz4_spec_parse.byte_of_char
        (Vox_sequence.iarray_get wire cursor) in
      let _ : {u : unit |
        token = Vox_lz4_spec_token.match_token literals match_code} = () in
      let literal_nibble : {n : int | 0 <= n && n <= 15} =
        Vox_lz4_spec_parse.high4 token in
      let _ : {u : unit | literal_nibble =
        Vox_lz4_spec_parse.high4 (Vox_lz4_spec_bytes.literal_token literals)} = () in
      let cursor1 : {i : int | 0 <= i && i <= Iarray.length wire} =
        (cursor + 1) in
      let _ : {u : unit | literals < 15 ||
        Vox_lz4_spec_bytes.extension_bytes wire cursor1 (literals - 15)} =
        () in
      let _ : {u : unit |
        literal_pos = cursor1 + R.extra_count literals} =
        () in
      let _ : {u : unit |
        cursor1 + R.extra_count literals <= Iarray.length wire} =
        () in
      let _ : {u : unit |
        R.extra_count literals <= Iarray.length wire - cursor1} =
        () in
      R.read_literal_length wire cursor1 literals literal_nibble;
      let _ : {u : unit | Vox_lz4_spec_parse.read_length wire cursor1
        literal_nibble === D.Length (literal_pos, literals)} =
        () in
      let distance = Vox_lz4_spec_token.split_distance step.distance in
      Vox_lz4_spec_bytes.wire_byte_def wire distance_pos distance.low;
      Vox_lz4_spec_bytes.source_at_def wire distance_pos;
      Vox_lz4_spec_bytes.wire_byte_def wire (distance_pos + 1) distance.high;
      Vox_lz4_spec_bytes.source_at_def wire (distance_pos + 1);
      R.wire_byte_get wire distance_pos distance.low;
      R.wire_byte_get wire (distance_pos + 1) distance.high;
      let low = Vox_lz4_spec_parse.byte_of_char
        (Vox_sequence.iarray_get wire distance_pos) in
      let high = Vox_lz4_spec_parse.byte_of_char
        (Vox_sequence.iarray_get wire (distance_pos + 1)) in
      let _ : {u : unit | low + 256 * high = step.distance} =
        () in
      let match_nibble : {n : int | 0 <= n && n <= 15} =
        Vox_lz4_spec_parse.low15 token in
      let match_cursor : {i : int | 0 <= i && i <= Iarray.length wire} =
        (distance_pos + 2) in
      R.read_match_length wire match_cursor match_code
        match_nibble;
      let _ : {u : unit | Vox_lz4_spec_parse.read_length wire match_cursor
        match_nibble ===
        D.Length (distance_pos + 2 + match_extensions, match_code)} =
        () in
      R.literal_heap_substitute heap block anchor wire literal_pos
        source anchor literals;
      let _ : {u : unit |
        Vox_lz4_spec_decode.literal_heap heap block anchor wire literal_pos literals ===
        Vox_lz4_spec_decode.literal_heap heap block anchor source anchor literals} =
        () in
      Vox_lz4_spec_decode.decode_model_def wire cursor last_match_start fuel
        capacity block anchor heap
    end;
    ())

let rec (decode_plan @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (block : M.t) -> (heap : G.heap) -> (plan : P.plan) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor plan
      && Iarray.length wire - cursor <= fuel
      && (last_match_start < 0 ||
          (anchor <= Iarray.length source - 5
           && last_match_start <= Iarray.length source - 12)))
      || let result =
           Vox_lz4_spec_decode.decode_model wire cursor last_match_start fuel
             capacity block anchor heap in
         result.D.kind === D.Done
         && result.D.count = Iarray.length source
         && result.D.state ===
              P.apply_plan heap block source anchor plan} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity block heap
    plan ->
  ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor cursor plan;
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    P.apply_plan_def heap block source anchor plan;
    if Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor plan
       && Iarray.length wire - cursor <= fuel
       && (last_match_start < 0 ||
           (anchor <= Iarray.length source - 5
            && last_match_start <= Iarray.length source - 12)) then
      (match plan with
       | P.End ->
         decode_end source wire anchor cursor last_match_start fuel
           capacity block heap
       | P.Sequence (step, rest) ->
         let literals = step.position - anchor in
         let match_code = step.length - 4 in
         let next_cursor = cursor + 1 + Vox_lz4_spec_wire.extra_count literals
           + literals + 2 + Vox_lz4_spec_wire.extra_count match_code in
         let after_literals =
           Vox_lz4_spec_decode.literal_heap heap block anchor source anchor literals in
         let after_match =
           Vox_lz4_spec_decode.copy_heap after_literals block step.position
             step.distance step.length in
         decode_sequence_step source wire anchor cursor
           last_match_start fuel capacity block heap step rest;
         let next_fuel : {n : int | 0 < n
           && Iarray.length wire - next_cursor <= n} =
           (fuel - 1) in
         decode_plan source wire (step.position + step.length)
           next_cursor step.position next_fuel capacity block after_match
           rest);
    ())

let (decode_wire_matches_source @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (plan : P.plan) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (block : M.t) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire 0 0 plan)
      || let result =
           Vox_lz4_spec_decode.decode_model wire 0 (-1) (Iarray.length wire)
             capacity block 0 (M.footprint block) in
         result.D.kind === D.Done
         && result.D.count = Iarray.length source
         && R.output_matches result.D.state block source result.D.count}
      @ ghost =
  fun source wire plan capacity block -> ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire 0 0 plan;
    if Vox_lz4_spec_wire.wire_matches_plan source wire 0 0 plan then begin
      decode_plan source wire 0 0 (-1) (Iarray.length wire)
        capacity block (M.footprint block) plan;
      R.output_matches_def (M.footprint block) block source 0;
      P.plan_reconstructs_source (M.footprint block) block source 0 plan
    end;
    ())
