module S = Vox_lz4_spec_decode_bytes
module D = Vox_lz4_spec_parse
module B = Vox_lz4_spec_bytes
module I = Vox_iarray
module P = Vox_lz4_spec_plan
module R = Vox_lz4_roundtrip
module W = Vox_lz4_general_wire

let (append @ total) :
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n < Iarray.length source}) ->
    (reversed : S.byte list) ->
    {u : unit | not (S.matches_bytes source used reversed)
      || S.matches_bytes source (used + 1)
           (D.byte_of_char (Vox_sequence.iarray_get source used) :: reversed)}
      @ ghost =
  fun source used reversed -> ghost_ (
    I.at_get source used;
    S.matches_bytes_def source (used + 1)
      (D.byte_of_char (Vox_sequence.iarray_get source used) :: reversed))

let rec (read_distance @ total) :
    (source : char iarray) -> (used : int) -> (reversed : S.byte list) ->
    (distance : {d : int | 0 < d && d <= used}) ->
    {u : unit | not (S.matches_bytes source used reversed)
      || match S.at_distance reversed distance,
               I.at source (used - distance) with
         | Some byte, Some c -> D.byte_of_char c = byte
         | _ -> false} @ ghost =
  fun source used reversed distance -> ghost_ (
    S.matches_bytes_def source used reversed;
    S.at_distance_def reversed distance;
    (match reversed with
     | [] -> ()
     | _ :: rest ->
       if S.matches_bytes source used reversed && distance > 1 then
         read_distance source (used - 1) rest (distance - 1));
    ())

let rec (literals @ total) :
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n <= Iarray.length source}) ->
    (reversed : S.byte list) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length source - used}) ->
    {u : unit | not (S.matches_bytes source used reversed)
      || S.matches_bytes source (used + remaining)
           (S.literal_bytes reversed source used remaining)} @ ghost =
  fun source used reversed remaining -> ghost_ (
    S.literal_bytes_def reversed source used remaining;
    if remaining > 0 && S.matches_bytes source used reversed then begin
      append source used reversed;
      literals source (used + 1)
        (D.byte_of_char (Vox_sequence.iarray_get source used) :: reversed)
        (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (matches @ total) :
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n <= Iarray.length source}) ->
    (reversed : S.byte list) ->
    (distance : {d : int | 0 < d && d <= used}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length source - used}) ->
    {u : unit | not (S.matches_bytes source used reversed
      && Vox_lz4_spec_match.source_matches_distance source used distance remaining)
      || S.matches_bytes source (used + remaining)
           (S.copy_bytes reversed distance remaining)} @ ghost =
  fun source used reversed distance remaining -> ghost_ (
    S.copy_bytes_def reversed distance remaining;
    Vox_lz4_spec_match.source_matches_distance_def source used distance remaining;
    if remaining > 0 && S.matches_bytes source used reversed
       && Vox_lz4_spec_match.source_matches_distance source used distance remaining
    then begin
      read_distance source used reversed distance;
      I.at_get source (used - distance);
      B.source_at_def source used;
      B.source_at_def source (used - distance);
      append source used reversed;
      matches source (used + 1)
        (D.byte_of_char (Vox_sequence.iarray_get source used) :: reversed)
        distance (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (literal_substitute @ total) :
    (reversed : S.byte list) -> (wire : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (source : char iarray) ->
    (anchor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length wire - first
      && n <= Iarray.length source - anchor}) ->
    {u : unit | not (B.literal_bytes wire first source anchor remaining)
      || S.literal_bytes reversed wire first remaining ===
           S.literal_bytes reversed source anchor remaining} @ ghost =
  fun reversed wire first source anchor remaining -> ghost_ (
    B.literal_bytes_def wire first source anchor remaining;
    S.literal_bytes_def reversed wire first remaining;
    S.literal_bytes_def reversed source anchor remaining;
    if remaining > 0 && B.literal_bytes wire first source anchor remaining then begin
      B.source_at_def wire first;
      B.source_at_def source anchor;
      let byte = D.byte_of_char (Vox_sequence.iarray_get source anchor) in
      literal_substitute (byte :: reversed) wire (first + 1)
        source (anchor + 1) (remaining - 1)
    end;
    ())
[@@decreases remaining]

let (decode_end @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (reversed : S.byte list) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor P.End
      && (last_match_start < 0 ||
          (anchor <= Iarray.length source - 5
           && last_match_start <= Iarray.length source - 12)))
      || let result =
           S.decode_model wire cursor last_match_start fuel
             capacity anchor reversed in
         result.S.kind === D.Done
         && result.S.count = Iarray.length source
         && result.S.reversed ===
              S.literal_bytes reversed source anchor
                (Iarray.length source - anchor)} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity reversed ->
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
      literal_substitute reversed wire
        (cursor + 1 + extensions) source anchor literals;
      S.decode_model_def wire cursor last_match_start fuel
        capacity anchor reversed
    end;
    ())

let (decode_sequence_step @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (reversed : S.byte list) ->
    (step : P.sequence) -> (rest : P.plan) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor
      (P.Sequence (step, rest)))
      || let literals = step.position - anchor in
         let match_code = step.length - 4 in
         let next_cursor = cursor + 1 + Vox_lz4_spec_wire.extra_count literals
           + literals + 2 + Vox_lz4_spec_wire.extra_count match_code in
         let after_literals =
           S.literal_bytes reversed source anchor literals in
         let after_match =
           S.copy_bytes after_literals
             step.distance step.length in
         S.decode_model wire cursor last_match_start fuel
           capacity anchor reversed ===
         S.decode_model wire next_cursor step.position (fuel - 1)
           capacity
           (step.position + step.length) after_match} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity reversed
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
      W.match_token_fields literals match_code;
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
      literal_substitute reversed wire literal_pos
        source anchor literals;
      let _ : {u : unit |
        S.literal_bytes reversed wire literal_pos literals ===
        S.literal_bytes reversed source anchor literals} =
        () in
      S.decode_model_def wire cursor last_match_start fuel
        capacity anchor reversed
    end;
    ())


let rec (decode_plan @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (cursor : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (last_match_start : int) -> (fuel : {n : int | 0 < n}) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    (reversed : S.byte list) -> (plan : P.plan) ->
    {u : unit | not (S.matches_bytes source anchor reversed
      && Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor plan
      && Iarray.length wire - cursor <= fuel
      && (last_match_start < 0 ||
          (anchor <= Iarray.length source - 5
           && last_match_start <= Iarray.length source - 12)))
      || let result = S.decode_model wire cursor last_match_start fuel
                       capacity anchor reversed in
         result.S.kind === D.Done && result.S.count = Iarray.length source
         && S.matches_bytes source result.S.count result.S.reversed} @ ghost =
  fun source wire anchor cursor last_match_start fuel capacity reversed plan ->
  ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor cursor plan;
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    if S.matches_bytes source anchor reversed
       && Vox_lz4_spec_wire.wire_matches_plan source wire anchor cursor plan
       && Iarray.length wire - cursor <= fuel
       && (last_match_start < 0 ||
           (anchor <= Iarray.length source - 5
            && last_match_start <= Iarray.length source - 12)) then
      (match plan with
       | P.End ->
         decode_end source wire anchor cursor last_match_start fuel
           capacity reversed;
         literals source anchor reversed (Iarray.length source - anchor)
       | P.Sequence (step, rest) ->
         let literal_count = step.position - anchor in
         let match_code = step.length - 4 in
         let next_cursor = cursor + 1 + Vox_lz4_spec_wire.extra_count literal_count
           + literal_count + 2 + Vox_lz4_spec_wire.extra_count match_code in
         let after_literals = S.literal_bytes reversed source anchor literal_count in
         let after_match = S.copy_bytes after_literals step.distance step.length in
         decode_sequence_step source wire anchor cursor last_match_start fuel
           capacity reversed step rest;
         literals source anchor reversed literal_count;
         matches source step.position after_literals step.distance step.length;
         let next_fuel : {n : int | 0 < n
           && Iarray.length wire - next_cursor <= n} = fuel - 1 in
         decode_plan source wire (step.position + step.length)
           next_cursor step.position next_fuel capacity after_match rest);
    ())

let (decode_wire @ total) :
    (source : char iarray) -> (wire : char iarray) -> (plan : P.plan) ->
    (capacity : {c : int | Iarray.length source <= c && c <= 4194304}) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan source wire 0 0 plan)
      || let result = S.decode_model wire 0 (-1) (Iarray.length wire)
                       capacity 0 [] in
         result.S.kind === D.Done && result.S.count = Iarray.length source
         && S.matches_bytes source result.S.count result.S.reversed} @ ghost =
  fun source wire plan capacity -> ghost_ (
    Vox_lz4_spec_wire.wire_matches_plan_def source wire 0 0 plan;
    S.matches_bytes_def source 0 [];
    if Vox_lz4_spec_wire.wire_matches_plan source wire 0 0 plan then
      decode_plan source wire 0 0 (-1) (Iarray.length wire) capacity [] plan;
    ())

let (equal_bytes @ total) :
    (left : char iarray) -> (right : char iarray) ->
    (count : {n : int | 0 <= n}) -> (reversed : S.byte list) ->
    {u : unit | not (Iarray.length left = count && Iarray.length right = count
      && S.matches_bytes left count reversed && S.matches_bytes right count reversed)
      || left === right} @ ghost =
  fun left right count reversed -> ghost_ (
    if Iarray.length left = count && Iarray.length right = count
       && S.matches_bytes left count reversed && S.matches_bytes right count reversed
    then
      I.extensional left right (fun index ->
        if 0 <= index && index < count then begin
          read_distance left count reversed (count - index);
          read_distance right count reversed (count - index);
          (match I.at left index, I.at right index with
           | Some l, Some r ->
             let lb = D.byte_of_char l in
             let rb = D.byte_of_char r in
             let _ = lb, rb in ()
           | _ -> ());
          ()
        end else begin
          I.at_outside left index;
          I.at_outside right index
        end);
    ())

let (wire_roundtrip @ total) :
    (source : string) -> (wire : string) -> (plan : P.plan) ->
    (capacity : int) ->
    (decoded : Vox_lz4_spec.decoded) ->
    {u : unit | not (Vox_lz4_spec_wire.wire_matches_plan
        (Vox_string_view.contents source) (Vox_string_view.contents wire) 0 0 plan
      && Iarray.length (Vox_string_view.contents source) <= capacity
      && capacity <= 4194304
      && Vox_lz4_spec.matches_model wire capacity decoded)
      || match decoded with
         | Error _ -> false
         | Ok output -> Vox_string_view.contents source ===
             Vox_string_view.contents output} @ ghost =
  fun source wire plan capacity decoded -> ghost_ (
    Vox_lz4_spec.matches_model_def wire capacity decoded;
    if Vox_lz4_spec_wire.wire_matches_plan
        (Vox_string_view.contents source) (Vox_string_view.contents wire) 0 0 plan
       && Iarray.length (Vox_string_view.contents source) <= capacity
       && capacity <= 4194304
       && Vox_lz4_spec.matches_model wire capacity decoded then begin
      let source_bytes = Vox_string_view.contents source in
      let wire_bytes = Vox_string_view.contents wire in
      decode_wire source_bytes wire_bytes
        plan capacity;
      let model = S.decode_model wire_bytes 0 (-1) (Iarray.length wire_bytes)
        capacity 0 [] in
      match decoded with
      | Error _ -> ()
      | Ok output -> equal_bytes source_bytes (Vox_string_view.contents output)
          model.S.count model.S.reversed
    end;
    ())

let (roundtrip @ total) :
    (source : string) -> (wire : string) -> (capacity : int) ->
    (decoded : Vox_lz4_spec.decoded) ->
    {u : unit | not (Vox_lz4_spec.compresses source wire
      && Iarray.length (Vox_string_view.contents source) <= capacity
      && capacity <= 4194304
      && Vox_lz4_spec.matches_model wire capacity decoded)
      || match decoded with
         | Error _ -> false
         | Ok output -> Vox_string_view.contents source ===
             Vox_string_view.contents output} @ ghost =
  fun source wire capacity decoded -> ghost_ (
    Vox_lz4_spec.compresses_def source wire;
    if Vox_lz4_spec.compresses source wire then
      wire_roundtrip source wire
        (Vox_lz4_spec_scan.from_source (Vox_string_view.contents source))
        capacity decoded;
    ())
