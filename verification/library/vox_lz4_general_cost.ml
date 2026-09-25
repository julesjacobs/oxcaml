module W = Vox_lz4_general_wire
module X = Vox_lz4_packed_encode
module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module M = Raw_memory
module G = Ghost_pref
module H = G.Heap

let (sequence_cost_bound @ total) :
    (literals : {n : int | 0 <= n && n <= 4194304}) ->
    (match_length : {n : int | 4 <= n && n <= 4194304}) ->
    {u : unit |
      255 * (3 + Vox_lz4_spec_wire.extra_count literals
        + Vox_lz4_spec_wire.extra_count (match_length - 4) - match_length)
      <= literals + match_length} @ ghost =
  fun literals match_length -> ghost_ (
    Vox_lz4_spec_wire.extra_count_def literals;
    Vox_lz4_spec_wire.extra_count_def (match_length - 4);
    (if literals >= 15 then
       let _ = Vox_lz4_spec_bytes.extension_count (literals - 15) in ());
    (if match_length >= 19 then
       let _ = Vox_lz4_spec_bytes.extension_count (match_length - 19) in ());
    ())

let (final_cost_bound @ total) :
    (literals : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit | 255 * (1 + Vox_lz4_spec_wire.extra_count literals)
      <= literals + 255 * 16} @ ghost =
  fun literals -> ghost_ (
    Vox_lz4_spec_wire.extra_count_def literals;
    (if literals >= 15 then
       let _ = Vox_lz4_spec_bytes.extension_count (literals - 15) in ());
    ())

let[@def] rec (encoded_size @ total)
    (source : char iarray @ immutable) (anchor : int)
    (plan : P.plan @ immutable) = ghost_ (
  Vox_lz4_spec_plan.valid_plan_def source anchor plan;
  if Iarray.length source > 4194304
     || not (Vox_lz4_spec_plan.valid_plan source anchor plan) then 0
  else match plan with
  | P.End ->
    let literals = Iarray.length source - anchor in
    1 + Vox_lz4_spec_wire.extra_count literals + literals
  | P.Sequence (step, rest) ->
    let literals = step.position - anchor in
    3 + Vox_lz4_spec_wire.extra_count literals + literals
      + Vox_lz4_spec_wire.extra_count (step.length - 4)
      + encoded_size source (step.position + step.length) rest)

let rec (encoded_size_loose_bound @ total) :
    (source : char iarray) -> (anchor : int) ->
    (plan : P.plan) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan)
      || 0 <= encoded_size source anchor plan
         && encoded_size source anchor plan
            <= 2 * (Iarray.length source - anchor) + 16}
      @ ghost =
  fun source anchor plan -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    encoded_size_def source anchor plan;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan then
      (match plan with
       | P.End ->
         final_cost_bound (Iarray.length source - anchor)
       | P.Sequence (step, rest) ->
         sequence_cost_bound (step.position - anchor) step.length;
         encoded_size_loose_bound source
           (step.position + step.length) rest);
    ())

let rec (encoded_size_bound @ total) :
    (source : char iarray) -> (anchor : int) ->
    (plan : P.plan) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan)
      || 0 <= encoded_size source anchor plan
         && 255 * (encoded_size source anchor plan
             - (Iarray.length source - anchor))
            <= (Iarray.length source - anchor) + 255 * 16}
      @ ghost =
  fun source anchor plan -> ghost_ (
    encoded_size_loose_bound source anchor plan;
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    encoded_size_def source anchor plan;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan then
      (match plan with
       | P.End ->
         final_cost_bound (Iarray.length source - anchor);
         let _ : {u : unit |
           255 * (encoded_size source anchor plan
             - (Iarray.length source - anchor))
           <= (Iarray.length source - anchor) + 255 * 16} =
           refine_ () in
         ()
       | P.Sequence (step, rest) ->
         let literals = step.position - anchor in
         let next = step.position + step.length in
         let consumed = literals + step.length in
         let segment = 3 + Vox_lz4_spec_wire.extra_count literals + literals
           + Vox_lz4_spec_wire.extra_count (step.length - 4) in
         sequence_cost_bound literals step.length;
         encoded_size_bound source next rest;
         let _ : {u : unit | Vox_lz4_spec_plan.valid_plan source next rest} =
           refine_ () in
         let _ : {u : unit |
           255 * (segment - consumed) <= consumed} =
           refine_ () in
         let _ : {u : unit |
           255 * (encoded_size source next rest
             - (Iarray.length source - next))
           <= (Iarray.length source - next) + 255 * 16} =
           refine_ () in
         let _ : {u : unit |
           encoded_size source anchor plan =
             segment + encoded_size source next rest} =
           refine_ () in
         let _ : {u : unit |
           Iarray.length source - anchor =
             consumed + (Iarray.length source - next)} =
           refine_ () in
         let _ : {u : unit |
           255 * (segment + encoded_size source next rest
             - (consumed + (Iarray.length source - next))) =
           255 * (segment - consumed)
             + 255 * (encoded_size source next rest
                 - (Iarray.length source - next))} =
           refine_ () in
         let _ : {u : unit |
           255 * (segment - consumed)
             + 255 * (encoded_size source next rest
                 - (Iarray.length source - next))
           <= consumed + (Iarray.length source - next)
                + 255 * 16} =
           refine_ () in
         let _ : {u : unit |
           255 * (encoded_size source anchor plan
             - (Iarray.length source - anchor))
           <= (Iarray.length source - anchor) + 255 * 16} =
           refine_ () in
         ());
    ())

let rec (extension_count_covers @ total) :
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit | length < 255 * Vox_lz4_spec_bytes.extension_count length} @ ghost =
  fun length -> ghost_ (
    Vox_lz4_spec_bytes.extension_count_def length;
    if length >= 255 then extension_count_covers (length - 255);
    ())
[@@decreases length]

let[@def] (extension_budget @ total) (length : int) = ghost_ (
  if 0 <= length && length <= 4194304 then Vox_lz4_spec_bytes.extension_count length
  else 0)

let (encoded_size_capacity @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (plan : P.plan) ->
    {u : unit | not (Vox_lz4_spec_plan.valid_plan source anchor plan)
      || encoded_size source anchor plan <=
           Iarray.length source - anchor
           + extension_budget (Iarray.length source - anchor) + 15}
      @ ghost =
  fun source anchor plan -> ghost_ (
    if Vox_lz4_spec_plan.valid_plan source anchor plan then begin
      encoded_size_loose_bound source anchor plan;
      encoded_size_bound source anchor plan;
      extension_budget_def (Iarray.length source - anchor);
      extension_count_covers (Iarray.length source - anchor);
      let remaining = Iarray.length source - anchor in
      let size = encoded_size source anchor plan in
      let budget = extension_budget remaining in
      let _ : {u : unit | 0 <= size && size <= 2 * remaining + 16} =
        refine_ () in
      let _ : {u : unit | 255 * (size - remaining)
        <= remaining + 255 * 16} = refine_ () in
      let _ : {u : unit |
        255 * (size - remaining - 16) =
          255 * (size - remaining) - 255 * 16} =
        refine_ () in
      let _ : {u : unit |
        255 * (size - remaining) - 255 * 16 <= remaining} =
        refine_ () in
      let _ : {u : unit | 255 * (size - remaining - 16)
        <= remaining} = refine_ () in
      let _ : {u : unit | remaining < 255 * budget} =
        refine_ () in
      let _ : {u : unit | size <= remaining + budget + 15} =
        refine_ () in
      ()
    end;
    ())

let rec (encode_model_size @ total) :
    (source : char iarray) -> (anchor : int) ->
    (plan : P.plan) -> (block : M.t) ->
    (used : int) -> (heap : G.heap) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan)
      || (E.encode_model source anchor plan block used heap).E.count =
           used + encoded_size source anchor plan} @ ghost =
  fun source anchor plan block used heap -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    E.encode_model_def source anchor plan block used heap;
    encoded_size_def source anchor plan;
    encoded_size_loose_bound source anchor plan;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan then
      (match plan with
       | P.End ->
         Vox_lz4_spec_wire.extra_count_def (Iarray.length source - anchor);
         let _ : {u : unit |
           (E.encode_model source anchor plan block used heap).E.count
             = used + encoded_size source anchor plan} =
           refine_ () in
         ()
       | P.Sequence (step, rest) ->
         let literals = step.position - anchor in
         let match_code = step.length - 4 in
         let literal_extensions = Vox_lz4_spec_wire.extra_count literals in
         let match_extensions = Vox_lz4_spec_wire.extra_count match_code in
         Vox_lz4_spec_wire.extra_count_def literals;
         Vox_lz4_spec_wire.extra_count_def match_code;
         let after_token =
           H.put heap (M.location block used)
             (Some (Vox_lz4_spec_token.match_token literals match_code)) in
         let after_literal_extensions =
           if literals >= 15 then
             X.extension_heap after_token block (used + 1)
               (literals - 15)
           else after_token in
         let literal_pos = used + 1 + literal_extensions in
         let after_literals =
           X.literal_heap after_literal_extensions block literal_pos
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
             X.extension_heap after_high block (distance_pos + 2)
               (match_code - 15)
           else after_high in
         encoded_size_loose_bound source
           (step.position + step.length) rest;
         encode_model_size source (step.position + step.length) rest
           block (distance_pos + 2 + match_extensions)
           after_match_extensions;
         let _ : {u : unit |
           (E.encode_model source anchor plan block used heap).E.count
             = used + encoded_size source anchor plan} =
           refine_ () in
         ());
    ())
