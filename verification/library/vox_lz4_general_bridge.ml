module P = Vox_lz4_general_plan
module E = Vox_lz4_general_encode
module C = Vox_lz4_general_cost
module W = Vox_lz4_general_wire
module X = Vox_lz4_packed_encode
module R = Vox_lz4_roundtrip
module S = Vox_lz4_snapshot
module M = Raw_memory
module G = Ghost_pref
module H = G.Heap

let rec (model_preserves_earlier_byte @ total) :
    (source : char iarray) -> (anchor : int) ->
    (plan : P.plan) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (heap : G.heap) -> (query : {i : int | i < used}) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used)
      || H.at (E.encode_model source anchor plan block used heap).E.state
           (M.location block query) ===
         H.at heap (M.location block query)} @ ghost =
  fun source anchor plan block used heap query -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    C.encoded_size_def source anchor plan;
    E.encode_model_def source anchor plan block used heap;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan
       && C.encoded_size source anchor plan <= 4210768 - used then begin
      match plan with
      | P.End ->
        Vox_lz4_spec_plan.valid_plan_def source anchor P.End;
        C.encoded_size_def source anchor P.End;
        Vox_lz4_spec_plan.valid_plan_def source anchor P.End;
        let literals : {n : int | 0 <= n && n <= 4194304} =
          refine_ (Iarray.length source - anchor) in
        let extensions = R.extra_count literals in
        R.extra_count_def literals;
        Vox_lz4_spec_wire.extra_count_def literals;
        let after_token =
          H.put heap (M.location block used)
            (Some (Vox_lz4_spec_bytes.literal_token literals)) in
        let after_extensions =
          if literals >= 15 then
            X.extension_heap after_token block (used + 1)
              (literals - 15)
          else after_token in
        R.heap_put_other heap block used query (Vox_lz4_spec_bytes.literal_token literals);
        if literals >= 15 then begin
          let remaining : {n : int | 0 <= n && n <= 4194304} =
            refine_ (literals - 15) in
          let _ : {u : unit |
            Vox_lz4_spec_bytes.extension_count remaining <=
              4210768 - (used + 1)} = refine_ () in
          R.extension_heap_outside after_token block (used + 1)
            (literals - 15) query
        end;
        R.literal_heap_outside after_extensions block
          (used + 1 + extensions) source anchor literals query;
        let _ : {u : unit |
          H.at (E.encode_model source anchor plan block used heap).E.state
            (M.location block query) ===
          H.at heap (M.location block query)} = refine_ () in
        ()
      | P.Sequence (step, rest) ->
        Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, rest));
        C.encoded_size_def source anchor (P.Sequence (step, rest));
        Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, rest));
        let literals : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.position - anchor) in
        let match_code : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.length - 4) in
        let literal_extensions = R.extra_count literals in
        let match_extensions = R.extra_count match_code in
        C.encoded_size_loose_bound source
          (step.position + step.length) rest;
        R.extra_count_def literals;
        R.extra_count_def match_code;
        Vox_lz4_spec_wire.extra_count_def literals;
        Vox_lz4_spec_wire.extra_count_def match_code;
        let token : M.byte = Vox_lz4_spec_token.match_token literals match_code in
        let after_token =
          H.put heap (M.location block used) (Some token) in
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
        let after_low = H.put after_literals
          (M.location block distance_pos) (Some distance.low) in
        let after_high = H.put after_low
          (M.location block (distance_pos + 1)) (Some distance.high) in
        let after_match_extensions =
          if match_code >= 15 then
            X.extension_heap after_high block (distance_pos + 2)
              (match_code - 15)
          else after_high in
        let next_used = distance_pos + 2 + match_extensions in
        R.heap_put_other heap block used query token;
        if literals >= 15 then
          R.extension_heap_outside after_token block (used + 1)
            (literals - 15) query;
        R.literal_heap_outside after_literal_extensions block
          literal_pos source anchor literals query;
        R.heap_put_other after_literals block distance_pos query
          distance.low;
        R.heap_put_other after_low block (distance_pos + 1) query
          distance.high;
        if match_code >= 15 then
          R.extension_heap_outside after_high block (distance_pos + 2)
            (match_code - 15) query;
        model_preserves_earlier_byte source
          (step.position + step.length) rest block next_used
          after_match_extensions query;
        let _ : {u : unit |
          H.at (E.encode_model source anchor plan block used heap).E.state
            (M.location block query) ===
          H.at heap (M.location block query)} = refine_ () in
        ()
      end;
    let result : {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used)
      || H.at (E.encode_model source anchor plan block used heap).E.state
           (M.location block query) ===
         H.at heap (M.location block query)} = refine_ () in
    result)

let rec (model_preserves_prefix @ total) :
    (wire : char iarray) -> (source : char iarray) ->
    (anchor : int) -> (plan : P.plan) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (heap : G.heap) -> (count : {n : int | 0 <= n && n <= used}) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor plan block used heap).E.state
           block count)
      || Vox_lz4_spec_bytes.prefix_matches wire heap block count} @ ghost =
  fun wire source anchor plan block used heap count -> ghost_ (
    Vox_lz4_spec_bytes.prefix_matches_def wire heap block count;
    Vox_lz4_spec_bytes.prefix_matches_def wire
      (E.encode_model source anchor plan block used heap).E.state
      block count;
    if count > 0 then begin
      model_preserves_earlier_byte source anchor plan block used heap
        (count - 1);
      model_preserves_prefix wire source anchor plan block used heap
        (count - 1)
    end;
    let result : {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor plan block used heap).E.state
           block count)
      || Vox_lz4_spec_bytes.prefix_matches wire heap block count} = refine_ () in
    result)
[@@decreases count]

let (end_model_wire @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (heap : G.heap) ->
    {u : unit | not (Iarray.length source <= 4194304
      && C.encoded_size source anchor P.End <= 4210768 - used
      && Iarray.length wire =
           (E.encode_model source anchor P.End block used heap).E.count
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor P.End block used heap).E.state
           block (Iarray.length wire))
      || Vox_lz4_spec_wire.wire_matches_plan source wire anchor used P.End} @ ghost =
  fun source wire anchor block used heap -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor P.End;
    C.encoded_size_def source anchor P.End;
    E.encode_model_def source anchor P.End block used heap;
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor used P.End;
    if Iarray.length source <= 4194304
       && C.encoded_size source anchor P.End <= 4210768 - used
       && Iarray.length wire =
            (E.encode_model source anchor P.End block used heap).E.count
       && Vox_lz4_spec_bytes.prefix_matches wire
            (E.encode_model source anchor P.End block used heap).E.state
            block (Iarray.length wire) then begin
      let literals : {n : int | 0 <= n && n <= 4194304} =
        refine_ (Iarray.length source - anchor) in
      let extensions = R.extra_count literals in
      R.extra_count_def literals;
      Vox_lz4_spec_wire.extra_count_def literals;
      let after_token = H.put heap (M.location block used)
        (Some (Vox_lz4_spec_bytes.literal_token literals)) in
      let after_extensions =
        if literals >= 15 then
          X.extension_heap after_token block (used + 1)
            (literals - 15)
        else after_token in
      let final = X.literal_heap after_extensions block
        (used + 1 + extensions) source anchor literals in
      C.encode_model_size source anchor P.End block used heap;
      R.literal_heap_wire wire after_extensions block
        (used + 1 + extensions) source anchor literals;
      S.prefix_matches_prefix wire final block
        (Iarray.length wire) (used + 1 + extensions);
      R.literal_heap_frame_prefix wire after_extensions block
        (used + 1 + extensions) source anchor literals
        (used + 1 + extensions);
      if literals >= 15 then begin
        R.extension_heap_wire wire after_token block (used + 1)
          (literals - 15);
        S.prefix_matches_prefix wire after_extensions block
          (used + 1 + extensions) (used + 1);
        R.extension_heap_frame_prefix wire after_token block (used + 1)
          (literals - 15) (used + 1)
      end;
      R.heap_put_at heap block used (Vox_lz4_spec_bytes.literal_token literals);
      R.snapshot_at wire after_token block (used + 1) used;
      Vox_lz4_spec_bytes.wire_byte_def wire used (Vox_lz4_spec_bytes.literal_token literals);
      let _ : {u : unit |
        Iarray.length wire = used + 1 + extensions + literals} =
        refine_ () in
      let _ : {u : unit |
        Vox_lz4_spec_bytes.wire_byte wire used (Vox_lz4_spec_bytes.literal_token literals)} =
        refine_ () in
      let _ : {u : unit | literals < 15 ||
        Vox_lz4_spec_bytes.extension_bytes wire (used + 1) (literals - 15)} =
        refine_ () in
      let _ : {u : unit |
        Vox_lz4_spec_bytes.literal_bytes wire (used + 1 + extensions)
          source anchor literals} = refine_ () in
      let _ : {u : unit |
        Vox_lz4_spec_wire.wire_matches_plan source wire anchor used P.End} =
        refine_ () in
      ()
    end;
    let result : {u : unit | not (Iarray.length source <= 4194304
      && C.encoded_size source anchor P.End <= 4210768 - used
      && Iarray.length wire =
           (E.encode_model source anchor P.End block used heap).E.count
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor P.End block used heap).E.state
           block (Iarray.length wire))
      || Vox_lz4_spec_wire.wire_matches_plan source wire anchor used P.End} =
      refine_ () in
    result)

let (encoded_size_positive @ total) :
    (source : char iarray) -> (anchor : int) -> (plan : P.plan) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan)
      || 0 < C.encoded_size source anchor plan} @ ghost =
  fun source anchor plan -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    C.encoded_size_def source anchor plan;
    C.encoded_size_loose_bound source anchor plan;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan then begin
      match plan with
      | P.End ->
        Vox_lz4_spec_plan.valid_plan_def source anchor P.End;
        C.encoded_size_def source anchor P.End;
        let literals : {n : int | 0 <= n && n <= 4194304} =
          refine_ (Iarray.length source - anchor) in
        let extensions = Vox_lz4_spec_wire.extra_count literals in
        Vox_lz4_spec_wire.extra_count_def literals;
        let _ : {u : unit |
          C.encoded_size source anchor P.End =
            1 + extensions + literals} = refine_ () in
        let _ : {u : unit |
          0 < C.encoded_size source anchor P.End} = refine_ () in
        ()
      | P.Sequence (step, rest) ->
        Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, rest));
        let literals : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.position - anchor) in
        let code : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.length - 4) in
        C.encoded_size_def source anchor (P.Sequence (step, rest));
        C.encoded_size_loose_bound source
          (step.position + step.length) rest;
        let literal_extensions = Vox_lz4_spec_wire.extra_count literals in
        let match_extensions = Vox_lz4_spec_wire.extra_count code in
        Vox_lz4_spec_wire.extra_count_def literals;
        Vox_lz4_spec_wire.extra_count_def code;
        let _ : {u : unit |
          0 <= C.encoded_size source (step.position + step.length)
            rest} = refine_ () in
        let _ : {u : unit | C.encoded_size source anchor
          (P.Sequence (step, rest)) =
          3 + literal_extensions + literals + match_extensions
            + C.encoded_size source (step.position + step.length)
                rest} = refine_ () in
        let _ : {u : unit | 0 < C.encoded_size source anchor
          (P.Sequence (step, rest))} = refine_ () in
        ()
    end;
    let result : {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan)
      || 0 < C.encoded_size source anchor plan} = refine_ () in
    result)

let rec (model_wire @ total) :
    (source : char iarray) -> (wire : char iarray) ->
    (anchor : {a : int | 0 <= a && a <= Iarray.length source}) ->
    (plan : P.plan) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (heap : G.heap) ->
    {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used
      && Iarray.length wire =
           (E.encode_model source anchor plan block used heap).E.count
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor plan block used heap).E.state
           block (Iarray.length wire))
      || Vox_lz4_spec_wire.wire_matches_plan source wire anchor used plan} @ ghost =
  fun source wire anchor plan block used heap -> ghost_ (
    Vox_lz4_spec_plan.valid_plan_def source anchor plan;
    C.encoded_size_def source anchor plan;
    E.encode_model_def source anchor plan block used heap;
    Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor used plan;
    if Iarray.length source <= 4194304
       && Vox_lz4_spec_plan.valid_plan source anchor plan
       && C.encoded_size source anchor plan <= 4210768 - used
       && Iarray.length wire =
            (E.encode_model source anchor plan block used heap).E.count
       && Vox_lz4_spec_bytes.prefix_matches wire
            (E.encode_model source anchor plan block used heap).E.state
            block (Iarray.length wire) then begin
      match plan with
      | P.End -> end_model_wire source wire anchor block used heap
      | P.Sequence (step, rest) ->
        Vox_lz4_spec_plan.valid_plan_def source anchor (P.Sequence (step, rest));
        C.encoded_size_def source anchor (P.Sequence (step, rest));
        E.encode_model_def source anchor (P.Sequence (step, rest))
          block used heap;
        Vox_lz4_spec_wire.wire_matches_plan_def source wire anchor used
          (P.Sequence (step, rest));
        let literals : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.position - anchor) in
        let match_code : {n : int | 0 <= n && n <= 4194304} =
          refine_ (step.length - 4) in
        let literal_extensions = R.extra_count literals in
        let match_extensions = R.extra_count match_code in
        R.extra_count_def literals;
        R.extra_count_def match_code;
        Vox_lz4_spec_wire.extra_count_def literals;
        Vox_lz4_spec_wire.extra_count_def match_code;
        C.encoded_size_loose_bound source
          (step.position + step.length) rest;
        let token : M.byte = Vox_lz4_spec_token.match_token literals match_code in
        let after_token =
          H.put heap (M.location block used) (Some token) in
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
        let after_low = H.put after_literals
          (M.location block distance_pos) (Some distance.low) in
        let after_high = H.put after_low
          (M.location block (distance_pos + 1)) (Some distance.high) in
        let after_match_extensions =
          if match_code >= 15 then
            X.extension_heap after_high block (distance_pos + 2)
              (match_code - 15)
          else after_high in
        let next_used = distance_pos + 2 + match_extensions in
        C.encode_model_size source anchor (P.Sequence (step, rest))
          block used heap;
        C.encode_model_size source (step.position + step.length) rest
          block next_used after_match_extensions;
        encoded_size_positive source (step.position + step.length) rest;
        let _ : {u : unit |
          C.encoded_size source (step.position + step.length) rest
            <= 4210768 - next_used} = refine_ () in
        let _ : {u : unit |
          (E.encode_model source anchor (P.Sequence (step, rest))
            block used heap).E.state ===
          (E.encode_model source (step.position + step.length) rest
            block next_used after_match_extensions).E.state} =
          refine_ () in
        S.prefix_matches_prefix wire
          (E.encode_model source (step.position + step.length) rest
            block next_used after_match_extensions).E.state block
          (Iarray.length wire) next_used;
        model_wire source wire (step.position + step.length) rest
          block next_used after_match_extensions;
        model_preserves_prefix wire source
          (step.position + step.length) rest block next_used
          after_match_extensions next_used;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire
          after_match_extensions block next_used} = refine_ () in
        S.prefix_matches_prefix wire after_match_extensions block
          next_used (distance_pos + 2);
        if match_code >= 15 then begin
          R.extension_heap_wire wire after_high block (distance_pos + 2)
            (match_code - 15);
          R.extension_heap_frame_prefix wire after_high block
            (distance_pos + 2) (match_code - 15)
            (distance_pos + 2)
        end;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire
          after_high block (distance_pos + 2)} = refine_ () in
        S.prefix_matches_prefix wire after_high block
          (distance_pos + 2) (distance_pos + 1);
        R.prefix_matches_put_outside wire after_low block
          (distance_pos + 1) (distance_pos + 1) distance.high;
        S.prefix_matches_prefix wire after_low block
          (distance_pos + 1) distance_pos;
        R.prefix_matches_put_outside wire after_literals block
          distance_pos distance_pos distance.low;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire
          after_literals block distance_pos} = refine_ () in
        R.literal_heap_wire wire after_literal_extensions block
          literal_pos source anchor literals;
        S.prefix_matches_prefix wire after_literals block
          distance_pos literal_pos;
        R.literal_heap_frame_prefix wire after_literal_extensions block
          literal_pos source anchor literals literal_pos;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire
          after_literal_extensions block literal_pos} = refine_ () in
        S.prefix_matches_prefix wire after_literal_extensions block
          literal_pos (used + 1);
        if literals >= 15 then begin
          R.extension_heap_wire wire after_token block (used + 1)
            (literals - 15);
          R.extension_heap_frame_prefix wire after_token block
            (used + 1) (literals - 15) (used + 1)
        end;
        let _ : {u : unit |
          Vox_lz4_spec_bytes.prefix_matches wire after_token block (used + 1)} =
          refine_ () in
        R.heap_put_at heap block used token;
        R.snapshot_at wire after_token block (used + 1) used;
        Vox_lz4_spec_bytes.wire_byte_def wire used token;
        R.heap_put_at after_literals block distance_pos distance.low;
        R.snapshot_at wire after_low block
          (distance_pos + 1) distance_pos;
        Vox_lz4_spec_bytes.wire_byte_def wire distance_pos distance.low;
        R.heap_put_at after_low block (distance_pos + 1) distance.high;
        R.snapshot_at wire after_high block
          (distance_pos + 2) (distance_pos + 1);
        Vox_lz4_spec_bytes.wire_byte_def wire (distance_pos + 1) distance.high;
        let _ : {u : unit | next_used < Iarray.length wire} =
          refine_ () in
        let _ : {u : unit | Vox_lz4_spec_bytes.wire_byte wire used token} =
          refine_ () in
        let _ : {u : unit | literals < 15 ||
          Vox_lz4_spec_bytes.extension_bytes wire (used + 1) (literals - 15)} =
          refine_ () in
        let _ : {u : unit |
          Vox_lz4_spec_bytes.literal_bytes wire literal_pos source anchor literals} =
          refine_ () in
        let _ : {u : unit |
          Vox_lz4_spec_bytes.wire_byte wire distance_pos distance.low
          && Vox_lz4_spec_bytes.wire_byte wire (distance_pos + 1) distance.high} =
          refine_ () in
        let _ : {u : unit | match_code < 15 ||
          Vox_lz4_spec_bytes.extension_bytes wire (distance_pos + 2)
            (match_code - 15)} = refine_ () in
        let _ : {u : unit |
          Vox_lz4_spec_wire.wire_matches_plan source wire
            (step.position + step.length) next_used rest} =
          refine_ () in
        let _ : {u : unit |
          Vox_lz4_spec_wire.wire_matches_plan source wire anchor used
            (P.Sequence (step, rest))} = refine_ () in
        ()
    end;
    let result : {u : unit | not (Iarray.length source <= 4194304
      && Vox_lz4_spec_plan.valid_plan source anchor plan
      && C.encoded_size source anchor plan <= 4210768 - used
      && Iarray.length wire =
           (E.encode_model source anchor plan block used heap).E.count
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.encode_model source anchor plan block used heap).E.state
           block (Iarray.length wire))
      || Vox_lz4_spec_wire.wire_matches_plan source wire anchor used plan} = refine_ () in
    result)
