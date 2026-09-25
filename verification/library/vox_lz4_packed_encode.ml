module B = Vox_lz4_encode_buffer
module M = Raw_memory
module P = Ghost_pref
module H = P.Heap
module S = Vox_sequence

include Vox_lz4_spec_bytes

let (match_token @ total) :
    (code : {n : int | 0 <= n && n <= 4194304}) ->
    {token : M.byte | token = (if code >= 15 then 31 else 16 + code)} =
  fun code ->
  if code >= 15 then 31 else (16 + code)

let[@def] rec (same_prefix @ total) (source : char iarray @ immutable)
    (first : char) (count : int) = ghost_ (
  if count <= 0 then true
  else if count > Iarray.length source then false
  else same_prefix source first (count - 1)
       && Vox_lz4_spec_bytes.source_at source (count - 1) === Some first)
[@@decreases count]

let (same_prefix_extend @ total) :
    (source : char iarray) -> (first : char) -> (count : int) ->
    {u : unit | not (0 <= count && count < Iarray.length source
      && same_prefix source first count
      && Vox_lz4_spec_bytes.source_at source count === Some first)
      || same_prefix source first (count + 1)} @ ghost =
  fun source first count -> ghost_ (
    same_prefix_def source first (count + 1);
    ())

let rec (same_prefix_at @ total) :
    (source : char iarray) -> (first : char) ->
    (count : int) -> (index : int) ->
    {u : unit | not (same_prefix source first count
      && 0 <= index && index < count)
      || Vox_lz4_spec_bytes.source_at source index === Some first} @ ghost =
  fun source first count index -> ghost_ (
    same_prefix_def source first count;
    if count > 0 && index >= 0 && index < count - 1 then
      same_prefix_at source first (count - 1) index;
    ())
[@@decreases count]

let (initial_run_match_at @ total) :
    (source : char iarray) -> (first : char) ->
    (run_end : {r : int | 1 <= r && r <= Iarray.length source}) ->
    (j : {i : int | 0 <= i && i < run_end - 1}) ->
    {u : unit | not (same_prefix source first run_end)
      || Vox_lz4_spec_bytes.source_at source (j + 1) === Vox_lz4_spec_bytes.source_at source j}
      @ ghost =
  fun source first run_end j -> ghost_ (
    same_prefix_at source first run_end j;
    same_prefix_at source first run_end (j + 1);
    ())

let[@def] rec (literal_heap @ total) (h : P.heap @ immutable)
    (block : M.t @ immutable) (used : int)
    (source : char iarray @ immutable) (first : int) (remaining : int) =
  ghost_ (
    if remaining <= 0 || first < 0 || first >= Iarray.length source then h
    else
      let value = Vox_lz4_spec_decode.byte_of_char (S.iarray_get source first) in
      literal_heap (H.put h (M.location block used) (Some value))
        block (used + 1) source (first + 1) (remaining - 1))
[@@decreases remaining]

let rec copy_literals :
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length source - first}) ->
    (buffer : {b : B.t | remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + remaining
      && P.own after.permission ===
           literal_heap (P.own buffer.permission) buffer.block buffer.used
             source first remaining} @ unique =
  fun source first remaining buffer ->
    if remaining = 0 then begin
      let { B.block; permission; used } = buffer in
      ghost_ (literal_heap_def (P.own (borrow_ permission)) block used
                source first remaining);
      { B.block; permission; used }
    end else
      let { B.block; permission; used } = buffer in
      let before = ghost_ (P.own (borrow_ permission)) in
      ghost_ (literal_heap_def before block used source first remaining);
      let buffer : B.t = { B.block; permission; used } in
      let value = Vox_lz4_spec_decode.byte_of_char (S.iarray_get source first) in
      let buffer = B.append buffer value in
      copy_literals source (first + 1) (remaining - 1) buffer
[@@decreases remaining]

let[@def] rec (extension_heap @ total) (h : P.heap @ immutable)
    (block : M.t @ immutable) (used : int) (remaining : int) = ghost_ (
  if remaining >= 255 then
    extension_heap (H.put h (M.location block used) (Some 255))
      block (used + 1) (remaining - 255)
  else if remaining >= 0 then
    H.put h (M.location block used) (Some remaining)
  else h)
[@@decreases remaining]

let rec emit_extensions :
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    (buffer : {b : B.t |
      Vox_lz4_spec_bytes.extension_count remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + Vox_lz4_spec_bytes.extension_count remaining
      && P.own after.permission ===
           extension_heap (P.own buffer.permission) buffer.block buffer.used
             remaining} @ unique =
  fun remaining buffer ->
    let { B.block; permission; used } = buffer in
    let before = ghost_ (P.own (borrow_ permission)) in
    ghost_ (Vox_lz4_spec_bytes.extension_count_def remaining);
    ghost_ (extension_heap_def before block used remaining);
    let buffer : B.t = { B.block; permission; used } in
    if remaining >= 255 then
      let buffer = B.append buffer 255 in
      emit_extensions (remaining - 255) buffer
    else
      let byte : M.byte = remaining in
      B.append buffer byte
[@@decreases remaining]

let (literal_capacity @ total) :
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (extensions : {e : int | 0 <= e && e <= 16449}) ->
    {capacity : int | capacity = 1 + length + extensions
      && 1 <= capacity && capacity <= 4210768} =
  fun length extensions ->
    (1 + length + extensions)

let (match_capacity @ total) :
    (match_code : {n : int | 0 <= n}) ->
    (suffix : {n : int | 5 <= n && n <= 4194299}) ->
    (match_extensions : {e : int | 0 <= e
      && 255 * e <= match_code + 255}) ->
    (literal_extensions : {e : int | 0 <= e
      && e <= 16450 - match_extensions}) ->
    {capacity : int | capacity = 5 + suffix + match_extensions
      + literal_extensions && 1 <= capacity && capacity <= 4210768} =
  fun match_code suffix match_extensions literal_extensions ->
    (5 + suffix + match_extensions + literal_extensions)

type literal_model_result = {
  literal_count : int;
  literal_state : P.heap @@ ghost;
}

let[@def] (literal_model @ total) (source : char iarray @ immutable)
    (block : M.t @ immutable) : literal_model_result @ ghost = ghost_ (
  let length = Iarray.length source in
  let initial = M.footprint block in
  if length > 4194304 then
    { literal_count = 0; literal_state = initial }
  else
    let extensions =
      if length >= 15 then Vox_lz4_spec_bytes.extension_count (length - 15) else 0 in
    let after_token =
      H.put initial (M.location block 0) (Some (Vox_lz4_spec_bytes.literal_token length)) in
    let after_extensions =
      if length >= 15 then
        extension_heap after_token block 1 (length - 15)
      else after_token in
    { literal_count = 1 + extensions + length;
      literal_state = literal_heap after_extensions block (1 + extensions)
                        source 0 length })

let encode_literals : (source : char iarray) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let model = literal_model source buffer.block in
        Iarray.length source <= 4194304
        && buffer.used = model.literal_count
        && P.own buffer.permission === model.literal_state} @ unique =
  fun source ->
  let length = Iarray.length source in
  if length > 4194304 then None
  else
    let extensions : {e : int | 0 <= e && e <= 16449} =
      if length >= 15 then Vox_lz4_spec_bytes.extension_count (length - 15) else 0 in
    let needed = literal_capacity length extensions in
    match B.create needed with
      | None -> None
      | Some buffer ->
        let token = Vox_lz4_spec_bytes.literal_token length in
        let buffer = B.append buffer token in
        let buffer =
          if length >= 15 then emit_extensions (length - 15) buffer
          else buffer in
        let buffer = copy_literals source 0 length buffer in
        let { B.block; permission; used } = buffer in
        ghost_ (literal_model_def source block);
        Some { B.block; permission; used }

let rec (initial_run_end @ total) :
    (source : char iarray) -> (first : char) ->
    (pos : {p : int | 1 <= p && p <= Iarray.length source
      && same_prefix source first p}) ->
    (limit : {l : int | pos <= l && l <= Iarray.length source}) ->
    (fuel : {f : int | 0 <= f}) ->
    {r : int | pos <= r && r <= limit
      && same_prefix source first r} =
  fun source first pos limit fuel ->
  if fuel = 0 || pos >= limit then pos
  else if Vox_lz4_spec_bytes.same_char (S.iarray_get source pos) first then begin
    ghost_ (Vox_lz4_spec_bytes.source_at_def source pos);
    ghost_ (same_prefix_extend source first pos);
    initial_run_end source first (pos + 1) limit (fuel - 1)
  end else pos
[@@decreases fuel]

type plan = Literal_only | Initial_run of int

let[@def] (plan_of_source @ total) :
    (source : char iarray) ->
    {p : plan | match p with
      | Literal_only -> true
      | Initial_run run_end ->
        13 <= Iarray.length source && Iarray.length source <= 4194304
        && 5 <= run_end && run_end <= Iarray.length source - 5
        && (match Vox_lz4_spec_bytes.source_at source 0 with
            | Some first -> same_prefix source first run_end
            | None -> false)} =
  fun source ->
  let length = Iarray.length source in
  if length < 13 || length > 4194304 then Literal_only
  else
    let first = S.iarray_get source 0 in
    ghost_ (Vox_lz4_spec_bytes.source_at_def source 0);
    ghost_ (same_prefix_def source first 0);
    ghost_ (same_prefix_def source first 1);
    let run_end = initial_run_end source first 1 (length - 5) length in
    if run_end < 5 || run_end > length - 5 then Literal_only
    else Initial_run run_end

type model_result = {
  choice : plan;
  count : int;
  state : P.heap @@ ghost;
}

let[@def] (encode_model @ total) (source : char iarray @ immutable)
    (block : M.t @ immutable) : model_result @ ghost = ghost_ (
  let length = Iarray.length source in
  let initial = M.footprint block in
  match plan_of_source source with
  | Literal_only ->
    let literal = literal_model source block in
    { choice = Literal_only; count = literal.literal_count;
      state = literal.literal_state }
  | Initial_run run_end ->
    let code = run_end - 5 in
    let suffix = length - run_end in
    let match_extensions =
      if code >= 15 then Vox_lz4_spec_bytes.extension_count (code - 15) else 0 in
    let literal_extensions =
      if suffix >= 15 then Vox_lz4_spec_bytes.extension_count (suffix - 15) else 0 in
    let after_token =
      H.put initial (M.location block 0) (Some (match_token code)) in
    let after_literal = literal_heap after_token block 1 source 0 1 in
    let after_offset_low =
      H.put after_literal (M.location block 2) (Some 1) in
    let after_offset =
      H.put after_offset_low (M.location block 3) (Some 0) in
    let after_match_extensions =
      if code >= 15 then
        extension_heap after_offset block 4 (code - 15)
      else after_offset in
    let final_token_pos = 4 + match_extensions in
    let after_final_token =
      H.put after_match_extensions (M.location block final_token_pos)
        (Some (Vox_lz4_spec_bytes.literal_token suffix)) in
    let after_literal_extensions =
      if suffix >= 15 then
        extension_heap after_final_token block (final_token_pos + 1)
          (suffix - 15)
      else after_final_token in
    let final_literals_pos = 5 + match_extensions + literal_extensions in
    { choice = Initial_run run_end;
      count = final_literals_pos + suffix;
      state = literal_heap after_literal_extensions block final_literals_pos
                source run_end suffix })

let encode : (source : char iarray) ->
    {r : B.t option | match r with
      | None -> true
      | Some buffer ->
        let model = encode_model source buffer.block in
        Iarray.length source <= 4194304
        && buffer.used = model.count
        && P.own buffer.permission === model.state} @ unique =
  fun source ->
  let length = Iarray.length source in
  match plan_of_source source with
  | Literal_only ->
    (match encode_literals source with
     | None -> None
     | Some buffer ->
       let { B.block; permission; used } = buffer in
       ghost_ (encode_model_def source block);
       Some { B.block; permission; used })
  | Initial_run end_pos ->
      let match_code = end_pos - 5 in
      let suffix = length - end_pos in
      let match_extensions :
          {e : int | 0 <= e && 255 * e <= match_code + 255} =
        if match_code >= 15 then Vox_lz4_spec_bytes.extension_count (match_code - 15) else 0 in
      let literal_extensions :
          {e : int | 0 <= e && 255 * e <= suffix + 255} =
        if suffix >= 15 then Vox_lz4_spec_bytes.extension_count (suffix - 15) else 0 in
      let needed =
        match_capacity match_code suffix match_extensions literal_extensions in
        match B.create needed with
        | None -> None
        | Some buffer ->
          let token = match_token match_code in
          let buffer = B.append buffer token in
          let buffer = copy_literals source 0 1 buffer in
          let buffer = B.append buffer 1 in
          let buffer = B.append buffer 0 in
          let buffer =
            if match_code >= 15 then
              emit_extensions (match_code - 15) buffer
            else buffer in
          let final_token = Vox_lz4_spec_bytes.literal_token suffix in
          let buffer = B.append buffer final_token in
          let buffer =
            if suffix >= 15 then emit_extensions (suffix - 15) buffer
            else buffer in
          let buffer = copy_literals source end_pos suffix buffer in
          let { B.block; permission; used } = buffer in
          ghost_ (encode_model_def source block);
          Some { B.block; permission; used }
