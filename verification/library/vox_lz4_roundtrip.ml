module D = Vox_lz4_packed
module E = Vox_lz4_packed_encode
module M = Raw_memory
module P = Ghost_pref
module H = P.Heap
module S = Vox_lz4_snapshot
module EB = Vox_lz4_encode_buffer
module DB = Vox_lz4_buffer

let[@def] (extra_count @ total)
    (length : {n : int | 0 <= n && n <= 4194304}) :
    {e : int | 0 <= e && e <= 16449
      && 255 * e <= length + 255} =
  if length >= 15 then Vox_lz4_spec_bytes.extension_count (length - 15) else 0

let (iarray_get_same @ total) :
    (values : char iarray) ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | Vox_iarray.get values index ===
      Vox_sequence.iarray_get values index} @ ghost =
  fun values index -> ghost_ (())

let (snapshot_at @ total) :
    (values : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (count : int) ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | not (index < count
      && Vox_lz4_spec_bytes.prefix_matches values heap block count)
      || match Vox_lz4_spec_bytes.source_at values index,
               H.at heap (M.location block index) with
         | Some c, Some (Some byte) -> Vox_lz4_spec_decode.byte_of_char c = byte
         | _ -> false} @ ghost =
  fun values heap block count index -> ghost_ (
    S.prefix_matches_get values heap block count index;
    Vox_iarray.at_get values index;
    iarray_get_same values index;
    Vox_lz4_spec_bytes.source_at_def values index;
    ())

let (heap_put_at @ total) :
    (heap : P.heap) -> (block : M.t) -> (index : int) ->
    (byte : M.byte) ->
    {u : unit | H.at (H.put heap (M.location block index) (Some byte))
      (M.location block index) === Some (Some byte)} @ ghost =
  fun heap block index byte -> ghost_ (())

let (heap_put_other @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (index : int) -> (query : int) -> (byte : M.byte) ->
    {u : unit | index = query ||
      H.at (H.put heap (M.location block index) (Some byte))
        (M.location block query) === H.at heap (M.location block query)}
      @ ghost =
  fun heap block index query byte -> ghost_ (
    M.location_law block block index query;
    ())

let rec (prefix_matches_put_outside @ total) :
    (wire : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (count : {n : int | 0 <= n}) ->
    (index : {i : int | count <= i}) -> (byte : M.byte) ->
    {u : unit | not (Vox_lz4_spec_bytes.prefix_matches wire
      (H.put heap (M.location block index) (Some byte)) block count)
      || Vox_lz4_spec_bytes.prefix_matches wire heap block count} @ ghost =
  fun wire heap block count index byte -> ghost_ (
    Vox_lz4_spec_bytes.prefix_matches_def wire heap block count;
    Vox_lz4_spec_bytes.prefix_matches_def wire
      (H.put heap (M.location block index) (Some byte)) block count;
    if count > 0 then begin
      heap_put_other heap block index (count - 1) byte;
      prefix_matches_put_outside wire heap block (count - 1) index byte
    end;
    ())
[@@decreases count]

let rec (literal_heap_outside @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length source - first && n <= 4210768 - used}) ->
    (query : int) ->
    {u : unit | not (query < used || used + remaining <= query)
      || H.at (E.literal_heap heap block used source first remaining)
           (M.location block query) === H.at heap (M.location block query)}
      @ ghost =
  fun heap block used source first remaining query -> ghost_ (
    E.literal_heap_def heap block used source first remaining;
    if remaining > 0 then begin
      let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source first) in
      let next = H.put heap (M.location block used) (Some byte) in
      heap_put_other heap block used query byte;
      literal_heap_outside next block (used + 1) source (first + 1)
        (remaining - 1) query
    end;
    ())
[@@decreases remaining]

let rec (literal_heap_at @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length source - first && n <= 4210768 - used}) ->
    (offset : {j : int | 0 <= j && j < remaining}) ->
    {u : unit |
      match Vox_lz4_spec_bytes.source_at source (first + offset) with
      | Some c ->
        H.at (E.literal_heap heap block used source first remaining)
          (M.location block (used + offset)) ===
          Some (Some (Vox_lz4_spec_decode.byte_of_char c))
      | None -> false} @ ghost =
  fun heap block used source first remaining offset -> ghost_ (
    E.literal_heap_def heap block used source first remaining;
    Vox_lz4_spec_bytes.source_at_def source first;
    let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source first) in
    let next = H.put heap (M.location block used) (Some byte) in
    if offset = 0 then begin
      literal_heap_outside next block (used + 1) source (first + 1)
        (remaining - 1) used;
      heap_put_at heap block used byte
    end else
      literal_heap_at next block (used + 1) source (first + 1)
        (remaining - 1) (offset - 1);
    ())
[@@decreases remaining]

let rec (extension_heap_outside @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    (query : int) ->
    {u : unit | not (Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used
      && (query < used
      || used + Vox_lz4_spec_bytes.extension_count remaining <= query))
      || H.at (E.extension_heap heap block used remaining)
           (M.location block query) === H.at heap (M.location block query)}
      @ ghost =
  fun heap block used remaining query -> ghost_ (
    E.extension_heap_def heap block used remaining;
    Vox_lz4_spec_bytes.extension_count_def remaining;
    if Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used then
      if remaining >= 255 then begin
        let next = H.put heap (M.location block used) (Some 255) in
        heap_put_other heap block used query 255;
        extension_heap_outside next block (used + 1) (remaining - 255) query
      end else
        heap_put_other heap block used query (remaining);
    ())
[@@decreases remaining]

let (extension_first_byte @ total) :
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    {byte : M.byte | byte = (if remaining >= 255 then 255 else remaining)} =
  fun remaining -> if remaining >= 255 then 255 else remaining

let (extension_heap_first @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit | not (Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used)
      || H.at (E.extension_heap heap block used remaining)
           (M.location block used) ===
           Some (Some (extension_first_byte remaining))}
      @ ghost =
  fun heap block used remaining -> ghost_ (
    E.extension_heap_def heap block used remaining;
    Vox_lz4_spec_bytes.extension_count_def remaining;
    if Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used then
      if remaining >= 255 then begin
        let next = H.put heap (M.location block used) (Some 255) in
        extension_heap_outside next block (used + 1) (remaining - 255) used;
        heap_put_at heap block used 255
      end else
        heap_put_at heap block used (remaining);
    ())

include Vox_lz4_spec_bytes

let rec (extension_heap_wire @ total) :
    (wire : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit | not (Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used
      && Vox_lz4_spec_bytes.extension_count remaining <= Iarray.length wire - used
      && Vox_lz4_spec_bytes.prefix_matches wire (E.extension_heap heap block used remaining)
           block (used + Vox_lz4_spec_bytes.extension_count remaining))
      || Vox_lz4_spec_bytes.extension_bytes wire used remaining} @ ghost =
  fun wire heap block used remaining -> ghost_ (
    Vox_lz4_spec_bytes.extension_count_def remaining;
    E.extension_heap_def heap block used remaining;
    Vox_lz4_spec_bytes.extension_bytes_def wire used remaining;
    if Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used
       && Vox_lz4_spec_bytes.extension_count remaining <= Iarray.length wire - used
       && Vox_lz4_spec_bytes.prefix_matches wire (E.extension_heap heap block used remaining)
            block (used + Vox_lz4_spec_bytes.extension_count remaining) then begin
      let index : {i : int | 0 <= i && i < Iarray.length wire} =
        used in
      extension_heap_first heap block used remaining;
      snapshot_at wire (E.extension_heap heap block used remaining) block
        (used + Vox_lz4_spec_bytes.extension_count remaining) index;
      if remaining >= 255 then begin
        let next = H.put heap (M.location block used) (Some 255) in
        extension_heap_wire wire next block (used + 1) (remaining - 255)
      end
    end;
    ())
[@@decreases remaining]

let rec (literal_heap_wire @ total) :
    (wire : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length source - first && n <= 4210768 - used}) ->
    {u : unit | not (remaining <= Iarray.length wire - used
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.literal_heap heap block used source first remaining)
           block (used + remaining))
      || Vox_lz4_spec_bytes.literal_bytes wire used source first remaining} @ ghost =
  fun wire heap block used source first remaining -> ghost_ (
    E.literal_heap_def heap block used source first remaining;
    Vox_lz4_spec_bytes.literal_bytes_def wire used source first remaining;
    if remaining > 0 && remaining <= Iarray.length wire - used
       && Vox_lz4_spec_bytes.prefix_matches wire
            (E.literal_heap heap block used source first remaining)
            block (used + remaining) then begin
      let index : {i : int | 0 <= i && i < Iarray.length wire} =
        used in
      literal_heap_at heap block used source first remaining 0;
      snapshot_at wire
        (E.literal_heap heap block used source first remaining)
        block (used + remaining) index;
      let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source first) in
      let next = H.put heap (M.location block used) (Some byte) in
      literal_heap_wire wire next block (used + 1) source (first + 1)
        (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (literal_heap_frame_prefix @ total) :
    (wire : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length source - first && n <= 4210768 - used}) ->
    (count : {n : int | 0 <= n && n <= used}) ->
    {u : unit | not (Vox_lz4_spec_bytes.prefix_matches wire
      (E.literal_heap heap block used source first remaining) block count)
      || Vox_lz4_spec_bytes.prefix_matches wire heap block count} @ ghost =
  fun wire heap block used source first remaining count -> ghost_ (
    Vox_lz4_spec_bytes.prefix_matches_def wire heap block count;
    Vox_lz4_spec_bytes.prefix_matches_def wire
      (E.literal_heap heap block used source first remaining) block count;
    if count > 0 then begin
      literal_heap_outside heap block used source first remaining (count - 1);
      literal_heap_frame_prefix wire heap block used source first remaining
        (count - 1)
    end;
    ())
[@@decreases count]

let rec (extension_heap_frame_prefix @ total) :
    (wire : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    (count : {n : int | 0 <= n && n <= used}) ->
    {u : unit | not (Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used
      && Vox_lz4_spec_bytes.prefix_matches wire
           (E.extension_heap heap block used remaining) block count)
      || Vox_lz4_spec_bytes.prefix_matches wire heap block count} @ ghost =
  fun wire heap block used remaining count -> ghost_ (
    Vox_lz4_spec_bytes.prefix_matches_def wire heap block count;
    Vox_lz4_spec_bytes.prefix_matches_def wire
      (E.extension_heap heap block used remaining) block count;
    if count > 0 && Vox_lz4_spec_bytes.extension_count remaining <= 4210768 - used then begin
      extension_heap_outside heap block used remaining (count - 1);
      extension_heap_frame_prefix wire heap block used remaining (count - 1)
    end;
    ())
[@@decreases count]

let rec (literal_heap_same @ total) :
    (heap : P.heap) -> (block : M.t) -> (used : int) ->
    (source : char iarray) -> (first : int) -> (remaining : int) ->
    {u : unit | Vox_lz4_spec_decode.literal_heap heap block used source first remaining ===
      E.literal_heap heap block used source first remaining} @ ghost =
  fun heap block used source first remaining -> ghost_ (
    Vox_lz4_spec_decode.literal_heap_def heap block used source first remaining;
    E.literal_heap_def heap block used source first remaining;
    if remaining > 0 && 0 <= first && first < Iarray.length source then begin
      let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source first) in
      let next = H.put heap (M.location block used) (Some byte) in
      literal_heap_same next block (used + 1) source (first + 1)
        (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (literal_heap_substitute @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4210768}) ->
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length wire - cursor
      && n <= Iarray.length source - first
      && n <= 4210768 - used}) ->
    {u : unit | not (Vox_lz4_spec_bytes.literal_bytes wire cursor source first remaining)
      || Vox_lz4_spec_decode.literal_heap heap block used wire cursor remaining ===
           Vox_lz4_spec_decode.literal_heap heap block used source first remaining} @ ghost =
  fun heap block used wire cursor source first remaining -> ghost_ (
    Vox_lz4_spec_decode.literal_heap_def heap block used wire cursor remaining;
    Vox_lz4_spec_decode.literal_heap_def heap block used source first remaining;
    Vox_lz4_spec_bytes.literal_bytes_def wire cursor source first remaining;
    if remaining = 0 || not (Vox_lz4_spec_bytes.literal_bytes wire cursor source first remaining)
    then let u = () in u
    else
      match Vox_lz4_spec_bytes.source_at wire cursor, Vox_lz4_spec_bytes.source_at source first with
      | Some c, Some s ->
        Vox_lz4_spec_bytes.source_at_def wire cursor;
        Vox_lz4_spec_bytes.source_at_def source first;
        let observed_byte = Vox_lz4_spec_decode.byte_of_char
                          (Vox_sequence.iarray_get wire cursor) in
        let next = H.put heap (M.location block used)
                     (Some observed_byte) in
        literal_heap_substitute next block (used + 1) wire (cursor + 1)
          source (first + 1) (remaining - 1);
        let u = () in u
      | _ ->
        let u = () in u)
[@@decreases remaining]

let[@def] (literal_token_of_source @ total)
    (source : char iarray @ immutable) : M.byte =
  let length = Iarray.length source in
  if length <= 4194304 then Vox_lz4_spec_bytes.literal_token length else 0

let[@def] (literal_extra_of_source @ total)
    (source : char iarray @ immutable) :
    {e : int | 0 <= e && e <= 16449} =
  let length = Iarray.length source in
  if length <= 4194304 then extra_count length else 0

let[@def] (literal_wire @ total) (source : char iarray @ immutable)
    (wire : char iarray @ immutable) = ghost_ (
  let length = Iarray.length source in
  length <= 4194304
  && Iarray.length wire = 1 + literal_extra_of_source source + length
  && (match Vox_lz4_spec_bytes.source_at wire 0 with
      | Some c -> Vox_lz4_spec_decode.byte_of_char c = literal_token_of_source source
      | None -> false)
  && (length < 15 || Vox_lz4_spec_bytes.extension_bytes wire 1 (length - 15))
  && Vox_lz4_spec_bytes.literal_bytes wire (1 + literal_extra_of_source source)
       source 0 length)

let (literal_layout @ total) :
    (source : char iarray) -> (wire : char iarray) -> (block : M.t) ->
    {u : unit |
      let model = E.literal_model source block in
      not (Iarray.length source <= 4194304
        && Iarray.length wire = model.E.literal_count
        && Vox_lz4_spec_bytes.prefix_matches wire model.E.literal_state block
             model.E.literal_count)
      || (match Vox_lz4_spec_bytes.source_at wire 0 with
          | Some c -> Vox_lz4_spec_decode.byte_of_char c = literal_token_of_source source
          | None -> false)
         && (Iarray.length source < 15
             || Vox_lz4_spec_bytes.extension_bytes wire 1 (Iarray.length source - 15))
         && Vox_lz4_spec_bytes.literal_bytes wire (1 + literal_extra_of_source source)
              source 0 (Iarray.length source)} @ ghost =
  fun source wire block -> ghost_ (
    let length = Iarray.length source in
    if length <= 4194304 then begin
      let extensions = extra_count length in
      extra_count_def length;
      literal_token_of_source_def source;
      literal_extra_of_source_def source;
      let bounded_extensions : {e : int | 0 <= e && e <= 16449} =
        extensions in
      let needed = E.literal_capacity length bounded_extensions in
      E.literal_model_def source block;
      let initial = M.footprint block in
      let after_token = H.put initial (M.location block 0)
                          (Some (Vox_lz4_spec_bytes.literal_token length)) in
      let after_extensions =
        if length >= 15 then E.extension_heap after_token block 1
                              (length - 15)
        else after_token in
      let final = E.literal_heap after_extensions block (1 + extensions)
                    source 0 length in
      let model = E.literal_model source block in
      let _ : {u : unit | model.E.literal_count = needed
        && model.E.literal_state === final} = () in
      if Iarray.length wire = needed
         && Vox_lz4_spec_bytes.prefix_matches wire final block needed then begin
        literal_heap_wire wire after_extensions block (1 + extensions)
          source 0 length;
        S.prefix_matches_prefix wire final block needed (1 + extensions);
        literal_heap_frame_prefix wire after_extensions block
          (1 + extensions) source 0 length (1 + extensions);
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_extensions block
          (1 + extensions)} = () in
        if length >= 15 then begin
          extension_heap_wire wire after_token block 1 (length - 15);
          S.prefix_matches_prefix wire after_extensions block
            (1 + extensions) 1;
          extension_heap_frame_prefix wire after_token block 1
            (length - 15) 1
        end;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_token block 1} =
          () in
        heap_put_at initial block 0 (Vox_lz4_spec_bytes.literal_token length);
        snapshot_at wire after_token block 1 0;
        let _ : {u : unit | match Vox_lz4_spec_bytes.source_at wire 0 with
          | Some c -> Vox_lz4_spec_decode.byte_of_char c = literal_token_of_source source
          | None -> false} = () in
        let _ : {u : unit | length < 15
          || Vox_lz4_spec_bytes.extension_bytes wire 1 (length - 15)} = () in
        let _ : {u : unit | Vox_lz4_spec_bytes.literal_bytes wire (1 + extensions)
          source 0 length} = () in
        ()
      end;
      let u = () in u
    end else
      let u = () in u)

let (literal_layout_wire @ total) :
    (source : char iarray) -> (wire : char iarray) -> (block : M.t) ->
    {u : unit |
      let model = E.literal_model source block in
      not (Iarray.length source <= 4194304
        && Iarray.length wire = model.E.literal_count
        && Vox_lz4_spec_bytes.prefix_matches wire model.E.literal_state block
             model.E.literal_count)
      || literal_wire source wire} @ ghost =
  fun source wire block -> ghost_ (
    literal_layout source wire block;
    E.literal_model_def source block;
    literal_wire_def source wire;
    literal_token_of_source_def source;
    literal_extra_of_source_def source;
    if Iarray.length source <= 4194304 then
      extra_count_def (Iarray.length source);
    ())

let rec (read_extension_bytes @ total) :
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (base : {n : int | 0 <= n && n <= 4194304}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304}) ->
    (fuel : {n : int | 0 <= n}) ->
    {u : unit | not (base + remaining <= 4194304
      && Vox_lz4_spec_bytes.extension_bytes wire cursor remaining
      && cursor + Vox_lz4_spec_bytes.extension_count remaining <= Iarray.length wire
      && Vox_lz4_spec_bytes.extension_count remaining <= fuel)
      || Vox_lz4_spec_decode.read_extended_length wire cursor base fuel ===
           D.Length (cursor + Vox_lz4_spec_bytes.extension_count remaining,
                     base + remaining)} @ ghost =
  fun wire cursor base remaining fuel -> ghost_ (
    Vox_lz4_spec_bytes.extension_count_def remaining;
    Vox_lz4_spec_bytes.extension_bytes_def wire cursor remaining;
    Vox_lz4_spec_decode.read_extended_length_def wire cursor base fuel;
    if base + remaining <= 4194304
       && Vox_lz4_spec_bytes.extension_bytes wire cursor remaining
       && cursor + Vox_lz4_spec_bytes.extension_count remaining <= Iarray.length wire
       && Vox_lz4_spec_bytes.extension_count remaining <= fuel then begin
      Vox_lz4_spec_bytes.source_at_def wire cursor;
      if remaining >= 255 then
        read_extension_bytes wire (cursor + 1) (base + 255)
          (remaining - 255) (fuel - 1)
    end;
    ())
[@@decreases remaining]

let (literal_token_fields @ total) :
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit |
      Vox_lz4_spec_decode.high4 (Vox_lz4_spec_bytes.literal_token length) =
        (if length >= 15 then 15 else length)
      && Vox_lz4_spec_decode.low15 (Vox_lz4_spec_bytes.literal_token length) = 0} @ ghost =
  fun length -> ghost_ (
    let token = Vox_lz4_spec_bytes.literal_token length in
    let high = Vox_lz4_spec_decode.high4 token in
    let low = Vox_lz4_spec_decode.low15 token in
    let _ = high, low in
    ())

let (match_token_fields @ total) :
    (code : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit |
      Vox_lz4_spec_decode.high4 (E.match_token code) = 1
      && Vox_lz4_spec_decode.low15 (E.match_token code) =
           (if code >= 15 then 15 else code)} @ ghost =
  fun code -> ghost_ (
    let token = E.match_token code in
    let high = Vox_lz4_spec_decode.high4 token in
    let low = Vox_lz4_spec_decode.low15 token in
    let _ = high, low in
    ())

let (read_short_literal_length @ total) :
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {u : unit | not (initial = Vox_lz4_spec_decode.high4 (Vox_lz4_spec_bytes.literal_token length)
      && length < 15
      && cursor + extra_count length <= Iarray.length wire)
      || Vox_lz4_spec_decode.read_length wire cursor initial ===
           D.Length (cursor + extra_count length, length)} @ ghost =
  fun wire cursor length initial -> ghost_ (
    literal_token_fields length;
    extra_count_def length;
    if length >= 15 && Vox_lz4_spec_bytes.extension_bytes wire cursor (length - 15)
       && cursor + extra_count length <= Iarray.length wire then
      read_extension_bytes wire cursor 15 (length - 15)
        (Iarray.length wire - cursor);
    Vox_lz4_spec_decode.read_length_def wire cursor initial;
    ())

let (read_long_literal_length @ total) :
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {u : unit | not (initial = 15 && length >= 15
      && Vox_lz4_spec_bytes.extension_bytes wire cursor (length - 15)
      && cursor + extra_count length <= Iarray.length wire
      && extra_count length <= Iarray.length wire - cursor)
      || Vox_lz4_spec_decode.read_length wire cursor initial ===
           D.Length (cursor + extra_count length, length)} @ ghost =
  fun wire cursor length initial -> ghost_ (
    extra_count_def length;
    if length >= 15 && Vox_lz4_spec_bytes.extension_bytes wire cursor (length - 15)
       && cursor + extra_count length <= Iarray.length wire
       && extra_count length <= Iarray.length wire - cursor then
      read_extension_bytes wire cursor 15 (length - 15)
        (Iarray.length wire - cursor);
    Vox_lz4_spec_decode.read_length_def wire cursor initial;
    ())

let (read_literal_length @ total) :
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {u : unit | not (initial = Vox_lz4_spec_decode.high4 (Vox_lz4_spec_bytes.literal_token length)
      && (length < 15 || Vox_lz4_spec_bytes.extension_bytes wire cursor (length - 15))
      && cursor + extra_count length <= Iarray.length wire
      && extra_count length <= Iarray.length wire - cursor)
      || Vox_lz4_spec_decode.read_length wire cursor initial ===
           D.Length (cursor + extra_count length, length)} @ ghost =
  fun wire cursor length initial -> ghost_ (
    literal_token_fields length;
    if length < 15 then
      read_short_literal_length wire cursor length initial
    else
      read_long_literal_length wire cursor length initial;
    ())

let (read_match_length @ total) :
    (wire : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length wire}) ->
    (code : {n : int | 0 <= n && n <= 4194304}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {u : unit | not (initial = Vox_lz4_spec_decode.low15 (E.match_token code)
      && (code < 15 || Vox_lz4_spec_bytes.extension_bytes wire cursor (code - 15))
      && cursor + extra_count code <= Iarray.length wire
      && extra_count code <= Iarray.length wire - cursor)
      || Vox_lz4_spec_decode.read_length wire cursor initial ===
           D.Length (cursor + extra_count code, code)} @ ghost =
  fun wire cursor code initial -> ghost_ (
    match_token_fields code;
    if code < 15 then begin
      extra_count_def code;
      Vox_lz4_spec_decode.read_length_def wire cursor initial
    end else
      read_long_literal_length wire cursor code initial;
    ())

let (decode_literal_wire @ total) :
    (source : char iarray) -> (wire : char iarray) -> (block : M.t) ->
    {u : unit | not (literal_wire source wire)
      || let result =
           Vox_lz4_spec_decode.decode_model wire 0 (-1) (Iarray.length wire)
             (Iarray.length source) block 0 (M.footprint block) in
         result.D.kind === D.Done
         && result.D.count = Iarray.length source
         && result.D.state ===
              Vox_lz4_spec_decode.literal_heap (M.footprint block) block 0 source 0
                (Iarray.length source)} @ ghost =
  fun source wire block -> ghost_ (
    literal_wire_def source wire;
    if literal_wire source wire then begin
      let length = Iarray.length source in
      let extra = literal_extra_of_source source in
      literal_extra_of_source_def source;
      literal_token_of_source_def source;
      let token = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get wire 0) in
      Vox_lz4_spec_bytes.source_at_def wire 0;
      literal_token_fields length;
      let initial = Vox_lz4_spec_decode.high4 token in
      read_literal_length wire 1 length initial;
      literal_heap_substitute (M.footprint block) block 0 wire (1 + extra)
        source 0 length;
      Vox_lz4_spec_decode.decode_model_def wire 0 (-1) (Iarray.length wire) length block 0
        (M.footprint block)
    end;
    ())

let rec (copy_constant_to_source @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 1 <= n && n <= 4194304}) ->
    (source : char iarray) -> (first_char : char) ->
    (run_end : {r : int | 1 <= r && r <= Iarray.length source}) ->
    (source_index : {i : int | 0 <= i && i <= run_end}) ->
    (remaining : {n : int | 0 <= n && n <= run_end - source_index
      && n <= 4194304 - used}) ->
    {u : unit | not (E.same_prefix source first_char run_end
      && H.at heap (M.location block (used - 1)) ===
           Some (Some (Vox_lz4_spec_decode.byte_of_char first_char)))
      || Vox_lz4_spec_decode.copy_heap heap block used 1 remaining ===
           Vox_lz4_spec_decode.literal_heap heap block used source source_index remaining}
      @ ghost =
  fun heap block used source first_char run_end source_index remaining ->
    ghost_ (
      Vox_lz4_spec_decode.copy_heap_def heap block used 1 remaining;
      Vox_lz4_spec_decode.literal_heap_def heap block used source source_index remaining;
      if remaining > 0 && E.same_prefix source first_char run_end
         && H.at heap (M.location block (used - 1)) ===
              Some (Some (Vox_lz4_spec_decode.byte_of_char first_char)) then begin
        E.same_prefix_at source first_char run_end source_index;
        Vox_lz4_spec_bytes.source_at_def source source_index;
        let byte = Vox_lz4_spec_decode.byte_of_char first_char in
        let next = H.put heap (M.location block used) (Some byte) in
        heap_put_at heap block used byte;
        copy_constant_to_source next block (used + 1) source first_char
          run_end (source_index + 1) (remaining - 1)
      end;
      ())
[@@decreases remaining]

let (wire_byte_get @ total) :
    (wire : char iarray) ->
    (position : {i : int | 0 <= i && i < Iarray.length wire}) ->
    (value : M.byte) ->
    {u : unit | not (Vox_lz4_spec_bytes.wire_byte wire position value)
      || Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get wire position) = value}
      @ ghost =
  fun wire position value -> ghost_ (
    Vox_lz4_spec_bytes.wire_byte_def wire position value;
    Vox_lz4_spec_bytes.source_at_def wire position;
    ())

let[@def] (run_wire @ total) (source : char iarray @ immutable)
    (wire : char iarray @ immutable) (run_end : int)
    (code : {n : int | 0 <= n && n <= 4194304})
    (suffix : {n : int | 0 <= n && n <= 4194304}) = ghost_ (
  let match_extra = extra_count code in
  let literal_extra = extra_count suffix in
  13 <= Iarray.length source && Iarray.length source <= 4194304
  && 5 <= run_end && run_end <= Iarray.length source - 5
  && code = run_end - 5 && suffix = Iarray.length source - run_end
  && (match Vox_lz4_spec_bytes.source_at source 0 with
      | Some first -> E.same_prefix source first run_end
      | None -> false)
  && Iarray.length wire = 5 + suffix + match_extra + literal_extra
  && Vox_lz4_spec_bytes.wire_byte wire 0 (E.match_token code)
  && Vox_lz4_spec_bytes.literal_bytes wire 1 source 0 1
  && Vox_lz4_spec_bytes.wire_byte wire 2 1 && Vox_lz4_spec_bytes.wire_byte wire 3 0
  && (code < 15 || Vox_lz4_spec_bytes.extension_bytes wire 4 (code - 15))
  && Vox_lz4_spec_bytes.wire_byte wire (4 + match_extra) (Vox_lz4_spec_bytes.literal_token suffix)
  && (suffix < 15
      || Vox_lz4_spec_bytes.extension_bytes wire (5 + match_extra) (suffix - 15))
  && Vox_lz4_spec_bytes.literal_bytes wire (5 + match_extra + literal_extra)
       source run_end suffix)

let rec (literal_heap_append @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4194304}) ->
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (left : {n : int | 0 <= n && n <= Iarray.length source - first}) ->
    (right : {n : int | 0 <= n && n <= Iarray.length source - first - left
      && n <= 4194304 - used - left}) ->
    {u : unit |
      Vox_lz4_spec_decode.literal_heap
        (Vox_lz4_spec_decode.literal_heap heap block used source first left)
        block (used + left) source (first + left) right ===
      Vox_lz4_spec_decode.literal_heap heap block used source first (left + right)} @ ghost =
  fun heap block used source first left right -> ghost_ (
    Vox_lz4_spec_decode.literal_heap_def heap block used source first left;
    Vox_lz4_spec_decode.literal_heap_def heap block used source first (left + right);
    if left > 0 then begin
      let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source first) in
      let next = H.put heap (M.location block used) (Some byte) in
      literal_heap_append next block (used + 1) source (first + 1)
        (left - 1) right
    end;
    ())
[@@decreases left]

let (decode_run_wire @ total) :
    (source : char iarray) -> (wire : char iarray) -> (block : M.t) ->
    (run_end : int) ->
    (code : {n : int | 0 <= n && n <= 4194304}) ->
    (suffix : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit | not (run_wire source wire run_end code suffix)
      || let result =
           Vox_lz4_spec_decode.decode_model wire 0 (-1) (Iarray.length wire)
             (Iarray.length source) block 0 (M.footprint block) in
         result.D.kind === D.Done
         && result.D.count = Iarray.length source
         && result.D.state ===
              Vox_lz4_spec_decode.literal_heap (M.footprint block) block 0 source 0
                (Iarray.length source)} @ ghost =
  fun source wire block run_end code suffix -> ghost_ (
    run_wire_def source wire run_end code suffix;
    if run_wire source wire run_end code suffix then begin
      let match_extra = extra_count code in
      let literal_extra = extra_count suffix in
      let length = Iarray.length source in
      let compressed = Iarray.length wire in
      let first = Vox_sequence.iarray_get source 0 in
      Vox_lz4_spec_bytes.source_at_def source 0;
      let first_token = Vox_lz4_spec_decode.byte_of_char
                          (Vox_sequence.iarray_get wire 0) in
      wire_byte_get wire 0 (E.match_token code);
      match_token_fields code;
      let first_nibble : {n : int | 0 <= n && n <= 15} =
        Vox_lz4_spec_decode.high4 first_token in
      let cursor1 : {i : int | 0 <= i && i <= Iarray.length wire} =
        1 in
      Vox_lz4_spec_decode.read_length_def wire cursor1 first_nibble;
      let _ : {u : unit | Vox_lz4_spec_decode.read_length wire cursor1 first_nibble ===
        D.Length (1, 1)} = () in
      wire_byte_get wire 2 1;
      wire_byte_get wire 3 0;
      let match_nibble : {n : int | 0 <= n && n <= 15} =
        Vox_lz4_spec_decode.low15 first_token in
      let cursor4 : {i : int | 0 <= i && i <= Iarray.length wire} =
        4 in
      read_match_length wire cursor4 code match_nibble;
      let _ : {u : unit | Vox_lz4_spec_decode.read_length wire cursor4 match_nibble ===
        D.Length (4 + match_extra, code)} = () in
      let final_token_pos = 4 + match_extra in
      let final_token = Vox_lz4_spec_decode.byte_of_char
                          (Vox_sequence.iarray_get wire final_token_pos) in
      wire_byte_get wire final_token_pos (Vox_lz4_spec_bytes.literal_token suffix);
      literal_token_fields suffix;
      let final_nibble : {n : int | 0 <= n && n <= 15} =
        Vox_lz4_spec_decode.high4 final_token in
      let final_cursor : {i : int | 0 <= i && i <= Iarray.length wire} =
        (final_token_pos + 1) in
      read_literal_length wire final_cursor suffix final_nibble;
      let _ : {u : unit | Vox_lz4_spec_decode.read_length wire final_cursor
        final_nibble === D.Length (5 + match_extra + literal_extra,
                                   suffix)} = () in
      let initial = M.footprint block in
      let after_first = Vox_lz4_spec_decode.literal_heap initial block 0 wire 1 1 in
      literal_heap_substitute initial block 0 wire 1 source 0 1;
      let _ : {u : unit | after_first ===
        Vox_lz4_spec_decode.literal_heap initial block 0 source 0 1} = () in
      Vox_lz4_spec_decode.literal_heap_def initial block 0 source 0 1;
      let after_put = H.put initial (M.location block 0)
                        (Some (Vox_lz4_spec_decode.byte_of_char first)) in
      Vox_lz4_spec_decode.literal_heap_def after_put block 1 source 1 0;
      heap_put_at initial block 0 (Vox_lz4_spec_decode.byte_of_char first);
      let _ : {u : unit | H.at after_first (M.location block 0) ===
        Some (Some (Vox_lz4_spec_decode.byte_of_char first))} = () in
      copy_constant_to_source after_first block 1 source first run_end 1
        (run_end - 1);
      let after_run = Vox_lz4_spec_decode.copy_heap after_first block 1 1 (run_end - 1) in
      let _ : {u : unit | after_run ===
        Vox_lz4_spec_decode.literal_heap after_first block 1 source 1 (run_end - 1)} =
        () in
      let final_literal_pos = 5 + match_extra + literal_extra in
      literal_heap_substitute after_run block run_end wire final_literal_pos
        source run_end suffix;
      literal_heap_append initial block 0 source 0 1 (run_end - 1);
      literal_heap_append initial block 0 source 0 run_end suffix;
      let _ : {u : unit | Vox_lz4_spec_decode.literal_heap after_run block run_end wire
        final_literal_pos suffix ===
        Vox_lz4_spec_decode.literal_heap initial block 0 source 0 length} = () in
      Vox_lz4_spec_decode.decode_model_def wire final_token_pos 1 (compressed - 1) length
        block run_end after_run;
      Vox_lz4_spec_decode.decode_model_def wire 0 (-1) compressed length block 0 initial
    end;
    ())

let (run_layout @ total) :
    (source : char iarray) -> (wire : char iarray) -> (block : M.t) ->
    (run_end : int) ->
    (code : {n : int | 0 <= n && n <= 4194304}) ->
    (suffix : {n : int | 0 <= n && n <= 4194304}) ->
    {u : unit |
      let model = E.encode_model source block in
      not (E.plan_of_source source === E.Initial_run run_end
        && code = run_end - 5
        && suffix = Iarray.length source - run_end
        && Iarray.length wire = model.E.count
        && Vox_lz4_spec_bytes.prefix_matches wire model.E.state block model.E.count)
      || run_wire source wire run_end code suffix} @ ghost =
  fun source wire block run_end code suffix -> ghost_ (
    E.encode_model_def source block;
    run_wire_def source wire run_end code suffix;
    if E.plan_of_source source === E.Initial_run run_end
       && code = run_end - 5
       && suffix = Iarray.length source - run_end then begin
      let match_extra = extra_count code in
      let literal_extra = extra_count suffix in
      extra_count_def code;
      extra_count_def suffix;
      let bounded_match : {e : int | 0 <= e
        && 255 * e <= code + 255} = match_extra in
      let bounded_literal : {e : int | 0 <= e
        && e <= 16450 - match_extra} = literal_extra in
      let bounded_suffix : {n : int | 5 <= n && n <= 4194299} =
        suffix in
      let needed = E.match_capacity code bounded_suffix bounded_match
                     bounded_literal in
      let initial = M.footprint block in
      let after_token = H.put initial (M.location block 0)
                          (Some (E.match_token code)) in
      let after_literal = E.literal_heap after_token block 1 source 0 1 in
      let after_offset_low = H.put after_literal (M.location block 2)
                               (Some 1) in
      let after_offset = H.put after_offset_low (M.location block 3)
                           (Some 0) in
      let after_match_extensions =
        if code >= 15 then E.extension_heap after_offset block 4
                             (code - 15)
        else after_offset in
      let final_token_pos = 4 + match_extra in
      let after_final_token =
        H.put after_match_extensions (M.location block final_token_pos)
          (Some (Vox_lz4_spec_bytes.literal_token suffix)) in
      let after_literal_extensions =
        if suffix >= 15 then
          E.extension_heap after_final_token block (final_token_pos + 1)
            (suffix - 15)
        else after_final_token in
      let final_literal_pos = 5 + match_extra + literal_extra in
      let final = E.literal_heap after_literal_extensions block
                    final_literal_pos source run_end suffix in
      let model = E.encode_model source block in
      let _ : {u : unit | model.E.count = needed
        && model.E.state === final} = () in
      if Iarray.length wire = needed
         && Vox_lz4_spec_bytes.prefix_matches wire final block needed then begin
        literal_heap_wire wire after_literal_extensions block
          final_literal_pos source run_end suffix;
        S.prefix_matches_prefix wire final block needed final_literal_pos;
        literal_heap_frame_prefix wire after_literal_extensions block
          final_literal_pos source run_end suffix final_literal_pos;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire
          after_literal_extensions block final_literal_pos} = () in
        if suffix >= 15 then begin
          extension_heap_wire wire after_final_token block
            (final_token_pos + 1) (suffix - 15);
          S.prefix_matches_prefix wire after_literal_extensions block
            final_literal_pos (final_token_pos + 1);
          extension_heap_frame_prefix wire after_final_token block
            (final_token_pos + 1) (suffix - 15) (final_token_pos + 1)
        end;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_final_token block
          (final_token_pos + 1)} = () in
        heap_put_at after_match_extensions block final_token_pos
          (Vox_lz4_spec_bytes.literal_token suffix);
        snapshot_at wire after_final_token block (final_token_pos + 1)
          final_token_pos;
        S.prefix_matches_prefix wire after_final_token block
          (final_token_pos + 1) final_token_pos;
        prefix_matches_put_outside wire after_match_extensions block
          final_token_pos final_token_pos (Vox_lz4_spec_bytes.literal_token suffix);
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_match_extensions
          block final_token_pos} = () in
        if code >= 15 then begin
          extension_heap_wire wire after_offset block 4 (code - 15);
          S.prefix_matches_prefix wire after_match_extensions block
            final_token_pos 4;
          extension_heap_frame_prefix wire after_offset block 4
            (code - 15) 4
        end;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_offset block 4} =
          () in
        heap_put_at after_offset_low block 3 0;
        snapshot_at wire after_offset block 4 3;
        S.prefix_matches_prefix wire after_offset block 4 3;
        prefix_matches_put_outside wire after_offset_low block 3 3 0;
        heap_put_at after_literal block 2 1;
        snapshot_at wire after_offset_low block 3 2;
        S.prefix_matches_prefix wire after_offset_low block 3 2;
        prefix_matches_put_outside wire after_literal block 2 2 1;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_literal block 2} =
          () in
        literal_heap_wire wire after_token block 1 source 0 1;
        S.prefix_matches_prefix wire after_literal block 2 1;
        literal_heap_frame_prefix wire after_token block 1 source 0 1 1;
        let _ : {u : unit | Vox_lz4_spec_bytes.prefix_matches wire after_token block 1} =
          () in
        heap_put_at initial block 0 (E.match_token code);
        snapshot_at wire after_token block 1 0;
        Vox_lz4_spec_bytes.wire_byte_def wire 0 (E.match_token code);
        let _ : {u : unit | Vox_lz4_spec_bytes.wire_byte wire 0 (E.match_token code)} =
          () in
        let _ : {u : unit | Vox_lz4_spec_bytes.literal_bytes wire 1 source 0 1} =
          () in
        let one : M.byte = 1 in
        let zero : M.byte = 0 in
        Vox_lz4_spec_bytes.wire_byte_def wire 2 one;
        Vox_lz4_spec_bytes.wire_byte_def wire 3 zero;
        let _ : {u : unit | Vox_lz4_spec_bytes.wire_byte wire 2 one
          && Vox_lz4_spec_bytes.wire_byte wire 3 zero} = () in
        let _ : {u : unit | code < 15
          || Vox_lz4_spec_bytes.extension_bytes wire 4 (code - 15)} = () in
        Vox_lz4_spec_bytes.wire_byte_def wire final_token_pos (Vox_lz4_spec_bytes.literal_token suffix);
        let _ : {u : unit | Vox_lz4_spec_bytes.wire_byte wire final_token_pos
          (Vox_lz4_spec_bytes.literal_token suffix)} = () in
        let _ : {u : unit | suffix < 15
          || Vox_lz4_spec_bytes.extension_bytes wire (final_token_pos + 1) (suffix - 15)} =
          () in
        let _ : {u : unit | Vox_lz4_spec_bytes.literal_bytes wire final_literal_pos
          source run_end suffix} = () in
        ()
      end
    end;
    ())

let[@def] rec (output_matches @ total) (heap : P.heap @ immutable)
    (block : M.t @ immutable) (source : char iarray @ immutable)
    (count : int) = ghost_ (
  if count <= 0 then true
  else
    (match Vox_lz4_spec_bytes.source_at source (count - 1) with
     | Some c -> H.at heap (M.location block (count - 1)) ===
                   Some (Some (Vox_lz4_spec_decode.byte_of_char c))
     | None -> false)
    && output_matches heap block source (count - 1))
[@@decreases count]

let rec (literal_heap_matches_source @ total) :
    (heap : P.heap) -> (block : M.t) -> (source : char iarray) ->
    (full_length : {n : int | 0 <= n && n <= 4194304
      && n <= Iarray.length source}) ->
    (count : {n : int | 0 <= n && n <= full_length}) ->
    {u : unit | output_matches
      (Vox_lz4_spec_decode.literal_heap heap block 0 source 0 full_length)
      block source count} @ ghost =
  fun heap block source full_length count -> ghost_ (
    let final = Vox_lz4_spec_decode.literal_heap heap block 0 source 0 full_length in
    output_matches_def final block source count;
    if count > 0 then begin
      literal_heap_same heap block 0 source 0 full_length;
      literal_heap_at heap block 0 source 0 full_length (count - 1);
      literal_heap_matches_source heap block source full_length (count - 1)
    end;
    ())
[@@decreases count]

let roundtrip : (source : char iarray) ->
    {r : (D.status * DB.t) option | match r with
      | None -> true
      | Some (status, decoded) ->
        Iarray.length source <= 4194304
        && status === D.Done
        && M.length decoded.block = Iarray.length source
        && decoded.used = Iarray.length source
        && P.own decoded.permission ===
             Vox_lz4_spec_decode.literal_heap (M.footprint decoded.block) decoded.block 0
               source 0 (Iarray.length source)
        && output_matches (P.own decoded.permission) decoded.block source
             decoded.used} @ unique =
  fun source ->
    match E.encode source with
    | None -> None
    | Some encoded ->
      let snapshot = S.snapshot_prefix encoded in
      let { S.values = wire; buffer = encoded } = snapshot in
      let { EB.block = encoded_block;
            permission = encoded_permission; used = encoded_used } = encoded in
      ghost_ (E.encode_model_def source encoded_block);
      let capacity = Iarray.length source in
      match D.decode wire capacity with
      | None ->
        EB.release { EB.block = encoded_block;
                     permission = encoded_permission; used = encoded_used };
        None
      | Some (status, decoded) ->
        let { DB.block = decoded_block;
              permission = decoded_permission; used = decoded_used } = decoded in
        ghost_ (
          (match E.plan_of_source source with
          | E.Literal_only ->
            literal_layout_wire source wire encoded_block;
            decode_literal_wire source wire decoded_block
          | E.Initial_run run_end ->
            let code = run_end - 5 in
            let suffix = capacity - run_end in
            run_layout source wire encoded_block run_end code suffix;
            decode_run_wire source wire decoded_block run_end code suffix);
          literal_heap_matches_source (M.footprint decoded_block)
            decoded_block source capacity capacity);
        EB.release { EB.block = encoded_block;
                     permission = encoded_permission; used = encoded_used };
        Some (status,
              { DB.block = decoded_block;
                permission = decoded_permission; used = decoded_used })
