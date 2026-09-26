module B = Vox_lz4_encode_buffer
module A = Borrow_iarray
module V = Vox_iarray
module M = Raw_memory
module P = Ghost_pref
module H = P.Heap

external char_of_byte : (value : M.byte) ->
  {c : char | Vox_lz4_spec_parse.byte_of_char c = value}
  @@ total = "%identity"

include Vox_lz4_spec_bytes
include Vox_lz4_heap_bytes

let rec (prefix_matches_get @ total) :
    (values : char iarray) -> (heap : M.contents P.heap) -> (block : M.t) ->
    (count : int) -> (index : int) ->
    {u : unit | not (Vox_lz4_heap_bytes.prefix_matches values heap block count
      && 0 <= index && index < count)
      || match V.at values index, H.at heap (M.location block index) with
         | Some c, Some (Some byte) -> Vox_lz4_spec_parse.byte_of_char c = byte
         | _ -> false} @ ghost =
  fun values heap block count index -> ghost_ (
    Vox_lz4_heap_bytes.prefix_matches_def values heap block count;
    if count > 0 && 0 <= index && index < count - 1 then
      prefix_matches_get values heap block (count - 1) index;
    ())
[@@decreases count]

let rec (prefix_matches_prefix @ total) :
    (values : char iarray) -> (heap : M.contents P.heap) -> (block : M.t) ->
    (larger : int) -> (smaller : int) ->
    {u : unit | not (0 <= smaller && smaller <= larger
      && Vox_lz4_heap_bytes.prefix_matches values heap block larger)
      || Vox_lz4_heap_bytes.prefix_matches values heap block smaller} @ ghost =
  fun values heap block larger smaller -> ghost_ (
    Vox_lz4_heap_bytes.prefix_matches_def values heap block larger;
    if 0 <= smaller && smaller < larger then
      prefix_matches_prefix values heap block (larger - 1) smaller;
    ())
[@@decreases larger]

let rec (prefix_set_outside @ total) :
    (values : char iarray) -> (heap : M.contents P.heap) -> (block : M.t) ->
    (count : int) -> (index : int) -> (value : char) ->
    {u : unit | not (0 <= count && count <= index
      && index < Iarray.length values && Vox_lz4_heap_bytes.prefix_matches values heap block count)
      || Vox_lz4_heap_bytes.prefix_matches (V.updated values index value) heap block count}
      @ ghost =
  fun values heap block count index value -> ghost_ (
    Vox_lz4_heap_bytes.prefix_matches_def values heap block count;
    Vox_lz4_heap_bytes.prefix_matches_def (V.updated values index value) heap block count;
    if count > 0 && count <= index && index < Iarray.length values then begin
      V.updated_read values index value (count - 1);
      prefix_set_outside values heap block (count - 1) index value
    end;
    ())
[@@decreases count]

let (prefix_set_next @ total) :
    (values : char iarray) -> (heap : M.contents P.heap) -> (block : M.t) ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    (value : char) ->
    {u : unit | not (Vox_lz4_heap_bytes.prefix_matches values heap block index
      && H.at heap (M.location block index) ===
           Some (Some (Vox_lz4_spec_parse.byte_of_char value)))
      || Vox_lz4_heap_bytes.prefix_matches (V.updated values index value) heap block (index + 1)}
      @ ghost =
  fun values heap block index value -> ghost_ (
    prefix_set_outside values heap block index index value;
    V.updated_read values index value index;
    Vox_lz4_heap_bytes.prefix_matches_def (V.updated values index value) heap block (index + 1);
    ())

let rec fill :
    (block : M.t) ->
    (used : {n : int | 0 <= n && n <= M.length block}) ->
    (permission : {p : M.contents P.token |
      Vox_lz4_spec_storage.initialized (P.own p) block used
      && M.covers (P.own p) block 0 (M.length block)}) @ local read ghost ->
    (index : {i : int | 0 <= i && i <= used}) ->
    (slice : {s : char A.Slice.t |
      Iarray.length (A.Slice.current s) = used
      && Vox_lz4_heap_bytes.prefix_matches (A.Slice.current s) (P.own permission) block index})
      @ local unique ->
    {u : unit |
      Vox_lz4_heap_bytes.prefix_matches (A.Slice.final slice) (P.own permission) block used}
      @ local =
  fun block used permission index slice -> exclave_ (
    if index = used then begin
      A.Slice.finish slice;
      ()
    end else begin
      ghost_ (B.initialized_get (P.own permission) block used index);
      ghost_ (M.covers_get (P.own permission) block 0 (M.length block) index);
      let byte = M.read block index permission in
      let value = char_of_byte byte in
      let before = ghost_ (A.Slice.current (borrow_ slice)) in
      let slice = A.Slice.set slice index value in
      ghost_ (prefix_set_next before (P.own permission) block index value);
      fill block used permission (index + 1) slice
    end)
[@@decreases used - index]

type result : value mod portable contended = {
  values : char iarray @@ aliased;
  buffer : B.t;
}

let snapshot_prefix : (buffer : B.t) @ unique ->
    {r : result |
      Iarray.length r.values = buffer.used
      && r.buffer.block === buffer.block
      && r.buffer.used = buffer.used
      && P.own r.buffer.permission === P.own buffer.permission
      && Vox_lz4_heap_bytes.prefix_matches r.values (P.own r.buffer.permission)
           r.buffer.block r.buffer.used} @ unique =
  fun buffer ->
    let { B.block; permission; used } = buffer in
    let initial = Iarray.init used (fun _ -> '\000') in
    let owned = A.Owned_array.of_iarray initial in
    let heap = ghost_ (P.own (borrow_ permission)) in
    let post = ghost_ (fun (_ : unit @ immutable total)
        (after : char iarray @ immutable) ->
          Vox_lz4_heap_bytes.prefix_matches after heap block used) in
    let step = A.Owned_array.with_mut owned post (fun slice ->
      ghost_ (Vox_lz4_heap_bytes.prefix_matches_def initial heap block 0);
      let _ = fill block used (borrow_ permission) 0 slice in
      ()) in
    let values = A.Owned_array.into_iarray step.state in
    { values; buffer = { B.block; permission; used } }
