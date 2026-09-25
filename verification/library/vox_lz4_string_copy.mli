(** Copy initialized raw bytes into a new GC-managed string. The source
    allocation and its permission remain owned by the caller. This contract
    is implemented by allocation followed by memcpy in runtime/pref.c. *)
module M = Raw_memory
module G = Ghost_pref
module B = Vox_lz4_encode_buffer
module S = Vox_lz4_snapshot
module V = Vox_string_view

external copy_prefix :
    (block : M.t) @ immutable ->
    (count : {n : int | 0 <= n && n <= M.length block}) ->
    (permission : {p : G.token |
      B.initialized (G.own p) block count
      && M.covers (G.own p) block 0 (M.length block)}) @ local read ghost ->
    {s : string | Iarray.length (V.contents s) = count
      && S.prefix_matches (V.contents s) (G.own permission) block count}
    = "caml_raw_memory_copy_string_bytecode" "caml_raw_memory_copy_string"
