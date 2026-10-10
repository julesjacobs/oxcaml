module M = Raw_memory
module G = Ghost_pref
module V = Vox_string_view

external copy_prefix :
    (block : M.t) @ immutable ->
    (count : {n : int | 0 <= n && n <= M.length block}) ->
    (permission : {p : M.contents G.token |
      Vox_lz4_spec_storage.initialized (G.own p) block count
      && M.covers (G.own p) block 0 (M.length block)}) @ local read ghost ->
    {s : string | Iarray.length (V.contents s) = count
      && Vox_lz4_heap_bytes.prefix_matches (V.contents s) (G.own permission) block count}
    = "caml_raw_memory_copy_string_bytecode" "caml_raw_memory_copy_string"
