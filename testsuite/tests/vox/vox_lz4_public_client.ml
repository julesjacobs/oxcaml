(* This client is compiled with only public semantic CMIs; see
   verification/benchmarks/lz4_boundary_check.py. *)
module C = Vox_lz4
module V = Vox_string_view

let roundtrip : (source : {s : string | Iarray.length (V.contents s) <= 4194304}) ->
    {output : string | V.contents output === V.contents source} =
  fun source ->
    let wire = C.compress source in
    let capacity = V.length source in
    let decoded = C.decompress_verified wire capacity in
    ghost_ (C.roundtrip source wire capacity decoded);
    match decoded with
    | Ok output -> output
    | Error _ -> assert false

let () =
  let source = "let square x = x * x\nlet square x = x * x\n" in
  if V.length source <= 4194304 then assert (roundtrip source = source)
  else assert false
