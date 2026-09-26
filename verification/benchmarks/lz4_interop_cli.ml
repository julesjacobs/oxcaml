let () =
  let input = In_channel.input_all stdin in
  let output =
    match Sys.argv.(1) with
    | "encode" -> Vox_lz4.compress input
    | "decode" ->
      (match Vox_lz4.decompress input with
       | Ok output -> output
       | Error _ -> failwith "Vox rejected the LZ4 block")
    | _ -> invalid_arg "expected encode or decode"
  in
  output_string stdout output
