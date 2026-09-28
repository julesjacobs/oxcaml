let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let measure name iterations source compress =
  ignore (Sys.opaque_identity (compress source));
  Gc.full_major ();
  let started = Sys.time () in
  let checksum = ref 0 in
  for _ = 1 to iterations do
    let compressed = Sys.opaque_identity (compress source) in
    checksum := !checksum + String.length compressed
  done;
  let elapsed = Sys.time () -. started in
  let bytes = float_of_int (String.length source) *. float_of_int iterations in
  Printf.printf "%s: %.1f MB/s, %.6f CPU seconds/block (checksum %d)\n"
    name (bytes /. elapsed /. 1e6) (elapsed /. float_of_int iterations) !checksum

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then
    failwith "usage: lz4_throughput SOURCE_FILE [ITERATIONS]";
  let iterations =
    if Array.length Sys.argv = 3 then int_of_string Sys.argv.(2) else 1000 in
  if iterations <= 0 then invalid_arg "iterations must be positive";
  let source = read_file Sys.argv.(1) in
  let fast = Vox_lz4_baseline.compress source in
  let verified = Vox_lz4.compress source in
  assert (fast = verified);
  assert (Vox_lz4.decompress ~capacity:(String.length source) verified = Ok source);
  Printf.printf "%d input bytes -> %d compressed bytes\n"
    (String.length source) (String.length verified);
  measure "baseline codec" iterations source Vox_lz4_baseline.compress;
  measure "verified public codec" iterations source
    (fun source -> Vox_lz4.compress source);
  let decode wire =
    match Vox_lz4.decompress wire with
    | Ok output -> output
    | Error _ -> failwith "decode failed" in
  assert (decode verified = source);
  let started = Sys.time () in
  for _ = 1 to iterations do ignore (Sys.opaque_identity (decode verified)) done;
  let elapsed = Sys.time () -. started in
  Printf.printf "verified decoder: %.1f MB/s (decoded bytes)\n"
    (float_of_int (String.length source * iterations) /. elapsed /. 1e6)
