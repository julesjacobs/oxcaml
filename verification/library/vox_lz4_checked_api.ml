module M = Vox_lz4_general_match
module P = Vox_lz4_general_plan
module Z = Vox_lz4_general_sized
module C = Vox_lz4_general_cost
module F = Vox_lz4_general_bridge
module W = Vox_lz4_general_wire
module S = Vox_lz4_snapshot
module EB = Vox_lz4_encode_buffer
module D = Vox_lz4_packed
module DB = Vox_lz4_buffer
module Raw = Raw_memory

let compress source =
  let length = String.length source in
  if length > 4194304 then invalid_arg "Vox_lz4.compress: block too large";
  let input = Iarray.init length (String.get source) in
  let table = Array.make 65536 (-1) in
  let hints = Iarray.init length (fun position ->
    if position < 0 || position > length - 4 then -1
    else
      let hash = M.hash4 input position in
      let previous = table.(hash) in
      table.(hash) <- position;
      previous) in
  if Iarray.length input <= 4194304
     && Iarray.length hints = Iarray.length input then
    let plan = P.from_hints input hints in
    match Z.encode input plan with
    | None -> raise Out_of_memory
    | Some buffer ->
      let snapshot = S.snapshot_prefix buffer in
      let { S.values = wire; buffer = encoded } = snapshot in
      let { EB.block; permission; used } = encoded in
      ghost_ (
        C.encoded_size_capacity input 0 plan;
        C.encode_model_size input 0 plan block 0 (Raw.footprint block);
        F.model_wire input wire 0 plan block 0 (Raw.footprint block);
        let _ : {u : unit | W.wire_matches_plan input wire 0 0 plan} =
          refine_ () in
        ());
      let output = String.init (Iarray.length wire) (Iarray.get wire) in
      EB.release { EB.block; permission; used };
      output
  else failwith "Vox_lz4.compress: invalid input length"

let rec fill_output (buffer : DB.t) (index : {i : int | 0 <= i}) output =
  if index >= DB.used (borrow_ buffer) then DB.release buffer
  else
    let byte, buffer = DB.get buffer index in
    Bytes.set output index (Char.chr byte);
    fill_output buffer (index + 1) output

let decode source capacity =
  let input = Iarray.init (String.length source) (String.get source) in
  match D.decode input capacity with
  | None -> raise Out_of_memory
  | Some (status, buffer) ->
    if status = D.Done then begin
      let output = Bytes.create (DB.used (borrow_ buffer)) in
      fill_output buffer 0 output;
      Some (Bytes.unsafe_to_string output)
    end else begin
      DB.release buffer;
      None
    end
