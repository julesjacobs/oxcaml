(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_control.mli vox_control.ml control_group.ml";
 { bytecode; }
*)
let scalar bytes offset target =
  let mask = ref 0 in
  for i = 0 to 15 do
    if Char.code (Bytes.get bytes (offset + i)) = target then
      mask := !mask lor (1 lsl i)
  done;
  !mask

external match16_empty : bytes -> int -> int -> int
  = "caml_vox_control_match16_empty" [@@noalloc] [@@builtin] [@@no_effects]

let check bytes offset target expected =
  assert (Vox_control.match16 bytes offset target = expected);
  let scanned = match16_empty bytes offset target in
  assert (scanned land 65535 = expected);
  assert ((scanned land 65536 <> 0) = (scalar bytes offset 128 <> 0));
  assert (0 <= scanned && scanned <= 131071)

let () =
  let bytes = Bytes.make 47 '\000' in
  for offset = 0 to 31 do
    for target = 0 to 255 do
      for lane = 0 to 15 do
        Bytes.fill bytes 0 47 (Char.chr ((target + 1) mod 256));
        Bytes.set bytes (offset + lane) (Char.chr target);
        check bytes offset target (1 lsl lane)
      done
    done
  done;
  for mask = 0 to 65535 do
    for lane = 0 to 15 do
      Bytes.set bytes lane
        (if mask land (1 lsl lane) <> 0 then '\128' else '\254')
    done;
    check bytes 0 128 mask
  done;
  let random = Random.State.make [|104729|] in
  for _ = 1 to 1000 do
    Bytes.iteri (fun i _ -> Bytes.set bytes i
      (Char.chr (Random.State.int random 256))) bytes;
    let offset = Random.State.int random 32 in
    for target = 0 to 255 do
      check bytes offset target (scalar bytes offset target)
    done
  done;
  List.iter (fun (length, offset, target) ->
    match Vox_control.match16 (Bytes.make length '\000') offset target with
    | _ -> failwith "accepted invalid group"
    | exception Invalid_argument _ -> ())
    [0, 0, 0; 15, 0, 0; 16, 1, 0; 16, -1, 0;
     16, 0, -1; 16, 0, 256; 16, max_int, 0];
  print_endline "control groups passed"
