external watch_finalizers : unit -> unit = "caml_lz4_watch_finalizers"
external reclaimed_buffers : unit -> int = "caml_lz4_reclaimed_buffers"
type buffer
external make_buffer : int -> buffer = "caml_lz4_make_buffer"
external check_buffer : buffer -> unit = "caml_lz4_check_buffer"
external release_buffer : buffer -> unit = "caml_lz4_release_buffer"

let collect () = Gc.full_major (); Gc.full_major ()

let retained () =
  let buffer = make_buffer 1024 in
  collect ();
  check_buffer buffer;
  release_buffer buffer

let abandon () =
  let buffer = make_buffer 1024 in
  check_buffer buffer;
  raise Out_of_memory

let compressor_failure source =
  let module V = Vox_string_view in
  if V.length source > 4194304 then invalid_arg "source"
  else
    match Vox_lz4_streaming.encode (ghost_ (V.contents source)) source with
    | None -> failwith "allocation failed before fault injection"
    | Some buffer ->
      assert (Vox_lz4_encode_buffer.used (borrow_ buffer) > 0);
      raise Out_of_memory

let decoder_failure wire =
  match Vox_lz4_string_decode.decode (ghost_ (Vox_string_view.contents wire))
          wire 8192 with
  | None -> failwith "allocation failed before fault injection"
  | Some { Vox_lz4_string_decode.status; buffer; error = _ } ->
    assert (status = Vox_lz4_spec_decode.Done);
    assert (Vox_lz4_buffer.used (borrow_ buffer) = 8192);
    raise Out_of_memory

let exercise_failures f =
  collect ();
  for _ = 1 to 20 do
    let before = reclaimed_buffers () in
    (try f (); assert false with Out_of_memory -> ());
    collect ();
    assert (reclaimed_buffers () = before + 1)
  done

let () =
  watch_finalizers ();
  collect ();
  let before = reclaimed_buffers () in
  retained ();
  collect ();
  assert (reclaimed_buffers () = before);
  (try abandon () with Out_of_memory -> ());
  collect ();
  assert (reclaimed_buffers () = before + 1);
  let source = String.init 8192 (fun i -> Char.chr (i mod 251)) in
  let wire = Vox_lz4.compress source in
  exercise_failures (fun () -> compressor_failure source);
  exercise_failures (fun () -> decoder_failure wire);
  assert (Vox_lz4.decompress ~capacity:8192 wire = Ok source);
  collect ();
  let before = reclaimed_buffers () in
  for _ = 1 to 20 do
    assert (Vox_lz4.decompress ~capacity:8192 (Vox_lz4.compress source) = Ok source)
  done;
  collect ();
  assert (reclaimed_buffers () = before);
  print_endline "LZ4 finalizers and allocation failures passed"
