(* Checks that the channel and lock libraries' emitted code has no ghost
   primitive; see concurrency_boundary.ml. Argument: the directory of the
   dumps (Lambda, and natively also Cmm). *)
open Emitted_code

let primitives =
  [ "caml_pref_own"; "caml_pref_heap_"; "caml_pref_split"; "caml_pref_join";
    "caml_vox_atomic_key"; "caml_unique_cell_location" ]

let () =
  List.iter
    (fun unit ->
      let dump = read (Filename.concat Sys.argv.(1) (unit ^ ".dump")) in
      check (occurs "(function" dump) (unit ^ " was dumped");
      List.iter
        (fun primitive ->
          check (not (occurs primitive dump))
            (Printf.sprintf "%s does not use %s" unit primitive))
        primitives)
    [ "one_shot"; "channel_buffer"; "spin_lock"; "unique_lock";
      "reference_lock" ];
  finish ()
