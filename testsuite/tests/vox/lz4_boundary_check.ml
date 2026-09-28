(* Checks the Lambda of the LZ4 public client; see lz4_boundary.ml. *)
open Emitted_code

let () =
  let client = read Sys.argv.(1) in
  check (not (occurs "Vox_lz4_spec" client))
    "the client's code refers to no specification module";
  check
    (count "(apply%s(field_imm%s%d%s(global%sVox_lz4!))" client = 2)
    "the client makes two calls to Vox_lz4";
  finish ()
