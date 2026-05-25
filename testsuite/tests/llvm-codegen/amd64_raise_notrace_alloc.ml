(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 flags += " -O3 -llvm-backend";
 native;
*)

exception Ok of string

let[@inline never] raise_exit () = raise Exit

let[@inline never] allocate_before_read exn =
  ignore (Sys.opaque_identity (Bytes.create 128));
  match exn with
  | Ok s -> assert (s = "OK")
  | _ -> assert false

let () =
  try
    try raise_exit () with
    | Exit ->
      let s = Sys.opaque_identity "O" ^ Sys.opaque_identity "K" in
      raise (Ok s)
  with
  | Ok _ as exn -> allocate_before_read exn
  | _ -> assert false
