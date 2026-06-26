(* TEST
 not-windows;
 not-macos;
 not-bsd;
 arch_amd64;
 llvm-backend;
 flags += " -O3 -llvm-backend";
 native;
*)

let[@inline never] f x = Sys.opaque_identity x + 1

let () =
  if f 41 <> 42 then failwith "bad result";
  print_endline "ok"
