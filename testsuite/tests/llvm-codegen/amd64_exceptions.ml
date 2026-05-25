(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 flags += " -O3 -llvm-backend";
 native;
*)

let[@inline never] catch_exit b x =
  try if b then raise Exit else x + 1 with Exit -> 0

let[@inline never] catch_argument b x =
  try
    if b then raise (Invalid_argument "amd64 llvm exception") else x * 2
  with Invalid_argument _ -> 17

let () =
  assert (catch_exit false 41 = 42);
  assert (catch_exit true 41 = 0);
  assert (catch_argument false 21 = 42);
  assert (catch_argument true 21 = 17)
