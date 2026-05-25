(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 flags += " -O3 -llvm-backend";
 native;
*)

let[@inline never] allocate n =
  let rec loop i acc =
    if i = 0 then acc else loop (i - 1) (i :: acc)
  in
  List.length (loop n [])

let[@inline never] caller n =
  let live = Array.make 16 7 in
  let len = allocate n in
  live.(0) + len

let () = assert (caller 200_000 = 200_007)
