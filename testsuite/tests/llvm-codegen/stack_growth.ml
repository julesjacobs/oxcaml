(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 native;
*)

open Effect
open Effect.Deep

type _ Effect.t += E : unit Effect.t

let[@inline never] id x = Sys.opaque_identity x

let[@inline never] rec grow n acc =
  let a0 = id (acc + 1) in
  let a1 = id (a0 + 1) in
  let a2 = id (a1 + 1) in
  let a3 = id (a2 + 1) in
  let a4 = id (a3 + 1) in
  let a5 = id (a4 + 1) in
  if n = 0 then (perform E; a5) else grow (n - 1) a5 + a4

let main () = grow 2000 0

let () =
  let effc : type a. a Effect.t -> ((a, int) continuation -> int) option =
    function
    | E -> Some (fun k -> continue k ())
    | _ -> None
  in
  let result = match_with main () { retc = Fun.id; exnc = raise; effc } in
  Printf.printf "%d\n" result
