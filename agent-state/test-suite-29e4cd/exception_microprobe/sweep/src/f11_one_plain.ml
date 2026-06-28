
exception E
let[@inline never] opaque x = Sys.opaque_identity x
let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = fun x -> x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
let n = 35000000
let run () =
  let f = opaque (make_closure 1 3 5 7 9 11 13 15 17 19 21) in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + (opaque (f i))
  done;
  !acc
let () = let x = run () in if x = 0 then print_endline "bad"
