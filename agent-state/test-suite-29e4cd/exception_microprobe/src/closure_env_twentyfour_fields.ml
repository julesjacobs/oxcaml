let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure
    a0 a1 a2 a3 a4 a5 a6 a7
    a8 a9 a10 a11 a12 a13 a14 a15
    a16 a17 a18 a19 a20 a21 a22 a23 =
  fun x ->
    x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7
    + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15
    + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23

let n = 16_000_000

let run () =
  let f =
    opaque
      (make_closure
         1 3 5 7 11 13 17 19
         23 29 31 37 41 43 47 53
         59 61 67 71 73 79 83 89)
  in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + opaque (f i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
