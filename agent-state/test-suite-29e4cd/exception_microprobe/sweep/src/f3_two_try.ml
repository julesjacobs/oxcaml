
exception E
let[@inline never] opaque x = Sys.opaque_identity x
let[@inline never] make_closure a0 a1 a2 = fun x -> x + a0 + a1 + a2
let n = 17500000
let run () =
  let f = opaque (make_closure 1 3 5) in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + (try opaque (f i) + opaque (f (i + 1)) with E -> i land 7)
  done;
  !acc
let () = let x = run () in if x = 0 then print_endline "bad"
