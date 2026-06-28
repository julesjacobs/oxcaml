let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make i =
  opaque (i, i + 1, i + 2, i + 3)

let[@inline never] use t =
  match t with
  | a, b, c, d -> a + (2 * b) + (3 * c) + (4 * d)

let n = 30_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + use (make i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
