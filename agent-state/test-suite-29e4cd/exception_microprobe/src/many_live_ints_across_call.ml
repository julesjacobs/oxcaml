let[@inline never] opaque x = Sys.opaque_identity x
let[@inline never] call i = opaque (i + 1)

let n = 25_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    let a0 = opaque (i + 1) in
    let a1 = opaque (i + 3) in
    let a2 = opaque (i + 5) in
    let a3 = opaque (i + 7) in
    let a4 = opaque (i + 11) in
    let a5 = opaque (i + 13) in
    let a6 = opaque (i + 17) in
    let a7 = opaque (i + 19) in
    let z = call i in
    acc := !acc + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + z
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
