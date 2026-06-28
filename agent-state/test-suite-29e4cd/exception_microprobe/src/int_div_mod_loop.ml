let[@inline never] opaque x = Sys.opaque_identity x

let n = 35_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    let x = opaque (i + 12345) in
    acc := !acc + ((x / 7) mod 97) + ((x / 13) mod 31)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
