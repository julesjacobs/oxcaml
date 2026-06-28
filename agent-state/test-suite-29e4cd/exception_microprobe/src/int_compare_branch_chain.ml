let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] classify x =
  if x < 10 then 1
  else if x < 20 then 2
  else if x < 30 then 3
  else if x < 40 then 4
  else if x < 50 then 5
  else if x < 60 then 6
  else if x < 70 then 7
  else if x < 80 then 8
  else if x < 90 then 9
  else 10

let n = 50_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + classify (opaque (i land 127))
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
