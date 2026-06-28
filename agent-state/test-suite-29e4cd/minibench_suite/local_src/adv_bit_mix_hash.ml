let n = 50_000_000

let[@inline never] mix x =
  let x = x lxor (x lsr 16) in
  let x = x * 0x7feb352d in
  let x = x lxor (x lsr 15) in
  let x = x * 0x846ca68b in
  x lxor (x lsr 16)

let[@inline never] run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc lxor mix (i + !acc)
  done;
  !acc

let () =
  let x = run () in
  if x = min_int then print_endline "bad" else Printf.printf "%d\n" x
