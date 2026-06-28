let n = 120_000_000

let[@inline never] run () =
  let acc = ref 0x1234567 in
  for i = 1 to n do
    let x = !acc lxor (i * 1103515245) in
    let y = (x lsl 7) lxor (x lsr 3) lxor (i * 97) in
    acc := (y + (x land 0x3fffffff)) land 0x3fffffff
  done;
  !acc

let () =
  Printf.printf "%d\n" (run ())
