let n = 100_000_000

let[@inline never] run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + ((i * 3) - (i lsr 1) + 17)
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
