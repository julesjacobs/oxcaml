let[@inline never] [@local never] many_args a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10

let[@inline never] [@local never] call_with x =
  many_args x (x + 1) (x + 2) (x + 3) (x + 4) (x + 5) (x + 6) (x + 7) (x + 8)
    (x + 9) (x + 10)

let () = Printf.printf "%d\n" (call_with 1)
