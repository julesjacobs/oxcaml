let n = 2_000_000
let rounds = 60

let[@inline never] seed i =
  float_of_int ((i * 131071 + 7) land 4095) *. 0.000244140625

let a = Array.init n seed
let b = Array.make n 0.0

let[@inline never] step src dst =
  for i = 1 to n - 2 do
    let left = Array.unsafe_get src (i - 1) in
    let mid = Array.unsafe_get src i in
    let right = Array.unsafe_get src (i + 1) in
    Array.unsafe_set dst i ((0.25 *. left) +. (0.5 *. mid) +. (0.25 *. right))
  done

let[@inline never] run () =
  for r = 1 to rounds do
    if r land 1 = 0 then step b a else step a b
  done;
  Array.unsafe_get a (n / 2) +. Array.unsafe_get b ((n / 2) + 1)

let () = Printf.printf "%.6f\n" (run ())
