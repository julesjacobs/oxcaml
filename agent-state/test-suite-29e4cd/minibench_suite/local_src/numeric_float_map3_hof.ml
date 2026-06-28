let n = 4_000_000
let rounds = 80

let[@inline never] seed i =
  float_of_int ((i * 17 + 13) land 1023) *. 0.0009765625

let a = Array.init n seed
let b = Array.init n (fun i -> seed (i + 7))
let c = Array.init n (fun i -> seed (i + 13))
let dst = Array.make n 0.0

let[@inline always] map3 f a b c dst =
  for i = 0 to n - 1 do
    Array.unsafe_set dst i
      (f (Array.unsafe_get a i) (Array.unsafe_get b i) (Array.unsafe_get c i))
  done

let[@inline always] kernel x y z =
  (x *. 1.0001) +. (y *. 0.5) -. (z *. 0.125)

let[@inline never] step a b c dst =
  map3 kernel a b c dst

let[@inline never] checksum a =
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := !acc +. Array.unsafe_get a i
  done;
  !acc

let[@inline never] run () =
  for _ = 1 to rounds do
    step a b c dst
  done;
  checksum dst

let () = Printf.printf "%.6f\n" (run ())
