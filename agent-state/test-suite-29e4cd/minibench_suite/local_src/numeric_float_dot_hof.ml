let n = 2_000_000
let rounds = 40

let[@inline never] seed i =
  float_of_int ((i * 17 + 13) land 1023) *. 0.0009765625

let a = Array.init n seed
let b = Array.init n (fun i -> seed (i + 7))

let[@inline always] fold2 f acc0 a b =
  let acc = ref acc0 in
  for i = 0 to n - 1 do
    acc := f !acc (Array.unsafe_get a i) (Array.unsafe_get b i)
  done;
  !acc

let[@inline always] combine acc x y =
  acc +. (x *. y) +. (x *. 0.125) -. (y *. 0.0625)

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    total := !total +. fold2 combine 0.0 a b +. float_of_int r
  done;
  !total

let () =
  let x = run () in
  Printf.printf "%.6f\n" x
