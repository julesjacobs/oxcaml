let n = 2_000_000
let rounds = 40

let[@inline never] seed i =
  float_of_int ((i * 17 + 13) land 1023) *. 0.0009765625

let a = Array.init n seed
let b = Array.init n (fun i -> seed (i + 7))

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    let acc = ref 0.0 in
    for i = 0 to n - 1 do
      let x = Array.unsafe_get a i in
      let y = Array.unsafe_get b i in
      acc := !acc +. (x *. y) +. (x *. 0.125) -. (y *. 0.0625)
    done;
    total := !total +. !acc +. float_of_int r
  done;
  !total

let () =
  let x = run () in
  Printf.printf "%.6f\n" x
