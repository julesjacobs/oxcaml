let n = 2_000_000
let rounds = 50

let[@inline never] seed i =
  float_of_int ((i * 8191 + 17) land 4095) *. 0.000244140625

let x = Array.init n seed
let y = Array.make n 0.0

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    let c0 = float_of_int r *. 0.000001 in
    for i = 0 to n - 1 do
      let xi = Array.unsafe_get x i in
      let v =
        (((((0.03125 *. xi +. 0.0625) *. xi +. 0.125) *. xi +. 0.25) *. xi
          +. 0.5)
         *. xi)
        +. c0
      in
      Array.unsafe_set y i v
    done;
    total := !total +. Array.unsafe_get y (r land (n - 1))
  done;
  !total

let () = Printf.printf "%.6f\n" (run ())
