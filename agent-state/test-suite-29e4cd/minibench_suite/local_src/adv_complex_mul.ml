let n = 2_000_000
let rounds = 40

let[@inline never] seed scale i =
  float_of_int ((i * 32749 + 91) land 4095) *. scale

let ar = Array.init n (seed 0.000244140625)
let ai = Array.init n (fun i -> seed 0.000244140625 (i + 5))
let br = Array.init n (fun i -> seed 0.0001220703125 (i + 11))
let bi = Array.init n (fun i -> seed 0.0001220703125 (i + 17))
let cr = Array.make n 0.0
let ci = Array.make n 0.0

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    let scale = 1.0 +. float_of_int r *. 0.000001 in
    for i = 0 to n - 1 do
      let xr = Array.unsafe_get ar i in
      let xi = Array.unsafe_get ai i in
      let yr = Array.unsafe_get br i in
      let yi = Array.unsafe_get bi i in
      Array.unsafe_set cr i (((xr *. yr) -. (xi *. yi)) *. scale);
      Array.unsafe_set ci i (((xr *. yi) +. (xi *. yr)) *. scale)
    done;
    total := !total +. Array.unsafe_get cr (r land (n - 1))
  done;
  !total

let () = Printf.printf "%.6f\n" (run ())
