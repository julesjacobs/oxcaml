let n = 2_000_000
let rounds = 50

let[@inline never] seed scale i =
  float_of_int ((i * 1103515245 + 12345) land 1023) *. scale

let spot = Array.init n (fun i -> 50.0 +. seed 0.05 i)
let delta = Array.init n (fun i -> -1.0 +. seed 0.002 (i + 11))
let gamma = Array.init n (fun i -> 0.0001 +. seed 0.000001 (i + 23))
let vega = Array.init n (fun i -> seed 0.0003 (i + 37))
let theta = Array.init n (fun i -> -. seed 0.0002 (i + 41))
let shock = Array.init n (fun i -> -2.5 +. seed 0.005 (i + 53))
let out = Array.make n 0.0

let[@inline never] mark_portfolio vol_shift day_frac =
  for i = 0 to n - 1 do
    let s = Array.unsafe_get spot i in
    let d = Array.unsafe_get delta i in
    let g = Array.unsafe_get gamma i in
    let v = Array.unsafe_get vega i in
    let t = Array.unsafe_get theta i in
    let ds = Array.unsafe_get shock i in
    let pnl =
      (s *. d *. ds)
      +. (0.5 *. g *. ds *. ds *. s *. s)
      +. (v *. vol_shift)
      +. (t *. day_frac)
    in
    Array.unsafe_set out i pnl
  done

let[@inline never] checksum () =
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := !acc +. Array.unsafe_get out i
  done;
  !acc

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    mark_portfolio
      (float_of_int ((r land 7) + 1) *. 0.0001)
      (float_of_int ((r land 31) + 1) *. 0.0027397260273972603);
    total := !total +. checksum ()
  done;
  !total

let () = Printf.printf "%.6f\n" (run ())
