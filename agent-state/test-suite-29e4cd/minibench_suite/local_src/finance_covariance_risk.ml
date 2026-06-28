let n = 256
let rounds = 1_000

let[@inline never] seed scale i =
  float_of_int ((i * 1664525 + 1013904223) land 4095) *. scale

let weights = Array.init n (fun i -> -0.5 +. seed 0.00025 i)

let covariance =
  Array.init (n * n) (fun k ->
      let i = k / n in
      let j = k - (i * n) in
      let d = if i = j then 0.05 else 0.0001 in
      d +. seed 0.0000001 ((i * 131) + j))

let work = Array.make n 0.0

let[@inline never] multiply_covariance () =
  for i = 0 to n - 1 do
    let row = i * n in
    let acc = ref 0.0 in
    for j = 0 to n - 1 do
      acc :=
        !acc
        +. Array.unsafe_get covariance (row + j)
           *. Array.unsafe_get weights j
    done;
    Array.unsafe_set work i !acc
  done

let[@inline never] portfolio_variance () =
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc :=
      !acc
      +. Array.unsafe_get weights i *. Array.unsafe_get work i
  done;
  !acc

let[@inline never] run () =
  let total = ref 0.0 in
  for r = 1 to rounds do
    multiply_covariance ();
    total := !total +. portfolio_variance () +. float_of_int r *. 0.0000001
  done;
  !total

let () = Printf.printf "%.6f\n" (run ())
