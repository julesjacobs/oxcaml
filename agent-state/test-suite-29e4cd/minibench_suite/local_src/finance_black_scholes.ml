let n = 1_000_000
let rounds = 20

let[@inline never] seed scale i =
  float_of_int ((i * 1664525 + 1013904223) land 4095) *. scale

let spot = Array.init n (fun i -> 80.0 +. seed 0.02 i)
let strike = Array.init n (fun i -> 75.0 +. seed 0.025 (i + 17))
let maturity = Array.init n (fun i -> 0.05 +. seed 0.0005 (i + 31))
let rate = Array.init n (fun i -> 0.01 +. seed 0.00001 (i + 43))
let volatility = Array.init n (fun i -> 0.10 +. seed 0.00008 (i + 59))
let out = Array.make n 0.0

let[@inline always] normal_cdf x =
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let z = abs_float x in
  let t = 1.0 /. (1.0 +. (0.2316419 *. z)) in
  let poly =
    (((((1.330274429 *. t) -. 1.821255978) *. t +. 1.781477937) *. t
      -. 0.356563782)
     *. t
    +. 0.319381530)
    *. t
  in
  let density = 0.3989422804014327 *. exp (-0.5 *. z *. z) in
  let cdf = 1.0 -. (density *. poly) in
  if sign > 0.0 then cdf else 1.0 -. cdf

let[@inline always] price_call s k t r sigma =
  let sigma_sqrt_t = sigma *. sqrt t in
  let d1 =
    ((log (s /. k)) +. ((r +. (0.5 *. sigma *. sigma)) *. t))
    /. sigma_sqrt_t
  in
  let d2 = d1 -. sigma_sqrt_t in
  (s *. normal_cdf d1) -. (k *. exp (-. r *. t) *. normal_cdf d2)

let[@inline never] price_book bump =
  for i = 0 to n - 1 do
    let s = Array.unsafe_get spot i +. bump in
    let k = Array.unsafe_get strike i in
    let t = Array.unsafe_get maturity i in
    let r = Array.unsafe_get rate i in
    let sigma = Array.unsafe_get volatility i in
    Array.unsafe_set out i (price_call s k t r sigma)
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
    price_book (float_of_int (r land 7) *. 0.01);
    total := !total +. checksum ()
  done;
  !total

let () = Printf.printf "%.6f\n" (run ())
