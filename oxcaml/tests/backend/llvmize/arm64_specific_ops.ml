external min_f64 : float -> float -> float
  = "caml_vec128_unreachable" "caml_simd_float64_min"
[@@noalloc] [@@builtin] [@@unboxed]

external max_f64 : float -> float -> float
  = "caml_vec128_unreachable" "caml_simd_float64_max"
[@@noalloc] [@@builtin] [@@unboxed]

external round_current_f64 : float -> float
  = "caml_vec128_unreachable" "caml_simd_float64_round_current"
[@@noalloc] [@@builtin] [@@unboxed]

external iround_f64 : float -> int64
  = "caml_vec128_unreachable" "caml_simd_cast_float64_int64"
[@@noalloc] [@@builtin] [@@unboxed]

external min_f32 : float32 -> float32 -> float32
  = "caml_vec128_unreachable" "caml_simd_float32_min"
[@@noalloc] [@@builtin] [@@unboxed]

external max_f32 : float32 -> float32 -> float32
  = "caml_vec128_unreachable" "caml_simd_float32_max"
[@@noalloc] [@@builtin] [@@unboxed]

external round_current_f32 : float32 -> float32
  = "caml_vec128_unreachable" "caml_simd_float32_round_current"
[@@noalloc] [@@builtin] [@@unboxed]

external iround_f32 : float32 -> int64
  = "caml_vec128_unreachable" "caml_simd_cast_float32_int64"
[@@noalloc] [@@builtin] [@@unboxed]

external f32_to_f64 : float32 -> float = "%floatoffloat32"

let[@inline never] shift_add x y = x + (y lsl 2)

let[@inline never] shift_sub x y = x - (y lsl 2)

let[@inline never] mul_add x y z = (x * y) + z

let[@inline never] mul_sub x y z = z - (x * y)

let[@inline never] signext16 x = (x lsl 48) asr 48

let[@inline never] neg_mul x y = -.(x *. y)

let[@inline never] mul_addf x y z = z +. (x *. y)

let[@inline never] mul_subf x y z = z -. (x *. y)

let[@inline never] neg_mul_subf x y z = (x *. y) -. z

let[@inline never] sqrtf x = sqrt x

let print_f32 label x = Printf.printf "%s %.1f\n" label (f32_to_f64 x)

let () =
  let x = Sys.opaque_identity 7 in
  let y = Sys.opaque_identity 3 in
  Printf.printf "shift_add %d\n" (shift_add x y);
  Printf.printf "shift_sub %d\n" (shift_sub x y);
  Printf.printf "mul_add %d\n" (mul_add x y 5);
  Printf.printf "mul_sub %d\n" (mul_sub x y 30);
  Printf.printf "signext16 %d\n" (signext16 0xffff);
  Printf.printf "neg_mul %.1f\n" (neg_mul 2. 3.);
  Printf.printf "mul_addf %.1f\n" (mul_addf 2. 3. 1.);
  Printf.printf "mul_subf %.1f\n" (mul_subf 2. 3. 10.);
  Printf.printf "neg_mul_subf %.1f\n" (neg_mul_subf 2. 3. 1.);
  Printf.printf "sqrtf %.1f\n" (sqrtf 9.);
  Printf.printf "min_f64 %.1f\n" (min_f64 3. 1.);
  Printf.printf "max_f64 %.1f\n" (max_f64 3. 1.);
  Printf.printf "round_f64 %.1f\n" (round_current_f64 1.5);
  Printf.printf "iround_f64 %Ld\n" (iround_f64 1.5);
  print_f32 "min_f32" (min_f32 3.s 1.s);
  print_f32 "max_f32" (max_f32 3.s 1.s);
  print_f32 "round_f32" (round_current_f32 1.5s);
  Printf.printf "iround_f32 %Ld\n" (iround_f32 1.5s)
