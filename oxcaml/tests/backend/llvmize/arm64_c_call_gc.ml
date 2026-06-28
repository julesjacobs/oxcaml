external force_gc_and_sum : int -> int -> int -> int -> int
  = "arm64_force_gc_and_sum"

let[@inline never] [@local never] call_with_live_values a b c d =
  let live0 = ref a in
  let live1 = ref b in
  let live2 = ref c in
  let res = force_gc_and_sum !live0 !live1 !live2 d in
  res + !live0 + !live1 + !live2

let () = Printf.printf "%d\n" (call_with_live_values 10 20 30 40)
