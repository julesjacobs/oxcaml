module I32 = Stdlib_upstream_compatible.Int32_u

let nstrings = 20_000
let len = 128
let rounds = 500

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline always] u32 x = I32.of_int x

let[@inline always] byte s i =
  u32 (Char.code (String.unsafe_get s i))

let[@inline always] mix h d =
  let h = I32.add h d in
  let h = I32.add h (I32.shift_left h 10) in
  let h = I32.logxor h (I32.shift_right_logical h 6) in
  let h = I32.add h (I32.shift_left h 3) in
  let h = I32.logxor h (I32.shift_right_logical h 11) in
  let h = I32.add h (I32.shift_left h 15) in
  h

let[@inline always] chunk s i =
  I32.logor (byte s i)
    (I32.logor
       (I32.shift_left (byte s (i + 1)) 8)
       (I32.logor
          (I32.shift_left (byte s (i + 2)) 16)
          (I32.shift_left (byte s (i + 3)) 24)))

let[@inline always] hash_string s =
  let len = String.length s in
  let rec loop h i =
    if i + 4 <= len then loop (mix h (chunk s i)) (i + 4)
    else I32.logxor h (u32 len)
  in
  I32.to_int (loop (u32 0) 0)

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
