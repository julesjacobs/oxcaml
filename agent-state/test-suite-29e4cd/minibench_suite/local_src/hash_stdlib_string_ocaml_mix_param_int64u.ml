module I64 = Stdlib_upstream_compatible.Int64_u

let nstrings = 20_000
let len = 128
let rounds = 500

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline always] u64 x = I64.of_int x

let[@inline always] byte s i =
  u64 (Char.code (String.unsafe_get s i))

let[@inline always] mix h d =
  let h = I64.add h d in
  let h = I64.add h (I64.shift_left h 10) in
  let h = I64.logxor h (I64.shift_right_logical h 6) in
  let h = I64.add h (I64.shift_left h 3) in
  let h = I64.logxor h (I64.shift_right_logical h 11) in
  I64.add h (I64.shift_left h 15)

let[@inline always] chunk8 s i =
  I64.logor (byte s i)
    (I64.logor
       (I64.shift_left (byte s (i + 1)) 8)
       (I64.logor
          (I64.shift_left (byte s (i + 2)) 16)
          (I64.logor
             (I64.shift_left (byte s (i + 3)) 24)
             (I64.logor
                (I64.shift_left (byte s (i + 4)) 32)
                (I64.logor
                   (I64.shift_left (byte s (i + 5)) 40)
                   (I64.logor
                      (I64.shift_left (byte s (i + 6)) 48)
                      (I64.shift_left (byte s (i + 7)) 56)))))))

let[@inline never] tail8 s len h i =
  let h = if i < len then I64.logor h (byte s i) else h in
  let h =
    if i + 1 < len then I64.logor h (I64.shift_left (byte s (i + 1)) 8)
    else h
  in
  let h =
    if i + 2 < len then I64.logor h (I64.shift_left (byte s (i + 2)) 16)
    else h
  in
  let h =
    if i + 3 < len then I64.logor h (I64.shift_left (byte s (i + 3)) 24)
    else h
  in
  let h =
    if i + 4 < len then I64.logor h (I64.shift_left (byte s (i + 4)) 32)
    else h
  in
  let h =
    if i + 5 < len then I64.logor h (I64.shift_left (byte s (i + 5)) 40)
    else h
  in
  if i + 6 < len then I64.logor h (I64.shift_left (byte s (i + 6)) 48)
  else h

let[@inline always] hash_with width chunk tail s =
  let len = String.length s in
  let rec loop h i =
    if i + width <= len then loop (mix h (chunk s i)) (i + width)
    else I64.logxor (tail s len h i) (u64 len)
  in
  I64.to_int (loop (u64 0) 0)

let[@inline always] hash_with_pipe2 width chunk tail s =
  let len = String.length s in
  let step = width + width in
  let rec loop h0 h1 i =
    if i + step <= len then
      loop (mix h0 (chunk s i)) (mix h1 (chunk s (i + width))) (i + step)
    else
      let h =
        if i + width <= len then mix h0 (chunk s i) else h0
      in
      I64.logxor (mix h h1) (tail s len (u64 0) (if i + width <= len then i + width else i))
  in
  I64.to_int (I64.logxor (loop (u64 0) (u64 0) 0) (u64 len))

let[@inline always] hash_string s =
  hash_with 8 chunk8 tail8 s

let[@inline always] hash_string_pipe2 s =
  hash_with_pipe2 8 chunk8 tail8 s

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      let s = Array.unsafe_get strings ((i + r) mod nstrings) in
      acc := !acc lxor hash_string s lxor hash_string_pipe2 s
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
