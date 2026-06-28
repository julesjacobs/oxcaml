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

let[@inline always] lift4 f #(h0, h1, h2, h3) #(d0, d1, d2, d3) =
  #(f h0 d0, f h1 d1, f h2 d2, f h3 d3)

let[@inline always] reduce4 f #(h0, h1, h2, h3) =
  f (f (f h0 h1) h2) h3

let[@inline always] hash_string s =
  let len = String.length s in
  let rec loop h0 h1 h2 h3 i =
    if i + 32 <= len then
      let #(h0, h1, h2, h3) =
        lift4 mix
          #(h0, h1, h2, h3)
          #(chunk8 s i, chunk8 s (i + 8), chunk8 s (i + 16), chunk8 s (i + 24))
      in
      loop h0 h1 h2 h3 (i + 32)
    else
      let h0 = if i + 8 <= len then mix h0 (chunk8 s i) else h0 in
      let i = if i + 8 <= len then i + 8 else i in
      let h1 = if i + 8 <= len then mix h1 (chunk8 s i) else h1 in
      let i = if i + 8 <= len then i + 8 else i in
      let h2 = if i + 8 <= len then mix h2 (chunk8 s i) else h2 in
      let i = if i + 8 <= len then i + 8 else i in
      let h3 = if i + 8 <= len then mix h3 (chunk8 s i) else h3 in
      let i = if i + 8 <= len then i + 8 else i in
      let h = reduce4 mix #(h0, h1, h2, h3) in
      I64.to_int (I64.logxor (tail8 s len h i) (u64 len))
  in
  loop (u64 0) (u64 0) (u64 0) (u64 0) 0

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
