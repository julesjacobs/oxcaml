module I64 = Stdlib_upstream_compatible.Int64_u

external unsafe_get64 : (string[@local_opt]) -> int -> int64#
  @@ portable = "%caml_string_get64u#" [@@warning "-187"]

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

let[@inline always] chunk8 s i = unsafe_get64 s i

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

let[@inline always] hash_with_pipe4 width chunk tail step combine zero s =
  let len = String.length s in
  let step_width = width * 4 in
  let rec loop hs i =
    if i + step_width <= len then
      let ds =
        #( chunk s i,
           chunk s (i + width),
           chunk s (i + (width * 2)),
           chunk s (i + (width * 3)) )
      in
      loop (lift4 step hs ds) (i + step_width)
    else
      let #(h0, h1, h2, h3) = hs in
      let h0 = if i + width <= len then step h0 (chunk s i) else h0 in
      let i = if i + width <= len then i + width else i in
      let h1 = if i + width <= len then step h1 (chunk s i) else h1 in
      let i = if i + width <= len then i + width else i in
      let h2 = if i + width <= len then step h2 (chunk s i) else h2 in
      let i = if i + width <= len then i + width else i in
      let h3 = if i + width <= len then step h3 (chunk s i) else h3 in
      let i = if i + width <= len then i + width else i in
      let h = reduce4 combine #(h0, h1, h2, h3) in
      I64.logxor (tail s len h i) (u64 len)
  in
  I64.to_int (loop #(zero, zero, zero, zero) 0)

let[@inline always] hash_string s =
  hash_with_pipe4 8 chunk8 tail8 mix mix (u64 0) s

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
