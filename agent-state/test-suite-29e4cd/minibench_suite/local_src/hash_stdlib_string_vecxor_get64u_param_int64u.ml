module I64 = Stdlib_upstream_compatible.Int64_u

external unsafe_get64 : (string[@local_opt]) -> int -> int64#
  @@ portable = "%caml_string_get64u#" [@@warning "-187"]

let nstrings = 20_000
let len = 1024
let rounds = 100

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline always] u64 x = I64.of_int x

let[@inline always] byte s i =
  u64 (Char.code (String.unsafe_get s i))

let[@inline always] chunk8 s i = unsafe_get64 s i

let[@inline never] tail8 s len h i =
  let h = if i < len then I64.logxor h (byte s i) else h in
  let h = if i + 1 < len then I64.logxor h (byte s (i + 1)) else h in
  let h = if i + 2 < len then I64.logxor h (byte s (i + 2)) else h in
  let h = if i + 3 < len then I64.logxor h (byte s (i + 3)) else h in
  let h = if i + 4 < len then I64.logxor h (byte s (i + 4)) else h in
  let h = if i + 5 < len then I64.logxor h (byte s (i + 5)) else h in
  if i + 6 < len then I64.logxor h (byte s (i + 6)) else h

let[@inline always] fold_chunks width chunk combine tail zero s =
  let len = String.length s in
  let rec loop h i =
    if i + width <= len then loop (combine h (chunk s i)) (i + width)
    else tail s len h i
  in
  loop zero 0

let[@inline always] hash_string s =
  let h = fold_chunks 8 chunk8 I64.logxor tail8 (u64 0) s in
  I64.to_int (I64.logxor h (u64 (String.length s)))

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
