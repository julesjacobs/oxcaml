module I64 = Stdlib_upstream_compatible.Int64_u

external unsafe_get64 : (string[@local_opt]) -> int -> int64#
  @@ portable = "%caml_string_get64u#" [@@warning "-187"]

let nstrings = 20_000
let len = 128
let rounds = 1_000

let prime1 = I64.of_int64 (-7046029288634856825L)
let prime2 = I64.of_int64 (-4417276706812531889L)
let prime3 = I64.of_int64 1609587929392839161L
let prime4 = I64.of_int64 (-8796714831421723037L)
let prime5 = I64.of_int64 2870177450012600261L

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline always] u64 x = I64.of_int x

let[@inline always] byte s i =
  u64 (Char.code (String.unsafe_get s i))

let[@inline always] rotl x n =
  I64.logor (I64.shift_left x n) (I64.shift_right_logical x (64 - n))

let[@inline always] round acc input =
  I64.mul (rotl (I64.add acc (I64.mul input prime2)) 31) prime1

let[@inline always] merge_round acc value =
  let acc = I64.logxor acc (round (u64 0) value) in
  I64.add (I64.mul acc prime1) prime4

let[@inline always] avalanche h =
  let h = I64.logxor h (I64.shift_right_logical h 33) in
  let h = I64.mul h prime2 in
  let h = I64.logxor h (I64.shift_right_logical h 29) in
  let h = I64.mul h prime3 in
  I64.logxor h (I64.shift_right_logical h 32)

let[@inline never] tail s len h i =
  let rec loop h i =
    if i + 8 <= len then
      let k1 = round (u64 0) (unsafe_get64 s i) in
      loop (I64.mul (rotl (I64.logxor h k1) 27) prime1 |> I64.add prime4) (i + 8)
    else #(h, i)
  in
  let #(h, i) = loop h i in
  let h = if i < len then I64.add (I64.mul (I64.logxor h (byte s i)) prime5) prime1 else h in
  let h = if i + 1 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 1)) 8)) prime5) prime1 else h in
  let h = if i + 2 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 2)) 16)) prime5) prime1 else h in
  let h = if i + 3 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 3)) 24)) prime5) prime1 else h in
  let h = if i + 4 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 4)) 32)) prime5) prime1 else h in
  let h = if i + 5 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 5)) 40)) prime5) prime1 else h in
  let h = if i + 6 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 6)) 48)) prime5) prime1 else h in
  if i + 7 < len then I64.add (I64.mul (I64.logxor h (I64.shift_left (byte s (i + 7)) 56)) prime5) prime1 else h

let[@inline always] hash64 s =
  let len = String.length s in
  let rec loop v1 v2 v3 v4 i =
    if i + 32 <= len then
      loop
        (round v1 (unsafe_get64 s i))
        (round v2 (unsafe_get64 s (i + 8)))
        (round v3 (unsafe_get64 s (i + 16)))
        (round v4 (unsafe_get64 s (i + 24)))
        (i + 32)
    else
      let h =
        I64.add
          (I64.add (rotl v1 1) (rotl v2 7))
          (I64.add (rotl v3 12) (rotl v4 18))
      in
      let h = merge_round h v1 in
      let h = merge_round h v2 in
      let h = merge_round h v3 in
      let h = merge_round h v4 in
      avalanche (tail s len (I64.add h (u64 len)) i)
  in
  if len >= 32 then
    loop
      (I64.add prime1 prime2)
      prime2
      (u64 0)
      (I64.neg prime1)
      0
  else
    avalanche (tail s len (I64.add prime5 (u64 len)) 0)

let[@inline always] hash_string s =
  let h = hash64 s in
  I64.to_int (I64.logxor h (I64.shift_right_logical h 33))

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
