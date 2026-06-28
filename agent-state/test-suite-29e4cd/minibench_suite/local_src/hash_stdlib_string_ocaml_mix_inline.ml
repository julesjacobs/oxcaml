let nstrings = 20_000
let len = 128
let rounds = 500

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline always] mix h d =
  let h = h + d in
  let h = h + (h lsl 10) in
  let h = h lxor (h lsr 6) in
  let h = h + (h lsl 3) in
  let h = h lxor (h lsr 11) in
  let h = h + (h lsl 15) in
  h

let[@inline always] chunk s i =
  Char.code (String.unsafe_get s i)
  lor (Char.code (String.unsafe_get s (i + 1)) lsl 8)
  lor (Char.code (String.unsafe_get s (i + 2)) lsl 16)
  lor (Char.code (String.unsafe_get s (i + 3)) lsl 24)

let[@inline always] hash_string s =
  let h = ref 0 in
  let i = ref 0 in
  while !i + 4 <= String.length s do
    h := mix !h (chunk s !i);
    i := !i + 4
  done;
  !h lxor String.length s

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor hash_string (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
