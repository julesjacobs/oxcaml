let n = 20_000_000

let[@inline never] seed i =
  ((i * 0x85ebca6b) lxor (i lsl 17) lxor (i lsr 11)) land max_int

let input = Array.init n seed
let output = Array.make n 0

let[@inline never] rotl x r =
  (x lsl r) lor (x lsr (63 - r))

let[@inline never] mix x =
  let h = x + 0x1e3779b185ebca87 in
  let h = rotl (h * 0x02b2ae3d27d4eb4f) 31 in
  let h = h * 0x065667b19e3779f9 in
  let h = h lxor (h lsr 33) in
  let h = h * 0x1f51afd7ed558ccd in
  h lxor (h lsr 29)

let[@inline never] run () =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    let h = mix (Array.unsafe_get input i) in
    Array.unsafe_set output i h;
    acc := !acc + h
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
