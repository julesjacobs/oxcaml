let n = 20_000_000

let[@inline never] seed i =
  ((i * 0x9e3779b1) lxor (i lsl 13) lxor (i lsr 7)) land max_int

let input = Array.init n seed
let output = Array.make n 0

let[@inline never] mix x =
  let x = x lxor (x lsr 33) in
  let x = x * 0x1f51afd7ed558ccd in
  let x = x lxor (x lsr 33) in
  let x = x * 0x04ceb9fe1a85ec53 in
  x lxor (x lsr 33)

let[@inline never] run () =
  let acc = ref 0 in
  for i = 0 to n - 1 do
    let h = mix (Array.unsafe_get input i) in
    Array.unsafe_set output i h;
    acc := !acc lxor h
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
