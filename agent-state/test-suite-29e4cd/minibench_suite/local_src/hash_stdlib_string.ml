let nstrings = 20_000
let len = 128
let rounds = 1_000

let[@inline never] make_string k =
  String.init len (fun i -> Char.chr (((k * 131) + (i * 17) + 23) land 255))

let strings = Array.init nstrings make_string

let[@inline never] run () =
  let acc = ref 0 in
  for r = 1 to rounds do
    for i = 0 to nstrings - 1 do
      acc := !acc lxor String.hash (Array.unsafe_get strings ((i + r) mod nstrings))
    done
  done;
  !acc

let () = Printf.printf "%d\n" (run ())
