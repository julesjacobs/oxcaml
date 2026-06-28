let n = 256
let rounds = 1_000_000

let bytes =
  Bytes.init n (fun i -> Char.chr (((i * 131) + 17) land 255))

let[@inline never] run () =
  let total = ref 0 in
  for r = 1 to rounds do
    let h = ref (0x0bf29ce484222325 lxor r) in
    for i = 0 to n - 1 do
      h := (!h lxor Char.code (Bytes.unsafe_get bytes i)) * 0x100000001b3
    done;
    total := !total lxor !h
  done;
  !total

let () = Printf.printf "%d\n" (run ())
