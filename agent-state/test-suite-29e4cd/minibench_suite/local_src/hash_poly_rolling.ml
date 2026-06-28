let n = 10_000_000

let data =
  Array.init n (fun i -> ((i * 1103515245 + 12345) lsr 16) land 255)

let[@inline never] run () =
  let h1 = ref 1 in
  let h2 = ref 7 in
  for i = 0 to n - 1 do
    let x = Array.unsafe_get data i in
    h1 := (!h1 * 257) + x;
    h2 := (!h2 * 65537) lxor x
  done;
  !h1 lxor !h2

let () = Printf.printf "%d\n" (run ())
