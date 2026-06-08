let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


exception Miss

let[@inline never] probe x =
  if x = 0 then raise Miss else x

let[@inline never] find x =
  try probe x with Miss -> 1

let[@inline never] run n reps =
  let acc = ref 0 in
  for _ = 1 to reps do
    for _ = 1 to n do
      acc := !acc + find 0
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
