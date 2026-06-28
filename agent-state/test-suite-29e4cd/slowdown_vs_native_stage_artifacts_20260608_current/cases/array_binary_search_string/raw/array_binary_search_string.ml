let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


let[@inline never] find key a =
  let rec loop lo hi =
    if lo > hi then -1
    else
      let mid = (lo + hi) lsr 1 in
      let k, v = Array.unsafe_get a mid in
      let c = String.compare key k in
      if c = 0 then v else if c < 0 then loop lo (mid - 1) else loop (mid + 1) hi
  in
  loop 0 (Array.length a - 1)

let[@inline never] run n reps =
  let a =
    Array.init 64 (fun i -> "array_binary_key_" ^ string_of_int i, i)
  in
  let keys = Array.map fst a in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) a
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
