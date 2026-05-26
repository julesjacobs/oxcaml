let[@inline never] black_box_int (x : int) = Sys.opaque_identity x

exception Miss

let[@inline never] find_hit key a =
  let len = Array.length a in
  let rec loop i =
    if i >= len then raise Miss
    else if Array.unsafe_get a i = key then i
    else loop (i + 1)
  in
  loop 0

let[@inline never] run n reps =
  let a = Array.init 8 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := (!acc + find_hit (((i + r) land 7) lsl 1) a) land 0x3fffffff
    done
  done;
  !acc

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in
  let reps =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 30
  in
  Printf.printf "%d\n%!" (run (black_box_int n) (black_box_int reps))
