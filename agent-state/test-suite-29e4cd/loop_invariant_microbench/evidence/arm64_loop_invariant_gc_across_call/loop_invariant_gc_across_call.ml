let[@inline never] tick i =
  Sys.opaque_identity (i + 1)

let print_result x =
  print_int x;
  print_newline ()


let rec loop x i acc =
  if i <= 0 then acc
  else
    let y = tick i in
    loop x (i - 1) (acc + String.length x + y)

let run n reps =
  let x = Sys.opaque_identity "loop_invariant_payload" in
  let acc = ref 0 in
  for _ = 1 to reps do
    acc := !acc + loop x n 0
  done;
  !acc

let () = print_result (run 12000000 5)
