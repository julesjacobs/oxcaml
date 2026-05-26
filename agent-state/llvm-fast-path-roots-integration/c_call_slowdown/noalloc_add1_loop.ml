external add1 : int -> int = "bench_noalloc_add1" [@@noalloc]

let run n reps =
  let acc = ref 0 in
  for rep = 1 to reps do
    for i = 0 to n - 1 do
      acc := !acc + add1 (Sys.opaque_identity (i + rep))
    done
  done;
  !acc

let () =
  let n = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (run n reps)
