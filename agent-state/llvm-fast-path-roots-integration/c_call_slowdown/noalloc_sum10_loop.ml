external sum10 :
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  = "bench_noalloc_sum10_byte" "bench_noalloc_sum10" [@@noalloc]

let run n reps =
  let acc = ref 0 in
  for rep = 1 to reps do
    for i = 0 to n - 1 do
      let x = Sys.opaque_identity (i + rep) in
      acc := !acc + sum10 x 1 2 3 4 5 6 7 8 9
    done
  done;
  !acc

let () =
  let n = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (run n reps)
