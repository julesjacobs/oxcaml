let run n reps =
  let acc = ref 0.0 in
  for rep = 1 to reps do
    for i = 0 to n - 1 do
      let x = Sys.opaque_identity (float i *. 0.000001 +. float rep) in
      acc := !acc +. Float.sin x
    done
  done;
  int_of_float (!acc *. 1000.0)

let () =
  let n = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (run n reps)
