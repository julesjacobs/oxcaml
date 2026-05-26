let make_values () =
  [| "1000001"; "1000002"; "1000003"; "1000004" |]

let run n reps =
  let values = Sys.opaque_identity (make_values ()) in
  let len = Array.length values in
  let acc = ref 0 in
  for _rep = 1 to reps do
    for i = 0 to n - 1 do
      let s = Array.unsafe_get values (i land (len - 1)) in
      acc := !acc + int_of_string s
    done
  done;
  !acc

let () =
  let n = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (run n reps)
