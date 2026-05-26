let make_values () =
  [|
    "alpha-000000000000000000000000000001";
    "alpha-000000000000000000000000000002";
    "alpha-000000000000000000000000000003";
    "beta-0000000000000000000000000000004";
  |]

let run n reps =
  let values = Sys.opaque_identity (make_values ()) in
  let target = Sys.opaque_identity "alpha-000000000000000000000000000003" in
  let len = Array.length values in
  let acc = ref 0 in
  for _rep = 1 to reps do
    for i = 0 to n - 1 do
      let s = Array.unsafe_get values (i land (len - 1)) in
      if String.equal s target then acc := !acc + 1 else acc := !acc + 3
    done
  done;
  !acc

let () =
  let n = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  Printf.printf "%d\n" (run n reps)
