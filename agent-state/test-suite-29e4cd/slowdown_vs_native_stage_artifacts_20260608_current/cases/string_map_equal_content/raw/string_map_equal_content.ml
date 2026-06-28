let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


module M = Map.Make (String)

let fresh s = Bytes.to_string (Bytes.of_string s)

let[@inline never] run n reps =
  let stored = Array.init 64 (fun i -> "compiler_equal_key_" ^ string_of_int i) in
  let lookup = Array.map fresh stored in
  let map =
    Array.fold_left (fun m k -> M.add k (String.length k) m) M.empty stored
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get lookup ((i + r) land 63) in
      acc := !acc + M.find key map
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
