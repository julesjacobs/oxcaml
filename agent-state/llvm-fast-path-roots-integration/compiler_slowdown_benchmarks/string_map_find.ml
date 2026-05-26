let[@inline never] black_box_int (x : int) = Sys.opaque_identity x

module M = Map.Make (String)

let[@inline never] run n reps =
  let map =
    List.init 64 (fun i -> "compiler_map_key_" ^ string_of_int i, i)
    |> List.fold_left (fun map (k, v) -> M.add k v map) M.empty
  in
  let keys = Array.init 64 (fun i -> "compiler_map_key_" ^ string_of_int i) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get keys ((i + r) land 63) in
      acc := (!acc + M.find key map) land 0x3fffffff
    done
  done;
  !acc

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in
  let reps =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 40
  in
  Printf.printf "%d\n%!" (run (black_box_int n) (black_box_int reps))
