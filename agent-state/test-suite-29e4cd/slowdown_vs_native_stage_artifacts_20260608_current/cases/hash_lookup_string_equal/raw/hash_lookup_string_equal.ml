let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


let[@inline never] find key buckets =
  let bucket = Array.unsafe_get buckets 0 in
  let rec loop = function
    | [] -> -1
    | (k, v) :: rest -> if String.equal key k then v else loop rest
  in
  loop bucket

let[@inline never] run n reps =
  let entries =
    List.init 32 (fun i -> "hash_collision_key_" ^ string_of_int i, i)
  in
  let buckets = [| entries |] in
  let keys = Array.of_list (List.map fst entries) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 31)) buckets
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
