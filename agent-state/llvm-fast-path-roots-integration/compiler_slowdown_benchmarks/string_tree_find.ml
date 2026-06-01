let[@inline never] black_box_int (x : int) = Sys.opaque_identity x

type tree =
  | Empty
  | Node of tree * string * int * tree

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    let name = "compiler_lookup_key_" ^ string_of_int mid in
    Node (build lo (mid - 1), name, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> -1
  | Node (left, name, value, right) ->
    let c = String.compare key name in
    if c = 0 then value else if c < 0 then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let keys = Array.init 64 (fun i -> "compiler_lookup_key_" ^ string_of_int i) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get keys ((i + r) land 63) in
      acc := (!acc + find key tree) land 0x3fffffff
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
