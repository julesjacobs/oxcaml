let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


type tree = Empty | Node of tree * string * int * tree

let key i =
  String.make 1 (Char.chr (65 + (i land 25))) ^ "_compiler_key_" ^ string_of_int i

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (build lo (mid - 1), key mid, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> -1
  | Node (left, name, value, right) ->
    let c = String.compare key name in
    if c = 0 then value else if c < 0 then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let keys = Array.init 64 key in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) tree
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
