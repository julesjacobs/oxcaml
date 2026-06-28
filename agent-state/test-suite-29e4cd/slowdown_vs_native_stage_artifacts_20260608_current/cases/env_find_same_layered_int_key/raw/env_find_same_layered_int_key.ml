let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


exception Not_found_same

type ident = { stamp : int }
type tree = Empty | Node of tree * ident * int * tree
type tbl = { current : tree; layer : layer }
and layer = Nothing | Open of tbl | Lock of tbl

let[@inline never] ident_find_same id = function
  | Empty -> raise Not_found_same
  | Node (_, k, value, _) ->
    if id.stamp = k.stamp then value else raise Not_found_same

let[@inline never] rec find_same_without_locks id tbl =
  try ident_find_same id tbl.current
  with Not_found_same ->
    match tbl.layer with
    | Open next -> find_same_without_locks id next
    | Lock next -> find_same_without_locks id next
    | Nothing -> raise Not_found_same

let rec open_layers n tail =
  if n = 0 then tail
  else
    { current = Empty;
      layer =
        if n land 1 = 0
        then Open (open_layers (n - 1) tail)
        else Lock (open_layers (n - 1) tail) }

let[@inline never] run n reps =
  let bottom =
    { current = Node (Empty, { stamp = 0 }, 1, Empty); layer = Nothing }
  in
  let tbl = open_layers 6 bottom in
  let id = { stamp = 0 } in
  let acc = ref 0 in
  for _ = 1 to reps do
    for _ = 1 to n do
      acc := !acc + find_same_without_locks id tbl
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
