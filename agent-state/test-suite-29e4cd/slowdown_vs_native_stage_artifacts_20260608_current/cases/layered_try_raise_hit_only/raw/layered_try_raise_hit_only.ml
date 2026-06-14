let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


exception Not_found_same

type tbl = { hit : bool; layer : layer }
and layer = Nothing | Open of tbl | Lock of tbl

let[@inline never] probe tbl =
  if tbl.hit then 1 else raise Not_found_same

let[@inline never] rec find tbl =
  try probe tbl
  with Not_found_same ->
    match tbl.layer with
    | Open next -> find next
    | Lock next -> find next
    | Nothing -> raise Not_found_same

let rec open_layers n tail =
  if n = 0 then tail
  else
    { hit = false;
      layer =
        if n land 1 = 0
        then Open (open_layers (n - 1) tail)
        else Lock (open_layers (n - 1) tail) }

let[@inline never] run n reps =
  let tbl = open_layers 6 { hit = true; layer = Nothing } in
  let acc = ref 0 in
  for _ = 1 to reps do
    for _ = 1 to n do
      acc := !acc + find tbl
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
