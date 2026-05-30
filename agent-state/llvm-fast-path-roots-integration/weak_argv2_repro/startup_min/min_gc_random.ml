module H = struct
  type t = string list
  let equal x y = x = y
  let hash x = Hashtbl.hash (List.hd x)
end

module W = Weak.Make(H)

let tbl = W.create 7
let r : string list ref = ref []
let loops = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1
let bunch = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 1

let () =
  Random.init 314;
  r := [];
  Gc.full_major ();
  ignore (tbl, loops, bunch);
  print_endline "pass"
