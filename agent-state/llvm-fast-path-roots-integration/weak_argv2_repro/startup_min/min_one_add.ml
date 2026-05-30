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
  let c = String.init 7 (fun _ -> Char.chr (32 + Random.int 95)) in
  r := c :: !r;
  W.add tbl !r;
  ignore (loops, bunch);
  print_endline "pass"
