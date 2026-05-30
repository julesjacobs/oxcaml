module Hashed = struct
  type t = string list
  let equal x y = x = y
  let hash x = Hashtbl.hash (List.hd x)
end

module HT = Weak.Make (Hashed)

let tbl = HT.create 7
let r = ref []
let loops = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1
let bunch = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 1

let () =
  Random.init 314;
  for _j = 0 to loops - 1 do
    r := [];
    Gc.full_major ();
    for _i = 1 to bunch do
      let c = String.init 7 (fun _ -> Char.chr (32 + Random.int 95)) in
      r := c :: !r;
      HT.add tbl !r
    done
  done;
  print_endline "pass"
