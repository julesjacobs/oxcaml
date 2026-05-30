module H = struct
  type t = string list
  let equal x y = x = y
  let hash x = Hashtbl.hash (List.hd x)
end
module W = Weak.Make(H)
let _ = W.create 7
let () = print_endline "pass"
