type t = int

(* Process-global, strong (never weakened), monotonic intern table. Idempotent by name, so
   two fresh Envs that declare the same names in the same order see identical ids (I6).
   Iteration order of the table is never observed. *)
let by_name : (string, int) Hashtbl.t = Hashtbl.create 64
let names : string Dynarray.t = Dynarray.create ()

let intern name =
  match Hashtbl.find_opt by_name name with
  | Some id -> id
  | None ->
    let id = Dynarray.length names in
    Dynarray.add_last names name;
    Hashtbl.add by_name name id;
    id
;;

let equal (a : t) (b : t) = a = b
let hash (t : t) = t
let name (t : t) = Dynarray.get names t
