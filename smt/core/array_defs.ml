(* Symbol.t is [private int], so it keys an immutable [Int] map by its underlying id
   (O(log n), functional [add]). The registry is tiny (a few (index, element)
   instantiations per query), so map overhead is irrelevant. Mirrors {!Datatype_defs}. *)
module Id_map = Map.Make (Int)

type role =
  | Select
  | Store

type entry =
  { role : role
  ; index : Sort.t
  ; element : Sort.t
  }

type t = { by_sym : entry Id_map.t }

let empty = { by_sym = Id_map.empty }
let is_empty t = Id_map.is_empty t.by_sym
let id (s : Symbol.t) = (s :> int)

(* A deterministic string identity for a sort. Distinct sorts yield distinct keys, so the
   generated operator name is unique per instantiation. *)
let rec sort_key (s : Sort.t) : string =
  match s with
  | Sort.Bool -> "Bool"
  | Sort.Int _ -> "Int"
  | Sort.Uninterpreted sym -> "U:" ^ Symbol.name sym
  | Sort.Datatype sym -> "D:" ^ Symbol.name sym
  | Sort.Array (i, e) -> Printf.sprintf "(A %s %s)" (sort_key i) (sort_key e)
;;

let op_symbol_name role ~index ~element =
  let role_str =
    match role with
    | Select -> "select"
    | Store -> "store"
  in
  Printf.sprintf "@arr.%s|%s|%s" role_str (sort_key index) (sort_key element)
;;

let role_equal a b =
  match a, b with
  | Select, Select | Store, Store -> true
  | (Select | Store), _ -> false
;;

let entry_equal a b =
  role_equal a.role b.role && Sort.equal a.index b.index && Sort.equal a.element b.element
;;

let add t sym role ~index ~element =
  let entry = { role; index; element } in
  match Id_map.find_opt (id sym) t.by_sym with
  | Some existing ->
    (* Idempotent on the identical entry; a genuine role/sort conflict is a front-end
       construction bug (a symbol is minted once per (role, index, element)). *)
    if entry_equal existing entry
    then t
    else invalid_arg "Array_defs: symbol already registered in a conflicting role"
  | None -> { by_sym = Id_map.add (id sym) entry t.by_sym }
;;

let role_of_sym t sym = Id_map.find_opt (id sym) t.by_sym
