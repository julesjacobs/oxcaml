(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

let (preserve @ total) :
    (p : (int @ immutable total -> bool @ ghost)) @ ghost ->
    (f : (int -> {y : int | p y})) @ total ->
    (x : int) -> {y : int | p y} = fun p f x ->
  let y = f x in y

let (cap @ total) : (limit : int) -> (input : int) ->
    {result : int | result <= limit} = fun limit input ->
  let p = ghost_ (fun (x : int) -> x <= limit) in
  let result = preserve p (fun x ->
    let result = if x < limit then x else limit in
    result) input in
  result

let (captured @ total) (limit : int) : {u : unit | true} =
  let p = ghost_ (fun (x : int) -> let difference = x - limit in
    difference === 0) in
  let old = limit in
  let limit = 0 in
  let u = () in
  let _proof : {u : unit | p old} = u in
  let _proof : {u : unit | p limit = (limit = old)} = u in
  u

let (matched @ total) (limit : int) : {u : unit | true} =
  let p = ghost_ (fun (values : int list) ->
    match values with [] -> false | head :: _ -> head === limit) in
  let nil = [] in
  let one = [limit] in
  let u = () in
  let _proof : {u : unit | not (p nil) && p one} = u in
  u

let (chosen @ total) (lower : bool) (value : int) : {u : unit | true} =
  let below = ghost_ (fun (x : int) -> x <= 0) in
  let above = ghost_ (fun (x : int) -> x >= 0) in
  let p = if lower then below else above in
  let u = () in
  let _proof : {u : unit | p value =
    (if lower then value <= 0 else value >= 0)} = u in
  u

let (tuple_pattern @ total) (value : int) : {u : unit | true} =
  let p = ghost_ (fun ((left, right) : int * int) -> left === right) in
  let pair = value, value in
  let u = () in
  let _proof : {u : unit | p pair} = u in
  u

let (specialized @ total) () =
  let empty = ghost_ (fun (u : unit) -> []) in
  let u = () in
  let integers : int list @ ghost = ghost_ (empty u) in
  let booleans : bool list @ ghost = ghost_ (empty u) in
  ghost_ (integers, booleans)

let () =
  let limit = 5 in
  let high = 9 in
  let low = -3 in
  let capped = cap limit high in
  let unchanged = cap limit low in
  assert (capped = 5);
  assert (unchanged = -3);
  print_endline "transparent ghost predicates: callback, capture, match, choice"
