(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
*)
type function_type = int @ total -> int @ total
let[@def] (observed @ total) (f : function_type @ total) (x : int) = ghost_ (f x >= 0)
let (identity @ total) : (f : function_type) @ total -> (x : int) ->
  {u : unit | observed f x} -> {u : unit | observed f x} @ ghost =
  fun _f _x premise -> ghost_ (let refine_ premise = premise in refine_ premise)
let wrapped : (f : function_type Ghost.t) @ total ->
    (x : {n : int | observed f.Ghost.ghost n}) -> unit = fun f x ->
  let refine_ x = x in
  ghost_ (let u = () in identity f.Ghost.ghost x (refine_ u); ()); ()


type callbacks = { first : function_type; second : function_type; stamp : int }

let known () =
  let positive = ghost_ (fun (_ : int) -> 1) in
  let negative = ghost_ (fun (_ : int) -> -1) in
  let record = ghost_ {first = positive; second = negative; stamp = 7} in
  ghost_ (let u = () in
    let proof : {u : unit | record.first 0 === 1 && record.second 0 === -1
      && record.stamp === 7} = refine_ u in let refine_ proof = proof in ()); ()

let replaced () =
  let positive = ghost_ (fun (_ : int) -> 1) in
  let negative = ghost_ (fun (_ : int) -> -1) in
  let record = ghost_ {first = positive; second = negative; stamp = 7} in
  let changed = ghost_ {record with first = negative; stamp = 9} in
  ghost_ (let u = () in
    let proof : {u : unit | changed.first 0 === -1 && changed.second 0 === -1
      && changed.stamp === 9} = refine_ u in let refine_ proof = proof in ()); ()

let chosen choose =
  let positive = ghost_ (fun (_ : int) -> 1) in
  let negative = ghost_ (fun (_ : int) -> -1) in
  let left = ghost_ {first = positive; second = negative; stamp = 7} in
  let right = ghost_ {first = negative; second = positive; stamp = 9} in
  let record = ghost_ (if choose then left else right) in
  ghost_ (let u = () in
    let proof : {u : unit | record.first 0 === (if choose then 1 else -1)
      && record.second 0 === (if choose then -1 else 1)
      && record.stamp === (if choose then 7 else 9)} = refine_ u in
    let refine_ proof = proof in ()); ()

let adapter : (f : function_type Ghost.t) @ total ->
    (x : {n : int | observed f.Ghost.ghost n}) -> unit = fun f x ->
  let refine_ x = x in
  ghost_ (let raw = f.Ghost.ghost in let u = () in identity raw x (refine_ u); ()); ()

let () = known (); replaced (); chosen true; chosen false

let same (r : callbacks @ immutable total) (x : int) :
    {u : unit | r.first x === r.first x} =
  let u = () in refine_ u
