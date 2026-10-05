(* TEST
 has-z3;
 flags = "-extension refinement_types -extension layouts_beta";
 all_modules = "dependent_records.mli dependent_records.ml dependent_records_client.ml";
 { bytecode; }
 { native; }
*)

type interval = { lower : int; upper : {u : int | lower <= u} }

let interval = { upper = 5; lower = 2 }

let (upper @ total) (r : interval) : {u : int | r.lower <= u} = r.upper

let (ordered @ total) (r : interval) :
    {b : bool | b = (r.lower <= r.upper) && b} =
  r.lower <= r.upper

let (make @ total) (lower : int) (upper : {u : int | lower <= u}) =
  { lower; upper }

let (replace @ total) (r : interval) (upper : {u : int | r.lower <= u}) =
  { r with upper }

type minimum = {
  value : int;
  optimality : (other : {n : int | 0 <= n}) ->
    {u : unit | value <= other} @@ ghost total;
}

let (minimum @ total) () : minimum =
  { value = 0; optimality = ghost_ (fun _other -> let u = () in refine_ u) }

let (use @ total) (r : minimum) (other : {n : int | 0 <= n}) :
    {u : unit | r.value <= other} =
  ghost_ (r.optimality other);
  let u = () in refine_ u

module type Interval = sig
  type t = { lower : int; upper : {u : int | lower <= u} }
  val make : (lower : int) -> (upper : {u : int | lower <= u}) -> t @@ total
end

module Interval : Interval = struct
  type t = { lower : int; upper : {u : int | lower <= u} }
  let (make @ total) (lower : int) (upper : {u : int | lower <= u}) =
    { lower; upper }
end

let (module_upper @ total) (r : Interval.t) : {u : int | r.lower <= u} =
  r.upper

let () =
  assert (upper interval = 5);
  assert (ordered interval);
  assert ((replace interval 6).upper = 6);
  assert ((minimum ()).value = 0);
  let block = Obj.repr (minimum ()) in
  match Sys.backend_type with
  | Native -> assert (Obj.size block = 1)
  | Bytecode -> assert (Obj.size block = 2)
  | Other _ -> assert false

let (destructure @ total) (r : interval) : {u : int | r.lower <= u} =
  let {upper; _} = r in upper

let (use_pattern @ total) (r : minimum) (other : {n : int | 0 <= n}) :
    {u : unit | r.value <= other} =
  let {optimality; _} = r in
  ghost_ (optimality other);
  let u = () in refine_ u

let (temporary @ total) () : {u : unit | 0 <= 5} =
  ghost_ ((minimum ()).optimality 5);
  let u = () in refine_ u

type ('a : logical_data) pair = { first : 'a; second : {v : 'a | v === first} }

let (pair @ total) (first : ('a : logical_data)) : 'a pair =
  { first; second = first }

let (pair_equal @ total) (p : 'a pair) : {u : unit | p.first === p.second} =
  let u = () in refine_ u

module Reexport = struct
  type t = Interval.t = { lower : int; upper : {u : int | lower <= u} }
end

module Make (X : sig type t : logical_data end) = struct
  type t = { first : X.t; second : {v : X.t | v === first} }
  let (make @ total) (first : X.t) : t = { first; second = first }
  let (same @ total) (r : t) : {u : unit | r.first === r.second} =
    let u = () in refine_ u
end

module Int_pair = Make (struct type t = int end)
let () = ignore (Int_pair.make 4); ignore (pair ([1; 2] : int list))

type unboxed_interval = #{ low : int; high : {u : int | low <= u} }
let unboxed_interval = #{low = 1; high = 2}
let (unboxed_upper @ total) (r : unboxed_interval) : {u : int | r.#low <= u} =
  r.#high

let evaluations = ref []
let eval_lower () : {v : int | v = 1} = evaluations := 1 :: !evaluations; 1
let eval_upper () : {v : int | v = 2} = evaluations := 2 :: !evaluations; 2
let evaluated = {lower = eval_lower (); upper = eval_upper ()}
let () = assert (!evaluations = [1; 2]); assert (evaluated.upper = 2)

let (two_patterns @ total) (r : interval) (s : interval) :
    {u : unit | r.lower <= r.upper && s.lower <= s.upper} =
  let ({upper = ru; _}, {upper = su; _}) = r, s in
  let u = () in
  let _ : {u : unit | r.lower <= ru && s.lower <= su} = refine_ u in
  refine_ u

let (choice @ total) (r : interval) (which : bool) :
    {u : int | r.lower <= u} =
  match which, r with
  | (true, {upper; _}) | (false, {upper; _}) -> upper

let (swap_patterns @ total) (which : bool) (r : interval) (s : interval) :
    int * int =
  match which, r, s with
  | true, {upper = u; _}, {lower = l; _}
  | false, {lower = l; _}, {upper = u; _} -> l, u

let () =
  assert (swap_patterns true interval (make 3 7) = (3, 5));
  assert (swap_patterns false interval (make 3 7) = (2, 7))

type ('a : any) layout_record = { payload : 'a }
type float_record = float# layout_record
type void_type : void
type void_record = void_type layout_record

type ('a : any) layout_interval = {
  payload : 'a;
  layout_lower : int;
  layout_upper : {u : int | layout_lower <= u};
}
type float_interval = float# layout_interval

type ('a : any) shadowed_field = {
  x : 'a;
  y : {v : int | let x = 0 in x = v};
}
type int_shadowed = int shadowed_field
type float_shadowed = float# shadowed_field

let alias ({upper; _} as r) = upper, r

let zero_endpoint (r : interval) =
  match r with {upper = 0; _} | {lower = 0; _} -> true | _ -> false

let (bounded_zero @ total) (r : interval) : {b : bool | b} =
  match r with {upper = 0; lower} -> lower <= 0 | _ -> true

let (constant_only @ total) (r : interval) : {b : bool | b} =
  match r with {upper = 0; _} -> r.lower <= 0 | _ -> true

let zero_endpoint_binding (r : interval) =
  let ({upper = 0; _} | {lower = 0; _} | _) = r in ()

let () =
  assert (fst (alias interval) = 5);
  assert (zero_endpoint (make 0 1));
  assert (zero_endpoint (make (-1) 0));
  assert (not (zero_endpoint interval));
  assert (bounded_zero (make (-1) 0));
  assert (constant_only (make (-1) 0));
  zero_endpoint_binding (make 0 1)

type mutable_record = { mutable n : int }
let mutable_pattern (r : mutable_record) : {x : int | x = 0} =
  let {n} = r in if n = 0 then n else 0

type unboxed_record = { n : int } [@@unboxed]
let (unboxed_pattern @ total) (r : unboxed_record) : {x : int | x = 0} =
  let {n} = r in if n = 0 then n else 0

let () =
  assert (mutable_pattern {n = 3} = 0);
  assert (unboxed_pattern {n = 0} = 0)
