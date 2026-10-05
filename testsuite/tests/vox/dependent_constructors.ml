(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "dependent_constructors.mli dependent_constructors.ml dependent_constructors_client.ml";
 { bytecode; }
 { native; }
*)

type _ bounded =
  | Bounded : {
      lower : int;
      value : {v : int | lower <= v};
    } -> int bounded
  | Empty : unit bounded

let (make @ total) (lower : int) (value : {v : int | lower <= v}) =
  Bounded { value; lower }

let (ordered @ total) (Bounded { lower; value }) : {b : bool | b} =
  lower <= value

let (ordered_alias @ total) (Bounded r) : {b : bool | b} =
  r.lower <= r.value

type minimum =
  | Minimum : {
      value : int;
      optimality : (other : {n : int | 0 <= n}) ->
        {u : unit | value <= other} @@ ghost total;
    } -> minimum

let (minimum @ total) () =
  Minimum {
    value = 0;
    optimality = ghost_ (fun _other -> let u = () in refine_ u);
  }

let (use @ total) (Minimum { value; optimality }) : {b : bool | b} =
  ghost_ (optimality 0);
  value <= 0

let () =
  assert (ordered (make 2 5));
  assert (ordered_alias (make 3 7));
  assert (use (minimum ()));
  let block = Obj.repr (minimum ()) in
  match Sys.backend_type with
  | Native -> assert (Obj.size block = 1)
  | Bytecode -> assert (Obj.size block = 2)
  | Other _ -> assert false

type interval = Interval of { low : int; high : {h : int | low <= h} }

let (ordinary @ total) (Interval { high; low }) : {b : bool | b} =
  low <= high

type packed = Pack : ('a : immediate). {
  first : 'a;
  second : {v : 'a | v === first};
} -> packed

let (packed_equal @ total) (Pack { first; second }) =
  ghost_ (let u = () in
    let _ : {u : unit | first === second} = refine_ u in ())

let packed = Pack { first = 42; second = 42 }

let (replace @ total) (Bounded r) =
  Bounded { r with value = r.lower }

let (choice @ total) (which : bool) (r : int bounded) : {b : bool | b} =
  match which, r with
  | true, Bounded { lower; value }
  | false, Bounded { value; lower } -> lower <= value

let evaluations = ref []
let eval_lower () : {v : int | v = 1} = evaluations := 1 :: !evaluations; 1
let eval_value () : {v : int | v = 2} = evaluations := 2 :: !evaluations; 2
let evaluated = Bounded {lower = eval_lower (); value = eval_value ()}

let () =
  assert (ordinary (Interval {low = 1; high = 2}));
  ghost_ (packed_equal packed);
  assert (ordered (replace (make 3 7)));
  assert (choice true (make 1 2));
  assert (choice false (make 1 2));
  assert (!evaluations = [1; 2]);
  assert (ordered evaluated)

module type Bounds = sig
  type t = Bounds : { lower : int; upper : {u : int | lower <= u} } -> t
  val make : (lower : int) -> (upper : {u : int | lower <= u}) -> t @@ total
end

module Bounds : Bounds = struct
  type t = Bounds : { lower : int; upper : {u : int | lower <= u} } -> t
  let (make @ total) (lower : int) (upper : {u : int | lower <= u}) =
    Bounds { lower; upper }
end

module Make (X : sig type t : immediate end) = struct
  type t = Pair : { first : X.t; second : {v : X.t | v === first} } -> t
  let (pair @ total) (first : X.t) = Pair {first; second = first}
  let (same @ total) (Pair {first; second}) =
    ghost_ (let u = () in
      let _ : {u : unit | first === second} = refine_ u in ())
end

module Int_pair = Make (struct type t = int end)
let () =
  let Bounds.Bounds { lower; upper } = Bounds.make 4 8 in
  assert (lower <= upper);
  ghost_ (Int_pair.same (Int_pair.pair 7));
  ()

type evidence =
  | First of { proof : unit @@ ghost }
  | Payload of int
  | Second of { proof : unit @@ ghost }

let (evidence_tag @ total) = function
  | First r -> ghost_ r.proof; 1
  | Payload _ -> 3
  | Second r -> ghost_ r.proof; 2

let (first @ total) () = First {proof = ghost_ ()}
let (second @ total) () = Second {proof = ghost_ ()}

let (copy @ total) = function
  | First r -> First r
  | Payload n -> Payload n
  | Second r -> Second r

let () =
  assert (evidence_tag (first ()) = 1);
  assert (evidence_tag (second ()) = 2);
  assert (evidence_tag (Payload 9) = 3);
  assert (Obj.is_int (Obj.repr (first ())));
  assert (Obj.is_int (Obj.repr (second ())))

type erased = Erased of {
  lower : int @@ ghost;
  upper : {u : int | lower <= u} @@ ghost;
}

let () =
  evaluations := [];
  let erased = Erased {lower = eval_lower (); upper = eval_value ()} in
  assert (!evaluations = [1; 2]);
  assert (Obj.is_int (Obj.repr erased))
