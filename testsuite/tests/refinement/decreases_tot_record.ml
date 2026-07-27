(* A record of children is made of parts of the value matched, exactly as a
   variant of children is.  [Node { left; right }] binds two values that are
   inside the value it matched, so the recursion descends on them and the
   function is total with no measure written.

   This is a companion to the measured cases: it is the shape where a measure
   should not be needed at all. *)
let (expects_total @ total) (f @ total) = f

type tree =
  | Leaf
  | Node of { left : tree; right : tree }

let rec size (t : tree) : int =
  match t with
  | Leaf -> 1
  | Node { left; right } -> size left + size right

(* The record need not sit inside a constructor: it can be the whole value
   matched. *)
type chain = { kid : chain option }

let rec depth (c : chain) : int =
  match c with
  | { kid = None } -> 0
  | { kid = Some kid } -> 1 + depth kid

(* And it can be nested, in which case every field crossed on the way to the
   argument has to be immutable, not just the outermost one. *)
type outer = { inner : inner }
and inner = { next : outer option }

let rec steps (o : outer) : int =
  match o with
  | { inner = { next = None } } -> 0
  | { inner = { next = Some o } } -> 1 + steps o

(* One field crossed by both branches of an or-pattern written inside it.
   The field is reached once per branch, so it is asked about twice and both
   questions are about the same field. *)
type step = { next : hop }
and hop = Left of step | Right of step | Stop

let rec hops (s : step) : int =
  match s with
  | { next = (Left s | Right s) } -> 1 + hops s
  | { next = Stop } -> 0

let total_use () =
  expects_total size, expects_total depth, expects_total steps,
  expects_total hops
