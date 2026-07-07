(* Smoke client for Vrel: the user's sketched shape verified against
   Vrel.cmi + VoxSig_Vrel.olean only.  Each shipped combinator and every
   substrate def is exercised so none is dead (blueprint 6.7 liveness).  The
   relation is supplied at the CALL SITE as an ordinary OCaml lambda (task
   #68), reflected to a Lean [fun .. => ..] and substituted at the binder;
   grind unfolds the lifting def (rHolds / relIter / listRel / allP) against
   the substituted lambda.  Goals are plain arithmetic / a named predicate,
   because a lambda may NOT appear in refinement text -- only in argument
   position (lambda-reflection doc). *)

[@@@warning "-6-32-26-27"]

[%%vox.lean {lean|
@[grind, expose] abbrev isPos : Int -> Prop := fun x => x > 0
|lean}]

(* NORTH STAR (the user's sketch): iter (fun x y -> y >= x) (fun a -> a+1) x.
   Three <=-steps from x0 give x0 <= result; relIter unfolds at the concrete
   count and grind chains the transitivity. *)
let iter_le (x0 : int) : int{ x0 <= _ } =
  Vrel.iter (fun x y -> y >= x) (fun a -> a + 1) x0 3

(* iter with a strict-decrease relation and a different count. *)
let iter_gt (x0 : int) : int{ _ < x0 } =
  Vrel.iter (fun x y -> y < x) (fun a -> a - 1) x0 2

(* map: lambda at the call site, plain goal (listRel preserves length -> the
   result of mapping has the same length as the input). *)
let map_len (xs : Vrel.ilist) : int{ _ = il_len xs } =
  let ys = Vrel.map (fun a b -> a <= b) (fun a -> a + 1) xs in
  Vrel.length ys

(* COMPOSITION: an r-step then an s-step is an (rcomp r s)-step, over two
   lambdas; the composite consequence x < result follows. *)
let compose_lt (x : int) : int{ x < _ } =
  Vrel.compose2 (fun a b -> a <= b) (fun a b -> a < b)
    (fun a -> a) (fun a -> a + 1) x

(* fold: a <=-related step over a one-element list is one relIter step, so
   the initial accumulator a satisfies a <= result. *)
let fold_le (a : int) (b : int) : int{ a <= _ } =
  let l = Vrel.Icons (b, Vrel.Inil) in
  Vrel.fold (fun p q -> p <= q) (fun acc x -> acc + 1) a l

(* filter: every kept element is positive.  The predicate p is a lambda at
   the call site; the decision procedure test ties its bool to [pHolds p x];
   the goal names the block predicate isPos (= the lambda, unfolded). *)
let filter_pos (xs : Vrel.ilist) : Vrel.ilist{ allP isPos _ } =
  Vrel.filter (fun x -> x > 0) (fun x -> x > 0) xs

(* --- keep the relation-algebra combinators (rand / ror / rconverse) live:
   thin call-site combinators, each proving x <= result. --- *)

let step_and :
      (r : (int -> int -> bool)) -> (s : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds (rand r s) x _ })) ->
      (x : int) -> int{ rHolds (rand r s) x _ } =
  fun r s f x -> ignore (r, s); f x

let rand_le (x : int) : int{ x <= _ } =
  step_and (fun a b -> a <= b) (fun a b -> b <= a + 100) (fun a -> a + 1) x

let step_or :
      (r : (int -> int -> bool)) -> (s : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds (ror r s) x _ })) ->
      (x : int) -> int{ rHolds (ror r s) x _ } =
  fun r s f x -> ignore (r, s); f x

let ror_le (x : int) : int{ x <= _ } =
  step_or (fun a b -> a <= b) (fun a b -> a < b) (fun a -> a + 1) x

let step_conv :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds (rconverse r) x _ })) ->
      (x : int) -> int{ rHolds (rconverse r) x _ } =
  fun r f x -> ignore r; f x

let rconv_le (x : int) : int{ x <= _ } =
  step_conv (fun a b -> a >= b) (fun a -> a + 1) x
