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

(* ============================ EXACT-OUTPUT demos ============================
   Picking the relation to be the callback's GRAPH turns each relational spec
   into a COMPLETE (exact) one.  These exercise the shipped [_exact] laws +
   the ihead/itail/il_sum accessors; every positive verifies, and the
   corresponding negative controls (wrong constant) fail closed -- their
   observed NOT-PROVED output is recorded in notes/vrel.md §5. *)

(* iter, CONCRETE count: graph (y = x+1), three steps -> exactly x0 + 3. *)
let iter_exact3 (x0 : int) : int{ _ = x0 + 3 } =
  Vrel.iter (fun x y -> y = x + 1) (fun a -> a + 1) x0 3

(* iter, NEGATIVE count clamps at zero via toNat -> exactly x0. *)
let iter_clamp (x0 : int) : int{ _ = x0 } =
  Vrel.iter (fun x y -> y = x + 1) (fun a -> a + 1) x0 (-5)

(* iter, SYMBOLIC count: relIter_succ_exact (rides the olean) fires on the
   relIter hypothesis and discharges the graph premise by beta. *)
let iter_exact_k (x0 : int) (k : int) : int{ k >= 0 -> _ = x0 + k } =
  Vrel.iter (fun x y -> y = x + 1) (fun a -> a + 1) x0 k

(* map, SYMBOLIC nonempty: head of the mapped list is exactly head+1 (the
   pointwise-exact consequence rides on the exposed listRel, no extra law). *)
let map_head_exact (a : int) (s : Vrel.ilist) : Vrel.ilist{ ihead _ = a + 1 } =
  let xs = Vrel.Icons (a, s) in
  Vrel.map (fun x y -> y = x + 1) (fun x -> x + 1) xs

(* map, CONCRETE: map (+1) over [1;2] gives head 2 and second element 3. *)
let map_concrete (u : unit) : Vrel.ilist{ ihead _ = 2 && ihead (itail _) = 3 } =
  ignore u;
  let xs = Vrel.Icons (1, Vrel.Icons (2, Vrel.Inil)) in
  Vrel.map (fun x y -> y = x + 1) (fun x -> x + 1) xs

(* fold3, SUM: ternary graph (acc' = acc + x) from 0 -> exactly il_sum xs.
   relFold_sum_exact fires on the relFold hypothesis. *)
let fold3_sum (xs : Vrel.ilist) : int{ _ = il_sum xs } =
  Vrel.fold3 (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) 0 xs

(* fold3, COUNT: ternary graph (acc' = acc + 1) from 0 -> exactly il_len xs. *)
let fold3_count (xs : Vrel.ilist) : int{ _ = il_len xs } =
  Vrel.fold3 (fun acc x acc' -> acc' = acc + 1) (fun acc x -> acc + 1) 0 xs

(* fold3, NONZERO init: exact output = init + il_sum xs. *)
let fold3_sum_from (init : int) (xs : Vrel.ilist) : int{ _ = init + il_sum xs } =
  Vrel.fold3 (fun acc x acc' -> acc' = acc + x) (fun acc x -> acc + x) init xs
