(* Per-module SMOKE client (dead-law check, blueprint §6.7): a few-line goal
   per shipped Voption law, so each law has a forcing consumer. Verified
   against voption.cmi + VoxSig_Voption.olean only.
   - some_is_some    forces vo_is_some_some  (is_some (Vsome x) = true)
   - none_is_not_some forces vo_not_some_none (is_some Vnone = false)
   - get_or_some      forces vo_get_or_some   (get_or d (Vsome x) = x)
   - get_some         forces vo_get_some at `get`'s result and vo_is_some_some
     at its precondition (the model defs are not `expose`d, so both must fire
     as named laws -- liveness re-verified: dropping any one law breaks smoke).
   Post-#53 (finding C1): a constructor application (Vsome x / Vnone) is a
   reflectable expression, so it now passes INLINE to a dependent parameter --
   the C1 let-binds are removed here (see notes/voption.md).

   HOF section (WP-1): map/bind/filter/fold/is_some_and. Voption is an EXPOSED
   ADT, so EXACT per-element output IS available (map_some_exact / fold_some_exact
   below extract it) -- the exposed-container payoff the via-abstracted Vlist
   cannot offer. Relation/predicate goals name block abbrevs (no lambda in
   refinement text). *)
open Voption

[%%vox.lean {lean|
@[grind, expose] abbrev rSucc : Int -> Int -> Prop := fun a b => b = a + 1
@[grind, expose] abbrev rAdd  : Int -> Int -> Int -> Prop := fun a x c => c = a + x
@[grind, expose] abbrev pPos  : Int -> Prop := fun x => x > 0
|lean}]

let some_is_some : (x : int) -> bool{ _ = true } = fun x -> is_some (Vsome x)
let none_is_not_some : bool{ _ = false } = is_some Vnone
let get_or_some : (d : int) -> (x : int) -> int{ _ = x } = fun d x -> get_or d (Vsome x)
let get_some : (x : int) -> int{ _ = x } = fun x -> get (Vsome x)

(* map: result option is r-related to input (relational, symbolic). *)
let map_rel (o : t) : t{ vo_maprel rSucc o _ } =
  Voption.map (fun a b -> b = a + 1) (fun z -> z + 1) o
(* map: EXACT element output on a concrete Some (exposed-container payoff). *)
let map_some_exact (x : int) : int{ _ = x + 1 } =
  let o = Vsome x in
  let m = Voption.map (fun a b -> b = a + 1) (fun z -> z + 1) o in
  Voption.get m

(* bind: result carries the bind relation. *)
let bind_rel (o : t) : t{ vo_bindrel rSucc o _ } =
  Voption.bind (fun a b -> b = a + 1) (fun x -> Vsome (x + 1)) o

(* filter: result carries the filter relation for the reflected predicate. *)
let filter_rel (o : t) : t{ vo_filterrel pPos o _ } =
  Voption.filter (fun x -> x > 0) (fun x -> x > 0) o

(* fold: EXACT result on a concrete Some (add step). *)
let fold_some_exact (init : int) (x : int) : int{ _ = init + x } =
  let o = Vsome x in
  Voption.fold (fun a y c -> c = a + y) (fun a y -> a + y) init o
(* fold: None returns init. *)
let fold_none_exact (init : int) : int{ _ = init } =
  Voption.fold (fun a y c -> c = a + y) (fun a y -> a + y) init Vnone

(* is_some_and: bool equals the lifted predicate. *)
let some_and (o : t) : bool{ _ = vo_is_some_and pPos o } =
  Voption.is_some_and (fun x -> x > 0) (fun x -> x > 0) o

(* COMBINATOR-OF-COMBINATOR: total does NOT forward (a total param variable is
   not accepted where a total arg is required -- WP-0 finding). A client that
   chains map into filter must therefore supply CALL-SITE lambdas to each, not
   forward a shared relation param. This composes cleanly: *)
let map_then_filter (o : t) : t =
  let m = Voption.map (fun a b -> b = a + 1) (fun z -> z + 1) o in
  Voption.filter (fun x -> x > 0) (fun x -> x > 0) m
