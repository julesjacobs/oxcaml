(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: the poly-refined-value unblock (lean_poly_refine.ml) must FAIL
   CLOSED.  Refining a value of type ['a] with a predicate is now an
   honest proof OBLIGATION, not a free pass: an unprovable poly
   refinement reaches the VC layer (no more occurs-check elaboration
   error) and is rejected as NOT PROVED -- you cannot forge a poly
   refinement.  Positive twin: lean_poly_refine.ml. *)

[@@@warning "-6-32-26-27"]

type 'a ord [@@vox.sort lean "POrd"]

[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ole {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]

(* The result is claimed <= itself, but reflexivity is NOT given.  The
   ascription now ELABORATES (formerly: "type variable 'a occurs inside
   'a{...}") and the obligation is refuted at the VC layer. *)
let pick_bad : (o : 'a ord) -> (a : 'a) -> 'a{ ole o _ a } = fun _o a -> a
[%%expect{|
type 'a ord
Line 13, characters 73-74:
13 | let pick_bad : (o : 'a ord) -> (a : 'a) -> 'a{ ole o _ a } = fun _o a -> a
                                                                              ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: ole _o a a
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* A forged order fact between two distinct poly elements: nothing ties
   [a] and [b], so [ole o a b] is unprovable. *)
let forged : (o : 'a ord) -> (a : 'a) -> (b : 'a) -> 'a{ ole o a _ } = fun _o a b -> b
[%%expect{|
Line 1, characters 85-86:
1 | let forged : (o : 'a ord) -> (a : 'a) -> (b : 'a) -> 'a{ ole o a _ } = fun _o a b -> b
                                                                                         ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: ole _o a b
Hypotheses: <none>
(lean: error: `grind` failed)
|}]
