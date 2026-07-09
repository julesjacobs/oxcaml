(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: a POLYMORPHIC value (type ['a]) may be refined by a predicate.
   Previously ['a] checked against ['a{ p }] hit a unification occurs
   check ('a occurs inside 'a{p}); a concrete [int{ p }] had no such
   trouble.  The refined-ascription path now treats a value whose type
   IS the refined skeleton (a type variable) exactly like the concrete
   case: it keeps the skeleton and emits [p] as a proof obligation.

   This unblocks ORDERED ACCESSORS that RETURN a key -- [min]/[max]/
   [floor]/[ceiling] -- over an abstract element, with the order carried
   as a [@vox.total] value (the eq-param / parameter-style route).  The
   fail twin is lean_poly_refine_fail.ml. *)

[@@@warning "-6-32-26-27"]

[%%vox.lean {lean|
@[grind, expose] def ole {a : Type} (o : a -> a -> Prop) (x y : a) : Prop := o x y
@[grind, expose] def oRefl {a : Type} (o : a -> a -> Prop) : Prop := forall x, o x x
abbrev intLe : Int -> Int -> Prop := fun a b => a <= b
|lean}]

(* A poly refined RESULT that mentions the argument, discharged from a
   threaded order axiom (reflexivity): the result is <= itself. *)
let self_le (o : (('a -> 'a -> bool) [@vox.total])) (a : 'a)
    : 'a{ oRefl o -> ole o _ a } =
  ignore o;
  a

(* min of two over an ABSTRACT element: the result is <= BOTH inputs,
   proved from the comparator's contract and reflexivity.  The order is
   a [@vox.total] value; the comparator ties its bool to the order. *)
let min2 (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp :
       ((x : 'a) -> (y : 'a) ->
        bool{ (_ = true -> ole o x y) && (_ = false -> ole o y x) }))
    (a : 'a) (b : 'a) : 'a{ oRefl o -> (ole o _ a && ole o _ b) } =
  ignore o;
  if cmp a b then a else b

(* The weaker minimality (result <= at least one input) needs no
   reflexivity, so it DISCHARGES end to end at a concrete [int] client
   through the value bridge: [<=] supplied as the total order. *)
let min2_weak (o : (('a -> 'a -> bool) [@vox.total]))
    (cmp :
       ((x : 'a) -> (y : 'a) ->
        bool{ (_ = true -> ole o x y) && (_ = false -> ole o y x) }))
    (a : 'a) (b : 'a) : 'a{ ole o _ a || ole o _ b } =
  ignore o;
  if cmp a b then a else b

let demo_int (a : int) (b : int) : int{ ole intLe _ a || ole intLe _ b } =
  min2_weak (fun p q -> p <= q) (fun x y -> x <= y) a b
