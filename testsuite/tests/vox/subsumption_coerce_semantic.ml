(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Semantic coercions (DESIGN.md 3.4): (e :> T) where T's refinements must be
   proved.  Typing records a Texp_subsumption extra and the verifier walks the
   pair of types in the current state.  Identity at run time.  Each block is
   the output after stages 1-4; comments say what trunk does. *)

let (g @ total) (x : int) : {r : int | r > 0} = if x > 0 then x else 1
let (d @ total) (x : int) : {r : int | r >= x} = x
let (s0 @ total) (x : int) : {r : int | r >= 0} = if x >= 0 then x else 0
let l : {x : int | x > 0} list = [1; 2]
let p : {x : int | x > 0} * {y : int | y > 1} = (1, 2)
let o : {x : int | x > 0} option = Some 3
let one : {x : int | x > 0} = 1
let r : {x : int | x > 0} ref = { contents = one };;
[%%expect{|
val g : int -> {r : int | r > 0} = <fun>
val d : (x : int) -> {r : int | r >= x} = <fun>
val s0 : int -> {r : int | r >= 0} = <fun>
val l : {x : int | x > 0} list = [1; 2]
val p : {x : int | x > 0} * {y : int | y > 1} = (1, 2)
val o : {x : int | x > 0} option = Some 3
val one : {x : int | x > 0} = 1
val r : {x : int | x > 0} ref = {contents = 1}
|}]

(* [stage 4] A weaker result contract, also through List.map.
   Currently: 'Type "int -> {r : int | r > 0}" is not a subtype of
   "int -> {r : int | r >= 0}"'. *)
let h3 = (g :> int -> {r : int | r >= 0})
let h4 xs = List.map (g :> int -> {r : int | r >= 0}) xs;;
[%%expect{|
val h3 : int -> {r : int | r >= 0} = <fun>
val h4 : int list -> {r : int | r >= 0} list = <fun>
|}]

(* [stage 4] Refined data under covariant constructors.
   Currently: e.g. 'Type "{x : int | x > 0} list" is not a subtype of
   "{x : int | x >= 0} list"'. *)
let l2 = (l :> {x : int | x >= 0} list)
let p3 = (p :> {x : int | x >= 0} * {y : int | y > 0})
let o3 = (o :> {x : int | x >= 0} option);;
[%%expect{|
val l2 : {x : int | x >= 0} list = [1; 2]
val p3 : {x : int | x >= 0} * {y : int | y > 0} = (1, 2)
val o3 : {x : int | x >= 0} option = Some 3
|}]

(* [stage 4] A dependent contract restated with other binder names.
   Currently: 'Type "(x : int) -> {r : int | r >= x}" is not a subtype of
   "(y : int) -> {s : int | y <= s}"'. *)
let d4 = (d :> (y : int) -> {s : int | y <= s});;
[%%expect{|
val d4 : (y : int) -> {s : int | y <= s} = <fun>
|}]

(* [stage 4] A callback contract (e16 of subsumption.md, which refine_
   cannot adapt).
   Currently: 'Type "(int -> {r : int | r >= 0}) -> int" is not a subtype of
   "(int -> {r : int | r > 0}) -> int"'. *)
let apply (k : int -> {r : int | r >= 0}) = k 0
let apply2 = (apply :> (int -> {r : int | r > 0}) -> int);;
[%%expect{|
val apply : (int -> {r : int | r >= 0}) -> int = <fun>
val apply2 : (int -> {r : int | r > 0}) -> int = <fun>
|}]

(* [stage 4] The coercion's facts are available afterwards.
   Currently: 'Type "{x : int | x > 0}" is not a subtype of
   "{x : int | x >= 0}"'. *)
let first (xs : {x : int | x > 0} list) : {r : int | r >= 0} =
  match (xs :> {x : int | x >= 0} list) with [] -> 0 | x :: _ -> x;;
[%%expect{|
val first : {x : int | x > 0} list -> {r : int | r >= 0} = <fun>
|}]

(* [stage 4] Strengthening fails.  s0 is defined in an earlier phrase, which
   the expect tool verifies separately, so its result is a fresh symbol named
   after the target's binder r (in one unit it is the reflected term s0 x).
   Currently: 'Type "int -> {r : int | r >= 0}" is not a subtype of
   "int -> {r : int | r > 0}"'. *)
let s1 = (s0 :> int -> {r : int | r > 0});;
[%%expect{|
Line 1, characters 9-41:
1 | let s1 = (s0 :> int -> {r : int | r > 0});;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: r = 0)
Line 1, characters 34-39:
1 | let s1 = (s0 :> int -> {r : int | r > 0});;
                                      ^^^^^
  The refinement is stated here.
|}]

(* [stage 4] Wrapping: r >= x does not imply r > x - 1 at x = min_int (r is
   a fresh symbol, as for s1).
   Currently: 'Type "(x : int) -> {r : int | r >= x}" is not a subtype of
   "(x : int) -> {r : int | r > (x - 1)}"'. *)
let d5 = (d :> (x : int) -> {r : int | r > x - 1});;
[%%expect{|
Line 1, characters 9-50:
1 | let d5 = (d :> (x : int) -> {r : int | r > x - 1});;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: r = -4611686018427387904, x = -4611686018427387904)
Line 1, characters 39-48:
1 | let d5 = (d :> (x : int) -> {r : int | r > x - 1});;
                                           ^^^^^^^^^
  The refinement is stated here.
|}]

(* [unchanged] ref is invariant; no proof is requested.
   Currently: the same error. *)
let r3 = (r :> {x : int | x >= 0} ref);;
[%%expect{|
Line 1, characters 9-38:
1 | let r3 = (r :> {x : int | x >= 0} ref);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "{x : int | x > 0} ref" is not a subtype of "{x : int | x >= 0} ref"
|}]

(* [stage 4] Root mode condition: the coerced value must be total,
   stateless and portable, as for any introduction (compare "let h : {f :
   int -> int | true} = k3", rejected the same way today).  The first
   failing axis is reported.
   Currently: 'Type "int -> int" is not a subtype of
   "{f : int -> int | true}"'. *)
let (k3 @ nonportable) = fun (x : int) -> x
let k4 = (k3 :> {f : int -> int | true});;
[%%expect{|
val k3 : int -> int = <fun>
Line 2, characters 10-12:
2 | let k4 = (k3 :> {f : int -> int | true});;
              ^^
Error: This value is "partial" but is expected to be "total".
|}]

(* [stage 4] The same for a partial closure.
   Currently: 'Type "int -> int" is not a subtype of
   "{f : int -> int | true}"'. *)
let partial (x : int) = if x > 0 then x else failwith "negative"
let k5 = (partial :> {f : int -> int | true});;
[%%expect{|
val partial : int -> int = <fun>
Line 2, characters 10-17:
2 | let k5 = (partial :> {f : int -> int | true});;
              ^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* [stage 4] Nested mode condition: the result of mk is a stateful closure;
   subtype_rec checks the source's return mode (DESIGN.md 3.1, the
   arrow-mode gap).
   Currently: 'Type "unit -> int -> int" is not a subtype of
   "unit -> {f : int -> int | true}"'. *)
let mk () = let c = ref 0 in fun (x : int) -> c := !c + x; !c
let mk2 = (mk :> unit -> {f : int -> int | true});;
[%%expect{|
val mk : unit -> int -> int = <fun>
Line 2, characters 10-49:
2 | let mk2 = (mk :> unit -> {f : int -> int | true});;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "unit -> int -> int" is not a subtype of
         "unit -> {f : int -> int | true}"
       The refined type "{f : int -> int | true}" requires values that are
       "total", "stateless" and "portable", but at this position they may be
       "partial".
|}]
