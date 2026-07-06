(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: synthesis mode.  [refine_ e] with no refined expected type
   synthesizes the EXACT refinement {v:t | v = e'} where e' is the
   logic translation of e -- definitionally true, so no obligation. *)
let three = refine_ 3
[%%expect{|
val three : int{ _ = 3 } = 3
|}]

(* The div/safe example in the explicit-synthesis spelling (the
   current implicit spelling lives in infer.ml and DESIGN.md): the
   comparison is reflected by refine_, its binder contributes the fact,
   and the path fact discharges div's precondition. *)
let div (a : int) (b : int{ not (_ = 0) }) : int = a / (b :> int)
[%%expect{|
val div : int -> int{ not (_ = 0) } -> int = <fun>
|}]

let safe (x : int) : int =
  let c = refine_ (0 < x) in
  if (c :> bool) then div 100 (refine_ x) else 0
[%%expect{|
Line 3, characters 39-40: vox VC:
  goal: not (x = 0)
  hypotheses:
  c
  c = (0 < x)
  three = 3
val safe : int -> int = <fun>
|}]

(* Compound conditions are translated directly into path facts: no
   binding needed at all. *)
let safe2 (x : int) : int =
  if 0 < x then div 100 (refine_ x) else 0
[%%expect{|
Line 2, characters 33-34: vox VC:
  goal: not (x = 0)
  hypotheses:
  0 < x
  three = 3
val safe2 : int -> int = <fun>
|}]

(* Checking position also reflects compound expressions into the goal
   (rather than a fresh unknown). *)
let bump (x : int) : int{ _ > x } = refine_ (x + 1)
[%%expect{|
Line 1, characters 44-51: vox VC:
  goal: x + 1 > x
  hypotheses:
  three = 3
val bump : (x : int) -> int{ _ > x } = <fun>
|}]

(* Untranslatable expressions need an annotation. *)
let bad = refine_ (String.length "a")
[%%expect{|
Line 1, characters 10-37:
1 | let bad = refine_ (String.length "a")
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: refine_ cannot translate this expression into the logic (only variables, int/bool constants, tuples, immutable field reads, fst/snd, calls to total_ functions, + - * / mod ~-, comparisons at int or bool, && || not, and constructors and records of simple types are supported); add a refined type annotation
|}]

(* Synthesized exact refinements obey the scope rules: at the module
   level the type would mention the program variable [three]. *)
let leak = refine_ ((three :> int) + 1)
[%%expect{|
Line 1, characters 4-8:
1 | let leak = refine_ ((three :> int) + 1)
        ^^^^
Error: vox: the type of leak carries a refinement mentioning three, which may not appear in a module-level type; annotate with a dependent arrow ((three : ...) -> ...) or a self-contained refinement
|}]

(* Unary minus is the logic's [0 - e] (and [- INT] a negative literal;
   an operator shape the grammar does not know is an error, never a
   silent spec function). *)
let neg (x : int) : int{ _ = - x } = refine_ (0 - x)
[%%expect{|
Line 1, characters 45-52: vox VC:
  goal: 0 - x = 0 - x
  hypotheses:
  three = 3
val neg : (x : int) -> int{ _ = 0 - x } = <fun>
|}]

let m : int{ _ = -1 } = refine_ (0 - 1)
[%%expect{|
Line 1, characters 32-39: vox VC:
  goal: 0 - 1 = -1
  hypotheses:
  three = 3
val m : int{ _ = -1 } = -1
|}]
