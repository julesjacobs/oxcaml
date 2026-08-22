(* TEST
 flags = "-extension let_mutable -drefinements";
 expect;
*)

(* Vox refinement flow: introduction strips and expectation obligations.
   Introductions (binders, occurrences, application results, field reads,
   patterns) use a refined value at its payload; expectations (annotations,
   arguments, codomains, assignments) check against the payload and record
   the site as an obligation in the typed tree.  No verification conditions
   are generated; every obligation is recorded and accepted.

   RED pinned the type-formers rejections these rules lift; this GREEN state
   is the accepting behaviour, so the RED-to-GREEN diff is exactly the set of
   programs this piece admits.

   The probe (-drefinements) runs over every fixture: it prints one line per
   recorded obligation marker (the complete obligation map — apply arguments
   are recorded on the function arrow instead, so they have no line), every
   expression node whose type has a refined head, and every pattern-bound
   variable whose environment entry has one.  The head/entry reports are
   empty except where pinned — the binder-strip exemptions (module-level and
   mutable entries), late-solved binders, and the variable-solving residue
   fixtures. *)

(* --- Occurrence strip + expectation funnel --------------------------- *)

(* Module-level binding: the declared type must survive to the signature
   (binder-strip exemption), so [val v : int{ _ > 0 }] prints; uses are
   covered by the occurrence strip. *)
let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 4-5: refined environment entry: v : int{ _ > 0 }
Line 1, characters 23-24: refinement obligation: int{ _ > 0 }
val v : int{ _ > 0 } = 5
|}]

(* the occurrence-strip discriminator: [v]'s environment entry is refined *)
let use_v = v + 1;;
[%%expect{|
val use_v : int = 6
|}]

(* Local binding: the binder strip puts [u : int] in the environment; the
   annotation is a funnel obligation on the right-hand side. *)
let use_u = let u : int{ _ > 0 } = 5 in u + 1;;
[%%expect{|
Line 1, characters 35-36: refinement obligation: int{ _ > 0 }
val use_u : int = 6
|}]

(* one obligation record per site: the binding's ghost rewrap does not
   duplicate the annotation's record (a single line for [v] and [u] above),
   while a distinct user-written constraint under the annotation records
   separately — two obligations here *)
let dbl : int{ _ > 0 } = (5 : int{ _ >= 0 });;
[%%expect{|
Line 1, characters 4-7: refined environment entry: dbl : int{ _ > 0 }
Line 1, characters 26-27: refinement obligation: int{ _ > 0 }
Line 1, characters 26-27: refinement obligation: int{ _ >= 0 }
val dbl : int{ _ > 0 } = 5
|}]

(* --- No propagation: a refinement is never invented by inference ----- *)

let w = v;;
[%%expect{|
val w : int = 5
|}]

let f c x = if c then x else v;;
[%%expect{|
val f : bool -> int -> int = <fun>
|}]

(* --- Apply-result strip ----------------------------------------------- *)

(* the definition is also the arrow-annotated codomain obligation *)
let g : unit -> int{ _ > 0 } = fun () -> 5;;
[%%expect{|
Line 1, characters 41-42: refinement obligation: int{ _ > 0 }
val g : unit -> int{ _ > 0 } = <fun>
|}]

let use_g = g () + 1;;
[%%expect{|
val use_g : int = 6
|}]

let x = g ();;
[%%expect{|
val x : int = 5
|}]

(* --- Argument obligations --------------------------------------------- *)

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
val f1 : int{ _ > 0 } -> int = <fun>
|}]

let a1 = f1 5;;
[%%expect{|
val a1 : int = 5
|}]

(* a second use of the same contract *)
let a2 = f1 6;;
[%%expect{|
val a2 : int = 6
|}]

(* labelled *)
let f2 : lbl:int{ _ > 0 } -> int = fun ~lbl -> lbl;;
[%%expect{|
val f2 : lbl:int{ _ > 0 } -> int = <fun>
|}]

let a3 = f2 ~lbl:5;;
[%%expect{|
val a3 : int = 5
|}]

(* commuted *)
let f3 : a:int{ _ > 0 } -> b:int -> int = fun ~a ~b -> a + b;;
[%%expect{|
val f3 : a:int{ _ > 0 } -> b:int -> int = <fun>
|}]

let a4 = f3 ~b:2 ~a:5;;
[%%expect{|
val a4 : int = 7
|}]

(* optional: the default and a supplied value are both obligations *)
let f4 : ?o:int{ _ > 0 } -> unit -> int = fun ?(o = 1) () -> o;;
[%%expect{|
Line 1, characters 52-53: refinement obligation: int{ _ > 0 }
val f4 : ?o:int{ _ > 0 } -> unit -> int = <fun>
|}]

let a5 = f4 ~o:5 ();;
[%%expect{|
Line 1, characters 15-16: refinement obligation: int{ _ > 0 }
val a5 : int = 5
|}]

let a6 = f4 ();;
[%%expect{|
val a6 : int = 1
|}]

(* --- Codomain obligations ---------------------------------------------- *)

(* the arrow-annotated form is [g] above; the return-annotation form
   funnels the codomain expectation into each arm *)
let k c : int{ _ > 0 } = if c then 1 else 2;;
[%%expect{|
Line 1, characters 25-43: refinement obligation: int{ _ > 0 }
val k : bool -> int{ _ > 0 } = <fun>
|}]

(* --- Aliases on both sides -------------------------------------------- *)

type nat = int{ _ >= 0 };;
[%%expect{|
type nat = int{ _ >= 0 }
|}]

let n : nat = 5;;
[%%expect{|
Line 1, characters 4-5: refined environment entry: n : nat
Line 1, characters 14-15: refinement obligation: nat
val n : nat = 5
|}]

(* the alias name is lost from the stripped occurrence: [int], not [nat] *)
let use_n = n + 1;;
[%%expect{|
val use_n : int = 6
|}]

let f5 : nat -> int = fun z -> z;;
[%%expect{|
val f5 : nat -> int = <fun>
|}]

let a7 = f5 5;;
[%%expect{|
val a7 : int = 5
|}]

(* apply-result strip through the alias: the head must be expanded before
   the gate decides *)
let gn : unit -> nat = fun () -> 5;;
[%%expect{|
Line 1, characters 33-34: refinement obligation: nat
val gn : unit -> nat = <fun>
|}]

let use_gn = gn () + 1;;
[%%expect{|
val use_gn : int = 6
|}]

(* --- Nested refinements flow via the expectation ----------------------- *)

let l : int{ _ > 0 } list = [5; v];;
[%%expect{|
Line 1, characters 29-30: refinement obligation: int{ _ > 0 }
Line 1, characters 32-33: refinement obligation: int{ _ > 0 }
val l : int{ _ > 0 } list = [5; 5]
|}]

(* still a rigid clash: stripping is head-only *)
let bad_l = (l : int list);;
[%%expect{|
Line 1, characters 13-14:
1 | let bad_l = (l : int list);;
                 ^
Error: The value "l" has type "int{ _ > 0 } list"
       but an expression was expected of type "int list"
       Type "int{ _ > 0 }" is not compatible with type "int"
|}]

(* --- Higher-order ------------------------------------------------------- *)

let mapped = List.map f1 l;;
[%%expect{|
val mapped : int list = [5; 5]
|}]

(* --- Mutability --------------------------------------------------------- *)

(* mutable binders are exempt from the binder strip: the write checks
   against the declared type (funnel), the read strips (occurrence) *)
let use_mv =
  let mutable mv : int{ _ > 0 } = 5 in
  mv <- 6;
  mv + 1;;
[%%expect{|
Line 2, characters 14-16: refined environment entry: mv : int{ _ > 0 }
Line 2, characters 34-35: refinement obligation: int{ _ > 0 }
Line 3, characters 8-9: refinement obligation: int{ _ > 0 }
val use_mv : int = 7
|}]

(* the [ref 5] spelling sits on the rigid-clash side of the order-dependent
   margin: the application solves the ref's parameter to [int] before the
   result meets the refined expectation (see the design doc's decisions) *)
let use_r1 =
  let r : int{ _ > 0 } ref = ref 5 in
  r := 6;
  !r + 1;;
[%%expect{|
Line 2, characters 29-34:
2 |   let r : int{ _ > 0 } ref = ref 5 in
                                 ^^^^^
Error: This expression has type "int ref"
       but an expression was expected of type "int{ _ > 0 } ref"
       Type "int" is not compatible with type "int{ _ > 0 }"
|}]

(* the record syntax propagates the expectation into the field, so the
   round trip typechecks: write via [:=] instantiation, read via [!] *)
let use_r2 =
  let r : int{ _ > 0 } ref = { contents = 5 } in
  r := 6;
  !r + 1;;
[%%expect{|
Line 2, characters 42-43: refinement obligation: int{ _ > 0 }
val use_r2 : int = 7
|}]

(* a mutable record field: create, write, read *)
type cell = { mutable c : int{ _ > 0 } };;
[%%expect{|
type cell = { mutable c : int{ _ > 0 }; }
|}]

let cl = { c = 5 };;
[%%expect{|
Line 1, characters 15-16: refinement obligation: int{ _ > 0 }
val cl : cell = {c = 5}
|}]

let () = cl.c <- 6;;
[%%expect{|
Line 1, characters 17-18: refinement obligation: int{ _ > 0 }
|}]

let use_c = cl.c + 1;;
[%%expect{|
val use_c : int = 7
|}]

(* --- Destructuring ------------------------------------------------------ *)

let use_ab = let (a, b) : (int * int){ true } = (1, 2) in a + b;;
[%%expect{|
Line 1, characters 48-54: refinement obligation: (int * int){ true }
val use_ab : int = 3
|}]

(* --- Export and inclusion ---------------------------------------------- *)

module M : sig val mv : int{ _ > 0 } end = struct
  let mv : int{ _ > 0 } = 5
end;;
[%%expect{|
Line 2, characters 6-8: refined environment entry: mv : int{ _ > 0 }
Line 2, characters 26-27: refinement obligation: int{ _ > 0 }
module M : sig val mv : int{ _ > 0 } end
|}]

let use_m = M.mv + 1;;
[%%expect{|
val use_m : int = 6
|}]

(* a bare definition does not satisfy a refined signature: the fix is to
   annotate the definition *)
module N : sig val nv : int{ _ > 0 } end = struct
  let nv = 5
end;;
[%%expect{|
Lines 1-3, characters 43-3:
1 | ...........................................struct
2 |   let nv = 5
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val nv : int end
       is not included in
         sig val nv : int{ _ > 0 } end
       Values do not match:
         val nv : int
       is not included in
         val nv : int{ _ > 0 }
       The type "int" is not compatible with the type "int{ _ > 0 }"
|}]

(* --- Recursion ----------------------------------------------------------- *)

let rec fact (y : int{ _ > 0 }) : int =
  if y <= 1 then 1 else y * fact (y - 1);;
[%%expect{|
val fact : int{ _ > 0 } -> int = <fun>
|}]

let use_fact = fact 5;;
[%%expect{|
val use_fact : int = 120
|}]

(* --- Still rejected ------------------------------------------------------ *)

(* dependent-arrow consumption *)
external d : m:int{ m > 0 } -> int = "%identity";;
[%%expect{|
external d : m:int{ m > 0 } -> int = "%identity"
|}]

let bad_dep = d 5;;
[%%expect{|
Line 1, characters 16-17:
1 | let bad_dep = d 5;;
                    ^
Error: Functions with a dependent arrow type cannot be defined or
       applied yet: the introduction and elimination rules for
       refinements are not implemented.
|}]

(* coercion never imposes an obligation *)
let bad_coerce = (5 :> int{ _ > 0 });;
[%%expect{|
Line 1, characters 17-36:
1 | let bad_coerce = (5 :> int{ _ > 0 });;
                     ^^^^^^^^^^^^^^^^^^^
Error: Type "int" is not a subtype of "int{ _ > 0 }"
|}]

(* --- The variable-solving residue, pinned ------------------------------- *)

(* the apply node is built while [g]'s codomain is undetermined; the later
   annotation solves it to the refined head, which the strip cannot reach *)
let f7 g = (g (), (g : unit -> int{ _ > 0 }));;
[%%expect{|
Line 1, characters 12-16: refined head on expression: int{ _ > 0 }
val f7 : (unit -> int{ _ > 0 }) -> int{ _ > 0 } * (unit -> int{ _ > 0 }) =
  <fun>
|}]

(* swapped: the annotation solves the codomain first, so the strip fires *)
let f8 g = ((g : unit -> int{ _ > 0 }), g ());;
[%%expect{|
val f8 : (unit -> int{ _ > 0 }) -> (unit -> int{ _ > 0 }) * int = <fun>
|}]

(* --- Class instance variables and methods ------------------------------- *)

(* a refined instance-variable annotation is rejected loudly: silently
   stripping it would record the bare payload in the class type — the
   initializer obligated but every later write unchecked *)
class cv = object val v : int{ _ > 0 } = 5 method g = v end;;
[%%expect{|
Line 1, characters 22-23:
1 | class cv = object val v : int{ _ > 0 } = 5 method g = v end;;
                          ^
Error: The instance variable "v" is annotated with the refined type "int{
                                                                    _ > 0 }".
       Refinement types are not supported on instance variables.
|}]

(* the mutable spelling is rejected the same way *)
class cc = object
  val mutable iv : int{ _ > 0 } = 5
  method get = iv
  method set n = iv <- n
end;;
[%%expect{|
Line 2, characters 14-16:
2 |   val mutable iv : int{ _ > 0 } = 5
                  ^^
Error: The instance variable "iv" is annotated with the refined type "int{
                                                                    _ > 0 }".
       Refinement types are not supported on instance variables.
|}]

(* a refined method type: out of scope for this piece — the rigid clash
   stays loud rather than silently dropping the contract *)
class cm = object method m : int{ _ > 0 } = 5 end;;
[%%expect{|
Line 1, characters 44-45: refinement obligation: int{ _ > 0 }
class cm : object method m : int{ _ > 0 } end
|}]

let cm_use = (new cm)#m + 1;;
[%%expect{|
Line 1, characters 13-23:
1 | let cm_use = (new cm)#m + 1;;
                 ^^^^^^^^^^
Error: This method call has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

(* the let-detour around the out-of-scope send: the send result binds as a
   late-solved entry (the probe pins it and the residue node) and the
   occurrence strip makes the use payload-typed — a sound, fact-true
   acceptance where the direct use stays a loud clash *)
let cm_detour = let z = (new cm)#m in z + 1;;
[%%expect{|
Line 1, characters 20-21: refined environment entry: z : int{ _ > 0 }
Line 1, characters 24-34: refined head on expression: int{ _ > 0 }
val cm_detour : int = 6
|}]

(* --- Binding operators --------------------------------------------------- *)

(* refined codomain on the operator: the body result meets the codomain
   obligation, the bound use strips *)
let ( let* ) : int -> (int -> int) -> int{ _ > 0 } = fun a f -> f a;;
[%%expect{|
Line 1, characters 64-67: refinement obligation: int{ _ > 0 }
val ( let* ) : int -> (int -> int) -> int{ _ > 0 } = <fun>
|}]

let lz = let* y = 1 in y;;
[%%expect{|
val lz : int = 1
|}]

let lz2 = (let* y = 1 in y) + 1;;
[%%expect{|
val lz2 : int = 2
|}]

(* refined domain on the operator: the bound expression is an argument
   obligation *)
let ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = fun _ f -> f 1;;
[%%expect{|
val ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = <fun>
|}]

let lp = let+ y = 5 in y;;
[%%expect{|
Line 1, characters 18-19: refinement obligation: int{ _ > 0 }
val lp : int = 1
|}]

(* --- Eta and optional-argument elimination ------------------------------- *)

(* passing a function with an optional parameter where a plain arrow is
   expected forces an eta-expansion whose synthesized application result
   must strip *)
let ho (h : int -> int{ _ > 0 }) = h 1;;
[%%expect{|
val ho : (int -> int{ _ > 0 }) -> int = <fun>
|}]

let gopt : ?o:bool -> int -> int{ _ > 0 } = fun ?o:_ y -> y;;
[%%expect{|
Line 1, characters 58-59: refinement obligation: int{ _ > 0 }
val gopt : ?o:bool -> int -> int{ _ > 0 } = <fun>
|}]

let he = ho gopt;;
[%%expect{|
val he : int = 1
|}]

(* --- Recursive refined-headed value --------------------------------------- *)

(* a refined-headed [let rec] value is admitted: the right-hand-side
   restriction allows constants, and the module-level exemption plus the
   occurrence strip cover the binding like any other module-level let *)
let rec rv : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 8-10: refined environment entry: rv : int{ _ > 0 }
Line 1, characters 28-29: refinement obligation: int{ _ > 0 }
val rv : int{ _ > 0 } = 5
|}]

let use_rv = rv + 1;;
[%%expect{|
val use_rv : int = 6
|}]
