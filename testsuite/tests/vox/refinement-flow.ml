(* TEST
 flags = "-extension let_mutable";
 expect;
*)

(* Vox refinement flow: introduction strips and expectation obligations.
   Introductions (binders, occurrences, application results, field reads,
   patterns) use a refined value at its payload; expectations (annotations,
   arguments, codomains, assignments) check against the payload and record
   the site as an obligation in the typed tree.  No verification conditions
   are generated; every obligation is recorded and accepted.

   RED: this file pins the type-formers rejections these rules lift.
   GREEN: the expectations flip to the accepting behaviour, so the diff is
   exactly the set of programs this piece admits. *)

(* --- Occurrence strip + expectation funnel --------------------------- *)

(* Module-level binding: the declared type must survive to the signature
   (binder-strip exemption), so [val v : int{ _ > 0 }] prints; uses are
   covered by the occurrence strip. *)
let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 23-24:
1 | let v : int{ _ > 0 } = 5;;
                           ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* the occurrence-strip discriminator: [v]'s environment entry is refined *)
let use_v = v + 1;;
[%%expect{|
Line 1, characters 12-13:
1 | let use_v = v + 1;;
                ^
Error: Unbound value "v"
|}]

(* Local binding: the binder strip puts [u : int] in the environment; the
   annotation is a funnel obligation on the right-hand side. *)
let use_u = let u : int{ _ > 0 } = 5 in u + 1;;
[%%expect{|
Line 1, characters 35-36:
1 | let use_u = let u : int{ _ > 0 } = 5 in u + 1;;
                                       ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* --- No propagation: a refinement is never invented by inference ----- *)

let w = v;;
[%%expect{|
Line 1, characters 8-9:
1 | let w = v;;
            ^
Error: Unbound value "v"
|}]

let f c x = if c then x else v;;
[%%expect{|
Line 1, characters 29-30:
1 | let f c x = if c then x else v;;
                                 ^
Error: Unbound value "v"
|}]

(* --- Apply-result strip ----------------------------------------------- *)

(* the definition is also the arrow-annotated codomain obligation *)
let g : unit -> int{ _ > 0 } = fun () -> 5;;
[%%expect{|
Line 1, characters 41-42:
1 | let g : unit -> int{ _ > 0 } = fun () -> 5;;
                                             ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let use_g = g () + 1;;
[%%expect{|
Line 1, characters 12-13:
1 | let use_g = g () + 1;;
                ^
Error: Unbound value "g"
|}]

let x = g ();;
[%%expect{|
Line 1, characters 8-9:
1 | let x = g ();;
            ^
Error: Unbound value "g"
|}]

(* --- Argument obligations --------------------------------------------- *)

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
Line 1, characters 40-41:
1 | let f1 : int{ _ > 0 } -> int = fun y -> y;;
                                            ^
Error: The value "y" has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

let a1 = f1 5;;
[%%expect{|
Line 1, characters 9-11:
1 | let a1 = f1 5;;
             ^^
Error: Unbound value "f1"
|}]

(* a second use of the same contract *)
let a2 = f1 6;;
[%%expect{|
Line 1, characters 9-11:
1 | let a2 = f1 6;;
             ^^
Error: Unbound value "f1"
|}]

(* labelled *)
let f2 : lbl:int{ _ > 0 } -> int = fun ~lbl -> lbl;;
[%%expect{|
Line 1, characters 47-50:
1 | let f2 : lbl:int{ _ > 0 } -> int = fun ~lbl -> lbl;;
                                                   ^^^
Error: The value "lbl" has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

let a3 = f2 ~lbl:5;;
[%%expect{|
Line 1, characters 9-11:
1 | let a3 = f2 ~lbl:5;;
             ^^
Error: Unbound value "f2"
|}]

(* commuted *)
let f3 : a:int{ _ > 0 } -> b:int -> int = fun ~a ~b -> a + b;;
[%%expect{|
Line 1, characters 55-56:
1 | let f3 : a:int{ _ > 0 } -> b:int -> int = fun ~a ~b -> a + b;;
                                                           ^
Error: The value "a" has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

let a4 = f3 ~b:2 ~a:5;;
[%%expect{|
Line 1, characters 9-11:
1 | let a4 = f3 ~b:2 ~a:5;;
             ^^
Error: Unbound value "f3"
|}]

(* optional: the default and a supplied value are both obligations *)
let f4 : ?o:int{ _ > 0 } -> unit -> int = fun ?(o = 1) () -> o;;
[%%expect{|
Line 1, characters 52-53:
1 | let f4 : ?o:int{ _ > 0 } -> unit -> int = fun ?(o = 1) () -> o;;
                                                        ^
Error: The constant "1" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let a5 = f4 ~o:5 ();;
[%%expect{|
Line 1, characters 9-11:
1 | let a5 = f4 ~o:5 ();;
             ^^
Error: Unbound value "f4"
|}]

let a6 = f4 ();;
[%%expect{|
Line 1, characters 9-11:
1 | let a6 = f4 ();;
             ^^
Error: Unbound value "f4"
|}]

(* --- Codomain obligations ---------------------------------------------- *)

(* the arrow-annotated form is [g] above; the return-annotation form
   funnels the codomain expectation into each arm *)
let k c : int{ _ > 0 } = if c then 1 else 2;;
[%%expect{|
Line 1, characters 35-36:
1 | let k c : int{ _ > 0 } = if c then 1 else 2;;
                                       ^
Error: The constant "1" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* --- Aliases on both sides -------------------------------------------- *)

type nat = int{ _ >= 0 };;
[%%expect{|
type nat = int{ _ >= 0 }
|}]

let n : nat = 5;;
[%%expect{|
Line 1, characters 14-15:
1 | let n : nat = 5;;
                  ^
Error: The constant "5" has type "int" but an expression was expected of type
         "nat" = "int{ _ >= 0 }"
|}]

(* the alias name is lost from the stripped occurrence: [int], not [nat] *)
let use_n = n + 1;;
[%%expect{|
Line 1, characters 12-13:
1 | let use_n = n + 1;;
                ^
Error: Unbound value "n"
|}]

let f5 : nat -> int = fun z -> z;;
[%%expect{|
Line 1, characters 31-32:
1 | let f5 : nat -> int = fun z -> z;;
                                   ^
Error: The value "z" has type "nat" = "int{ _ >= 0 }"
       but an expression was expected of type "int"
|}]

let a7 = f5 5;;
[%%expect{|
Line 1, characters 9-11:
1 | let a7 = f5 5;;
             ^^
Error: Unbound value "f5"
|}]

(* apply-result strip through the alias: the head must be expanded before
   the gate decides *)
let gn : unit -> nat = fun () -> 5;;
[%%expect{|
Line 1, characters 33-34:
1 | let gn : unit -> nat = fun () -> 5;;
                                     ^
Error: The constant "5" has type "int" but an expression was expected of type
         "nat" = "int{ _ >= 0 }"
|}]

let use_gn = gn () + 1;;
[%%expect{|
Line 1, characters 13-15:
1 | let use_gn = gn () + 1;;
                 ^^
Error: Unbound value "gn"
|}]

(* --- Nested refinements flow via the expectation ----------------------- *)

let l : int{ _ > 0 } list = [5; v];;
[%%expect{|
Line 1, characters 29-30:
1 | let l : int{ _ > 0 } list = [5; v];;
                                 ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* still a rigid clash: stripping is head-only *)
let bad_l = (l : int list);;
[%%expect{|
Line 1, characters 13-14:
1 | let bad_l = (l : int list);;
                 ^
Error: Unbound value "l"
|}]

(* --- Higher-order ------------------------------------------------------- *)

let mapped = List.map f1 l;;
[%%expect{|
Line 1, characters 22-24:
1 | let mapped = List.map f1 l;;
                          ^^
Error: Unbound value "f1"
|}]

(* --- Mutability --------------------------------------------------------- *)

(* mutable binders are exempt from the binder strip: the write checks
   against the declared type (funnel), the read strips (occurrence) *)
let use_mv =
  let mutable mv : int{ _ > 0 } = 5 in
  mv <- 6;
  mv + 1;;
[%%expect{|
Line 2, characters 34-35:
2 |   let mutable mv : int{ _ > 0 } = 5 in
                                      ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* the ref round trip: [ref 5]'s result meets the refined expectation only
   after the argument is typed, so creation sits at the order-dependent
   margin; the expectation pins which side it lands on *)
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
Line 2, characters 42-43:
2 |   let r : int{ _ > 0 } ref = { contents = 5 } in
                                              ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* a mutable record field: create, write, read *)
type cell = { mutable c : int{ _ > 0 } };;
[%%expect{|
type cell = { mutable c : int{ _ > 0 }; }
|}]

let cl = { c = 5 };;
[%%expect{|
Line 1, characters 15-16:
1 | let cl = { c = 5 };;
                   ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let () = cl.c <- 6;;
[%%expect{|
Line 1, characters 9-11:
1 | let () = cl.c <- 6;;
             ^^
Error: Unbound value "cl"
|}]

let use_c = cl.c + 1;;
[%%expect{|
Line 1, characters 12-14:
1 | let use_c = cl.c + 1;;
                ^^
Error: Unbound value "cl"
|}]

(* --- Destructuring ------------------------------------------------------ *)

let use_ab = let (a, b) : (int * int){ true } = (1, 2) in a + b;;
[%%expect{|
Line 1, characters 17-23:
1 | let use_ab = let (a, b) : (int * int){ true } = (1, 2) in a + b;;
                     ^^^^^^
Error: This pattern matches values of type "'a * 'b"
       but a pattern was expected which matches values of type
         "(int * int){ true }"
|}]

(* --- Export and inclusion ---------------------------------------------- *)

module M : sig val mv : int{ _ > 0 } end = struct
  let mv : int{ _ > 0 } = 5
end;;
[%%expect{|
Line 2, characters 26-27:
2 |   let mv : int{ _ > 0 } = 5
                              ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let use_m = M.mv + 1;;
[%%expect{|
Line 1, characters 12-13:
1 | let use_m = M.mv + 1;;
                ^
Error: Unbound module "M"
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
Line 2, characters 10-11:
2 |   if y <= 1 then 1 else y * fact (y - 1);;
              ^
Error: The constant "1" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let use_fact = fact 5;;
[%%expect{|
Line 1, characters 15-19:
1 | let use_fact = fact 5;;
                   ^^^^
Error: Unbound value "fact"
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
val f7 : (unit -> int{ _ > 0 }) -> int{ _ > 0 } * (unit -> int{ _ > 0 }) =
  <fun>
|}]

(* swapped: the annotation solves the codomain first, so the strip fires *)
let f8 g = ((g : unit -> int{ _ > 0 }), g ());;
[%%expect{|
val f8 : (unit -> int{ _ > 0 }) -> (unit -> int{ _ > 0 }) * int{ _ > 0 } =
  <fun>
|}]

(* --- Class instance variables and methods ------------------------------- *)

(* an immutable instance variable with a refined type: the initializer is a
   funnel obligation, reads inside the object strip *)
class cv = object val v : int{ _ > 0 } = 5 method g = v end;;
[%%expect{|
Line 1, characters 41-42:
1 | class cv = object val v : int{ _ > 0 } = 5 method g = v end;;
                                             ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* mutable instance variable: init and setter writes are obligations, the
   getter read strips *)
class cc = object
  val mutable iv : int{ _ > 0 } = 5
  method get = iv
  method set n = iv <- n
end;;
[%%expect{|
Line 2, characters 34-35:
2 |   val mutable iv : int{ _ > 0 } = 5
                                      ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* a refined method type: out of scope for this piece — the rigid clash
   stays loud rather than silently dropping the contract *)
class cm = object method m : int{ _ > 0 } = 5 end;;
[%%expect{|
Line 1, characters 44-45:
1 | class cm = object method m : int{ _ > 0 } = 5 end;;
                                                ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let cm_use = (new cm)#m + 1;;
[%%expect{|
Line 1, characters 18-20:
1 | let cm_use = (new cm)#m + 1;;
                      ^^
Error: Unbound class "cm"
|}]

(* the let-detour around the out-of-scope send: binding the send result and
   then using it must not smuggle the refined head past the clash *)
let cm_detour = let z = (new cm)#m in z + 1;;
[%%expect{|
Line 1, characters 29-31:
1 | let cm_detour = let z = (new cm)#m in z + 1;;
                                 ^^
Error: Unbound class "cm"
|}]

(* --- Binding operators --------------------------------------------------- *)

(* refined codomain on the operator: the body result meets the codomain
   obligation, the bound use strips *)
let ( let* ) : int -> (int -> int) -> int{ _ > 0 } = fun a f -> f a;;
[%%expect{|
Line 1, characters 64-67:
1 | let ( let* ) : int -> (int -> int) -> int{ _ > 0 } = fun a f -> f a;;
                                                                    ^^^
Error: This expression has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let lz = let* y = 1 in y;;
[%%expect{|
Line 1, characters 9-13:
1 | let lz = let* y = 1 in y;;
             ^^^^
Error: Unbound value "( let* )"
|}]

let lz2 = (let* y = 1 in y) + 1;;
[%%expect{|
Line 1, characters 11-15:
1 | let lz2 = (let* y = 1 in y) + 1;;
               ^^^^
Error: Unbound value "( let* )"
|}]

(* refined domain on the operator: the bound expression is an argument
   obligation *)
let ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = fun _ f -> f 1;;
[%%expect{|
val ( let+ ) : int{ _ > 0 } -> (int -> int) -> int = <fun>
|}]

let lp = let+ y = 5 in y;;
[%%expect{|
Line 1, characters 18-19:
1 | let lp = let+ y = 5 in y;;
                      ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

(* --- Eta and optional-argument elimination ------------------------------- *)

(* passing a function with an optional parameter where a plain arrow is
   expected forces an eta-expansion whose synthesized application result
   must strip *)
let ho (h : int -> int{ _ > 0 }) = h 1;;
[%%expect{|
val ho : (int -> int{ _ > 0 }) -> int{ _ > 0 } = <fun>
|}]

let gopt : ?o:bool -> int -> int{ _ > 0 } = fun ?o:_ y -> y;;
[%%expect{|
Line 1, characters 58-59:
1 | let gopt : ?o:bool -> int -> int{ _ > 0 } = fun ?o:_ y -> y;;
                                                              ^
Error: The value "y" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let he = ho gopt;;
[%%expect{|
Line 1, characters 12-16:
1 | let he = ho gopt;;
                ^^^^
Error: Unbound value "gopt"
|}]

(* --- Recursive refined-headed value --------------------------------------- *)

(* a refined-headed [let rec] value: ruled out by the right-hand-side
   restriction rather than admitted by the binder strip *)
let rec rv : int{ _ > 0 } = 5;;
[%%expect{|
Line 1, characters 28-29:
1 | let rec rv : int{ _ > 0 } = 5;;
                                ^
Error: The constant "5" has type "int" but an expression was expected of type
         "int{ _ > 0 }"
|}]

let use_rv = rv + 1;;
[%%expect{|
Line 1, characters 13-15:
1 | let use_rv = rv + 1;;
                 ^^
Error: Unbound value "rv"
|}]
