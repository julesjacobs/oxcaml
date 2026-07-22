(* TEST
 expect;
*)

(* Part 2 of definitional equations.  [let[@vox.def] f p ... = rhs] forces
   [f @ total] and generates a companion trusted-lemma binding [f_def] whose
   refinement asserts [f]'s definitional equation [f p ... = rhs].  [f] itself
   stays an uninterpreted solver symbol -- nothing about its body reaches the
   solver except through [f_def].  Writing [let () = f_def a ...] deposits the
   ground equation [f a ... = rhs[a,...]] as a fact (the existing
   refined-application-becomes-a-fact mechanism). *)

let[@vox.def] double x = x + x
[%%expect {|
val double : int -> int = <fun>
val double_def : (x : int) -> unit{ double x = x + x } = <fun>
|}]

(* Opaque: with [double] uninterpreted, [double 5 = 10] is NOT provable. *)
let opaque_is_unproved = (double 5 : int{ _ = 10 })
[%%expect {|
Line 1, characters 25-51:
1 | let opaque_is_unproved = (double 5 : int{ _ = 10 })
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Depositing [double 5 = 5 + 5] (via [double_def 5]) makes the same goal
   provable. *)
let () = double_def 5
let after_def_is_proved = (double 5 : int{ _ = 10 })
[%%expect {|
val after_def_is_proved : int{ _ = 10 } = 10
|}]

(* A refined proof value can be destructured by a plain constructor pattern
   inside an expression [let], while its equation is still deposited. *)
let inner_constructor_is_proved =
  let () = double_def 6 in
  (double 6 : int{ _ = 12 })
[%%expect {|
val inner_constructor_is_proved : int{ _ = 12 } = 12
|}]

(* Binder-shaped local patterns continue to deposit the same equation. *)
let inner_wildcard_is_proved =
  let _ = double_def 7 in
  (double 7 : int{ _ = 14 })
let inner_variable_is_proved =
  let _proof = double_def 8 in
  (double 8 : int{ _ = 16 })
[%%expect {|
val inner_wildcard_is_proved : int{ _ = 14 } = 14
val inner_variable_is_proved : int{ _ = 16 } = 16
|}]

(* The reverse direction remains rigid: a plain value does not acquire a
   refinement merely because the pattern expects one. *)
let reverse_direction_is_rejected =
  let (() : unit{ false }) = () in
  ()
[%%expect {|
Line 2, characters 6-26:
2 |   let (() : unit{ false }) = () in
          ^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "unit{ false }"
       but a pattern was expected which matches values of type "unit"
|}]

(* A false consequence of the deposited equation is disproved. *)
let () = double_def 5
let false_consequence = (double 5 : int{ _ = 11 })
[%%expect {|
Line 2, characters 24-50:
2 | let false_consequence = (double 5 : int{ _ = 11 })
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A two-argument definitional equation. *)
let[@vox.def] add3 x y = x + y + 3
[%%expect {|
val add3 : int @ total logical -> int -> int = <fun>
val add3_def :
  (x : int) @ total logical -> (y : int) -> unit{ add3 x y = x + y + 3 } =
  <fun>
|}]

let () = add3_def 10 20
let add3_used = (add3 10 20 : int{ _ = 33 })
[%%expect {|
val add3_used : int{ _ = 33 } = 33
|}]

(* Recursive definitional equations use the same structural-totality verdict as
   recursive mode checking.  Instantiation remains explicit and ground: each
   [length_def] call contributes exactly one constructor equation. *)
type lst = Nil | Cons of int * lst

let[@vox.def] rec length l =
  match l with
  | Nil -> 0
  | Cons (_, tail) -> 1 + length tail

[%%expect {|
type lst = Nil | Cons of int * lst
val length : lst -> int = <fun>
val length_def :
  (l : lst) ->
  unit{ length l = (match l with | Nil -> 0 | Cons _ tail -> 1 + length tail)
   } =
  <fun>
|}]

let () = length_def Nil
let () = length_def (Cons (2, Nil))
let () = length_def (Cons (1, Cons (2, Nil)))

let recursive_ground =
  (length (Cons (1, Cons (2, Nil))) : int{ _ = 2 })

[%%expect {|
val recursive_ground : int{ _ = 2 } = 2
|}]

let recursive_ground_false =
  (length (Cons (1, Cons (2, Nil))) : int{ _ = 3 })

[%%expect {|
Line 2, characters 2-51:
2 |   (length (Cons (1, Cons (2, Nil))) : int{ _ = 3 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Variant A: the structurally smaller [append tail l2] call contributes the
   instantiated declared result refinement as its induction hypothesis.  The
   length equations are still requested manually. *)
let[@vox.def] rec append (l1 : lst) (l2 : lst)
    : lst{ length _ = length l1 + length l2 } =
  match l1 with
  | Nil ->
    let _ = length_def Nil in
    l2
  | Cons (head, tail) ->
    let result = append tail l2 in
    let _ = length_def (Cons (head, result)) in
    let _ = length_def (Cons (head, tail)) in
    Cons (head, result)

[%%expect {|
val append :
  (l1 : lst) -> (l2 : lst) -> lst{ length _ = length l1 + length l2 } = <fun>
val append_def :
  (l1 : lst) @ total logical ->
  (l2 : lst) ->
  unit{
   append l1 l2 = (match l1 with | Nil -> let value = length_def Nil in l2 | Cons head tail -> let result = append tail l2 in let value = length_def (Cons (head, result)) in let value__2 = length_def (Cons (head, tail)) in Cons (head, result))
   } =
  <fun>
|}]

(* Variant B: a separate structural lemma about the same [append]. *)
let rec append_length (l1 : lst) (l2 : lst)
    : unit{
        length (append l1 l2)
        = length l1 + length l2
      } =
  match l1 with
  | Nil ->
    let _ = append_def Nil l2 in
    let _ = length_def Nil in
    ()
  | Cons (head, tail) ->
    let _ = append_length tail l2 in
    let _ = append_def (Cons (head, tail)) l2 in
    let _ = length_def (Cons (head, append tail l2)) in
    let _ = length_def (Cons (head, tail)) in
    ()

[%%expect {|
val append_length :
  (l1 : lst) ->
  (l2 : lst) -> unit{ length (append l1 l2) = length l1 + length l2 } = <fun>
|}]

(* A structurally total recursive body still has to prove its own result
   contract.  The Nil arm makes this false claim unprovable. *)
let rec append_wrong (l1 : lst) (l2 : lst)
    : lst{ length _ = length l1 } =
  match l1 with
  | Nil ->
    let _ = length_def Nil in
    l2
  | Cons (head, tail) ->
    let result = append_wrong tail l2 in
    let _ = length_def (Cons (head, result)) in
    let _ = length_def (Cons (head, tail)) in
    Cons (head, result)

[%%expect {|
Lines 5-6, characters 4-6:
5 | ....let _ = length_def Nil in
6 |     l2
Error: Refinement verification failed (not-proved)
|}]

(* Fail-closed: a body using integer division is partial, so it cannot be
   [@vox.def]. *)
let[@vox.def] bad_div x = 100 / x
[%%expect {|
Line 1, characters 30-31:
1 | let[@vox.def] bad_div x = 100 / x
                                  ^
Error: The value "(/)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 22-33
         which is expected to be "total".
|}]

(* Fail-closed: [raise] is partial. *)
let[@vox.def] bad_raise b = if b then raise Not_found else 0
[%%expect {|
Line 1, characters 38-43:
1 | let[@vox.def] bad_raise b = if b then raise Not_found else 0
                                          ^^^^^
Error: The value "raise" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 24-60
         which is expected to be "total".
|}]

(* Fail-closed: non-structural recursion cannot be reflected. *)
let[@vox.def] rec bad_rec x = bad_rec x
[%%expect {|
Line 1, characters 0-39:
1 | let[@vox.def] rec bad_rec x = bad_rec x
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.def] cannot be used on this recursive binding: its recursive group is not structurally total
|}]

(* Fail-closed: a [@vox.def] binding must be a function with parameters. *)
let[@vox.def] not_a_function = 42
[%%expect {|
Line 1, characters 0-33:
1 | let[@vox.def] not_a_function = 42
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.def] requires a function binding with explicit parameters
|}]

(* Forgeability guard: the verify-skip is keyed on expander provenance, NOT on a
   spellable attribute.  A hand-written [@@vox.def.axiom] on a false-refinement
   unit body is verified normally and REJECTED -- it does not deposit anything. *)
let[@vox.def.axiom] forged (x : int) = (() : unit{ 0 = 1 })
[%%expect {|
Line 1, characters 39-59:
1 | let[@vox.def.axiom] forged (x : int) = (() : unit{ 0 = 1 })
                                           ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* The verifier's exploit program: forge the axiom marker, "unpack" it, and try
   to prove a falsehood.  It must be rejected at the forged binding itself. *)
let bad (x : int) = (() : unit{ 0 = 1 }) [@@vox.def.axiom]
let () = bad 0
let exploit = (7 : int{ _ = 99 })
[%%expect {|
Line 1, characters 20-40:
1 | let bad (x : int) = (() : unit{ 0 = 1 }) [@@vox.def.axiom]
                        ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]
