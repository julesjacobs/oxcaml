(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: the TOTAL spec-function space [@vox.total] must FAIL CLOSED at the
   TYPE/argument layer.  Every non-reflectable argument to a total
   parameter -- an effectful lambda, a plain named function, an
   eta-wrapper around one, an [%vox.assume] cast, or a nested
   non-nameable expression (which a NON-total dependent parameter would
   ANF-name) -- is rejected with a dedicated message, never silently
   accepted as an opaque relation.  Positive twin: demo/lean_total.ml. *)

[@@@warning "-6-32-26"]

[%%vox.lean {lean|
abbrev IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
|lean}]

let apply_step :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x
[%%expect{|
val apply_step :
  (r : (int -> int -> bool) vox_total) ->
  ((x : int) -> int{ rHolds r x _ }) -> (x : int) -> int{ rHolds r x _ } =
  <fun>
|}]

(* an effectful lambda body is not reflectable *)
let bad_effect (x : int) : int{ x <= _ } =
  apply_step (fun p q -> ignore (Stdlib.print_int p); p <= q) (fun a -> a + 1) x
[%%expect{|
Line 2, characters 13-61:
2 |   apply_step (fun p q -> ignore (Stdlib.print_int p); p <= q) (fun a -> a + 1) x
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the argument for this parameter must be a TOTAL spec function -- the parameter is declared [@vox.total], a total function space that admits only reflectable, effect-free functions (a lambda with a pure, logic-nameable body, or a value marked [@vox.reflect]); this argument is not one of those
|}]

(* a plain named function is not in the total (reflectable) space *)
let myrel p q = p <= q
let bad_named (x : int) : int{ x <= _ } =
  apply_step myrel (fun a -> a + 1) x
[%%expect{|
val myrel : 'a -> 'a -> bool = <fun>
Line 3, characters 13-18:
3 |   apply_step myrel (fun a -> a + 1) x
                 ^^^^^
Error: vox: the argument for this parameter must be a TOTAL spec function -- the parameter is declared [@vox.total], a total function space that admits only reflectable, effect-free functions (a lambda with a pure, logic-nameable body, or a value marked [@vox.reflect]); this argument is not one of those
|}]

(* eta-wrapping a plain named function: the body is a non-reflectable call *)
let bad_eta (x : int) : int{ x <= _ } =
  apply_step (fun a b -> myrel a b) (fun a -> a + 1) x
[%%expect{|
Line 2, characters 13-35:
2 |   apply_step (fun a b -> myrel a b) (fun a -> a + 1) x
                 ^^^^^^^^^^^^^^^^^^^^^^
Error: vox: the argument for this parameter must be a TOTAL spec function -- the parameter is declared [@vox.total], a total function space that admits only reflectable, effect-free functions (a lambda with a pure, logic-nameable body, or a value marked [@vox.reflect]); this argument is not one of those
|}]

(* a nested NON-nameable argument (a call returning a function): a non-total
   dependent parameter would name it by a loc-keyed ANF ident; a total one
   rejects it *)
let mk (b : bool) : (int -> int -> bool) =
  if b then (fun p q -> p <= q) else (fun p q -> p >= q)
let bad_nested (x : int) : int{ x <= _ } =
  apply_step (mk true) (fun a -> a + 1) x
[%%expect{|
val mk : bool -> int -> int -> bool = <fun>
Line 4, characters 13-22:
4 |   apply_step (mk true) (fun a -> a + 1) x
                 ^^^^^^^^^
Error: vox: the argument for this parameter must be a TOTAL spec function -- the parameter is declared [@vox.total], a total function space that admits only reflectable, effect-free functions (a lambda with a pure, logic-nameable body, or a value marked [@vox.reflect]); this argument is not one of those
|}]
