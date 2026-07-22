(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: refined-slot imposition channels            *)
(*                                                                *)
(* Besides the expression annotation (annotation_obligations.ml)  *)
(* and the function contract argument (contract_obligations.ml), a *)
(* refined type is imposed by a record field, a constructor        *)
(* argument, an array element, a mutable-cell assignment, and an   *)
(* optional-argument default.  This file is the DETERMINATION of   *)
(* whether these unmarked channels can reach a silent false        *)
(* refinement, per the user ruling that unsoundness reachable ONLY *)
(* via [Obj.magic] (an unsafe cast) is ACCEPTED.                   *)
(*                                                                 *)
(* Every magic-free vehicle is sound or blocked:                   *)
(*   - a CONCRETE value creates a disproved obligation                    *)
(*     [imp_concrete_...];                                                *)
(*   - a PARAMETER whose refinement is inferred from the slot      *)
(*     propagates the obligation to call sites, where the contract *)
(*     fires [imp_propagate_...]: [let mk x = { f = x }] gives            *)
(*     [mk : int{_>0} -> r], and [mk 0] is rejected;               *)
(*   - a BOTTOM value (a diverging/never-returning expression)     *)
(*     may inhabit any type soundly, so refining it is sound       *)
(*     [imp_bottom_...];                                                  *)
(*   - an IMPURE function CAN have a proved refined result, but the *)
(*     result fact is its PROVEN contract, which holds for every    *)
(*     evaluation, so cross-occurrence use is sound (fact_pollution *)
(*     .ml); a false contract needs an unsafe cast.  (Impurity and  *)
(*     representability are orthogonal: a sequence-bodied function  *)
(*     is rejected by the representability limit, not by impurity.) *)
(*                                                                 *)
(* The one vehicle that produces a REAL mis-typed value is         *)
(* [Obj.magic] [imp_magic_...].  Exact primitive identity remains   *)
(* visible for direct calls, which are tightened here.  An indirect *)
(* result is not distinguishable from useful parametric bottom and  *)
(* remains the ACCEPTED KNOWN LIMITATION from the historical Tvar   *)
(* ruling and the user's statement that                             *)
(* "using Obj.magic is basically saying that you want               *)
(* to do something arbitrary there".                               *)
(*                                                                 *)
(* Marker legend: see binder_facts.ml.                             *)
(* ============================================================= *)

let mk_pos () : int{ _ > 0 } = 1
[%%expect {|
val mk_pos : unit -> int{ _ > 0 } = <fun>
|}]

type r = { f : int{ _ > 0 } }
[%%expect {|
type r = { f : int{ _ > 0 }; }
|}]

type w = Wrap of int{ _ > 0 }
[%%expect {|
type w = Wrap of int{ _ > 0 }
|}]

(* --- concrete: disproved obligation (magic-free, sound) --- *)

(* @acc id=imp_concrete_field final=REJECT today=REJECT stable=yes unlocks=verification *)
let imp_concrete_field = { f = 0 }
[%%expect {|
Line 1, characters 31-32:
1 | let imp_concrete_field = { f = 0 }
                                   ^
Error: Refinement verification failed (disproved)
|}]

(* @acc id=imp_concrete_ctor final=REJECT today=REJECT stable=yes unlocks=verification *)
let imp_concrete_ctor = Wrap 0
[%%expect {|
Line 1, characters 29-30:
1 | let imp_concrete_ctor = Wrap 0
                                 ^
Error: Refinement verification failed (disproved)
|}]

(* --- parameter propagation: obligation moves to call sites (sound) --- *)

(* @acc id=imp_propagate_def final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The field infers the parameter as refined: [mk : int{ _ > 0 } -> r]. *)
let mk x = { f = x }
[%%expect {|
val mk : int{ _ > 0 } -> r = <fun>
|}]

(* @acc id=imp_propagate_badcall final=REJECT today=REJECT stable=yes unlocks=verification
   The contract fires at the call site even though the refined domain
   arrived by inference rather than annotation. *)
let imp_propagate_badcall = mk 0
[%%expect {|
Line 1, characters 31-32:
1 | let imp_propagate_badcall = mk 0
                                   ^
Error: Refinement verification failed (disproved)
|}]

(* @acc id=imp_propagate_goodcall final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_propagate_goodcall = mk 5
[%%expect {|
val imp_propagate_goodcall : r = {f = 5}
|}]

(* Function-case syntax uses a separate arrow-construction path but consumes
   the same parameter-origin refinement. *)
let mk_function = function x -> { f = x }
[%%expect {|
val mk_function : int{ _ > 0 } -> r = <fun>
|}]

let imp_propagate_function_bad = mk_function 0
[%%expect {|
Line 1, characters 45-46:
1 | let imp_propagate_function_bad = mk_function 0
                                                 ^
Error: Refinement verification failed (disproved)
|}]

let function_multiple = function
  | x when x = 1 -> { f = x }
  | x -> { f = x }
[%%expect {|
val function_multiple : int{ _ > 0 } -> r = <fun>
|}]

let imp_function_multiple_bad = function_multiple 0
[%%expect {|
Line 1, characters 50-51:
1 | let imp_function_multiple_bad = function_multiple 0
                                                      ^
Error: Refinement verification failed (disproved)
|}]

let function_refutable = function
  | 1 -> { f = 1 }
  | x -> { f = x }
[%%expect {|
val function_refutable : int{ _ > 0 } -> r = <fun>
|}]

let imp_function_refutable_bad = function_refutable 0
[%%expect {|
Line 1, characters 52-53:
1 | let imp_function_refutable_bad = function_refutable 0
                                                        ^
Error: Refinement verification failed (disproved)
|}]

let function_root_alias = function (x as y) -> { f = y }
[%%expect {|
val function_root_alias : int{ _ > 0 } -> r = <fun>
|}]

let function_let_alias = function x ->
  let y = x in
  let z = y in
  { f = z }
[%%expect {|
val function_let_alias : int{ _ > 0 } -> r = <fun>
|}]

let function_tuple_component = function (x, _y) -> { f = x }
[%%expect {|
Line 1, characters 57-58:
1 | let function_tuple_component = function (x, _y) -> { f = x }
                                                             ^
Error: Refinement verification failed (not-proved)
|}]

let function_guard_only = function
  | x when (ignore { f = x }; true) -> { f = 1 }
  | _ -> { f = 1 }
[%%expect {|
Line 2, characters 25-26:
2 |   | x when (ignore { f = x }; true) -> { f = 1 }
                             ^
Error: Refinement verification failed (not-proved)
|}]

let rec function_bottom () : 'a = function_bottom ()
[%%expect {|
val function_bottom : unit -> 'a = <fun>
|}]

let function_mixed_bottom = function
  | 0 -> { f = function_bottom () }
  | x -> { f = x }
[%%expect {|
val function_mixed_bottom : int{ _ > 0 } -> r = <fun>
|}]

let function_all_bottom = function
  | 0 -> { f = function_bottom () }
  | _ -> { f = function_bottom () }
[%%expect {|
val function_all_bottom : int -> r = <fun>
|}]

type function_either =
  | Function_pos of int{ _ > 0 }
  | Function_neg of int{ _ < 0 }
[%%expect {|
type function_either =
    Function_pos of int{ _ > 0 }
  | Function_neg of int{ _ < 0 }
|}]

let function_inconsistent = function
  | x when x = 1 -> Function_pos x
  | x -> Function_neg x
[%%expect {|
Line 3, characters 9-23:
3 |   | x -> Function_neg x
             ^^^^^^^^^^^^^^
Error: function cases impose inconsistent parameter refinements
|}]

let function_inconsistent_reversed = function
  | x when x = -1 -> Function_neg x
  | x -> Function_pos x
[%%expect {|
Line 3, characters 9-23:
3 |   | x -> Function_pos x
             ^^^^^^^^^^^^^^
Error: function cases impose inconsistent parameter refinements
|}]

let rec function_recursive = function x -> { f = x }
[%%expect {|
val function_recursive : int{ _ > 0 } -> r = <fun>
|}]

let function_mutable_alias = function x ->
  let mutable y = x in
  { f = y }
[%%expect {|
Line 3, characters 8-9:
3 |   { f = y }
            ^
Error: Refinement verification failed: a mutable variable cannot yet be represented in a verification condition
|}]

let function_unified_carrier = function x ->
  let y = if Sys.opaque_identity true then x else 0 in
  { f = y }
[%%expect {|
Line 3, characters 8-9:
3 |   { f = y }
            ^
Error: Refinement verification failed (not-proved)
|}]

let function_nested_scope = function x ->
  let inner y = { f = y } in
  ignore x;
  inner
[%%expect {|
val function_nested_scope : 'a -> int{ _ > 0 } -> r = <fun>
|}]

(* Constructor, array, and mutable-assignment slots propagate the same
   parameter type origin as the record-field case above. *)
let mk_ctor x = Wrap x
[%%expect {|
val mk_ctor : int{ _ > 0 } -> w = <fun>
|}]

let imp_propagate_ctor_bad = mk_ctor 0
[%%expect {|
Line 1, characters 37-38:
1 | let imp_propagate_ctor_bad = mk_ctor 0
                                         ^
Error: Refinement verification failed (disproved)
|}]

let mk_array x : int{ _ > 0 } array = [| x |]
[%%expect {|
val mk_array : int{ _ > 0 } -> int{ _ > 0 } array = <fun>
|}]

let imp_propagate_array_bad = mk_array 0
[%%expect {|
Line 1, characters 39-40:
1 | let imp_propagate_array_bad = mk_array 0
                                           ^
Error: Refinement verification failed (disproved)
|}]

let mk_mutation x =
  let mutable y : int{ _ > 0 } = 1 in
  y <- x;
  y
[%%expect {|
val mk_mutation : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

let imp_propagate_mutation_bad = mk_mutation 0
[%%expect {|
Line 1, characters 45-46:
1 | let imp_propagate_mutation_bad = mk_mutation 0
                                                 ^
Error: Refinement verification failed (disproved)
|}]

(* Provenance is the shared monomorphic type origin, not the spelling or exact
   identity of a parameter occurrence.  Ordinary and nested let aliases
   therefore propagate the contextual refinement. *)
let parameter_alias x =
  let y = x in
  { f = y }
[%%expect {|
val parameter_alias : int{ _ > 0 } -> r = <fun>
|}]

let imp_parameter_alias_bad = parameter_alias 0
[%%expect {|
Line 1, characters 46-47:
1 | let imp_parameter_alias_bad = parameter_alias 0
                                                  ^
Error: Refinement verification failed (disproved)
|}]

let parameter_nested_alias x =
  let y = x in
  let z = y in
  { f = z }
[%%expect {|
val parameter_nested_alias : int{ _ > 0 } -> r = <fun>
|}]

let imp_parameter_nested_alias_bad = parameter_nested_alias 0
[%%expect {|
Line 1, characters 60-61:
1 | let imp_parameter_nested_alias_bad = parameter_nested_alias 0
                                                                ^
Error: Refinement verification failed (disproved)
|}]

(* Unifying parameter carrier types does not merge their value origins. *)
let parameter_unified_if x y =
  let _ = if Sys.opaque_identity true then x else y in
  { f = y }
[%%expect {|
val parameter_unified_if : int -> int{ _ > 0 } -> r = <fun>
|}]

let parameter_unified_list x y =
  ignore [x; y];
  { f = y }
[%%expect {|
val parameter_unified_list : int -> int{ _ > 0 } -> r = <fun>
|}]

let parameter_recursive_alias x =
  let rec y = x in
  { f = y }
[%%expect {|
Line 3, characters 8-9:
3 |   { f = y }
            ^
Error: Refinement verification failed (not-proved)
|}]

let parameter_mutable_bad_write x =
  let mutable y = x in
  y <- 0;
  { f = y }
[%%expect {|
Line 4, characters 8-9:
4 |   { f = y }
            ^
Error: Refinement verification failed: a mutable variable cannot yet be represented in a verification condition
|}]

let parameter_cross_scope x =
  let y = x in
  let inner () = { f = y } in
  inner
[%%expect {|
Line 3, characters 23-24:
3 |   let inner () = { f = y } in
                           ^
Error: Refinement verification failed (not-proved)
|}]

(* Root pattern aliases are registered as equivalent parameter origins. *)
let parameter_pattern_alias (x as y) = { f = y }
[%%expect {|
val parameter_pattern_alias : int{ _ > 0 } -> r = <fun>
|}]

let imp_parameter_pattern_alias_bad = parameter_pattern_alias 0
[%%expect {|
Line 1, characters 62-63:
1 | let imp_parameter_pattern_alias_bad = parameter_pattern_alias 0
                                                                  ^
Error: Refinement verification failed (disproved)
|}]

(* Destructured variables are distinct value origins, not aliases of one
   another.  Until an individual component can be reflected back into the
   function domain, imposing a refinement on either component stays an
   obligation rather than refining every binder in the pattern. *)
let parameter_tuple_left (x, _y) = { f = x }
[%%expect {|
Line 1, characters 41-42:
1 | let parameter_tuple_left (x, _y) = { f = x }
                                             ^
Error: Refinement verification failed (not-proved)
|}]

let parameter_tuple_right (_x, y) = { f = y }
[%%expect {|
Line 1, characters 42-43:
1 | let parameter_tuple_right (_x, y) = { f = y }
                                              ^
Error: Refinement verification failed (not-proved)
|}]

(* A carrier-only annotation fixes the parameter to bare [int], so the alias
   no longer has an inferred parameter type origin that can be refined. *)
let parameter_alias_carrier_annotation x =
  let y : int = x in
  { f = y }
[%%expect {|
Line 3, characters 8-9:
3 |   { f = y }
            ^
Error: Refinement verification failed (not-proved)
|}]

let parameter_alias_refinement_annotation x =
  let y : int{ _ > 0 } = x in
  { f = y }
[%%expect {|
val parameter_alias_refinement_annotation : int{ _ > 0 } -> r = <fun>
|}]

(* An unrelated weak local variable does not share a dependent-parameter type
   origin and must retain an obligation. *)
let inferred_nonparameter () =
  let cell = ref None in
  match !cell with
  | None -> { f = 1 }
  | Some y -> { f = y }
[%%expect {|
Line 5, characters 20-21:
5 |   | Some y -> { f = y }
                        ^
Error: Refinement verification failed (not-proved)
|}]

(* --- bottom values: sound to refine (never produce a value) --- *)

let rec loop () : 'a = loop ()
[%%expect {|
val loop : unit -> 'a = <fun>
|}]

(* @acc id=imp_bottom_field final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   A diverging value in a refined field: sound, as bottom inhabits every
   type. *)
let imp_bottom_field () = { f = loop () }
[%%expect {|
val imp_bottom_field : unit -> r = <fun>
|}]

(* @acc id=imp_bottom_ctor final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_bottom_ctor () = Wrap (loop ())
[%%expect {|
val imp_bottom_ctor : unit -> w = <fun>
|}]

let imp_bottom_array () : int{ _ > 0 } array = [| loop () |]
[%%expect {|
val imp_bottom_array : unit -> int{ _ > 0 } array = <fun>
|}]

let exception_bottom () : 'a = raise Exit
[%%expect {|
val exception_bottom : unit -> 'a = <fun>
|}]

let imp_exception_bottom_field () = { f = exception_bottom () }
[%%expect {|
val imp_exception_bottom_field : unit -> r = <fun>
|}]

(* A labeled polymorphic bottom retains application metadata both when fully
   applied and when reached through a partial application. *)
let rec labeled_loop ~tag:_ () : 'a = labeled_loop ~tag:() ()
[%%expect {|
val labeled_loop : tag:unit -> unit -> 'a = <fun>
|}]

let imp_bottom_labeled () = { f = labeled_loop ~tag:() () }
[%%expect {|
val imp_bottom_labeled : unit -> r = <fun>
|}]

let imp_bottom_partial () =
  let continue = labeled_loop ~tag:() in
  { f = continue () }
[%%expect {|
val imp_bottom_partial : unit -> r = <fun>
|}]

(* Ordinary polymorphic applications remain ordinary without a refined
   context.  Once imposed into a refined slot, a non-bottom concrete result
   still creates a real obligation rather than inheriting the bottom rule. *)
let ordinary_id x = x
[%%expect {|
val ordinary_id : 'a -> 'a = <fun>
|}]

let imp_poly_plain = ordinary_id 0
[%%expect {|
val imp_poly_plain : int = 0
|}]

let imp_poly_obligation_positive = { f = ordinary_id 5 }
[%%expect {|
Line 1, characters 41-54:
1 | let imp_poly_obligation_positive = { f = ordinary_id 5 }
                                             ^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let imp_poly_obligation_zero = { f = ordinary_id 0 }
[%%expect {|
Line 1, characters 37-50:
1 | let imp_poly_obligation_zero = { f = ordinary_id 0 }
                                         ^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let parameter_via_identity x = { f = ordinary_id x }
[%%expect {|
val parameter_via_identity : int{ _ > 0 } -> r = <fun>
|}]

let imp_parameter_via_identity_bad = parameter_via_identity 0
[%%expect {|
Line 1, characters 60-61:
1 | let imp_parameter_via_identity_bad = parameter_via_identity 0
                                                                ^
Error: Refinement verification failed (disproved)
|}]

let labeled_id ~x () = x
[%%expect {|
val labeled_id : x:'a -> unit -> 'a = <fun>
|}]

let imp_partial_plain =
  let continue = labeled_id ~x:0 in
  continue ()
[%%expect {|
val imp_partial_plain : int = 0
|}]

(* --- refined call result: fact discharges the slot (sound) --- *)

(* @acc id=imp_result_field final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_result_field = { f = mk_pos () }
[%%expect {|
val imp_result_field : r = {f = 1}
|}]

(* --- Obj.magic: direct tightening and indirect accepted anchor --- *)

(* Exact direct [%obj_magic] applications retain primitive identity and leave
   a real obligation. *)
let imp_magic_field = { f = Obj.magic 0 }
[%%expect {|
Line 1, characters 28-39:
1 | let imp_magic_field = { f = Obj.magic 0 }
                                ^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let imp_magic_ctor = Wrap (Obj.magic 0)
[%%expect {|
Line 1, characters 26-39:
1 | let imp_magic_ctor = Wrap (Obj.magic 0)
                              ^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let imp_magic_array : int{ _ > 0 } array = [| Obj.magic 0 |]
[%%expect {|
Line 1, characters 46-57:
1 | let imp_magic_array : int{ _ > 0 } array = [| Obj.magic 0 |]
                                                  ^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let imp_magic_mutassign () =
  let mutable x : int{ _ > 0 } = 1 in
  x <- Obj.magic 0;
  x
[%%expect {|
Line 3, characters 7-18:
3 |   x <- Obj.magic 0;
           ^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* @acc id=imp_magic_alias_field final=ACCEPT today=ACCEPT stable=no unlocks=none
   Once hidden behind an ordinary value, the unsafe origin is not soundly
   distinguishable from a parametric result.  This remains the documented
   [Obj.magic] limitation from the historical Tvar ruling. *)
let hidden_magic = Obj.magic
[%%expect {|
val hidden_magic : 'a -> 'b = <fun>
|}]

let imp_magic_alias_field = { f = hidden_magic 0 }
[%%expect {|
val imp_magic_alias_field : r = {f = 0}
|}]

(* An eta wrapper also loses exact primitive identity and remains in the
   documented historical limitation. *)
let hidden_magic_wrapper x = Obj.magic x
[%%expect {|
val hidden_magic_wrapper : 'a -> 'b = <fun>
|}]

let imp_magic_wrapper_field = { f = hidden_magic_wrapper 0 }
[%%expect {|
val imp_magic_wrapper_field : r = {f = 0}
|}]

(* A user function with the same source-level name is not the primitive.  Its
   concrete application result retains a real obligation. *)
module Ordinary = struct
  let magic x = x
end
[%%expect {|
module Ordinary : sig val magic : 'a -> 'a end
|}]

let imp_ordinary_named_magic = { f = Ordinary.magic 0 }
[%%expect {|
Line 1, characters 37-53:
1 | let imp_ordinary_named_magic = { f = Ordinary.magic 0 }
                                         ^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]
