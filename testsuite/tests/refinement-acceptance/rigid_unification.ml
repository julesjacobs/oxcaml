(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: rigid unification (permanent clashes)       *)
(*                                                                *)
(* plan.html "Equality and unification": a refined type unifies   *)
(* only with a refined type whose skeleton relates AND whose      *)
(* predicate is structurally (alpha-)equal; a refined type meeting *)
(* a bare type is a clash "always, in every relation and at every *)
(* depth" ([int list] does not unify with [int{_>0} list]).       *)
(*                                                                *)
(* These are the cases that REJECT today and must keep rejecting  *)
(* unchanged forever -- they are stable=yes ANCHORS. Crucially,   *)
(* they are all NESTED: a top-level refinement on a USED value is *)
(* intercepted by the (upcoming) integration rules and weakened   *)
(* to its skeleton or routed to an obligation, so top-level       *)
(* clashes are NOT permanent (see skeleton_weakening.ml). Nesting *)
(* below a type constructor is never skeleton-stripped, so a      *)
(* nested refined-vs-bare or unequal-predicate mismatch is a hard *)
(* clash in the finished system exactly as it is today.           *)
(*                                                                *)
(* The symmetric Ctype.unify contract (refined-vs-bare, unequal   *)
(* predicates, alpha-equal accept, occurs/cycle safety) is pinned *)
(* separately as a unit test in refinement/type_integration.ml.   *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=ru_nested_refined_to_bare final=REJECT today=REJECT stable=yes unlocks=-
   Nested refined element flowing to a bare element list.
   Permanent rigid clash: [int{_=1}] nested under [list] is never
   weakened, so it can never meet bare [int]. *)
let ru_nested_refined_to_bare (xs : int{ _ = 1 } list) = (xs : int list)
[%%expect {|
Line 1, characters 58-60:
1 | let ru_nested_refined_to_bare (xs : int{ _ = 1 } list) = (xs : int list)
                                                              ^^
Error: The value "xs" has type "int{ (app[Stdlib!.=] _ 1) } list"
       but an expression was expected of type "int list"
       Type "int{ (app[Stdlib!.=] _ 1) }" is not compatible with type "int"
|}]

(* @acc id=ru_nested_bare_to_refined final=REJECT today=REJECT stable=yes unlocks=-
   The other direction: a bare element list where a refined-element
   list is demanded. A bare value cannot silently GAIN a nested
   refinement -- permanent rigid clash. *)
let ru_nested_bare_to_refined (xs : int list) = (xs : int{ _ = 1 } list)
[%%expect {|
Line 1, characters 49-51:
1 | let ru_nested_bare_to_refined (xs : int list) = (xs : int{ _ = 1 } list)
                                                     ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 1) } list"
       Type "int" is not compatible with type "int{ (app[Stdlib!.=] _ 1) }"
|}]

(* @acc id=ru_nested_unequal_preds final=REJECT today=REJECT stable=yes unlocks=-
   Unequal predicates nested under a constructor: [_ = 1] vs [_ = 2].
   Predicates must be structurally equal; these are different types. *)
let ru_nested_unequal_preds (xs : int{ _ = 1 } list) = (xs : int{ _ = 2 } list)
[%%expect {|
Line 1, characters 56-58:
1 | let ru_nested_unequal_preds (xs : int{ _ = 1 } list) = (xs : int{ _ = 2 } list)
                                                            ^^
Error: The value "xs" has type "int{ (app[Stdlib!.=] _ 1) } list"
       but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 2) } list"
       Type "int{ (app[Stdlib!.=] _ 1) }" is not compatible with type
         "int{ (app[Stdlib!.=] _ 2) }"
|}]

(* @acc id=ru_syntactically_distinct_preds final=REJECT today=REJECT stable=yes unlocks=-
   The plan's canonical example: [int{ _ > 0 }] and [int{ 0 < _ }] are
   DELIBERATELY different types (structural, not semantic, equality),
   so they clash even though they denote the same set. Nested to make
   the clash permanent. *)
let ru_syntactically_distinct_preds (xs : int{ _ > 0 } list) =
  (xs : int{ 0 < _ } list)
[%%expect {|
Line 2, characters 3-5:
2 |   (xs : int{ 0 < _ } list)
       ^^
Error: The value "xs" has type "int{ (app[Stdlib!.>] _ 0) } list"
       but an expression was expected of type
         "int{ (app[Stdlib!.<] 0 _) } list"
       Type "int{ (app[Stdlib!.>] _ 0) }" is not compatible with type
         "int{ (app[Stdlib!.<] 0 _) }"
|}]

(* @acc id=ru_tuple_nested final=REJECT today=REJECT stable=yes unlocks=-
   Nested inside a tuple rather than a list -- same rule, any depth. *)
let ru_tuple_nested (p : int{ _ = 1 } * int) = (p : int * int)
[%%expect {|
Line 1, characters 48-49:
1 | let ru_tuple_nested (p : int{ _ = 1 } * int) = (p : int * int)
                                                    ^
Error: The value "p" has type "int{ (app[Stdlib!.=] _ 1) } * int"
       but an expression was expected of type "int * int"
       Type "int{ (app[Stdlib!.=] _ 1) }" is not compatible with type "int"
|}]
