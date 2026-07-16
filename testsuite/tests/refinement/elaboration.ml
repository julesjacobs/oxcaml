(* TEST
 expect;
*)

type record = { immutable_field : int; mutable mutable_field : int }
let record_value = { immutable_field = 3; mutable_field = 4 }

type rich = int{
  let pair = (_, 1) in
  let same = (fun x -> x = _) 1 in
  if same then
    fst pair = _ && Some _ = Some 1 && record_value.immutable_field = 3
  else false
}

[%%expect {|
type record = { immutable_field : int; mutable mutable_field : int; }
val record_value : record = {immutable_field = 3; mutable_field = 4}
type rich =
    int{
     (let pair = (_, 1) in
   (let same = ((fun x -> (app[Stdlib!.=] x _)) 1) in
      (if same
         then (app[Stdlib!.&&] (app[Stdlib!.=] (app[Stdlib!.fst] pair) _)
                 (app[Stdlib!.&&]
                    (app[Stdlib!.=] constructor[option/13!.Some] _
                       constructor[option/13!.Some] 1)
                    (app[Stdlib!.=]
                       (global[record_value/294]).field[record/289[1].immutable_field]
                       3)))
         else constructor[bool/6!.false])))
     }
|}]

(* A predicate is checked at [total] with the refined self viewed [logical].
   Comparing the self reads its representation, which requires [physical]
   access.  For a self whose type CROSSES logicality (an immediate such as
   [int], see [int_reentrant] below) that is fine.  For a POLYMORPHIC self the
   type is not known to cross, so the comparison is rejected: a polymorphic self
   cannot be compared in its own predicate until the self's kind is known to
   cross logicality.  A function self also remains logical because arrows do not
   cross totality and so are not modelable.  Restriction deferred, unlocked by
   kind-constrained declarations (the same feature that unlocks total
   comparisons); the sibling [int_reentrant] keeps the parametric-refinement
   elaboration coverage. *)
type 'a reentrant = 'a{ ((_ : 'a) = _) }

[%%expect {|
Line 1, characters 26-27:
1 | type 'a reentrant = 'a{ ((_ : 'a) = _) }
                              ^
Error: This value is "logical" but is expected to be "physical".
|}]

type fn_reentrant = (int -> int){ ((_ : int -> int) = _) }

[%%expect {|
Line 1, characters 54-55:
1 | type fn_reentrant = (int -> int){ ((_ : int -> int) = _) }
                                                          ^
Error: This value is "logical" but is expected to be "physical".
|}]

type int_reentrant = int{ ((_ : int) = _) }

[%%expect {|
type int_reentrant = int{ (app[Stdlib!.=] _ _) }
|}]

type drains_delayed_checks = int{
  let unused = 0 in
  _ = 0
}

[%%expect {|
Line 2, characters 6-12:
2 |   let unused = 0 in
          ^^^^^^
Warning 26 [unused-var]: unused variable "unused".

type drains_delayed_checks = int{ (let unused = 0 in (app[Stdlib!.=] _ 0)) }
|}]

type unsupported = int{ match _ with 0 -> true | _ -> false }

[%%expect {|
Line 1, characters 24-59:
1 | type unsupported = int{ match _ with 0 -> true | _ -> false }
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A match expression is not yet supported in refinements.
|}]

type restored_after_lowering_error = int{ ((_ : int) = _) }

[%%expect {|
type restored_after_lowering_error = int{ (app[Stdlib!.=] _ _) }
|}]

type unresolved = int{ let f = fun x -> true in true }

[%%expect {|
Line 1, characters 18-54:
1 | type unresolved = int{ let f = fun x -> true in true }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This refinement has an unresolved type variable "'a".
       Refinement predicates must have fully determined inferred types at their point of formation.
|}]

type restored_after_unresolved_error = int{ _ = 0 }

[%%expect {|
type restored_after_unresolved_error = int{ (app[Stdlib!.=] _ 0) }
|}]

let named_unresolved x =
  (x : int{ let f = fun (y : 'a) -> true in _ = 0 })

[%%expect {|
Line 2, characters 7-51:
2 |   (x : int{ let f = fun (y : 'a) -> true in _ = 0 })
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This refinement has an unresolved type variable "'a".
       Refinement predicates must have fully determined inferred types at their point of formation.
|}]
