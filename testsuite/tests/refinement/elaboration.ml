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

type 'a reentrant = 'a{ ((_ : 'a) = _) }

[%%expect {|
type 'a reentrant = 'a{ (app[Stdlib!.=] _ _) }
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
