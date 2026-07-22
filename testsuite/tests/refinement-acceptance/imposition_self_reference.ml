(* TEST
 expect;
*)

let direct = function x -> (x : int{ _ = x })
let direct_two : int{ _ = 2 } = direct 2

let root_alias = function (x as y) -> (y : int{ _ = x })
let root_alias_reversed = function (x as y) -> (x : int{ _ = y })

let let_alias = function x ->
  let y = x in
  (y : int{ _ = x })

[%%expect {|
val direct : (argument : int) -> int{ _ = argument } = <fun>
val direct_two : int{ _ = 2 } = 2
val root_alias : (argument : int) -> int{ _ = argument } = <fun>
val root_alias_reversed : (argument : int) -> int{ _ = argument } = <fun>
val let_alias : (argument : int) -> int{ _ = argument } = <fun>
|}]

let wrong_self = function x -> (x : int{ _ = x + 1 })

[%%expect {|
Line 1, characters 31-53:
1 | let wrong_self = function x -> (x : int{ _ = x + 1 })
                                   ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let outer_dependency (outer : int) =
  function value -> (value : int{ _ > outer })

let outer_dependency_bad = outer_dependency 10 5

[%%expect {|
val outer_dependency : (outer : int) -> int{ _ > outer } -> int{ _ > outer } =
  <fun>
Line 4, characters 47-48:
4 | let outer_dependency_bad = outer_dependency 10 5
                                                   ^
Error: Refinement verification failed (disproved)
|}]
