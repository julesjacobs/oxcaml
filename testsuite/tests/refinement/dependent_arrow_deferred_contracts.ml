(* TEST
 expect;
*)

module Accepted : sig end = struct
  let equal ~(x : int) ~(y : int{ _ = x }) = ()

  let single = equal ~y:1
  let () = single ~x:1

  let returning ~(x : int) ~(y : int{ _ = x }) : int{ _ = x } =
    let _ = y in
    x

  let returning_partial = returning ~y:1
  let returning_value : int{ _ = 1 } = returning_partial ~x:1

  let annotated : x:int{ 1 = _ } -> unit = equal ~y:1
  let () = annotated ~x:1

  let sum ~(x : int) ~(z : int) ~(y : int{ _ = x + z }) = ()

  let multiple = sum ~y:3
  let () = multiple ~x:1 ~z:2

  let staged = sum ~y:3
  let staged_again = staged ~z:2
  let () = staged_again ~x:1

  let sum_returning
      ~(x : int) ~(z : int) ~(y : int{ _ = x + z })
      : int{ _ = x + z } =
    let _ = y in
    x + z

  let staged_returning = sum_returning ~y:3
  let staged_returning_again = staged_returning ~z:2
  let staged_returning_value : int{ _ = 3 } =
    staged_returning_again ~x:1

  let twice
      ~(x : int) ~(y : int{ _ = x }) ~(z : int{ _ = x }) =
    ()

  let two_contracts = twice ~y:1 ~z:1
  let () = two_contracts ~x:1

  let result ~(x : int) ~(y : int) : int{ _ = x + y } = x + y
  let result_partial = result ~y:1
  let result_value : int{ _ = 2 } = result_partial ~x:1

  let positive ~(x : int) ~(y : int{ _ > 0 }) = ()
  let independent = positive ~y:1
  let () = independent ~x:2

  let with_optional
      ~(x : int) ?ignored:_ ~(y : int{ _ = x }) () =
    ()

  let optional_partial = with_optional ~y:1 ()
  let () = optional_partial ~x:1

  let () = equal ~y:1 ~x:1
end

[%%expect {|
module Accepted : sig end
|}]

module Single_rejected : sig end = struct
  let equal ~(x : int) ~(y : int{ _ = x }) = ()
  let partial = equal ~y:1
  let () = partial ~x:2
end

[%%expect {|
Line 4, characters 22-23:
4 |   let () = partial ~x:2
                          ^
Error: Refinement verification failed (disproved)
|}]

module Returning_rejected : sig end = struct
  let returning ~(x : int) ~(y : int{ _ = x }) : int{ _ = x } =
    let _ = y in
    x
  let partial = returning ~y:1
  let value = partial ~x:2
end

[%%expect {|
Line 6, characters 25-26:
6 |   let value = partial ~x:2
                             ^
Error: Refinement verification failed (disproved)
|}]

module Returning_result_rejected : sig end = struct
  let returning ~(x : int) ~(y : int{ _ = x }) : int{ _ = x } =
    let _ = y in
    x
  let partial = returning ~y:1
  let value = (partial ~x:1 : int{ _ = 2 })
end

[%%expect {|
Line 6, characters 14-43:
6 |   let value = (partial ~x:1 : int{ _ = 2 })
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

module Staged_returning_rejected : sig end = struct
  let returning
      ~(x : int) ~(z : int) ~(y : int{ _ = x + z })
      : int{ _ = x + z } =
    let _ = y in
    x + z
  let partial = returning ~y:3
  let partial_again = partial ~z:2
  let value = partial_again ~x:2
end

[%%expect {|
Line 9, characters 31-32:
9 |   let value = partial_again ~x:2
                                   ^
Error: Refinement verification failed (disproved)
|}]

module Multiple_rejected : sig end = struct
  let twice
      ~(x : int) ~(y : int{ _ = x }) ~(z : int{ _ = x }) =
    ()
  let partial = twice ~y:1 ~z:2
  let () = partial ~x:1
end

[%%expect {|
Line 6, characters 22-23:
6 |   let () = partial ~x:1
                          ^
Error: Refinement verification failed (disproved)
|}]

module Saturated_rejected : sig end = struct
  let equal ~(x : int) ~(y : int{ _ = x }) = ()
  let () = equal ~y:1 ~x:2
end

[%%expect {|
Line 3, characters 20-21:
3 |   let () = equal ~y:1 ~x:2
                        ^
Error: Refinement verification failed (disproved)
|}]
