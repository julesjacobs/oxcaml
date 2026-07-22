(* TEST
 expect;
*)

module Ordinary : sig end = struct
  let rec step (n : int) : int{ _ = n } =
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Ordinary : sig end
|}]

module Whole_annotation : sig end = struct
  let rec step : (x : int) -> int{ _ = x } =
    fun n -> if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Whole_annotation : sig end
|}]

module Function_case : sig end = struct
  let identity : (x : int) -> int{ _ = x } = function
    | y -> y
end

[%%expect {|
module Function_case : sig end
|}]

module Labeled_recursive : sig end = struct
  let rec step ~(n : int) : int{ _ = n } =
    if n = 0 then 0 else step ~n:(n - 1) + 1
end

[%%expect {|
module Labeled_recursive : sig end
|}]

module Multiple_arguments : sig end = struct
  let rec add (n : int) (acc : int) : int{ _ = n + acc } =
    if n = 0 then acc else add (n - 1) (acc + 1)
end

[%%expect {|
module Multiple_arguments : sig end
|}]

module Mutual : sig end = struct
  let rec left (n : int) : int{ _ = n } =
    if n = 0 then 0 else right (n - 1) + 1
  and right (n : int) : int{ _ = n } =
    if n = 0 then 0 else left (n - 1) + 1
end

[%%expect {|
module Mutual : sig end
|}]

module Alias_outer_constraint : sig end = struct
  let rec step ((n as whole) : int) : int{ _ = whole } =
    let _ = whole in
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Alias_outer_constraint : sig end
|}]

module Alias_inner_constraint : sig end = struct
  let rec step ((n : int) as whole) : int{ _ = whole } =
    let _ = whole in
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Alias_inner_constraint : sig end
|}]

module Alias_nested_constraint : sig end = struct
  let rec step (((n as whole) : int) : int) : int{ _ = whole } =
    let _ = whole in
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Alias_nested_constraint : sig end
|}]

module Labeled_alias : sig end = struct
  let rec step ~value:((n as whole) : int) : int{ _ = whole } =
    let _ = whole in
    if n = 0 then 0 else step ~value:(n - 1) + 1
end

[%%expect {|
module Labeled_alias : sig end
|}]

module Alias_multiple_arguments : sig end = struct
  let rec add ((n as whole) : int) (acc : int)
      : int{ _ = whole + acc } =
    let _ = whole in
    if n = 0 then acc else add (n - 1) (acc + 1)
end

[%%expect {|
module Alias_multiple_arguments : sig end
|}]

module Alias_inner_name = struct
  let identity ((n as whole) : int) : int{ _ = n && _ = whole } = n
end

[%%expect {|
module Alias_inner_name :
  sig val identity : (whole : int) -> int{ _ = whole && _ = whole } end
|}]

module Recursive_alias_inner_name : sig end = struct
  let rec step ((n as whole) : int) : int{ _ = n } =
    let _ = whole in
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Recursive_alias_inner_name : sig end
|}]

module Recursive_double_alias : sig end = struct
  let rec step (((n as inner) as whole) : int)
      : int{ _ = n && _ = inner && _ = whole } =
    let _ = inner, whole in
    if n = 0 then 0 else step (n - 1) + 1
end

[%%expect {|
module Recursive_double_alias : sig end
|}]

module Alias_function_cases = struct
  let explicit : (whole : int) -> int{ _ = whole } = function
    | (inner as whole) ->
      let _ = whole in
      inner

  let inferred = function
    | (inner as whole) ->
      let _ = whole in
      let value = inner in
      (value : int{ _ = inner })
end

[%%expect {|
module Alias_function_cases :
  sig
    val explicit : (whole : int) -> int{ _ = whole }
    val inferred : (argument : int) -> int{ _ = argument }
  end
|}]

module Alias_translation_edges = struct
  let rec predicate_shadow ((n as whole) : int)
      : int{ let n = 0 in _ >= n } =
    let _ = whole in
    if n >= 0 then n else predicate_shadow (-n)

  let rec later_domain ((n as whole) : int)
      (acc : int{ _ >= n }) : int{ _ >= whole } =
    let _ = whole in
    let _ = if n = 0 then acc else later_domain (n - 1) acc in
    acc
end

[%%expect {|
module Alias_translation_edges :
  sig
    val predicate_shadow : int -> int{ let n = 0 in _ >= n }
    val later_domain :
      (whole : int) -> int{ _ >= whole } -> int{ _ >= whole }
  end
|}]

module Function_case_rejected : sig end = struct
  let identity : (x : int) -> int{ _ = x } = function
    | y -> y + 1
end

[%%expect {|
Line 3, characters 11-16:
3 |     | y -> y + 1
               ^^^^^
Error: Refinement verification failed (disproved)
|}]

module Wrong_base_rejected : sig end = struct
  let rec step (n : int) : int{ _ = n } =
    if n = 0 then 1 else step (n - 1) + 1
end

[%%expect {|
Line 3, characters 18-19:
3 |     if n = 0 then 1 else step (n - 1) + 1
                      ^
Error: Refinement verification failed (disproved)
|}]

module Wrong_step_rejected : sig end = struct
  let rec step (n : int) : int{ _ = n } =
    if n = 0 then 0 else step (n - 1) + 2
end

[%%expect {|
Line 3, characters 25-41:
3 |     if n = 0 then 0 else step (n - 1) + 2
                             ^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

module Alias_wrong_step_rejected : sig end = struct
  let rec step ((n as whole) : int) : int{ _ = whole } =
    if n = 0 then 0 else step (n - 1) + 2
end

[%%expect {|
Line 3, characters 25-41:
3 |     if n = 0 then 0 else step (n - 1) + 2
                             ^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]
