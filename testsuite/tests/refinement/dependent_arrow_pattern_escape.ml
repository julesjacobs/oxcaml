(* TEST
 expect;
*)

module Direct_rejected : sig end = struct
  type t = Some of int
  let project (Some n) : int{ _ = n } = n
end

[%%expect {|
Line 3, characters 14-22:
3 |   let project (Some n) : int{ _ = n } = n
                  ^^^^^^^^
Error: a dependent function result cannot refer to non-root pattern variable n
|}]

module Alias_rejected : sig end = struct
  type t = Some of int
  let project ((Some n) as whole) : int{ _ = n } =
    let _ = whole in
    n
end

[%%expect {|
Line 3, characters 14-33:
3 |   let project ((Some n) as whole) : int{ _ = n } =
                  ^^^^^^^^^^^^^^^^^^^
Error: a dependent function result cannot refer to non-root pattern variable n
|}]

module Function_case_rejected : sig end = struct
  type t = Some of int
  let project = function
    | Some n ->
      let value = n in
      (value : int{ _ = n })
end

[%%expect {|
Line 4, characters 6-12:
4 |     | Some n ->
          ^^^^^^
Error: a dependent function result cannot refer to non-root pattern variable n
|}]
