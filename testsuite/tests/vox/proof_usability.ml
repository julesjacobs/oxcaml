(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

let loop_bounds () =
  for i = 0 to 15 do
    let (_ : {v : int | 0 <= v && v <= 15}) = i in ()
  done;
  for i = 15 downto 0 do
    let (_ : {v : int | 0 <= v && v <= 15}) = i in ()
  done;;
[%%expect{|
val loop_bounds : unit -> unit = <fun>
|}]

let bad_loop_bound () =
  for i = 0 to 15 do
    let (_ : {v : int | v < 15}) = i in ()
  done;;
[%%expect{|
Line 3, characters 35-36:
3 |     let (_ : {v : int | v < 15}) = i in ()
                                       ^
Error: Refinement could not be proved (counterexample)
|}]

let empty_loop () =
  for i = 1 to 0 do unreachable_ () done;;
[%%expect{|
Line 2, characters 20-35:
2 |   for i = 1 to 0 do unreachable_ () done;;
                        ^^^^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val empty_loop : unit -> unit = <fun>
|}]

let body_fact_does_not_escape () : {u : unit | false} =
  for i = 1 to 0 do unreachable_ () done;
  ();;
[%%expect{|
Line 2, characters 20-35:
2 |   for i = 1 to 0 do unreachable_ () done;
                        ^^^^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

Line 3, characters 2-4:
3 |   ();;
      ^^
Error: Refinement could not be proved (counterexample)
|}]

let string_roundtrip () : {s : string | s === "hello"} = "hello";;
[%%expect{|
val string_roundtrip : unit -> {s : string | s === "hello"} = <fun>
|}]

let string_distinct () : {u : unit | not ("hello" === "world")} = ();;
[%%expect{|
val string_distinct : unit -> {u : unit | not ("hello" === "world")} = <fun>
|}]

let wrong_string () : {s : string | s === "world"} = "hello";;
[%%expect{|
Line 1, characters 53-60:
1 | let wrong_string () : {s : string | s === "world"} = "hello";;
                                                         ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let string_pattern (s : {s : string | s === "hello"}) :
    {b : bool | b} =
  match s with "hello" -> true | _ -> false;;
[%%expect{|
val string_pattern : {s : string | s === "hello"} -> {b : bool | b} = <fun>
|}]

let byte_strings () : {u : unit | not ("\000" === "") &&
    not ("\255" === "\254") && "\034" === "\034"} = ();;
[%%expect{|
val byte_strings :
  unit ->
  {u : unit
    | (not ("\000" === "")) && ((not ("\255" === "\254")) && ("\"" === "\""))} =
  <fun>
|}]

let physical_string_equality (s : {s : string | s === "hello"}) :
    {b : bool | b} = s == "hello";;
[%%expect{|
Line 2, characters 21-33:
2 |     {b : bool | b} = s == "hello";;
                         ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Invariant (X : sig val p : int -> bool @@ total end) = struct
  let[@def] (valid @ total) (x : int) = X.p x
end;;
[%%expect{|
module Invariant :
  functor (X : sig val p : int -> bool @@ total end) ->
    sig
      val valid : int -> bool
      val valid_def : (x : int) -> {u : unit | (valid x) === (X.p x)}
    end
|}]

module Aliased (X : sig val p : int -> bool @@ total end) = struct
  module I = Invariant(X)
  module Alias = I
  let (identity @ total) (x : {x : int | I.valid x}) :
      {y : int | Alias.valid y} = x
end;;
[%%expect{|
module Aliased :
  functor (X : sig val p : int -> bool @@ total end) ->
    sig
      module I :
        sig
          val valid : int -> bool
          val valid_def : (x : int) -> {u : unit | (valid x) === (X.p x)}
        end
      module Alias = I
      val identity : {x : int | I.valid x} -> {y : int | Alias.valid y}
    end
|}]

let dependent_bounds (low : int) (high : int) =
  for i = low to high do
    let (_ : {v : int | low <= v && v <= high}) = i in
    for j = i downto low do
      let (_ : {v : int | low <= v && v <= high}) = j in ()
    done
  done;;
[%%expect{|
val dependent_bounds : int -> int -> unit = <fun>
|}]

let extreme_bounds () =
  for i = -4611686018427387904 to 4611686018427387903 do
    let (_ : {v : int | -4611686018427387904 <= v &&
        v <= 4611686018427387903}) = i in ()
  done;;
[%%expect{|
val extreme_bounds : unit -> unit = <fun>
|}]

let descending_bad () =
  for i = 15 downto 0 do
    let (_ : {v : int | 0 < v}) = i in ()
  done;;
[%%expect{|
Line 3, characters 34-35:
3 |     let (_ : {v : int | 0 < v}) = i in ()
                                      ^
Error: Refinement could not be proved (counterexample)
|}]

module Used_alias (X : sig val p : int -> bool @@ total end) = struct
  module A = Aliased(X)
  let (identity @ total) (x : {x : int | A.I.valid x}) :
      {y : int | A.Alias.valid y} = x
end;;
[%%expect{|
module Used_alias :
  functor (X : sig val p : int -> bool @@ total end) ->
    sig
      module A :
        sig
          module I :
            sig
              val valid : int -> bool
              val valid_def : (x : int) -> {u : unit | (valid x) === (X.p x)}
            end
          module Alias = I
          val identity : {x : int | I.valid x} -> {y : int | Alias.valid y}
        end
      val identity : {x : int | A.I.valid x} -> {y : int | A.Alias.valid y}
    end
|}]

module Separate_alias (X : sig val p : int -> bool @@ total end) = struct
  module A = Aliased(X)
  module I = Invariant(X)
  let (identity @ total) (x : {x : int | A.I.valid x}) :
      {y : int | I.valid y} =
    A.I.valid_def x; I.valid_def x; x
end;;
[%%expect{|
module Separate_alias :
  functor (X : sig val p : int -> bool @@ total end) ->
    sig
      module A :
        sig
          module I :
            sig
              val valid : int -> bool
              val valid_def : (x : int) -> {u : unit | (valid x) === (X.p x)}
            end
          module Alias = I
          val identity : {x : int | I.valid x} -> {y : int | Alias.valid y}
        end
      module I :
        sig
          val valid : int -> bool
          val valid_def : (x : int) -> {u : unit | (valid x) === (X.p x)}
        end
      val identity : {x : int | A.I.valid x} -> {y : int | I.valid y}
    end
|}]
