(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Labelled and optional arguments can be named by dependent binders. The
   binder of an optional argument denotes the option the caller passed. *)

type at_least = capacity:(c : int) -> {r : int | r >= c}

type or_default =
  ?capacity:(c : int) -> unit ->
  {r : int | match c with None -> r = 16 | Some n -> r = n};;
[%%expect{|
type at_least = capacity:(c : int) -> {r : int | r >= c}
type or_default =
    ?capacity:(c : int) ->
    unit -> {r : int | match c with | None -> r = 16 | Some n -> r = n}
|}]

let at_least : at_least = fun ~capacity -> capacity

let or_default : or_default = fun ?capacity () ->
  match capacity with None -> 16 | Some n -> n;;
[%%expect{|
val at_least : at_least = <fun>
val or_default : or_default = <fun>
|}]

(* A parameter annotation and the result mention labelled parameters. *)
let clamp ~(low : int) ~(high : {h : int | h >= low}) (x : int)
    : {r : int | low <= r && r <= high} =
  if x < low then low else if x > high then high else x;;
[%%expect{|
val clamp :
  low:(low : int) ->
  high:(high : {h : int | h >= low}) ->
  int -> {r : int | (low <= r) && (r <= high)} = <fun>
|}]

(* Mixed: an optional binder before a positional one. *)
let shift ?by (x : int)
    : {r : int | match by with None -> r = x | Some n -> r = x + n} =
  match by with None -> x | Some n -> x + n;;
[%%expect{|
val shift :
  ?by:(by : int) ->
  (x : int) ->
  {r : int | match by with | None -> r = x | Some n -> r = (x + n)} = <fun>
|}]

(* Application substitutes the binder: an identifier, a constant, a
   compound argument, [Some] for [~capacity] and [None] for an omitted
   optional argument. *)
let use_labelled (n : int) : {r : int | r >= n} = at_least ~capacity:n
let use_constant () : {r : int | r >= 3} = at_least ~capacity:3
let use_compound (n : int) : {r : int | r >= n + 1} =
  at_least ~capacity:(n + 1)
let use_none () : {r : int | r = 16} = or_default ()
let use_some (n : int) : {r : int | r = n} = or_default ~capacity:n ()
let use_some_constant () : {r : int | r = 3} = or_default ~capacity:3 ()
let use_some_compound (n : int) : {r : int | r = n + 1} =
  or_default ~capacity:(n + 1) ()
let use_option (o : int option)
    : {r : int | match o with None -> r = 16 | Some n -> r = n} =
  or_default ?capacity:o ()
let use_option_literal () : {r : int | r = 3} = or_default ?capacity:(Some 3) ()
let use_shift (x : int) : {r : int | r = x} = shift x
let use_shift_by (x : int) : {r : int | r = x + 2} = shift ~by:2 x;;
[%%expect{|
val use_labelled : (n : int) -> {r : int | r >= n} = <fun>
val use_constant : unit -> {r : int | r >= 3} = <fun>
val use_compound : (n : int) -> {r : int | r >= (n + 1)} = <fun>
val use_none : unit -> {r : int | r = 16} = <fun>
val use_some : (n : int) -> {r : int | r = n} = <fun>
val use_some_constant : unit -> {r : int | r = 3} = <fun>
val use_some_compound : (n : int) -> {r : int | r = (n + 1)} = <fun>
val use_option :
  (o : int option) ->
  {r : int | match o with | None -> r = 16 | Some n -> r = n} = <fun>
val use_option_literal : unit -> {r : int | r = 3} = <fun>
val use_shift : (x : int) -> {r : int | r = x} = <fun>
val use_shift_by : (x : int) -> {r : int | r = (x + 2)} = <fun>
|}]

(* Arguments in any order. *)
let clamp_positional_first (x : int) : {r : int | 0 <= r && r <= 10} =
  clamp x ~high:10 ~low:0
let clamp_commuted (lo : int) (hi : {h : int | h >= lo}) (x : int)
    : {r : int | lo <= r && r <= hi} =
  clamp ~high:hi x ~low:lo
let shift_commuted (x : int) : {r : int | r = x + 2} = shift x ~by:2
let clamp_unlabelled (x : int) : {r : int | 0 <= r && r <= 10} =
  clamp 0 10 x;;
[%%expect{|
val clamp_positional_first : int -> {r : int | (0 <= r) && (r <= 10)} = <fun>
val clamp_commuted :
  (lo : int) ->
  (hi : {h : int | h >= lo}) -> int -> {r : int | (lo <= r) && (r <= hi)} =
  <fun>
val shift_commuted : (x : int) -> {r : int | r = (x + 2)} = <fun>
Line 8, characters 2-7:
8 |   clamp 0 10 x;;
      ^^^^^
Warning 6 [labels-omitted]: labels "low", "high" were omitted in the application
  of this function.

val clamp_unlabelled : int -> {r : int | (0 <= r) && (r <= 10)} = <fun>
|}]

(* Omitting arguments whose binders no later supplied argument mentions. *)
let clamp_3 = clamp 3
let clamp_low = clamp ~low:0

(* Passing [or_default] where no optional argument is expected applies it
   to [None]. *)
let apply
    (g : unit -> {r : int | match (None : int option) with
                            | None -> r = 16 | Some n -> r = n})
    : {r : int | r = 16} = g ()
let eliminated = apply or_default;;
[%%expect{|
val clamp_3 :
  low:(low : int) ->
  high:(high : {h : int | h >= low}) -> {r : int | (low <= r) && (r <= high)} =
  <fun>
val clamp_low :
  high:(high : {h : int | h >= 0}) ->
  int -> {r : int | (0 <= r) && (r <= high)} = <fun>
val apply :
  (unit ->
   {r : int
     | match (None : int option) with | None -> r = 16 | Some n -> r = n}) ->
  {r : int | r = 16} = <fun>
val eliminated : int = 16
|}]

(* Rejected: [~high]'s type mentions [low], which this application omits. *)
let omitted_low = clamp ~high:10;;
[%%expect{|
Line 1, characters 30-32:
1 | let omitted_low = clamp ~high:10;;
                                  ^^
Error: The type of this argument mentions the dependent parameter "low", which this application omits
|}]

(* The same for an omitted positional binder. *)
let omitted_positional
    (f : (a : int) -> lab:{b : int | b > a} -> int) = f ~lab:3;;
[%%expect{|
Line 2, characters 61-62:
2 |     (f : (a : int) -> lab:{b : int | b > a} -> int) = f ~lab:3;;
                                                                 ^
Error: The type of this argument mentions the dependent parameter "a", which this application omits
|}]

(* Rejected: the omitted optional argument is [None]. *)
let wrong_default () : {r : int | r = 17} = or_default ();;
[%%expect{|
Line 1, characters 44-57:
1 | let wrong_default () : {r : int | r = 17} = or_default ();;
                                                ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 34-40:
1 | let wrong_default () : {r : int | r = 17} = or_default ();;
                                      ^^^^^^
  The refinement is stated here.
|}]

(* Rejected: a defaulted optional parameter does not name the option. *)
let defaulted : or_default = fun ?(capacity = 16) () -> capacity;;
[%%expect{|
Line 1, characters 33-49:
1 | let defaulted : or_default = fun ?(capacity = 16) () -> capacity;;
                                     ^^^^^^^^^^^^^^^^
Error: An optional parameter with a default value cannot be named by a dependent binder; take the option and match on it instead
|}]

let defaulted_inferred ?(capacity : int = 16) ()
    : {r : int | r >= capacity} =
  capacity;;
[%%expect{|
Line 1, characters 23-45:
1 | let defaulted_inferred ?(capacity : int = 16) ()
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: An optional parameter with a default value cannot be named by a dependent binder; take the option and match on it instead
|}]

(* Signature inclusion. *)
module Included : sig
  val at_least : capacity:(c : int) -> {r : int | r >= c}
  val shift : ?by:(b : int) -> (x : int) ->
    {r : int | match b with None -> r = x | Some n -> r = x + n}
end = struct
  let at_least ~(capacity : int) : {r : int | r >= capacity} = capacity
  let shift = shift
end;;
[%%expect{|
module Included :
  sig
    val at_least : capacity:(c : int) -> {r : int | r >= c}
    val shift :
      ?by:(b : int) ->
      (x : int) ->
      {r : int | match b with | None -> r = x | Some n -> r = (x + n)}
  end
|}]

module Too_weak : sig
  val at_least : capacity:(c : int) -> {r : int | r > c}
end = struct
  let at_least ~(capacity : int) : {r : int | r >= capacity} = capacity
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let at_least ~(capacity : int) : {r : int | r >= capacity} = capacity
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val at_least :
             capacity:(capacity : int) -> {r : int | r >= capacity}
         end
       is not included in
         sig val at_least : capacity:(c : int) -> {r : int | r > c} end
       Values do not match:
         val at_least :
           capacity:(capacity : int) -> {r : int | r >= capacity}
       is not included in
         val at_least : capacity:(c : int) -> {r : int | r > c}
       The type "capacity:(capacity : int) -> {r : int | r >= capacity}"
       is not compatible with the type
         "capacity:(c : int) -> {r : int | r > c}"
       Type "{r : int | r >= c}" is not compatible with type "{r : int | r > c}"
|}]

module Wrong_label : sig
  val at_least : size:(c : int) -> {r : int | r >= c}
end = struct
  let at_least ~(capacity : int) : {r : int | r >= capacity} = capacity
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let at_least ~(capacity : int) : {r : int | r >= capacity} = capacity
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val at_least :
             capacity:(capacity : int) -> {r : int | r >= capacity}
         end
       is not included in
         sig val at_least : size:(c : int) -> {r : int | r >= c} end
       Values do not match:
         val at_least :
           capacity:(capacity : int) -> {r : int | r >= capacity}
       is not included in
         val at_least : size:(c : int) -> {r : int | r >= c}
       The type "capacity:(capacity : int) -> {r : int | r >= capacity}"
       is not compatible with the type "size:(c : int) -> {r : int | r >= c}"
       Labels "capacity" and "size" do not match
|}]
