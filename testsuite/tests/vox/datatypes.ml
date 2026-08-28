(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

type point = {x : int; y : int}

let refined_record_first (q : {q : point | q.x = 0}) : {r : int | r = 0} =
  let refine_ q = q in
  let r = q.x in
  refine_ r;;
[%%expect{|
type point = { x : int; y : int; }
val refined_record_first : {q : point | q.x = 0} -> {r : int | r = 0} = <fun>
|}]

let tuple () : {r : int * bool | r === (1, true)} =
  let r = (1, true) in
  refine_ r

let record () : {r : point | r.x = 1 && r.y = 2} =
  let r = {x = 1; y = 2} in
  refine_ r

let update (p : point) : {r : point | r.x = 1 && r.y = p.y} =
  let r = {p with x = 1} in
  refine_ r

let record_pattern (p : point) :
    {r : int | match p with {x; y = _} -> r = x} =
  match p with
  | {x; y = _} -> refine_ x;;
[%%expect{|
val tuple : unit -> {r : int * bool | r === (1, true)} = <fun>
val record : unit -> {r : point | (r.x = 1) && (r.y = 2)} = <fun>
val update : (p : point) -> {r : point | (r.x = 1) && (r.y = p.y)} = <fun>
val record_pattern :
  (p : point) -> {r : int | match p with | { x; y = _ } -> r = x} = <fun>
|}]

let bool_pattern (b : bool) :
    {r : bool | match r with true -> true | false -> true} =
  refine_ b;;
[%%expect{|
val bool_pattern :
  bool -> {r : bool | match r with | true -> true | false -> true} = <fun>
|}]

type pair = Pair of int * int

type 'a box = Box of 'a

let separate_instances (i : int @ immutable) (b : bool @ immutable) :
    {u : unit | Box i === Box i && Box b === Box b} =
  let u = () in
  refine_ u;;
[%%expect{|
type pair = Pair of int * int
type 'a box = Box of 'a
val separate_instances :
  (i : int) ->
  (b : bool) -> {u : unit | ((Box i) === (Box i)) && ((Box b) === (Box b))} =
  <fun>
|}]

let alias (p : pair) : {r : pair | r === p} =
  match p with
  | (Pair (_, _) as whole) ->
    let r = whole in
    refine_ r

let or_pattern (p : pair) :
    {r : int |
      match p with
      | Pair (0, x) | Pair (x, 0) -> r = x
      | Pair (_, _) -> r = 0} =
  match p with
  | Pair (0, x) | Pair (x, 0) ->
    refine_ x
  | Pair (_, _) ->
    let r = 0 in
    refine_ r;;
[%%expect{|
val alias : (p : pair) -> {r : pair | r === p} = <fun>
val or_pattern :
  (p : pair) ->
  {r : int
    | match p with
      | Pair (0, x) | Pair (x, 0) -> r = x
      | Pair (_, _) -> r = 0} =
  <fun>
|}]

let positive_axis (p : pair) : {r : int | r > 0} =
  match p with
  | Pair (0, x) when x > 0 -> refine_ x
  | _ -> raise Not_found;;
[%%expect{|
val positive_axis : pair -> {r : int | r > 0} = <fun>
|}]

type sum = First of int | Second of int

type token
type wrapped = Wrap of token

let opaque_payload_injective (left @ immutable) (right @ immutable)
    (premise : {u : unit | Wrap left === Wrap right}) :
    {u : unit | left === right} =
  let refine_ _proof = premise in
  let u = () in
  refine_ u

let injective (left @ immutable) (right @ immutable)
    (premise : {u : unit | First left === First right}) :
    {u : unit | left === right} =
  let refine_ _proof = premise in
  let u = () in
  refine_ u

let disjoint (left @ immutable) (right @ immutable)
    (premise : {u : unit | First left === Second right}) :
    {u : unit | (First left === Second right) && false} =
  let refine_ _proof = premise in
  let u = () in
  refine_ u;;
[%%expect{|
type sum = First of int | Second of int
type token
type wrapped = Wrap of token
val opaque_payload_injective :
  (left : token) ->
  (right : token) ->
  {u : unit | (Wrap left) === (Wrap right)} -> {u : unit | left === right} =
  <fun>
val injective :
  (left : int) ->
  (right : int) ->
  {u : unit | (First left) === (First right)} -> {u : unit | left === right} =
  <fun>
val disjoint :
  (left : int) ->
  (right : int) ->
  {u : unit | (First left) === (Second right)} ->
  {u : unit | ((First left) === (Second right)) && false} = <fun>
|}]

type 'a tree = Leaf of 'a | Node of 'a * 'a tree [@@inductive]

module Tree = struct
  let rec (copy @ total) :
      (tree : 'a tree) ->
      {r : 'a tree | r === tree} @ immutable contended =
    fun tree -> match tree with
    | Leaf value ->
      let r = Leaf value in
      refine_ r
    | Node (value, rest) ->
      let copied : {r : 'a tree | r === rest} = copy rest in
      let refine_ copied = copied in
      let r = Node (value, copied) in
      refine_ r

  let rec (size @ total) tree : {r : Bigint.t | r > 0Z} =
    match tree with
    | Leaf _ ->
      let r = 1Z in
      refine_ r
    | Node (_, rest) ->
      let rest : {r : Bigint.t | r > 0Z} = size rest in
      let refine_ rest = rest in
      let r = Bigint.(1Z + rest) in
      refine_ r
end;;
[%%expect{|
type 'a tree = Leaf of 'a | Node of 'a * 'a tree [@@inductive]
module Tree :
  sig
    val copy : (tree : 'a tree) -> {r : 'a tree | r === tree} @ immutable
    val size : 'a tree -> {r : Bigint.t | r > (Bigint.of_int 0)}
  end
|}]

module Stable = struct
  let (id @ total) x = x
end

let total_congruence (p : point) (q : {q : point | q === p})
    : {u : unit |
        let refine_ q = q in
        Stable.id q === Stable.id p} =
  let u = () in
  refine_ u;;
[%%expect{|
module Stable : sig val id : 'a -> 'a end
val total_congruence :
  (p : point) ->
  (q : {q : point | q === p}) ->
  {u : unit | let refine_ q = q in (Stable.id q) === (Stable.id p)} = <fun>
|}]

type ordinary = Ordinary_stop | Ordinary_more of ordinary

let ordinary_stop () : {r : ordinary | r === Ordinary_stop} =
  let r = Ordinary_stop in
  refine_ r;;
[%%expect{|
type ordinary = Ordinary_stop | Ordinary_more of ordinary
val ordinary_stop : unit -> {r : ordinary | r === Ordinary_stop} = <fun>
|}]

let ordinary_opaque () : {r : ordinary | r === r} =
  let r = Ordinary_more Ordinary_stop in
  refine_ r;;
[%%expect{|
val ordinary_opaque : unit -> {r : ordinary | r === r} = <fun>
|}]

type mutable_point = {mutable mx : int; my : int}

let mutable_record_opaque () : {r : mutable_point | r === r} =
  let r = {mx = 0; my = 1} in
  refine_ r

let mutable_field_opaque (p : mutable_point) : {r : int | r = r} =
  let r = p.mx in
  refine_ r;;
[%%expect{|
type mutable_point = { mutable mx : int; my : int; }
val mutable_record_opaque : unit -> {r : mutable_point | r === r} = <fun>
val mutable_field_opaque : mutable_point -> {r : int | r = r} = <fun>
|}]

let mutable_field_not_known (p : mutable_point) : {r : int | r = 0} =
  let r = p.mx in
  refine_ r;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ r;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let ordinary_is_not_native () :
    {u : unit |
      (Ordinary_stop === Ordinary_more Ordinary_stop) === false} =
  let u = () in
  refine_ u;;
[%%expect{|
Line 3, characters 25-52:
3 |       (Ordinary_stop === Ordinary_more Ordinary_stop) === false} =
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 5, characters 2-11:
5 |   refine_ u;;
      ^^^^^^^^^
  Required by this refinement introduction
|}]

type non_well_founded = Loop of non_well_founded [@@inductive]

let non_well_founded_is_opaque (tree @ immutable) :
    {u : unit | tree === Loop tree} =
  let u = () in
  refine_ u;;
[%%expect{|
type non_well_founded = Loop of non_well_founded [@@inductive]
Line 4, characters 25-34:
4 |     {u : unit | tree === Loop tree} =
                             ^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 6, characters 2-11:
6 |   refine_ u;;
      ^^^^^^^^^
  Required by this refinement introduction
|}]

type nested_ordinary =
  | Nested_stop
  | Nested_more of (nested_ordinary * int)

let nested_ordinary_is_not_native (tree @ immutable) :
    {u : unit |
      match tree with
      | Nested_stop -> true
      | Nested_more (rest, n) -> rest === rest && n === n} =
  let u = () in
  refine_ u;;
[%%expect{|
type nested_ordinary = Nested_stop | Nested_more of (nested_ordinary * int)
Line 9, characters 8-29:
9 |       | Nested_more (rest, n) -> rest === rest && n === n} =
            ^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used in an expression (at lines 7-9, characters 6-57).
|}]

type 'a nonregular =
  | Nonregular_stop
  | Nonregular_more of ('a * 'a) nonregular

let nonregular_is_not_native (tree : int nonregular) :
    {u : unit |
      match tree with
      | Nonregular_stop -> true
      | Nonregular_more _ -> true} =
  let u = () in
  refine_ u;;
[%%expect{|
type 'a nonregular =
    Nonregular_stop
  | Nonregular_more of ('a * 'a) nonregular
Line 9, characters 8-25:
9 |       | Nonregular_more _ -> true} =
            ^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used in an expression (at lines 7-9, characters 6-33).
|}]
