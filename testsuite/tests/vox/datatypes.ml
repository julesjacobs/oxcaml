(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

type point = {x : int; y : int}

let refined_record_first (q : {q : point | q.x = 0}) : {r : int | r = 0} =
  let q = q in
  let r = q.x in
  r;;
[%%expect{|
type point = { x : int; y : int; }
val refined_record_first : {q : point | q.x = 0} -> {r : int | r = 0} = <fun>
|}]

let tuple () : {r : int * bool | r === (1, true)} =
  let r = (1, true) in
  r

let record () : {r : point | r.x = 1 && r.y = 2} =
  let r = {x = 1; y = 2} in
  r

let update (p : point) : {r : point | r.x = 1 && r.y = p.y} =
  let r = {p with x = 1} in
  r

let record_pattern (p : point) :
    {r : int | match p with {x; y = _} -> r = x} =
  match p with
  | {x; y = _} -> x;;
[%%expect{|
val tuple : unit -> {r : int * bool | r === (1, true)} = <fun>
val record : unit -> {r : point | (r.x = 1) && (r.y = 2)} = <fun>
val update : (p : point) -> {r : point | (r.x = 1) && (r.y = p.y)} = <fun>
val record_pattern :
  (p : point) -> {r : int | match p with | { x; y = _ } -> r = x} = <fun>
|}]

let bool_pattern (b : bool) :
    {r : bool | match r with true -> true | false -> true} =
  b;;
[%%expect{|
val bool_pattern :
  bool -> {r : bool | match r with | true -> true | false -> true} = <fun>
|}]

type pair = Pair of int * int

type 'a box = Box of 'a

let separate_instances (i : int @ immutable) (b : bool @ immutable) :
    {u : unit | Box i === Box i && Box b === Box b} =
  ();;
[%%expect{|
type pair = Pair of int * int
type 'a box = Box of 'a
val separate_instances :
  (i : int) @ immutable ->
  (b : bool) @ immutable ->
  {u : unit | ((Box i) === (Box i)) && ((Box b) === (Box b))} = <fun>
|}]

module Nullary = struct
  type 'a maybe = Empty | Other [@@inductive]
  let polymorphic_empty = Empty
  type 'a maybe_alias = 'a maybe = Empty | Other [@@inductive]

  let instantiate_empty_int () : {r : int maybe | r === Empty} =
    let (empty @ total) : int maybe = polymorphic_empty in
    empty

  let instantiate_empty_bool () : {r : bool maybe | r === Empty} =
    let (empty @ total) : bool maybe = polymorphic_empty in
    empty

  let instantiate_empty_alias () : {r : int maybe_alias | r === Empty} =
    let (empty @ total) : int maybe_alias = polymorphic_empty in
    empty
end;;
[%%expect{|
module Nullary :
  sig
    type 'a maybe = Empty | Other
    [@@inductive]
    val polymorphic_empty : 'a maybe
    type 'a maybe_alias = 'a maybe = Empty | Other
    [@@inductive]
    val instantiate_empty_int : unit -> {r : int maybe | r === Empty}
    val instantiate_empty_bool : unit -> {r : bool maybe | r === Empty}
    val instantiate_empty_alias : unit -> {r : int maybe_alias | r === Empty}
  end
|}]

module Wrong_nullary = struct
  type 'a maybe = Empty | Other [@@inductive]
  let polymorphic_empty = Empty
  let wrong_empty () : {r : int maybe | r === Other} =
    let (empty @ total) : int maybe = polymorphic_empty in
    empty
end;;
[%%expect{|
Line 6, characters 4-9:
6 |     empty
        ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let alias (p : pair) : {r : pair | r === p} =
  match p with
  | (Pair (_, _) as whole) ->
    let r = whole in
    r

let or_pattern (p : pair) :
    {r : int |
      match p with
      | Pair (0, x) | Pair (x, 0) -> r = x
      | Pair (_, _) -> r = 0} =
  match p with
  | Pair (0, x) | Pair (x, 0) ->
    x
  | Pair (_, _) ->
    let r = 0 in
    r;;
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
  | Pair (0, x) when x > 0 -> x
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
  premise;
  ()

let injective (left @ immutable) (right @ immutable)
    (premise : {u : unit | First left === First right}) :
    {u : unit | left === right} =
  premise;
  ()

let disjoint (left @ immutable) (right @ immutable) :
    {u : unit | (First left === Second right) === false} =
  ();;
[%%expect{|
type sum = First of int | Second of int
type token
type wrapped = Wrap of token
val opaque_payload_injective :
  (left : token) @ immutable ->
  (right : token) @ immutable ->
  {u : unit | (Wrap left) === (Wrap right)} -> {u : unit | left === right} =
  <fun>
val injective :
  (left : int) @ immutable ->
  (right : int) @ immutable ->
  {u : unit | (First left) === (First right)} -> {u : unit | left === right} =
  <fun>
val disjoint :
  (left : int) @ immutable ->
  (right : int) @ immutable ->
  {u : unit | ((First left) === (Second right)) === false} = <fun>
|}]

type 'a tree = Leaf of 'a | Node of 'a * 'a tree [@@inductive]

module Tree = struct
  let rec (copy @ total) :
      (tree : 'a tree) ->
      {r : 'a tree | r === tree} @ immutable contended =
    fun tree -> match tree with
    | Leaf value ->
      let r = Leaf value in
      r
    | Node (value, rest) ->
      let copied : {r : 'a tree | r === rest} = copy rest in
      let copied = copied in
      let r = Node (value, copied) in
      r

  let rec (size @ total) tree : {r : Bigint.t | r > 0Z} =
    match tree with
    | Leaf _ ->
      let r = 1Z in
      r
    | Node (_, rest) ->
      let rest : {r : Bigint.t | r > 0Z} = size rest in
      let rest = rest in
      let r = Bigint.(1Z + rest) in
      r
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
        let q = q in
        Stable.id q === Stable.id p} =
  ();;
[%%expect{|
module Stable : sig val id : 'a -> 'a end
val total_congruence :
  (p : point) ->
  (q' : {q : point | q === p}) ->
  {u : unit | let q = q' in (Stable.id q) === (Stable.id p)} = <fun>
|}]

type ordinary = Ordinary_stop | Ordinary_more of ordinary

let ordinary_stop () : {r : ordinary | r === Ordinary_stop} =
  let r = Ordinary_stop in
  r;;
[%%expect{|
type ordinary = Ordinary_stop | Ordinary_more of ordinary
val ordinary_stop : unit -> {r : ordinary | r === Ordinary_stop} = <fun>
|}]

let ordinary_opaque () : {r : ordinary | r === r} =
  let r = Ordinary_more Ordinary_stop in
  r;;
[%%expect{|
val ordinary_opaque : unit -> {r : ordinary | r === r} = <fun>
|}]

type ordinary_wrapper = Ordinary_wrap of ordinary

let wrapper_first (wrapped @ immutable) (tree @ immutable) :
    {u : unit |
      match wrapped with Ordinary_wrap _ -> tree === tree} =
  ();;
[%%expect{|
type ordinary_wrapper = Ordinary_wrap of ordinary
val wrapper_first :
  (wrapped : ordinary_wrapper) @ immutable ->
  (tree : 'a) @ immutable ->
  {u : unit | match wrapped with | Ordinary_wrap _ -> tree === tree} = <fun>
|}]

let recursive_first (tree @ immutable) (wrapped @ immutable) :
    {u : unit |
      match wrapped with Ordinary_wrap _ -> tree === tree} =
  ();;
[%%expect{|
val recursive_first :
  (tree : 'a) @ immutable ->
  (wrapped : ordinary_wrapper) @ immutable ->
  {u : unit | match wrapped with | Ordinary_wrap _ -> tree === tree} = <fun>
|}]

(* Ordinary wrappers are now encoded with an opaque payload; a false claim
   about the payload is still rejected. *)
let wrapper_payload_unknown (wrapped @ immutable) :
    {u : unit |
      match wrapped with Ordinary_wrap tree -> tree === Ordinary_stop} =
  ();;
[%%expect{|
Line 4, characters 2-4:
4 |   ();;
      ^^
Error: Refinement could not be proved (counterexample)
|}]

type mutable_point = {mutable mx : int; my : int}

let mutable_record_opaque () : {r : mutable_point | r === r} =
  let r = {mx = 0; my = 1} in
  r

let mutable_field_opaque (p : mutable_point) : {r : int | r = r} =
  let r = p.mx in
  r;;
[%%expect{|
type mutable_point = { mutable mx : int; my : int; }
val mutable_record_opaque : unit -> {r : mutable_point | r === r} = <fun>
val mutable_field_opaque : mutable_point -> {r : int | r = r} = <fun>
|}]

let mutable_field_not_known (p : mutable_point) : {r : int | r = 0} =
  let r = p.mx in
  r;;
[%%expect{|
Line 3, characters 2-3:
3 |   r;;
      ^
Error: Refinement could not be proved (counterexample)
|}]

let ordinary_is_not_native () :
    {u : unit |
      (Ordinary_stop === Ordinary_more Ordinary_stop) === false} =
  ();;
[%%expect{|
Line 3, characters 25-52:
3 |       (Ordinary_stop === Ordinary_more Ordinary_stop) === false} =
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 4, characters 2-4:
4 |   ();;
      ^^
  Required by this refinement introduction
|}]

type non_well_founded = Loop of non_well_founded [@@inductive]

let non_well_founded_is_opaque (tree @ immutable) :
    {u : unit | tree === Loop tree} =
  ();;
[%%expect{|
type non_well_founded = Loop of non_well_founded [@@inductive]
Line 4, characters 25-34:
4 |     {u : unit | tree === Loop tree} =
                             ^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 5, characters 2-4:
5 |   ();;
      ^^
  Required by this refinement introduction
|}]

type non_well_wrapper = Non_well_wrap of non_well_founded

let non_well_wrapper_first (wrapped @ immutable) (tree @ immutable) :
    {u : unit |
      match wrapped with Non_well_wrap _ -> tree === tree} =
  ();;
[%%expect{|
type non_well_wrapper = Non_well_wrap of non_well_founded
Line 5, characters 25-40:
5 |       match wrapped with Non_well_wrap _ -> tree === tree} =
                             ^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 6, characters 2-4:
6 |   ();;
      ^^
  Required by this refinement introduction
|}]

let non_well_recursive_first (tree @ immutable) (wrapped @ immutable) :
    {u : unit |
      match wrapped with Non_well_wrap _ -> tree === tree} =
  ();;
[%%expect{|
Line 3, characters 25-40:
3 |       match wrapped with Non_well_wrap _ -> tree === tree} =
                             ^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 4, characters 2-4:
4 |   ();;
      ^^
  Required by this refinement introduction
|}]

type finite_point = {value : int}
type non_well_with_point =
  | Loop_with_point of non_well_with_point * finite_point
  [@@inductive]

let finite_dependency_survives
    (loop : non_well_with_point @ immutable) (point @ immutable) :
    {u : unit | loop === loop && point.value = point.value} =
  ();;
[%%expect{|
type finite_point = { value : int; }
type non_well_with_point =
    Loop_with_point of non_well_with_point * finite_point
[@@inductive]
val finite_dependency_survives :
  (loop : non_well_with_point) @ immutable ->
  (point : finite_point) @ immutable ->
  {u : unit | (loop === loop) && (point.value = point.value)} = <fun>
|}]

type nested_ordinary =
  | Nested_stop
  | Nested_more of (nested_ordinary * int)

let nested_ordinary_is_not_native (tree @ immutable) :
    {u : unit |
      match tree with
      | Nested_stop -> true
      | Nested_more (rest, n) -> rest === rest && n === n} =
  ();;
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
  ();;
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

type 'a changing_record = {payload : 'a; count : int}

let change_record_type (p : int changing_record) :
    {r : bool changing_record | r.payload && r.count = p.count} =
  let r = {p with payload = true} in
  r;;
[%%expect{|
type 'a changing_record = { payload : 'a; count : int; }
val change_record_type :
  (p : int changing_record) ->
  {r : bool changing_record | r.payload && (r.count = p.count)} = <fun>
|}]

let record_update_predicate (p : int changing_record @ immutable) :
    {u : unit | ({p with payload = true}).count = p.count} =
  ();;
[%%expect{|
val record_update_predicate :
  (p : int changing_record) @ immutable ->
  {u : unit | { p with payload = true }.count = p.count} = <fun>
|}]

module Nested_box = struct
  type 'a t = Box of 'a
  let unwrap (nested : int t t) :
      {r : int | nested === Box (Box r)} =
    match nested with
    | Box (Box r) -> r
end;;
[%%expect{|
module Nested_box :
  sig
    type 'a t = Box of 'a
    val unwrap : (nested : int t t) -> {r : int | nested === (Box (Box r))}
  end
|}]

let polymorphic_tuple (x : 'a) :
    {r : 'a * 'a | match r with a, b -> a === x && b === x} =
  let r = x, x in
  r;;
[%%expect{|
val polymorphic_tuple :
  (x : 'a) -> {r : 'a * 'a | match r with | (a, b) -> (a === x) && (b === x)} =
  <fun>
|}]
