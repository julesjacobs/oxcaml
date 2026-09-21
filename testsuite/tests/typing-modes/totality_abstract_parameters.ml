(* TEST expect; *)

type 'a heap;;
type callback = { run : callback heap -> bool @@ total };;
let (project @ total) x = x.run;;
[%%expect{|
type 'a heap
type callback = { run : callback heap -> bool @@ total; }
Line 3, characters 26-31:
3 | let (project @ total) x = x.run;;
                              ^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 22-31
         which is expected to be "total".
|}]

let project_runtime x = x.run;;
[%%expect{|
val project_runtime : callback -> callback heap -> bool = <fun>
|}]

let (destructure @ total) { run } = run;;
[%%expect{|
Line 1, characters 26-33:
1 | let (destructure @ total) { run } = run;;
                              ^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-39
         which is expected to be "total".
|}]

type 'a handle [@@phantom_parameters];;
type node = { value : int; next : node handle };;
let (value @ total) n = n.value;;
[%%expect{|
type 'a handle [@@phantom_parameters]
type node = { value : int; next : node handle; }
val value : node -> int = <fun>
|}]

type 'a handle_alias = 'a handle;;
type aliased_node = { next : aliased_node handle_alias };;
let (next @ total) n = n.next;;
[%%expect{|
type 'a handle_alias = 'a handle
type aliased_node = { next : aliased_node handle_alias; }
val next : aliased_node -> aliased_node handle_alias = <fun>
|}]

type 'a invalid = { contents : 'a } [@@phantom_parameters];;
[%%expect{|
Line 1, characters 0-58:
1 | type 'a invalid = { contents : 'a } [@@phantom_parameters];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A phantom parameter cannot occur in the type's representation.
|}]

type 'a invalid_function = 'a -> bool [@@phantom_parameters];;
[%%expect{|
Line 1, characters 0-60:
1 | type 'a invalid_function = 'a -> bool [@@phantom_parameters];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A phantom parameter cannot occur in the type's representation.
|}]

type 'a constant = { id : int } [@@phantom_parameters];;
module type Phantom = sig type 'a t [@@phantom_parameters] end;;
module Constant : Phantom = struct
  type 'a t = 'a constant [@@phantom_parameters]
end;;
[%%expect{|
type 'a constant = { id : int; } [@@phantom_parameters]
module type Phantom = sig type 'a t [@@phantom_parameters] end
module Constant : Phantom
|}]

module Forged : Phantom = struct type 'a t = { contents : 'a } end;;
[%%expect{|
Line 1, characters 26-66:
1 | module Forged : Phantom = struct type 'a t = { contents : 'a } end;;
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { contents : 'a; } end
       is not included in
         Phantom
       Type declarations do not match:
         type 'a t = { contents : 'a; }
       is not included in
         type 'a t
       [@@phantom_parameters]
       Their phantom parameter guarantees do not match.
|}]

module type Forged_constraint = Phantom with type 'a t = 'a list;;
[%%expect{|
Line 1, characters 45-64:
1 | module type Forged_constraint = Phantom with type 'a t = 'a list;;
                                                 ^^^^^^^^^^^^^^^^^^^
Error: A phantom parameter cannot occur in the type's representation.
|}]

module Hide : sig type 'a t end = Constant;;
type hidden = { field : hidden Hide.t };;
let (hidden @ total) h = h.field;;
[%%expect{|
module Hide : sig type 'a t end
type hidden = { field : hidden Hide.t; }
Line 3, characters 25-32:
3 | let (hidden @ total) h = h.field;;
                             ^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 21-32
         which is expected to be "total".
|}]

module F (H : sig type 'a t end) = struct
  type t = { call : t H.t -> bool @@ total }
  let (call @ total) x = x.call
end;;
[%%expect{|
Line 3, characters 25-31:
3 |   let (call @ total) x = x.call
                             ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 21-31
         which is expected to be "total".
|}]

module G (H : Phantom) = struct
  type t = { field : t H.t }
  let (field @ total) x = x.field
end;;
[%%expect{|
module G :
  functor (H : Phantom) ->
    sig type t = { field : t H.t; } val field : t -> t H.t end
|}]

module rec Circular : Phantom = Circular;;
[%%expect{|
Line 2, characters 26-58:
2 | module type Phantom = sig type 'a t [@@phantom_parameters] end;;
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Recursive module signatures cannot assert phantom parameter guarantees.
|}]

type (_, _) eq = Refl : ('a, 'a) eq;;
type 'a hidden_callback =
  Hide : ('a, 'b) eq * ('b -> bool) -> 'a hidden_callback;;
type hidden_callback_record = { stored : hidden_callback_record hidden_callback };;
let (hidden_callback @ total) x = x.stored;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
type 'a hidden_callback =
    Hide : ('a, 'b) eq * ('b -> bool) -> 'a hidden_callback
type hidden_callback_record = {
  stored : hidden_callback_record hidden_callback;
}
Line 5, characters 34-42:
5 | let (hidden_callback @ total) x = x.stored;;
                                      ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 5, characters 30-42
         which is expected to be "total".
|}]

type 'a invalid_gadt = Index : int invalid_gadt [@@phantom_parameters];;
[%%expect{|
Line 1, characters 0-70:
1 | type 'a invalid_gadt = Index : int invalid_gadt [@@phantom_parameters];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: GADTs cannot guarantee phantom parameters.
|}]

type 'a invalid_open = .. [@@phantom_parameters];;
[%%expect{|
Line 1, characters 0-48:
1 | type 'a invalid_open = .. [@@phantom_parameters];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types cannot guarantee phantom parameters.
|}]


type 'a constrained_alias = 'b constraint 'a = 'b list
  [@@phantom_parameters];;
[%%expect{|
Lines 1-2, characters 0-24:
1 | type 'a constrained_alias = 'b constraint 'a = 'b list
2 |   [@@phantom_parameters]..
Error: Phantom parameters must be type variables.
|}]

type 'a constrained_record = { payload : 'b }
  constraint 'a = 'b list [@@phantom_parameters];;
[%%expect{|
Lines 1-2, characters 0-48:
1 | type 'a constrained_record = { payload : 'b }
2 |   constraint 'a = 'b list [@@phantom_parameters]..
Error: Phantom parameters must be type variables.
|}]

module type Constrained_phantom = sig
  type 'a t constraint 'a = 'b list [@@phantom_parameters]
end;;
[%%expect{|
Line 2, characters 2-58:
2 |   type 'a t constraint 'a = 'b list [@@phantom_parameters]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Phantom parameters must be type variables.
|}]

type 'a box = Box of 'a;;
type nested = { value : int box box };;
let (get_nested @ total) (x : nested) = x.value;;
type nested_heap = { value : int heap heap };;
let (get_nested_heap @ total) (x : nested_heap) = x.value;;
[%%expect{|
type 'a box = Box of 'a
type nested = { value : int box box; }
val get_nested : nested -> int box box = <fun>
type nested_heap = { value : int heap heap; }
val get_nested_heap : nested_heap -> int heap heap = <fun>
|}]

type growing = { value : int growing_wrapper }
and 'a growing_wrapper = Grow of ('a list) growing_wrapper;;
let (get_growing @ total) (x : growing) = x.value;;
[%%expect{|
type growing = { value : int growing_wrapper; }
and 'a growing_wrapper = Grow of 'a list growing_wrapper
Line 3, characters 42-49:
3 | let (get_growing @ total) (x : growing) = x.value;;
                                              ^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 26-49
         which is expected to be "total".
|}]

type callbacks = { run : unit -> unit @@ total };;
let good : callbacks = { run = fun () -> () };;
let (call @ total) (c : callbacks) = c.run ();;
[%%expect{|
type callbacks = { run : unit -> unit @@ total; }
val good : callbacks = {run = <fun>}
val call : callbacks -> unit = <fun>
|}]

let rec callback_cycle : callbacks =
  { run = fun () -> callback_cycle.run () };;
[%%expect{|
Line 2, characters 2-43:
2 |   { run = fun () -> callback_cycle.run () };;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

type pack =
  Pack : 'a @@ total immutable *
    ('a @ total immutable -> unit) @@ total immutable -> pack;;
module Explicit_pack_cycle = struct
  let (invoke @ total) (p : pack @ total immutable) =
    match p with Pack (x, f) -> f x
  let rec packed_cycle : pack = Pack (packed_cycle, invoke)
end;;
[%%expect{|
type pack =
    Pack : 'a @@ total immutable * ('a @ total immutable -> unit) @@ total
      immutable -> pack
Line 7, characters 32-59:
7 |   let rec packed_cycle : pack = Pack (packed_cycle, invoke)
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

type runtime_callbacks = { runtime_run : unit -> unit };;
let rec runtime_cycle : runtime_callbacks =
  { runtime_run = fun () -> runtime_cycle.runtime_run () };;
[%%expect{|
type runtime_callbacks = { runtime_run : unit -> unit; }
val runtime_cycle : runtime_callbacks = {runtime_run = <fun>}
|}]

let rec alias_cycle : callbacks =
  let alias = alias_cycle in
  { run = fun () -> alias.run () };;
[%%expect{|
Lines 2-3, characters 2-34:
2 | ..let alias = alias_cycle in
3 |   { run = fun () -> alias.run () }..
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec module_cycle : callbacks =
  let module M = struct let alias = module_cycle end in
  let module N = M in
  { run = fun () -> N.alias.run () };;
[%%expect{|
Lines 2-4, characters 2-36:
2 | ..let module M = struct let alias = module_cycle end in
3 |   let module N = M in
4 |   { run = fun () -> N.alias.run () }..
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

type callback_first_pack =
  Callback_first_pack :
    ('a @ total immutable -> unit) @@ total immutable *
    'a @@ total immutable -> callback_first_pack;;
module Callback_first_cycle = struct
  let (invoke @ total) (p : callback_first_pack @ total immutable) =
    match p with Callback_first_pack (f, x) -> f x
  let rec packed_cycle : callback_first_pack =
    Callback_first_pack (invoke, packed_cycle)
end;;
[%%expect{|
type callback_first_pack =
    Callback_first_pack : ('a @ total immutable -> unit) @@ total immutable *
      'a @@ total immutable -> callback_first_pack
Line 9, characters 4-46:
9 |     Callback_first_pack (invoke, packed_cycle)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec independent_in_recursive : runtime_callbacks =
  let good : callbacks = { run = fun () -> () } in
  { runtime_run = fun () -> good.run (); independent_in_recursive.runtime_run () };;
[%%expect{|
val independent_in_recursive : runtime_callbacks = {runtime_run = <fun>}
|}]

type bounded_pack =
  Bounded_pack : ('a : value mod total immutable).
    'a * ('a @ immutable -> unit) @@ total immutable -> bounded_pack;;
module Bounded_pack_cycle = struct
  let (invoke @ total) (p : bounded_pack @ immutable) =
    match p with Bounded_pack (x, f) -> f x
  let rec packed_cycle : bounded_pack = Bounded_pack (packed_cycle, invoke)
end;;
[%%expect{|
type bounded_pack =
    Bounded_pack : ('a : value mod total immutable). 'a *
      ('a @ immutable -> unit) @@ total immutable -> bounded_pack
Line 7, characters 40-75:
7 |   let rec packed_cycle : bounded_pack = Bounded_pack (packed_cycle, invoke)
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec tuple_alias_cycle : callbacks =
  let alias = (tuple_alias_cycle, ()) in
  { run = fun () -> let c, () = alias in c.run () };;
[%%expect{|
Lines 2-3, characters 2-51:
2 | ..let alias = (tuple_alias_cycle, ()) in
3 |   { run = fun () -> let c, () = alias in c.run () }..
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let good_pack : pack = Pack ((), fun () -> ());;
let good_bounded_pack : bounded_pack = Bounded_pack ((), fun () -> ());;
[%%expect{|
val good_pack : pack = Pack (<poly>, <fun>)
val good_bounded_pack : bounded_pack = Bounded_pack (<poly>, <fun>)
|}]

type runtime_pack = Runtime_pack : 'a * ('a -> unit) -> runtime_pack;;
let invoke_runtime p = match p with Runtime_pack (x, f) -> f x;;
let rec runtime_packed_cycle = Runtime_pack (runtime_packed_cycle, invoke_runtime);;
[%%expect{|
type runtime_pack = Runtime_pack : 'a * ('a -> unit) -> runtime_pack
val invoke_runtime : runtime_pack -> unit = <fun>
val runtime_packed_cycle : runtime_pack = Runtime_pack (<cycle>, <fun>)
|}]

module Unboxed_omitted_cycle = struct
  type callbacks = #{ run : (unit -> unit) @@ total immutable }
  let (invoke @ total) () ~(c : callbacks @ total immutable) = c.#run ()
  let rec cycle : callbacks = #{ run = invoke ~c:cycle }
end;;
[%%expect{|
Line 4, characters 30-56:
4 |   let rec cycle : callbacks = #{ run = invoke ~c:cycle }
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

module Unboxed_omitted_independent = struct
  type callbacks = #{ run : (unit -> unit) @@ total immutable }
  let (invoke @ total) () ~(c : callbacks @ total immutable) = c.#run ()
  let good : callbacks = #{ run = fun () -> () }
  let via_omission : callbacks = #{ run = invoke ~c:good }
  let (call @ total) () = via_omission.#run ()
end;;
[%%expect{|
module Unboxed_omitted_independent :
  sig
    type callbacks = #{ run : unit -> unit @@ total immutable; }
    val invoke : unit -> c:callbacks @ total immutable -> unit
    val good : callbacks
    val via_omission : callbacks
    val call : unit -> unit
  end
|}]

module Unboxed_omitted_runtime = struct
  type callbacks = #{ run : unit -> unit }
  let invoke () ~(c : callbacks) = c.#run ()
  let rec cycle : callbacks = #{ run = invoke ~c:cycle }
end;;
[%%expect{|
module Unboxed_omitted_runtime :
  sig
    type callbacks = #{ run : unit -> unit; }
    val invoke : unit -> c:callbacks -> unit
    val cycle : callbacks
  end
|}]
