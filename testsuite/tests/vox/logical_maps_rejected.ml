(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Key = struct
  type t = int
  let[@def] equal (x : int) (y : int) = x = y
  let (reflexive @ total) x : {u : unit | equal x x} = equal_def x x; ()
  let (symmetric @ total) x y : {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) x y z :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x y; equal_def y z; equal_def x z; ()
end
module M = Map.MakeLogical (Key);;
[%%expect{|
module Key :
  sig
    type t = int
    val equal : int -> int -> bool
    val equal_def :
      (x : int) -> (y : int) -> {u : unit | (equal x y) === (x = y)}
    val reflexive : (x : int) -> {u : unit | equal x x}
    val symmetric :
      (x : int) -> (y : int) -> {u : unit | (equal x y) = (equal y x)}
    val transitive :
      (x : int) ->
      (y : int) ->
      (z : int) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)}
  end
module M :
  sig
    type ('a : immutable_data) t = 'a Map.MakeLogical(Key).t
    external empty : ('a : immutable_data). unit -> 'a t @ ghost @@ total
      = "caml_logical_map_empty"
    external find_opt :
      ('a : immutable_data). Key.t -> 'a t -> 'a option @ ghost @@ total
      = "caml_logical_map_find_opt"
    external mem : ('a : immutable_data). Key.t -> 'a t -> bool @ ghost @@
      total = "caml_logical_map_mem"
    external add : ('a : immutable_data). Key.t -> 'a -> 'a t -> 'a t @ ghost
      @@ total = "caml_logical_map_add"
    external remove : ('a : immutable_data). Key.t -> 'a t -> 'a t @ ghost @@
      total = "caml_logical_map_remove"
    external cardinal :
      ('a : immutable_data).
        'a t -> {n : Bigint.t | (Bigint.of_int 0) <= n} @ ghost
      @@ total = "caml_logical_map_cardinal"
    module Proof :
      sig
        external difference :
          ('a : immutable_data).
            (left : 'a t) ->
            (right : 'a t) ->
            {result : Key.t option
              | match result with
                | None -> left === right
                | Some key ->
                    not ((find_opt key left) === (find_opt key right))} @ ghost
          @@ total = "caml_logical_map_difference"
      end
  end
|}]

let (wrong_lookup @ total) (m : int M.t) (k : Key.t) :
    {u : unit | M.find_opt k (M.add k 42 m) === Some 43} @ ghost = ghost_ ();;
[%%expect{|
Line 2, characters 74-76:
2 |     {u : unit | M.find_opt k (M.add k 42 m) === Some 43} @ ghost = ghost_ ();;
                                                                              ^^
Error: Refinement could not be proved (counterexample: k = 0)
Line 2, characters 16-55:
2 |     {u : unit | M.find_opt k (M.add k 42 m) === Some 43} @ ghost = ghost_ ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let (wrong_equality @ total) (k : Key.t) :
    {u : unit | M.add k 42 (M.empty ()) === M.empty ()} @ ghost = ghost_ ();;
[%%expect{|
Line 2, characters 73-75:
2 |     {u : unit | M.add k 42 (M.empty ()) === M.empty ()} @ ghost = ghost_ ();;
                                                                             ^^
Error: Refinement could not be proved (counterexample: k = 0)
Line 2, characters 16-54:
2 |     {u : unit | M.add k 42 (M.empty ()) === M.empty ()} @ ghost = ghost_ ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let (wrong_cardinal @ total) (k : Key.t) :
    {u : unit | M.cardinal (M.add k 42 (M.add k 41 (M.empty ()))) = 2Z}
    @ ghost = ghost_ ();;
[%%expect{|
Line 3, characters 21-23:
3 |     @ ghost = ghost_ ();;
                         ^^
Error: Refinement could not be proved (counterexample: k = 0)
Line 2, characters 16-70:
2 |     {u : unit | M.cardinal (M.add k 42 (M.add k 41 (M.empty ()))) = 2Z}
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let (distinct_keys @ total) (x : Key.t) (y : Key.t) :
    {u : unit | not (Key.equal x y) ||
      M.find_opt y (M.add x 42 (M.empty ())) === None} @ ghost = ghost_ ();;
[%%expect{|
Line 3, characters 72-74:
3 |       M.find_opt y (M.add x 42 (M.empty ())) === None} @ ghost = ghost_ ();;
                                                                            ^^
Error: Refinement could not be proved (counterexample: x = 0, y = 0)
Lines 2-3, characters 16-53:
2 | ................not (Key.equal x y) ||
3 |       M.find_opt y (M.add x 42 (M.empty ())) === None.......................
  The refinement is stated here.
|}]

let escape (k : Key.t) : int option = M.find_opt k (M.add k 42 (M.empty ()));;
[%%expect{|
Line 1, characters 63-75:
1 | let escape (k : Key.t) : int option = M.find_opt k (M.add k 42 (M.empty ()));;
                                                                   ^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]

module Missing_laws = Map.MakeLogical (struct
  type t = int
  let (equal @ total) (x : int) (y : int) = x = y
end);;
[%%expect{|
Lines 1-4, characters 22-4:
1 | ......................Map.MakeLogical (struct
2 |   type t = int
3 |   let (equal @ total) (x : int) (y : int) = x = y
4 | end)..
Error: Modules do not match:
       sig type t = int val equal : int -> int -> bool end
     is not included in Map.EquatableType
     The value "reflexive" is required but not provided
     File "map.mli", line 485, characters 2-60: Expected declaration
     The value "symmetric" is required but not provided
     File "map.mli", lines 486-487, characters 2-47: Expected declaration
     The value "transitive" is required but not provided
     File "map.mli", lines 488-489, characters 2-67: Expected declaration
|}]

module False_law = struct
  type t = int
  let (equal @ total) _ _ = false
  let (reflexive @ total) x : {u : unit | equal x x} = ()
end;;
[%%expect{|
Line 4, characters 55-57:
4 |   let (reflexive @ total) x : {u : unit | equal x x} = ()
                                                           ^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 42-51:
4 |   let (reflexive @ total) x : {u : unit | equal x x} = ()
                                              ^^^^^^^^^
  The refinement is stated here.
|}]

module Forged = struct
  external empty : unit -> int M.t @ ghost @@ total = "caml_logical_map_empty"
  let (claim @ total) (k : Key.t) :
      {u : unit | M.find_opt k (empty ()) === None} @ ghost = ghost_ ()
end;;
[%%expect{|
Line 2, characters 2-78:
2 |   external empty : unit -> int M.t @ ghost @@ total = "caml_logical_map_empty"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 228 [trusted-external]: The verifier assumes this external's totality;
  nothing checks it.

Line 4, characters 69-71:
4 |       {u : unit | M.find_opt k (empty ()) === None} @ ghost = ghost_ ()
                                                                         ^^
Error: Refinement could not be proved (counterexample: k = 0)
Line 4, characters 18-50:
4 |       {u : unit | M.find_opt k (empty ()) === None} @ ghost = ghost_ ()
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Disguised (Ignored : Map.EquatableType with type t = int) : sig
  type ('a : immutable_data) t : logical_data with 'a
  val empty : ('a : immutable_data). unit -> 'a t @ ghost @@ total
  val find_opt : ('a : immutable_data). int -> 'a t -> 'a option @ ghost @@ total
  val add : ('a : immutable_data). int -> 'a -> 'a t -> 'a t @ ghost @@ total
end = Map.MakeLogical (Key)
module Everything = struct
  type t = int
  let[@def] equal (_ : int) (_ : int) = true
  let (reflexive @ total) x : {u : unit | equal x x} = equal_def x x; ()
  let (symmetric @ total) x y : {u : unit | equal x y = equal y x} =
    equal_def x y; equal_def y x; ()
  let (transitive @ total) x y z :
      {u : unit | not (equal x y && equal y z) || equal x z} =
    equal_def x z; ()
end
module D = Disguised (Everything);;
[%%expect{|
module Disguised :
  functor
    (Ignored : sig
                 type t = int
                 val equal : t -> t -> bool @@ total
                 val reflexive : (x : t) -> {u : unit | equal x x} @@ total
                 val symmetric :
                   (x : t) ->
                   (y : t) -> {u : unit | (equal x y) = (equal y x)} @@ total
                 val transitive :
                   (x : t) ->
                   (y : t) ->
                   (z : t) ->
                   {u : unit
                     | (not ((equal x y) && (equal y z))) || (equal x z)}
                   @@ total
               end)
    ->
    sig
      type ('a : immutable_data) t : logical_data with 'a
      val empty : ('a : immutable_data). unit -> 'a t @ ghost @@ total
      val find_opt : ('a : immutable_data). int -> 'a t -> 'a option @ ghost
        @@ total
      val add : ('a : immutable_data). int -> 'a -> 'a t -> 'a t @ ghost @@
        total
    end
module Everything :
  sig
    type t = int
    val equal : int -> int -> bool
    val equal_def :
      (arg : int) ->
      (arg1 : int) ->
      {u : unit
        | (equal arg arg1) ===
            (match arg with | _ -> (match arg1 with | _ -> true))}
    val reflexive : (x : int) -> {u : unit | equal x x}
    val symmetric :
      (x : int) -> (y : int) -> {u : unit | (equal x y) = (equal y x)}
    val transitive :
      (x : int) ->
      (y : int) ->
      (z : int) ->
      {u : unit | (not ((equal x y) && (equal y z))) || (equal x z)}
  end
module D :
  sig
    type ('a : immutable_data) t = 'a Disguised(Everything).t
    val empty : ('a : immutable_data). unit -> 'a t @ ghost @@ total
    val find_opt : ('a : immutable_data). int -> 'a t -> 'a option @ ghost @@
      total
    val add : ('a : immutable_data). int -> 'a -> 'a t -> 'a t @ ghost @@
      total
  end
|}]

let (wrong_functor_argument @ total) (x : int) (y : int) :
    {u : unit | D.find_opt y (D.add x 42 (D.empty ())) === Some 42}
    @ ghost = ghost_ (Everything.equal_def x y; ());;
[%%expect{|
Line 3, characters 48-50:
3 |     @ ghost = ghost_ (Everything.equal_def x y; ());;
                                                    ^^
Error: Refinement could not be proved (counterexample: x = 0, y = 0)
Line 2, characters 16-66:
2 |     {u : unit | D.find_opt y (D.add x 42 (D.empty ())) === Some 42}
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Anonymous_forgery : sig end = struct
  module A = Disguised (struct include Everything end)
  let (claim @ total) (x : int) (y : int) :
      {u : unit | A.find_opt y (A.add x 42 (A.empty ())) === Some 42}
      @ ghost = ghost_ (Everything.equal_def x y; ())
end;;
[%%expect{|
Line 5, characters 50-52:
5 |       @ ghost = ghost_ (Everything.equal_def x y; ())
                                                      ^^
Error: Refinement could not be proved (counterexample: x = 0, y = 0)
Line 4, characters 18-68:
4 |       {u : unit | A.find_opt y (A.add x 42 (A.empty ())) === Some 42}
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Anonymous_wrong_lookup : sig end = struct
  module A = Map.MakeLogical (struct include Key end)
  let (claim @ total) (x : int) (y : int) :
      {u : unit | A.find_opt y (A.add x 42 (A.empty ())) === Some 42}
      @ ghost = ghost_ ()
end;;
[%%expect{|
Line 5, characters 23-25:
5 |       @ ghost = ghost_ ()
                           ^^
Error: Refinement could not be proved (counterexample: x = 0, y = -1)
Line 4, characters 18-68:
4 |       {u : unit | A.find_opt y (A.add x 42 (A.empty ())) === Some 42}
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
