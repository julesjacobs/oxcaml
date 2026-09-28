(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Total code may match a type only if the type cannot reach itself through
   its representation (see negative_totality.ml). A type the checker cannot
   see into could be the matched type: a constructor's existential variable,
   an abstract type reached as a function argument (negative position), an
   open type, or the abstract types of an unpacked module. Such a hidden type
   is allowed only if its jkind rules out pointers -- a non-scannable layout
   or an immediate -- because the knot it could tie needs it to be a boxed
   function or the boxed matched type. [immutable_data] is not enough: a record
   whose field modality caps a total arrow across every axis is
   [immutable_data] yet holds a callable function. *)

type (_, _) eq = Refl : ('a, 'a) eq

(* An existential could be any type, including one built from [t]. *)
module Existential = struct
  type t = Pack : 'a * ('a -> int) -> t
  let (use @ total) (x : t) = match x with Pack (v, f) -> f v
end;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
Line 6, characters 43-54:
6 |   let (use @ total) (x : t) = match x with Pack (v, f) -> f v
                                               ^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 6, characters 20-61
         which is expected to be "total".
|}]

module Existential_immediate = struct
  type t = Pack : ('a : immediate). 'a * ('a -> int) -> t
  let (use @ total) (x : t) = match x with Pack (v, f) -> f v
end;;
[%%expect{|
module Existential_immediate :
  sig
    type t = Pack : ('a : immediate). 'a * ('a -> int) -> t
    val use : t -> int
  end
|}]

(* An [immutable_data] type CAN hold a callable function: a field modality can
   cap the arrow across every mode axis. So [immutable_data] does not make a
   hidden type safe. *)
module Function_as_data = struct
  type r =
    { run : int -> int @@ many portable forkable unyielding stateless total }
  type check : immutable_data = r
end;;
[%%expect{|
Line 3, characters 31-39:
3 |     { run : int -> int @@ many portable forkable unyielding stateless total }
                                   ^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 3, characters 60-69:
3 |     { run : int -> int @@ many portable forkable unyielding stateless total }
                                                                ^^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

module Function_as_data :
  sig
    type r = { run : int -> int @@ forkable unyielding many total; }
    type check = r
  end
|}]

(* Hence an [immutable_data] existential is rejected too: it can be such a
   record, and the knot through it is real. *)
module Existential_immutable_data = struct
  type t = Pack : ('a : immutable_data). 'a * ('a -> int) -> t
  let (use @ total) (x : t) = match x with Pack (v, f) -> f v
end;;
[%%expect{|
Line 3, characters 43-54:
3 |   let (use @ total) (x : t) = match x with Pack (v, f) -> f v
                                               ^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 20-61
         which is expected to be "total".
|}]

(* The witness that recovers the existential is in the same recursive group,
   so the representation walk never sees [t -> int] directly. *)
module Witness_same_group = struct
  type t = Pack : 'a * 'a w -> t
  and _ w = W : (t -> int) w
  let (use @ total) (x : t) = match x with Pack (v, W) -> v x
end;;
[%%expect{|
Line 4, characters 43-54:
4 |   let (use @ total) (x : t) = match x with Pack (v, W) -> v x
                                               ^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 20-61
         which is expected to be "total".
|}]

module Witness_same_group_immediate = struct
  type t = Pack : ('a : immediate). 'a * 'a w -> t
  and _ w = W : int w
  let (use @ total) (x : t) = match x with Pack (v, W) -> v + 1
end;;
[%%expect{|
module Witness_same_group_immediate :
  sig
    type t = Pack : ('a : immediate). 'a * 'a w -> t
    and _ w = W : int w
    val use : t -> int
  end
|}]

module Witness_separate = struct
  type t = Pack : 'a * ('a, t -> int) eq -> t
  let (use @ total) (x : t) = match x with Pack (v, Refl) -> v x
end;;
[%%expect{|
Line 3, characters 43-57:
3 |   let (use @ total) (x : t) = match x with Pack (v, Refl) -> v x
                                               ^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 20-64
         which is expected to be "total".
|}]

module Witness_separate_immediate = struct
  type t = Pack : ('a : immediate). 'a * ('a, int) eq -> t
  let (use @ total) (x : t) = match x with Pack (v, Refl) -> v + 1
end;;
[%%expect{|
module Witness_separate_immediate :
  sig
    type t = Pack : ('a : immediate). 'a * ('a, int) eq -> t
    val use : t -> int
  end
|}]

(* An extensible GADT as the witness. *)
module Extensible = struct
  type _ key = ..
  type t = Pack : 'a * 'a key -> t
  type _ key += K : (t -> int) key
  let (use @ total) (x : t) = match x with Pack (_, _) -> 0
end;;
[%%expect{|
Line 5, characters 43-54:
5 |   let (use @ total) (x : t) = match x with Pack (_, _) -> 0
                                               ^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 5, characters 20-59
         which is expected to be "total".
|}]

module Extensible_immediate = struct
  type _ key = ..
  type t = Pack : ('a : immediate). 'a * 'a key -> t
  let (use @ total) (x : t) = match x with Pack (_, _) -> 0
end;;
[%%expect{|
module Extensible_immediate :
  sig
    type _ key = ..
    type t = Pack : ('a : immediate). 'a * 'a key -> t
    val use : t -> int
  end
|}]

(* Matching an open type itself is always rejected. *)
module Extensible_witness = struct
  type _ key = ..
  type _ key += K : int key
  let (use @ total) (k : int key) = match k with K -> 0 | _ -> 1
end;;
[%%expect{|
Line 4, characters 49-50:
4 |   let (use @ total) (k : int key) = match k with K -> 0 | _ -> 1
                                                     ^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 20-64
         which is expected to be "total".
|}]

(* Ordinary partial code may still open existentials. *)
module Existential_partial = struct
  type t = Pack : 'a * 'a w -> t
  and _ w = W : (t -> int) w
  let use (x : t) = match x with Pack (v, W) -> v x
end;;
[%%expect{|
module Existential_partial :
  sig
    type t = Pack : 'a * 'a w -> t
    and _ w = W : (t -> int) w
    val use : t -> int
  end
|}, Principal{|
Line 4, characters 48-51:
4 |   let use (x : t) = match x with Pack (v, W) -> v x
                                                    ^^^
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

(* A signature can hide [type u = t]; then [t] looks nonrecursive outside, and
   [u] is reached as the argument of [u -> int] (negative). *)
module Abstract = struct
  module M : sig
    type u
    type t = Roll of (u -> int)
  end = struct
    type t = Roll of (t -> int)
    type u = t
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 9, characters 45-53:
9 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                 ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 9, characters 20-58
         which is expected to be "total".
|}]

(* The knot still closes when [u] is [immutable_data], because the matched
   type can itself be [immutable_data]. *)
module Abstract_immutable_data = struct
  module M : sig
    type u : immutable_data
    type t =
      Roll of (u -> int) @@ many portable forkable unyielding stateless total
  end = struct
    type t =
      Roll of (t -> int) @@ many portable forkable unyielding stateless total
    type u = t
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 8, characters 33-41:
8 |       Roll of (t -> int) @@ many portable forkable unyielding stateless total
                                     ^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 8, characters 62-71:
8 |       Roll of (t -> int) @@ many portable forkable unyielding stateless total
                                                                  ^^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 5, characters 33-41:
5 |       Roll of (u -> int) @@ many portable forkable unyielding stateless total
                                     ^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 5, characters 62-71:
5 |       Roll of (u -> int) @@ many portable forkable unyielding stateless total
                                                                  ^^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 11, characters 45-53:
11 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                  ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 11, characters 20-58
         which is expected to be "total".
|}]

(* An immediate abstract type cannot be the boxed matched type. *)
module Abstract_immediate = struct
  module M : sig
    type u : immediate
    type t = Roll of (u -> int)
  end = struct
    type u = int
    type t = Roll of (u -> int)
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
module Abstract_immediate :
  sig
    module M : sig type u : immediate type t = Roll of (u -> int) end
    val use : M.t -> int
  end
|}]

(* A predefined abstract type ([string], [float array]) cannot hide a user
   type, and here occurs only positively anyway. *)
module Abstract_predefined = struct
  type t = Roll of (string -> int) * float array
  let (use @ total) (x : t) = match x with Roll _ -> 0
end;;
[%%expect{|
module Abstract_predefined :
  sig type t = Roll of (string -> int) * float array val use : t -> int end
|}]

(* A functor parameter is abstract too, and reached negatively here. *)
module Functor_parameter (X : sig type u end) = struct
  type t = Roll of (X.u -> int)
  let (use @ total) (x : t) = match x with Roll _ -> 0
end;;
[%%expect{|
Line 3, characters 43-49:
3 |   let (use @ total) (x : t) = match x with Roll _ -> 0
                                               ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 20-54
         which is expected to be "total".
|}]

module Functor_parameter_immediate (X : sig type u : immediate end) = struct
  type t = Roll of (X.u -> int)
  let (use @ total) (x : t) = match x with Roll _ -> 0
end;;
[%%expect{|
module Functor_parameter_immediate :
  functor (X : sig type u : immediate end) ->
    sig type t = Roll of (X.u -> int) val use : t -> int end
|}]

(* The hidden type [u] is reached negatively (in [b -> int]) only after [b] has
   already been reached positively (in [b option]). The walk must still check
   [b] negatively, so its polarity-keyed declaration cache does not skip it. *)
module Cache_polarity = struct
  module M : sig
    type u
    type b = B of u
    type t = Roll of b option * (b -> int)
  end = struct
    type u = t
    and b = B of u
    and t = Roll of b option * (b -> int)
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 11, characters 45-53:
11 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                  ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 11, characters 20-58
         which is expected to be "total".
|}]

(* Unpacking a first-class module brings its abstract types into scope; a
   value in the module can consume one, so a boxed abstract type is rejected. *)
module type Hidden = sig
  type u
  val v : u
end

module Unpack_parameter = struct
  let (use @ total) (module M : Hidden) = let _ = M.v in ()
end;;
[%%expect{|
module type Hidden = sig type u val v : u end
Line 7, characters 28-29:
7 |   let (use @ total) (module M : Hidden) = let _ = M.v in ()
                                ^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 7, characters 20-59
         which is expected to be "total".
|}]

module type Hidden_immediate = sig
  type u : immediate
  val v : u
end

(* An immediate abstract type cannot be a function or the boxed matched type. *)
module Unpack_immediate = struct
  let (use @ total) (module M : Hidden_immediate) = let _ = M.v in ()
end;;
[%%expect{|
module type Hidden_immediate = sig type u : immediate val v : u end
module Unpack_immediate : sig val use : (module Hidden_immediate) -> unit end
|}]

(* A constraint fixes the type, so nothing is hidden. *)
module Unpack_constrained = struct
  let (use @ total) (module M : Hidden with type u = int) = M.v + 1
end;;
[%%expect{|
module Unpack_constrained :
  sig val use : (module Hidden with type u = int) -> int end
|}]

(* The [let (module M) = e] form leaves the package type unresolved at the
   check point, so it is conservatively rejected even when the signature is
   function-free. *)
module Unpack_let_conservative = struct
  let (use @ total) (p : (module Hidden_immediate)) =
    let (module M) = p in
    let _ = M.v in ()
end;;
[%%expect{|
Line 3, characters 8-18:
3 |     let (module M) = p in
            ^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 20-21
         which is expected to be "total".
|}]

(* Non-total code may unpack anything. *)
module Unpack_partial = struct
  let use (module M : Hidden) = let _ = M.v in ()
end;;
[%%expect{|
module Unpack_partial : sig val use : (module Hidden) -> unit end
|}]
