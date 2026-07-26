(* TEST
 expect;
*)

module Accepted : sig end = struct
  module type Result_exact = sig
    val f : (x : int) -> int{ _ = x }
  end

  module type Result_lower = sig
    val f : (y : int) -> int{ _ >= y }
  end

  module Result (X : Result_exact) : Result_lower = X

  (* [x + 5] can only be relied on to exceed [x] where [x] is bounded away
     from the largest representable integer, so the parameter carries that
     bound and the seal implication reads it. *)
  module type Domain_strong = sig
    val f : (x : int{ _ < 1000 }) -> int{ _ > x + 5 } -> unit
  end

  module type Domain_weak = sig
    val f : (y : int{ _ < 1000 }) -> int{ _ > y } -> unit
  end

  module Domain (X : Domain_weak) : Domain_strong = X
end

[%%expect {|
module Accepted : sig end
|}]

module Result_rejected : sig end = struct
  module type Lower = sig
    val f : (x : int) -> int{ _ >= x }
  end

  module type Exact = sig
    val f : (y : int) -> int{ _ = y }
  end

  module Bad (X : Lower) : Exact = X
end

[%%expect {|
Line 10, characters 35-36:
10 |   module Bad (X : Lower) : Exact = X
                                        ^
Error: Refinement verification failed at module seal for value "f" (not-proved)
Line 7, characters 4-37:
7 |     val f : (y : int) -> int{ _ = y }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value f
Line 3, characters 4-38:
3 |     val f : (x : int) -> int{ _ >= x }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value f
|}]

module Domain_rejected : sig end = struct
  module type Strong = sig
    val f : (x : int) -> int{ _ > x + 5 } -> unit
  end

  module type Weak = sig
    val f : (y : int) -> int{ _ > y } -> unit
  end

  module Bad (X : Strong) : Weak = X
end

[%%expect {|
Line 10, characters 35-36:
10 |   module Bad (X : Strong) : Weak = X
                                        ^
Error: Refinement verification failed at module seal for value "f" (not-proved)
Line 7, characters 4-45:
7 |     val f : (y : int) -> int{ _ > y } -> unit
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value f
Line 3, characters 4-49:
3 |     val f : (x : int) -> int{ _ > x + 5 } -> unit
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value f
|}]

module Binder_presence_rejected : sig end = struct
  module type Dependent = sig
    val f : (x : int) -> int{ _ = x }
  end

  module type Constant = sig
    val f : int -> int{ _ = 0 }
  end

  module Missing (X : Dependent) : Constant = X
end

[%%expect {|
Line 10, characters 46-47:
10 |   module Missing (X : Dependent) : Constant = X
                                                   ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : (x : int) -> int{ _ = x } end
       is not included in
         Constant
       Values do not match:
         val f : (x : int) -> int{ _ = x }
       is not included in
         val f : int -> int{ _ = 0 }
       The type "(x : int) -> int{ _ = x }" is not compatible with the type
         "int -> int{ _ = 0 }"
|}]

module Positional_capture_rejected : sig end = struct
  module type Ordered = sig
    val f :
      (outer : int) ->
      (inner : int) ->
      int{ _ = outer + 2 * inner }
  end

  module type Swapped = sig
    val f :
      (left : int) ->
      (right : int) ->
      int{ _ = right + 2 * left }
  end

  module Bad (X : Ordered) : Swapped = X
end

[%%expect {|
Line 16, characters 39-40:
16 |   module Bad (X : Ordered) : Swapped = X
                                            ^
Error: Refinement verification failed at module seal for value "f" (not-proved)
Lines 10-13, characters 4-33:
10 | ....val f :
11 |       (left : int) ->
12 |       (right : int) ->
13 |       int{ _ = right + 2 * left }
  Interface declaration for value f
Lines 3-6, characters 4-34:
3 | ....val f :
4 |       (outer : int) ->
5 |       (inner : int) ->
6 |       int{ _ = outer + 2 * inner }
  Implementation declaration for value f
|}]
