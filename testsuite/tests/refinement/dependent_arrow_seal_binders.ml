(* TEST
 expect;
*)

(* A dependent arrow's codomain may mention the parameter, so the parameter's
   own refinement has to reach the seal implication.  The seal below is asked
   to prove [value + x > 0] from [value = x], which holds only because [x] is
   positive. *)

module Covariant_codomain : sig end = struct
  module type Exact = sig
    val f : (x : int{ _ > 0 }) -> int{ _ = x }
  end

  module type Sum_positive = sig
    val f : (x : int{ _ > 0 }) -> int{ _ + x > 0 }
  end

  module Weaken (X : Exact) : Sum_positive = X
end

[%%expect {|
module Covariant_codomain : sig end
|}]

(* In an argument position the direction reverses, and the parameter is
   guaranteed by the implementation, which is the one that calls it. *)

module Contravariant_codomain : sig end = struct
  module type Sum_positive_argument = sig
    val f : ((x : int{ _ > 0 }) -> int{ _ + x > 0 }) -> unit
  end

  module type Exact_argument = sig
    val f : ((x : int{ _ > 0 }) -> int{ _ = x }) -> unit
  end

  module Accept (X : Sum_positive_argument) : Exact_argument = X
end

[%%expect {|
module Contravariant_codomain : sig end
|}]

(* The parameter's refinement is a hypothesis, not a licence: a codomain that
   does not follow from it is still refused. *)

module Insufficient : sig end = struct
  module type Exact = sig
    val f : (x : int{ _ > 0 }) -> int{ _ = x }
  end

  module type Above_parameter = sig
    val f : (x : int{ _ > 0 }) -> int{ _ > x + 5 }
  end

  module Bad (X : Exact) : Above_parameter = X
end

[%%expect {|
Line 10, characters 45-46:
10 |   module Bad (X : Exact) : Above_parameter = X
                                                  ^
Error: Refinement verification failed at module seal for value "f" (disproved)
Line 7, characters 4-50:
7 |     val f : (x : int{ _ > 0 }) -> int{ _ > x + 5 }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value f
Line 3, characters 4-46:
3 |     val f : (x : int{ _ > 0 }) -> int{ _ = x }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value f
|}]

(* The parameter's own obligation is unaffected: a domain that admits more
   values than the implementation accepts is still refused. *)

module Domain_unchanged : sig end = struct
  module type Positive = sig
    val f : (x : int{ _ > 0 }) -> int{ _ = x }
  end

  module type Nonnegative = sig
    val f : (x : int{ _ >= 0 }) -> int{ _ = x }
  end

  module Bad (X : Positive) : Nonnegative = X
end

[%%expect {|
Line 10, characters 44-45:
10 |   module Bad (X : Positive) : Nonnegative = X
                                                 ^
Error: Refinement verification failed at module seal for value "f" (not-proved)
Line 7, characters 4-47:
7 |     val f : (x : int{ _ >= 0 }) -> int{ _ = x }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value f
Line 3, characters 4-46:
3 |     val f : (x : int{ _ > 0 }) -> int{ _ = x }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value f
|}]
