(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* A module alias inside a structure exports the aliased values, also when
   the signature hides the alias. Each phrase is verified on its own, so the
   uses are in the same phrase as the definitions. *)
module Aliases = struct
  module Base = struct
    let (double @ total) (x : int) = x + x
    module Inner = struct
      let (triple @ total) (x : int) = x + x + x
    end
  end

  module R : sig
    module B : sig
      val double : int -> int @@ total
      module Inner : sig val triple : int -> int @@ total end
    end
    module C : sig val triple : int -> int @@ total end
  end = struct
    module B = Base
    module C = B.Inner
  end

  let same (x : int) : {u : unit | R.B.double x = Base.double x} = ()

  let same_inner (x : int) :
      {u : unit | R.B.Inner.triple x = Base.Inner.triple x
        && R.C.triple x = Base.Inner.triple x} = ()
end;;
[%%expect{|
module Aliases :
  sig
    module Base :
      sig
        val double : int -> int
        module Inner : sig val triple : int -> int end
      end
    module R :
      sig
        module B :
          sig
            val double : int -> int @@ total
            module Inner : sig val triple : int -> int @@ total end
          end
        module C : sig val triple : int -> int @@ total end
      end
    val same : (x : int) -> {u : unit | (R.B.double x) = (Base.double x)}
    val same_inner :
      (x : int) ->
      {u : unit
        | ((R.B.Inner.triple x) = (Base.Inner.triple x)) &&
            ((R.C.triple x) = (Base.Inner.triple x))}
  end
|}]

(* Distinct functions stay distinct. *)
module Distinct = struct
  module Base = struct
    let (double @ total) (x : int) = x + x
  end
  module Other = struct
    let (double @ total) (x : int) = x + x
  end

  module S : sig
    module B : sig val double : int -> int @@ total end
  end = struct
    module B = Other
  end

  let different (x : int) : {u : unit | S.B.double x = Base.double x} = ()
end;;
[%%expect{|
Line 15, characters 72-74:
15 |   let different (x : int) : {u : unit | S.B.double x = Base.double x} = ()
                                                                             ^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 15, characters 40-68:
15 |   let different (x : int) : {u : unit | S.B.double x = Base.double x} = ()
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
