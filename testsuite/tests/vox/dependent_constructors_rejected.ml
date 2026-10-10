(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

type _ bounded =
  | Bounded : { lower : int; value : {v : int | lower <= v} } -> int bounded
  | Empty : unit bounded;;
[%%expect{|
type _ bounded =
    Bounded : { lower : int @@ total; value : {v : int | lower <= v};
    } -> int bounded
  | Empty : unit bounded
|}]

let bad = Bounded { lower = 5; value = 2 };;
[%%expect{|
Line 1, characters 39-40:
1 | let bad = Bounded { lower = 5; value = 2 };;
                                           ^
Error: Refinement could not be proved (counterexample)
Line 2, characters 48-58:
2 |   | Bounded : { lower : int; value : {v : int | lower <= v} } -> int bounded
                                                    ^^^^^^^^^^
  The refinement is stated here.
|}]

let bad (Bounded { lower; value }) : {b : bool | b} = value < lower;;
[%%expect{|
Line 1, characters 54-67:
1 | let bad (Bounded { lower; value }) : {b : bool | b} = value < lower;;
                                                          ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 49-50:
1 | let bad (Bounded { lower; value }) : {b : bool | b} = value < lower;;
                                                     ^
  The refinement is stated here.
|}]

type minimum = Minimum : {
  value : int;
  optimality : (other : {n : int | 0 <= n}) ->
    {u : unit | value <= other} @@ ghost total;
} -> minimum;;
[%%expect{|
type minimum =
    Minimum : { value : int @@ total;
      optimality :
        (other : {n : int | 0 <= n}) -> {u : unit | value <= other} @@ ghost
        total;
    } -> minimum
|}]

let bad = Minimum {
  value = 1;
  optimality = ghost_ (fun _other -> let u = () in refine_ u);
};;
[%%expect{|
Line 3, characters 51-60:
3 |   optimality = ghost_ (fun _other -> let u = () in refine_ u);
                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample: _other = 0)
Line 4, characters 16-30:
4 |     {u : unit | value <= other} @@ ghost total;
                    ^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let bad (Minimum { optimality; _ }) = optimality 0;;
[%%expect{|
Line 1, characters 38-48:
1 | let bad (Minimum { optimality; _ }) = optimality 0;;
                                          ^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]

type mutable_dependency = Mutable : {
  mutable lower : int;
  value : {v : int | lower <= v};
} -> mutable_dependency;;
[%%expect{|
Line 2, characters 2-22:
2 |   mutable lower : int;
      ^^^^^^^^^^^^^^^^^^^^
Error: Dependent record fields must be immutable
|}]

type forward_dependency = Forward : {
  value : {v : int | lower <= v};
  lower : int;
} -> forward_dependency;;
[%%expect{|
Line 2, characters 21-26:
2 |   value : {v : int | lower <= v};
                         ^^^^^
Error: Unbound value "lower"
Hint:   Did you mean "lor"?
|}]

let bad (Bounded r) = Bounded { r with lower = 100 };;
[%%expect{|
Line 1, characters 30-52:
1 | let bad (Bounded r) = Bounded { r with lower = 100 };;
                                  ^^^^^^^^^^^^^^^^^^^^^^
Error: Updating this record also requires replacing the dependent field value
|}]

module Bad : sig
  type t = Bounds : { lower : int; upper : {u : int | lower < u} } -> t
end = struct
  type t = Bounds : { lower : int; upper : {u : int | lower <= u} } -> t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bounds : { lower : int; upper : {u : int | lower <= u} } -> t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t =
               Bounds : { lower : int @@ total;
                 upper : {u : int | lower <= u};
               } -> t
         end
       is not included in
         sig
           type t =
               Bounds : { lower : int @@ total;
                 upper : {u : int | lower < u};
               } -> t
         end
       Type declarations do not match:
         type t =
             Bounds : { lower : int @@ total; upper : {u : int | lower <= u};
             } -> t
       is not included in
         type t =
             Bounds : { lower : int @@ total; upper : {u : int | lower < u};
             } -> t
       Constructors do not match:
         "Bounds : { lower : int @@ total; upper : {u : int | lower <= u};
         } -> t"
       is not the same as:
         "Bounds : { lower : int @@ total; upper : {u : int | lower < u};
         } -> t"
       Fields do not match:
         "upper : {u : int | lower <= u};"
       is not the same as:
         "upper : {u : int | lower < u};"
       The type "{u : int | lower <= u}" is not equal to the type
         "{u : int | lower < u}"
|}]

let bad (Bounded {lower; _}) (Bounded {value; _}) : {b : bool | b} =
  lower <= value;;
[%%expect{|
Line 2, characters 2-16:
2 |   lower <= value;;
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 64-65:
1 | let bad (Bounded {lower; _}) (Bounded {value; _}) : {b : bool | b} =
                                                                    ^
  The refinement is stated here.
|}]

type choice =
  | Impossible of { value : int; proof : {u : unit | value < value} @@ ghost }
  | Anything;;
[%%expect{|
type choice =
    Impossible of { value : int @@ total;
      proof : {u : unit | value < value} @@ ghost;
    }
  | Anything
|}]

let bad (x : choice) : {b : bool | b} =
  match x with
  | Impossible {proof; _} -> ghost_ proof; true
  | Anything -> false;;
[%%expect{|
Line 4, characters 16-21:
4 |   | Anything -> false;;
                    ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 35-36:
1 | let bad (x : choice) : {b : bool | b} =
                                       ^
  The refinement is stated here.
|}]
