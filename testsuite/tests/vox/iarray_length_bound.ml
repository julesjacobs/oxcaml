(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Every iarray, ghost or real, has a length in [0, 2^60 - 1]. *)
let successor (a : int iarray) : {n : int | n > Iarray.length a} =
  Iarray.length a + 1;;
[%%expect{|
val successor : (a : int iarray) -> {n : int | n > (Iarray.length a)} = <fun>
|}]

let bounded (a : int iarray) (b : int iarray) :
    {u : unit | 0 <= Iarray.length a
      && Iarray.length a <= 1152921504606846975
      && Iarray.length a + Iarray.length b >= Iarray.length a} = ();;
[%%expect{|
val bounded :
  (a : int iarray) ->
  (b : int iarray) ->
  {u : unit
    | (0 <= (Iarray.length a)) &&
        (((Iarray.length a) <= 1152921504606846975) &&
           (((Iarray.length a) + (Iarray.length b)) >= (Iarray.length a)))} =
  <fun>
|}]

let appended (a : int iarray) (b : int iarray) :
    {n : int | n >= Iarray.length a} =
  Iarray.length (Iarray.append a b);;
[%%expect{|
val appended :
  (a : int iarray) -> int iarray -> {n : int | n >= (Iarray.length a)} =
  <fun>
|}]

(* The bound is not tighter than stated. *)
let too_tight (a : int iarray) :
    {u : unit | Iarray.length a < 1152921504606846975} = ();;
[%%expect{|
Line 2, characters 57-59:
2 |     {u : unit | Iarray.length a < 1152921504606846975} = ();;
                                                             ^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 16-53:
2 |     {u : unit | Iarray.length a < 1152921504606846975} = ();;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
