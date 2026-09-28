(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Talk, scene 22 ("totality"), the "why totality" example. [bogus] claims
   [false] and never returns. A claim holds on every execution that reaches
   it, so an ordinary call of [bogus] is accepted: after the call nothing
   runs, and [use] never returns a wrong value. A ghost call is erased, so
   [use] would return [x] although its type says the result exceeds [x].
   Ghost code must be total, and the ghost call is rejected because [bogus]
   is partial. The control: without the call, [use] is rejected with the
   counterexample [x = 0]. From the investigation's bogus.ml and bogus2.ml
   (research/mastery-20260927/scratch/verifier/). *)

(* Accepted: the call never returns, so the result is never produced. *)
let rec bogus () : {u : unit | false} = bogus ()
let use (x : int) : {v : int | v > x} = bogus (); x;;
[%%expect{|
val bogus : unit -> {u : unit | false} = <fun>
val use : (x : int) -> {v : int | v > x} = <fun>
|}]

(* Rejected: the ghost call is erased, so it cannot be a proof unless it is
   total. *)
let rec bogus () : {u : unit | false} = bogus ()
let use (x : int) : {v : int | v > x} = ghost_ (bogus ()); x;;
[%%expect{|
val bogus : unit -> {u : unit | false} = <fun>
Line 2, characters 48-53:
2 | let use (x : int) : {v : int | v > x} = ghost_ (bogus ()); x;;
                                                    ^^^^^
Error: The value "bogus" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 40-57).
|}]

(* Control: without the call, the claim is false. *)
let use (x : int) : {v : int | v > x} = x;;
[%%expect{|
Line 1, characters 40-41:
1 | let use (x : int) : {v : int | v > x} = x;;
                                            ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 31-36:
1 | let use (x : int) : {v : int | v > x} = x;;
                                   ^^^^^
  The refinement is stated here.
|}]
