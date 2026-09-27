(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A polymorphic value built only from constructors and literals is
   reflected at every instance in its compilation unit (here, its phrase). *)

module Reflected = struct
  let (e1 @ total) = [None]
  let (c1 @ total) () : {u : unit | (e1 : int option list) === [None]} = ()
  let (c2 @ total)
      : type a. unit -> {u : unit | (e1 : a option list) === [None]} =
    fun () -> ()
  let (e2 @ total) = ([], 0)
  let (c3 @ total) () : {u : unit | (e2 : int list * int) === ([], 0)} = ()
  let (e3 @ total) = Some []
  let (c4 @ total) () : {u : unit | (e3 : bool list option) === Some []} = ()
end;;
[%%expect{|
module Reflected :
  sig
    val e1 : 'a option list
    val c1 : unit -> {u : unit | (e1 : int option list) === [None]}
    val c2 : unit -> {u : unit | (e1 : 'a option list) === [None]}
    val e2 : 'a list * int
    val c3 : unit -> {u : unit | (e2 : int list * int) === ([], 0)}
    val e3 : 'a list option
    val c4 : unit -> {u : unit | (e3 : bool list option) === (Some [])}
  end
|}]

(* The rebuilt term is the value, not any other. *)
module Wrong_length = struct
  let (e1 @ total) = [None]
  let (wrong @ total) () :
    {u : unit | (e1 : int option list) === [None; None]} = ()
end;;
[%%expect{|
Line 4, characters 59-61:
4 |     {u : unit | (e1 : int option list) === [None; None]} = ()
                                                               ^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 16-55:
4 |     {u : unit | (e1 : int option list) === [None; None]} = ()
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Wrong_literal = struct
  let (e2 @ total) = ([], 0)
  let (wrong @ total) () : {u : unit | (e2 : int list * int) === ([], 1)} = ()
end;;
[%%expect{|
Line 3, characters 76-78:
3 |   let (wrong @ total) () : {u : unit | (e2 : int list * int) === ([], 1)} = ()
                                                                                ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 39-72:
3 |   let (wrong @ total) () : {u : unit | (e2 : int list * int) === ([], 1)} = ()
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Wrong_payload = struct
  let (e3 @ total) = Some []
  let (wrong @ total) () :
    {u : unit | (e3 : bool list option) === Some [true]} = ()
end;;
[%%expect{|
Line 4, characters 59-61:
4 |     {u : unit | (e3 : bool list option) === Some [true]} = ()
                                                               ^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 16-55:
4 |     {u : unit | (e3 : bool list option) === Some [true]} = ()
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Another unit sees only the type. *)
let (elsewhere @ total) () :
  {u : unit | (Reflected.e1 : int option list) === [None]} = ();;
[%%expect{|
Line 2, characters 61-63:
2 |   {u : unit | (Reflected.e1 : int option list) === [None]} = ();;
                                                                 ^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 14-57:
2 |   {u : unit | (Reflected.e1 : int option list) === [None]} = ();;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
