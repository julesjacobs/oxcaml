(* TEST
 has-z3;
 flags = "-extension refinement_types -w +unused-proof-step";
 { expect; }
*)

(* Warning 227 reports lemma calls, [assume_]s and refined arguments whose
   facts no refinement proof in their function used, found from Z3's unsat
   cores. *)

module Lemmas = struct
  let[@def] (double @ total) (x : int) = x + x

  let (double_even @ total) (x : int) :
      {u : unit | double x = 2 * x} @ ghost =
    ghost_ (double_def x)

  let (double_positive @ total) (x : {x : int | 0 < x && x < 1000}) :
      {u : unit | double x > x} @ ghost =
    ghost_ (double_def x)
end;;
[%%expect{|
module Lemmas :
  sig
    val double : int -> int
    val double_def : (x : int) -> {u : unit | (double x) === (x + x)}
    val double_even : (x : int) -> {u : unit | (double x) = (2 * x)} @ ghost
    val double_positive :
      (x : {x : int | (0 < x) && (x < 1000)}) ->
      {u : unit | (double x) > x} @ ghost
  end
|}]

(* The proof of the result uses the lemma: no warning. *)
module Needed = struct
  open Lemmas

  let (f @ total) (x : int) : {y : int | y = 2 * x} @ ghost =
    ghost_ (double_even x; double x)
end;;
[%%expect{|
module Needed : sig val f : (x : int) -> {y : int | y = (2 * x)} @ ghost end
|}]

(* The second lemma call adds nothing the proof needs. *)
module Not_needed = struct
  open Lemmas

  let (f @ total) (x : int) : {y : int | y = 2 * x} @ ghost =
    ghost_ (double_even x; double_def 7; double x)
end;;
[%%expect{|
Line 5, characters 27-39:
5 |     ghost_ (double_even x; double_def 7; double x)
                               ^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_def".

module Not_needed :
  sig val f : (x : int) -> {y : int | y = (2 * x)} @ ghost end
|}]

(* Unsat cores are not minimal. Both lemmas are in Z3's core for the result,
   though [double_even x] and [0 < x] imply [r >= 2]: neither is reported
   (unused_proof_steps_precise.ml reports [double_positive x]). *)
module Redundant = struct
  open Lemmas

  let (f @ total) (x : {x : int | 0 < x && x < 1000}) :
      {r : int | r >= 2 && r = 2 * x} @ ghost =
    ghost_ (double_positive x; double_even x; double x)
end;;
[%%expect{|
module Redundant :
  sig
    val f :
      (x : {x : int | (0 < x) && (x < 1000)}) ->
      {r : int | (r >= 2) && (r = (2 * x))} @ ghost
  end
|}]

(* A lemma call whose refined result is the function's own result, at the
   same type, is needed by the typer rather than by a proof: not checked. *)
module Same_type = struct
  open Lemmas

  let (again @ total) (x : int) : {u : unit | double x = 2 * x} @ ghost =
    ghost_ (double_even x)
end;;
[%%expect{|
module Same_type :
  sig
    val again : (x : int) -> {u : unit | (Lemmas.double x) = (2 * x)} @ ghost
  end
|}]

(* An [assume_] whose fact no proof uses. One that produces a value of the
   expected refined type is needed by the typer: not checked. *)
module Assumptions = struct
  type positive = {n : int | n > 0}

  let check x : positive = assume_ x

  let unused (x : int) : int =
    let _ = (assume_ x : positive) in
    x + 1

  let used (x : int) : {n : int | n >= 0} = (assume_ x : positive)
end;;
[%%expect{|
Line 7, characters 13-22:
7 |     let _ = (assume_ x : positive) in
                 ^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this "assume_".

module Assumptions :
  sig
    type positive = {n : int | n > 0}
    val check : int @ total -> positive
    val unused : int -> int
    val used : int -> {n : int | n >= 0}
  end
|}]

(* An assumption that makes the rest of its path impossible is used: the
   obligations on that path are not generated. *)
module Impossible = struct
  let f () : {n : int | n > 0} =
    let u = () in
    let _ = (assume_ u : {u : unit | false}) in
    0
end;;
[%%expect{|
module Impossible : sig val f : unit -> {n : int | n > 0} end
|}]

(* An [assume_] of a parameter: the facts it exposes belong to it as well as to
   the parameter, and here the proof needs them. *)
module Assume_parameter = struct
  let f (x : {x : int | x >= 0}) : {r : int | r > 0} =
    let _ = (assume_ x : {x : int | x > 0}) in
    x + 0
end;;
[%%expect{|
module Assume_parameter :
  sig val f : {x : int | x >= 0} -> {r : int | r > 0} end
|}]

(* A refinement written on an argument that no proof in the function uses,
   next to one that is used, one passed on at its own type (needed by the
   typer), and one named by a type abbreviation (part of that type): only
   the first is reported. *)
module Arguments = struct
  type small = {x : int | 0 < x && x < 100}

  let (used @ total) (x : {x : int | 0 < x && x < 100}) : {y : int | y > 1} =
    x + 1

  let (unused @ total) (x : {x : int | 0 < x && x < 100}) (y : int) :
      {z : int | z = y + 1} =
    y + 1

  let (passed @ total) (x : {x : int | 0 < x && x < 100}) (y : int) :
      small * int =
    x, y

  let (named @ total) (x : small) (y : int) : {z : int | z = y + 1} = y + 1
end;;
[%%expect{|
Line 7, characters 24-25:
7 |   let (unused @ total) (x : {x : int | 0 < x && x < 100}) (y : int) :
                            ^
Warning 227 [unused-proof-step]: No refinement proof in this function used the refinement of
  argument "x".

module Arguments :
  sig
    type small = {x : int | (0 < x) && (x < 100)}
    val used : {x : int | (0 < x) && (x < 100)} -> {y : int | y > 1}
    val unused :
      {x : int | (0 < x) && (x < 100)} ->
      (y : int) -> {z : int | z = (y + 1)}
    val passed : {x : int | (0 < x) && (x < 100)} -> int -> small * int
    val named : small -> (y : int) -> {z : int | z = (y + 1)}
  end
|}]

(* Several goals in one function are proved in one query, whose core covers
   all of them: each lemma used by some goal is used. *)
module Batched = struct
  open Lemmas

  let (f @ total) (x : int) (y : int) : {r : int | r = 2 * x} @ ghost =
    ghost_ (
      double_even x;
      double_even y;
      let (_ : {b : int | b = 2 * y}) = double y in
      double_def 7;
      double x)
end;;
[%%expect{|
Line 9, characters 6-18:
9 |       double_def 7;
          ^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_def".

module Batched :
  sig val f : (x : int) -> int -> {r : int | r = (2 * x)} @ ghost end
|}]

(* Steps about arrays, whose terms request observations: the length fact is
   not needed (every array length is nonnegative), the bound on [i] is. *)
module Arrays = struct
  let (length_nonnegative @ total) (a : int iarray) :
      {u : unit | Iarray.length a >= 0} @ ghost =
    ghost_ ()

  let (first @ total) (a : int iarray) (i : {i : int | 0 <= i}) : int =
    ghost_ (length_nonnegative a);
    if i < Iarray.length a then Iarray.Refined.get a i else 0
end;;
[%%expect{|
Line 7, characters 11-33:
7 |     ghost_ (length_nonnegative a);
               ^^^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "length_nonnegative".

module Arrays :
  sig
    val length_nonnegative :
      (a : int iarray) -> {u : unit | (Iarray.length a) >= 0} @ ghost
    val first : int iarray -> {i : int | 0 <= i} -> int
  end
|}]

module Iarray_observations = struct
  let (length_nonnegative @ total) (a : int iarray) :
      {u : unit | Iarray.length a >= 0} @ ghost = ghost_ ()

  let (literal_length @ total) (key : int) : {n : int | n = 1} @ ghost =
    ghost_ (
      let a = [: key :] in
      length_nonnegative a;
      Iarray.length a)

  let append_length (key : int) : {n : int | n = 2} =
    let a = Iarray.append [: key :] [: key :] in
    ghost_ (length_nonnegative a);
    Iarray.length a

  let (read_reflexive @ total) (a : int iarray)
      (index : {i : int | 0 <= i && i < Iarray.length a}) :
      {u : unit | Iarray.Refined.get a index = Iarray.Refined.get a index}
        @ ghost = ghost_ ()

  let literal_read (key : int) : {n : int | n = key} =
    let a = [: key :] in
    let zero = 0 in
    let index : {i : int | 0 <= i && i < Iarray.length a} = zero in
    ghost_ (read_reflexive a index);
    Iarray.Refined.get a index

  let append_read (key : int) : {n : int | n = key} =
    let a = Iarray.append [: key :] [: key :] in
    let zero = 0 in
    let index : {i : int | 0 <= i && i < Iarray.length a} = zero in
    ghost_ (read_reflexive a index);
    Iarray.Refined.get a index

  let[@def] (wrapped @ total) (a : int iarray) = a

  let (needed @ total) (a : int iarray) :
      {n : int | n = Iarray.length a} @ ghost = ghost_ (
    wrapped_def a;
    Iarray.length (wrapped a))

  let (needed_read @ total) (a : int iarray)
      (index : {i : int | 0 <= i && i < Iarray.length a}) :
      {n : int | n = Iarray.Refined.get a index} @ ghost = ghost_ (
    wrapped_def a;
    let b = wrapped a in
    let bounded : {i : int | 0 <= i && i < Iarray.length b} = index in
    Iarray.Refined.get b bounded)
end;;
[%%expect{|
Line 8, characters 6-26:
8 |       length_nonnegative a;
          ^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "length_nonnegative".

Line 13, characters 11-33:
13 |     ghost_ (length_nonnegative a);
                ^^^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "length_nonnegative".

Line 25, characters 11-35:
25 |     ghost_ (read_reflexive a index);
                ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "read_reflexive".

Line 32, characters 11-35:
32 |     ghost_ (read_reflexive a index);
                ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "read_reflexive".

module Iarray_observations :
  sig
    val length_nonnegative :
      (a : int iarray) -> {u : unit | (Iarray.length a) >= 0} @ ghost
    val literal_length : int -> {n : int | n = 1} @ ghost
    val append_length : int -> {n : int | n = 2}
    val read_reflexive :
      (a : int iarray) ->
      (index : {i : int | (0 <= i) && (i < (Iarray.length a))}) ->
      {u : unit
        | (Iarray.Refined.get a index) = (Iarray.Refined.get a index)} @ ghost
    val literal_read : (key : int) -> {n : int | n = key}
    val append_read : (key : int) -> {n : int | n = key}
    val wrapped : int iarray -> int iarray
    val wrapped_def : (a : int iarray) -> {u : unit | (wrapped a) === a}
    val needed :
      (a : int iarray) -> {n : int | n = (Iarray.length a)} @ ghost
    val needed_read :
      (a : int iarray) ->
      (index : {i : int | (0 <= i) && (i < (Iarray.length a))}) ->
      {n : int | n = (Iarray.Refined.get a index)} @ ghost
  end
|}]

let (without_definition @ total) (a : int iarray) :
    {n : int | n = Iarray.length a} @ ghost =
  ghost_ (Iarray.length (Iarray_observations.wrapped a));;
[%%expect{|
Line 3, characters 9-56:
3 |   ghost_ (Iarray.length (Iarray_observations.wrapped a));;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 15-34:
2 |     {n : int | n = Iarray.length a} @ ghost =
                   ^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* A function with no proof at all uses none of its steps. *)
module No_proof = struct
  open Lemmas

  let (f @ total) (x : int) : int @ ghost = ghost_ (double_even x; x)
end;;
[%%expect{|
Line 4, characters 52-65:
4 |   let (f @ total) (x : int) : int @ ghost = ghost_ (double_even x; x)
                                                        ^^^^^^^^^^^^^
Warning 227 [unused-proof-step]: No refinement proof in this function used the fact from this
  call to "Lemmas.double_even".

module No_proof : sig val f : int -> int @ ghost end
|}]

(* The warning follows [@warning] attributes. *)
module Disabled = struct
  open Lemmas

  let[@warning "-unused-proof-step"] (f @ total) (x : int) : int @ ghost =
    ghost_ (double_even x; x)
end;;
[%%expect{|
module Disabled : sig val f : int -> int @ ghost end
|}]
