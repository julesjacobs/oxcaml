(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Talk, section 5 ("testing"): the verdicts around property_testing.ml.
   1. [assume_ ()] is a compile error; the idiom binds the unit first.
   2. The property is also a lemma: [insert_sorted] verifies because it
      calls [prop_insert]; without the call it is rejected.
   3. Preconditions are proved at every call: without the driver's
      [if sorted xs] filter, the call of the property is rejected at compile
      time, so a failing property can only mean the property is false. *)

let prop_add_comm (x : int) (y : int) : {u : unit | x + y = y + x} = assume_ ();;
[%%expect{|
Line 1, characters 77-79:
1 | let prop_add_comm (x : int) (y : int) : {u : unit | x + y = y + x} = assume_ ();;
                                                                                 ^^
Error: "assume_" requires a plain local variable
|}]

let[@def] rec (sorted @ total) (xs : int list) : bool =
  match xs with
  | [] | [_] -> true
  | a :: (b :: _ as rest) -> a <= b && sorted rest

let rec (insert @ total) (x : int) (xs : int list) : int list =
  match xs with
  | [] -> [x]
  | y :: ys -> if x <= y then x :: xs else y :: insert x ys

let prop_insert (x : int) (xs : {xs : int list | sorted xs}) :
    {u : unit | sorted (insert x xs)} =
  let u = () in assume_ u;;
[%%expect{|
val sorted : int list -> bool = <fun>
val sorted_def :
  (xs : int list) ->
  {u : unit
    | (sorted xs) ===
        (match xs with
         | [] | _::[] -> true
         | a::(b::_ as rest) -> (a <= b) && (sorted rest) : bool)} =
  <fun>
val insert : int -> int list -> int list = <fun>
val prop_insert :
  (x : int) ->
  (xs : {xs : int list | sorted xs}) -> {u : unit | sorted (insert x xs)} =
  <fun>
|}]

(* The lemma carries the proof. *)
let insert_sorted (x : int) (xs : {xs : int list | sorted xs}) :
    {ys : int list | sorted ys} =
  prop_insert x xs;
  insert x xs;;
[%%expect{|
val insert_sorted :
  int -> {xs : int list | sorted xs} -> {ys : int list | sorted ys} = <fun>
|}]

(* Without the lemma call, [sorted (insert x xs)] is not proved. *)
let insert_sorted (x : int) (xs : {xs : int list | sorted xs}) :
    {ys : int list | sorted ys} =
  insert x xs;;
[%%expect{|
Line 3, characters 2-13:
3 |   insert x xs;;
      ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 2, characters 21-30:
2 |     {ys : int list | sorted ys} =
                         ^^^^^^^^^
  The refinement is stated here.
|}]

(* The driver without its filter: [xs] is not known to be sorted where the
   property is called. *)
let run name (prop : int -> {xs : int list | sorted xs} -> unit) =
  let passed = ref 0 in
  let rec loop n =
    if n = 0 then Printf.printf "%-18s OK, %d cases\n" name !passed
    else begin
      let x = Random.int 10 - 5 in
      let xs = List.sort compare (List.init (Random.int 6) (fun _ -> Random.int 10 - 5)) in
      if true then begin
        match prop x xs with
        | () -> incr passed; loop (n - 1)
        | exception Assert_failure (_, _, _) -> ()
      end else loop (n - 1)
    end
  in
  loop 10_000;;
[%%expect{|
Line 9, characters 21-23:
9 |         match prop x xs with
                         ^^
Error: Refinement could not be proved (counterexample: n = -1)
Line 1, characters 45-54:
1 | let run name (prop : int -> {xs : int list | sorted xs} -> unit) =
                                                 ^^^^^^^^^
  The refinement is stated here.
|}]
