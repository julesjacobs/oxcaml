(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { flags += " -principal"; bytecode; }
 { flags += " -O3 -noassert"; native; }
*)

(* Talk, section 5 ("testing: assume_, properties, generators"): the idiom
   [let u = () in assume_ u], the random driver, and the recorded run. The
   driver uses a fixed seed, so the run is deterministic, and the output is
   the same in bytecode, with -principal, and in native code with -O3
   -noassert ([assume_] is never removed). property_testing.reference is the
   slide's transcript. Its rejected variants are in
   property_testing_rejected.ml. *)

(* Properties as lemmas whose proof is a runtime check.

   A property is a function whose parameter types state its preconditions
   and whose refined-unit result states the claim. Its body checks the claim
   with [assume_]. A random driver (or fuzzer, or AI agent) calls it. *)

let[@def] rec (sorted @ total) (xs : int list) : bool =
  match xs with
  | [] | [_] -> true
  | a :: (b :: _ as rest) -> a <= b && sorted rest

let rec (insert @ total) (x : int) (xs : int list) : int list =
  match xs with
  | [] -> [x]
  | y :: ys -> if x <= y then x :: xs else y :: insert x ys

(* Bug: inserts after the first smaller element without recursing. *)
let (insert_buggy @ total) (x : int) (xs : int list) : int list =
  match xs with
  | [] -> [x]
  | y :: ys -> if x <= y then x :: xs else y :: x :: ys

(* The idiom. [assume_ ()] is rejected ("requires a plain local variable"),
   so the unit is bound first. Only the result predicate is checked at run
   time; the precondition [sorted xs] is discharged at every call site. *)
let prop_insert (x : int) (xs : {xs : int list | sorted xs}) :
    {u : unit | sorted (insert x xs)} =
  let u = () in assume_ u

let prop_insert_buggy (x : int) (xs : {xs : int list | sorted xs}) :
    {u : unit | sorted (insert_buggy x xs)} =
  let u = () in assume_ u

(* The property is also a lemma: calling it gives the verifier the fact. *)
let insert_sorted (x : int) (xs : {xs : int list | sorted xs}) :
    {ys : int list | sorted ys} =
  prop_insert x xs;
  insert x xs

let show xs = "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"

(* The driver. The filter [if sorted xs] is what lets [xs] reach the
   property: the branch condition discharges the precondition statically.
   A property that has passed the filter can only fail if it is false. *)
let run name (prop : int -> {xs : int list | sorted xs} -> unit) =
  let passed = ref 0 in
  let rec loop n =
    if n = 0 then Printf.printf "%-18s OK, %d cases\n" name !passed
    else begin
      let x = Random.int 10 - 5 in
      let xs = List.sort compare (List.init (Random.int 6) (fun _ -> Random.int 10 - 5)) in
      if sorted xs then begin
        match prop x xs with
        | () -> incr passed; loop (n - 1)
        | exception Assert_failure (file, line, _) ->
            Printf.printf "%-18s FAILED after %d cases: x = %d, xs = %s (%s:%d)\n"
              name !passed x (show xs) file line
      end else loop (n - 1)
    end
  in
  loop 10_000

let () =
  Random.init 2026;
  run "prop_insert" (refine_ prop_insert);
  run "prop_insert_buggy" (refine_ prop_insert_buggy);
  let empty = [] in
  ghost_ (sorted_def empty);
  let ys = insert_sorted 2 (insert_sorted 1 empty) in
  let refine_ ys = ys in
  Printf.printf "insert_sorted: %s\n" (show ys)
