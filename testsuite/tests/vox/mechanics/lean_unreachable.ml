(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* [unreachable_] claims the current path is dead: its proof obligation is
   [false] under the path facts.  Contradictory facts discharge it. *)

let dead (n : int{ _ >= 0 }) : int =
  if n < 0 then unreachable_ else n
[%%expect{|
val dead : int{ _ >= 0 } -> int = <fun>
|}]

(* A reachable [unreachable_] is a proof failure with a witness. *)

let alive (n : int) : int =
  if n < 0 then unreachable_ else n
[%%expect{|
Line 2, characters 16-28:
2 |   if n < 0 then unreachable_ else n
                    ^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: false
Hypotheses:
  n < 0
Possible counterexample:
  n = -1
(lean: error: `grind` failed)
|}]

(* The direct spelling [let rec f (a : t) (b : t) : r = e] hoists into
   the dependent arrow when an annotation carries a refinement: the
   result refinement below mentions both parameters, and the recursion
   is typed at the full contract (each recursive call's instantiated
   signature is the induction hypothesis). *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec append (a : ilist) (b : ilist) : ilist{ len _ = len a + len b } =
  match a with
  | Nil -> b
  | Cons (h, t) ->
    let r = append t b in
    Cons (h, r)
[%%expect{|
type ilist = Nil | Cons of int * ilist
val len : ilist -> int = <fun>
val append :
  (a : ilist) -> (b : ilist) -> ilist{ (len _) = ((len a) + (len b)) } =
  <fun>
|}]
