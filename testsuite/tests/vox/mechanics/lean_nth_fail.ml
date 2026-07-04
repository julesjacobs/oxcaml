(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* The page's "break it" example, pinned so CI verifies exactly the
   failure output shown there.  The off-by-one precondition
   ([i <= len l] instead of [i < len l]) makes the [Nil] arm
   reachable: the [unreachable_ ()] call's obligation ([false]) is no
   longer provable, and the counterexample names the boundary case the
   sloppy bound lets through -- element 0 of the empty list. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec unreachable_ (u : unit{ false }) : 'a = unreachable_ u

let rec nth (l : ilist) (i : int{ 0 <= _ && _ <= len l }) : int =
  match l with
  | Nil -> unreachable_ ()
  | Cons (h, t) -> if i = 0 then h else nth t (i - 1)
[%%expect{|
type ilist = Nil | Cons of int * ilist
val len : ilist -> int = <fun>
val unreachable_ : unit{ false } -> 'a = <fun>
Line 14, characters 24-26:
14 |   | Nil -> unreachable_ ()
                             ^^
Error: vox: verification failed (lean).
       Goal: false
Hypotheses:
  l = Nil
  (0 <= i) && (i <= (len l))
Possible counterexample:
  i = 0
  len l = 0
  len Vox_ilist.Nil = 0
(lean: error: `grind` failed)
|}]
