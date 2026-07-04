(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: PARAMETERIZED simple datatypes, discharged by the lean backend.
   The generic declaration is emitted once ([inductive Vox_..mylist
   (a0 : Type) ...]) and instantiated at each use; injectivity and
   distinctness hold at every instantiation.  has-lean.sh locates the
   solver (VOX_LEAN, PATH, or a pinned copy) and skips otherwise. *)

type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist

(* Injectivity of [Cons] at the [int] instantiation really discharges. *)
let get_i (s : (int mylist){ _ = Cons (3, Nil) }) : {r:int | r = 3} =
  match s with
  | Cons (h, t) -> refine_ h
  | Nil -> assume_ 0
[%%expect{|
type 'a mylist = Nil | Cons of 'a * 'a mylist
val get_i : int mylist{ _ = (Cons (3, Nil)) } -> int{ _ = 3 } = <fun>
|}]

(* A false goal must FAIL: [Cons (3, Nil)] is not [Cons (4, Nil)], so
   [h] cannot be 4 (injectivity gives the counterexample [h = 3]). *)
let bad (s : (int mylist){ _ = Cons (3, Nil) }) : {r:int | r = 4} =
  match s with
  | Cons (h, t) -> refine_ h
  | Nil -> assume_ 0
[%%expect{|
Line 3, characters 27-28:
3 |   | Cons (h, t) -> refine_ h
                               ^
Error: vox: verification failed (lean).
       Goal: h = 4
Hypotheses:
  s = (Cons (h, t))
  s = (Cons (3, Nil))
Possible counterexample:
  h = 3
(lean: error: `grind` failed)
|}]
