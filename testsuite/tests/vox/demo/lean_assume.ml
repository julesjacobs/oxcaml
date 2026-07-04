(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Demo: the page's "specs as tests" section.  [assume_ e] enforces a
   contract by runtime check instead of proof: the compiler compiles
   the refinement into a test that raises [Failure] on violation.
   Inline, it validates a value once at a trust boundary, and code
   past the boundary is proved against the fact with no further
   checks.  Lemma-style, a stated-but-unproved property over
   reflected (executable) functions is an oracle checked on every
   call. *)

(* Validated once, at the boundary: the check is compiled from the
   type. *)
let percent (n : int) : int{ 0 <= _ && _ <= 100 } = assume_ n

(* Past the boundary: proved, no check compiled. *)
let complement (p : int{ 0 <= _ && _ <= 100 }) : int{ 0 <= _ && _ <= 100 } =
  100 - p
[%%expect{|
val percent : int -> int{ (0 <= _) && (_ <= 100) } = <fun>
val complement :
  int{ (0 <= _) && (_ <= 100) } -> int{ (0 <= _) && (_ <= 100) } = <fun>
|}]

let ok = let p = percent 30 in complement p
[%%expect{|
val ok : int{ (0 <= _) && (_ <= 100) } = 70
|}]

let boom = percent 130
[%%expect{|
Exception:
Failure "vox: assume_ check failed at :1:60: (0 <= _) && (_ <= 100)".
|}]

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ append a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, append t b)

let rec total_ rev l =
  match l with
  | Nil -> Nil
  | Cons (h, t) -> append (rev t) (Cons (h, Nil))
[%%expect{|
type ilist = Nil | Cons of int * ilist
val append : ilist -> ilist -> ilist = <fun>
val rev : ilist -> ilist = <fun>
|}]

(* Stated, not proved: the reflected functions run as ordinary code,
   and every call checks the property. *)
let rev_involutive (l : ilist) : bool{ _ = true } =
  assume_ (rev (rev l) = l)

let checked = let l = Cons (1, Cons (2, Cons (3, Nil))) in rev_involutive l
[%%expect{|
val rev_involutive : ilist -> bool{ _ = true } = <fun>
val checked : bool{ _ = true } = true
|}]
