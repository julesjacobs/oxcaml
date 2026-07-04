(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Demo: the page's "specs as tests" section.  [assume_ e] enforces a
   contract by runtime check instead of proof: the compiler compiles
   the refinement into a test that raises [Failure] on violation.
   Checks may call this unit's reflected (total_) functions -- the
   definition IS the runtime function -- and compare simple-variant
   values structurally, so the same contracts that are proved
   elsewhere compile to checks here.  Inline, a check validates a
   value once at a trust boundary, and code past the boundary is
   proved against the fact.  Lemma-style, a stated-but-unproved
   property is an oracle checked on every call. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec total_ append a b =
  match a with
  | Nil -> b
  | Cons (h, t) -> Cons (h, append t b)

let rec total_ rev l =
  match l with
  | Nil -> Nil
  | Cons (h, t) -> append (rev t) (Cons (h, Nil))

let rec unreachable_ (u : unit{ false }) : 'a = unreachable_ u

let rec nth (l : ilist) (i : int{ 0 <= _ && _ < len l }) : int =
  match l with
  | Nil -> unreachable_ ()
  | Cons (h, t) -> if i = 0 then h else nth t (i - 1)
[%%expect{|
type ilist = Nil | Cons of int * ilist
val len : ilist -> int = <fun>
val append : ilist -> ilist -> ilist = <fun>
val rev : ilist -> ilist = <fun>
val unreachable_ : unit{ false } -> 'a = <fun>
val nth : (l : ilist) -> int{ (0 <= _) && (_ < (len l)) } -> int = <fun>
|}]

(* Inline, at the boundary: [nth]'s own contract compiles to the
   check, [len] call included; past it, [nth] runs proved and
   checkless. *)
let nth_checked (l : ilist) (i : int) : int = nth l (assume_ i)
[%%expect{|
val nth_checked : ilist -> int -> int = <fun>
|}]

let ok = let l = Cons (1, Cons (2, Cons (3, Nil))) in nth_checked l 1
[%%expect{|
val ok : int = 2
|}]

let boom = let l = Cons (1, Nil) in nth_checked l 5
[%%expect{|
Exception:
Failure "vox: assume_ check failed at :1:61: (0 <= _) && (_ < (len l))".
|}]

(* Lemma-style: stated, not proved; the reflected functions run as
   ordinary code, and every call checks the property. *)
let rev_involutive (l : ilist) : unit{ rev (rev l) = l } = assume_ ()
[%%expect{|
val rev_involutive : (l : ilist) -> unit{ (rev (rev l)) = l } = <fun>
|}]

let checked =
  let l = Cons (1, Cons (2, Cons (3, Nil))) in
  let _ = rev_involutive l in
  "involution held"
[%%expect{|
val checked : string = "involution held"
|}]

(* A false lemma raises at its first counterexample. *)
let rev_id (l : ilist) : unit{ rev l = l } = assume_ ()

let refuted = let l = Cons (1, Cons (2, Nil)) in let _ = rev_id l in "?"
[%%expect{|
val rev_id : (l : ilist) -> unit{ (rev l) = l } = <fun>
Exception: Failure "vox: assume_ check failed at :1:53: (rev l) = l".
|}]
