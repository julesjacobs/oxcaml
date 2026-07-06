(* TEST
 flags = "-vox-dry-run";
 expect;
*)

(* Compiled [assume_] checks may call this unit's reflected (total_)
   functions, build simple-variant constructor terms, and compare
   hereditarily-structural data with structural equality.  The gate
   (Vox_verify.runtime_check_gate) admits exactly the forms whose
   runtime evaluation agrees with the logic; this file pins both the
   admitted behavior and the rejections. *)

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
[%%expect{|
type ilist = Nil | Cons of int * ilist
val len : ilist -> int = <fun>
val append : ilist -> ilist -> ilist = <fun>
val rev : ilist -> ilist = <fun>
|}]

(* Reflected calls and constructor terms in a compiled check: passing
   and failing, at run time. *)
let singleton_rev (l : ilist) : unit{ rev l = Cons (1, Nil) } = assume_ ()

let ok = let l = Cons (1, Nil) in let _ = singleton_rev l in "held"
[%%expect{|
val singleton_rev : (l : ilist) -> unit{ rev l = Cons (1, Nil) } = <fun>
val ok : string = "held"
|}]

let no = let l = Cons (2, Nil) in let _ = singleton_rev l in "held"
[%%expect{|
Exception:
Failure "vox: assume_ check failed at :1:72: rev l = Cons (1, Nil)".
|}]

(* A spec-function result can feed arithmetic and order when it is
   int-sorted. *)
let short (l : ilist) : unit{ len l + 1 <= 3 } = assume_ ()

let ok2 = let l = Cons (1, Nil) in let _ = short l in "held"
[%%expect{|
val short : (l : ilist) -> unit{ len l + 1 <= 3 } = <fun>
val ok2 : string = "held"
|}]

(* Rejections: a variant-returning function in arithmetic. *)
let bad1 (l : ilist) : unit{ rev l + 1 = 1 } = assume_ ()
[%%expect{|
Line 1, characters 55-57:
1 | let bad1 (l : ilist) : unit{ rev l + 1 = 1 } = assume_ ()
                                                           ^^
Error: vox: assume_ compiles a runtime check of this refinement, but an arithmetic or order operand is the datatype ilist, not Int; use assume_unchecked_
|}]

(* Rejections: an equality across sorts. *)
let bad2 (l : ilist) : unit{ len l = rev l } = assume_ ()
[%%expect{|
Line 1, characters 55-57:
1 | let bad2 (l : ilist) : unit{ len l = rev l } = assume_ ()
                                                           ^^
Error: vox: assume_ compiles a runtime check of this refinement, but an equality compares Int against the datatype ilist; use assume_unchecked_
|}]

(* Rejections: a datatype with an atom-sorted component (structural
   comparison at run time would not be the logic's atom identity). *)
type tagged =
  | Tag of string

let bad3 (t : tagged) : unit{ t = t } = assume_ ()
[%%expect{|
type tagged = Tag of string
Line 4, characters 48-50:
4 | let bad3 (t : tagged) : unit{ t = t } = assume_ ()
                                                    ^^
Error: vox: assume_ compiles a runtime check of this refinement, but t has a sort the check cannot evaluate faithfully (only ints, bools, and datatypes built from them can be checked); use assume_unchecked_
|}]

(* Rejections: a spec function with no runtime definition (the iarray
   theory's length is solver-side only). *)
let bad4 (a : int iarray) : unit{ Iarray.length a = 0 } = assume_ ()
[%%expect{|
Line 1, characters 66-68:
1 | let bad4 (a : int iarray) : unit{ Iarray.length a = 0 } = assume_ ()
                                                                      ^^
Error: vox: assume_ compiles a runtime check of this refinement, but Vox_ia_len has no runtime definition this check could call (only this unit's total_ functions do); use assume_unchecked_
|}]

(* PROBE: shadowing a reflected function's name makes checks that
   mention it a compile error.  The predicate still denotes the
   reflected definition (spec functions have their own namespace),
   but the toplevel's value store is name-keyed at the defining
   phrase, so a compiled check could otherwise call the shadowing
   binding -- the capture would silently verify falsehoods. *)
let rev = fun (_ : ilist) -> Nil

let shadow_probe (l : ilist) : unit{ rev l = Nil } = assume_ ()
[%%expect{|
val rev : ilist -> ilist = <fun>
Line 3, characters 61-63:
3 | let shadow_probe (l : ilist) : unit{ rev l = Nil } = assume_ ()
                                                                 ^^
Error: vox: assume_ compiles a runtime check of this refinement, but rev is shadowed by another binding at this point, so the check could not call the reflected definition the predicate denotes; use assume_unchecked_
|}]
