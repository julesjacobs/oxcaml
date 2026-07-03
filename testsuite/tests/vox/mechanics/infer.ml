(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: implicit refinement introduction/elimination.  Every form
   below is written WITHOUT [refine_]; elaboration inserts the intro
   (or elim) where rigid unification would otherwise fail.  Run with
   -vox-dry-run; update with [make promote-one TEST=vox/mechanics/infer.ml]. *)

(* Implicit intro on a literal: same VC as [refine_ 3]. *)
let x : {v:int | v > 0} = 3
[%%expect{|
Line 1, characters 26-27: vox VC:
  goal: 3 > 0
  hypotheses: <none>
val x : int{ _ > 0 } = 3
|}]

(* Implicit intros at the leaves of an [if]: one VC per branch, each
   under its path fact. *)
let branch (c : bool) : {v:int | v >= 1} = if c then 1 else 2
[%%expect{|
Line 1, characters 53-54: vox VC:
  goal: 1 >= 1
  hypotheses:
  c
  x = 3
  x > 0
Line 1, characters 60-61: vox VC:
  goal: 2 >= 1
  hypotheses:
  not c
  x = 3
  x > 0
val branch : bool -> int{ _ >= 1 } = <fun>
|}]

(* Binders as facts: the parameter binds at the skeleton [int] with
   its refinement as a fact, so carrier uses need nothing.  The old
   spelling was [let refine_ n = n in n + 1]. *)
let succ_pos (n : {v:int | v > 0}) : int = n + 1
[%%expect{|
val succ_pos : int{ _ > 0 } -> int = <fun>
|}]

(* Implicit intro on an application result: the argument [n] is
   implicitly eliminated inside, and the translatable result names
   itself, so the goal is [n + 1 >= 1] under the binder fact
   [n >= 0]. *)
let inc (n : {v:int | v >= 0}) : {v:int | v >= 1} = n + 1
[%%expect{|
Line 1, characters 52-57: vox VC:
  goal: (n + 1) >= 1
  hypotheses:
  n >= 0
  x = 3
  x > 0
val inc : int{ _ >= 0 } -> int{ _ >= 1 } = <fun>
|}]

(* Round trip: [n] binds at the skeleton with its fact, and the result
   annotation re-introduces -- a trivial obligation discharged by the
   binder fact. *)
let pass (n : {v:int | v > 0}) : {v:int | v > 0} = n
[%%expect{|
Line 1, characters 51-52: vox VC:
  goal: n > 0
  hypotheses:
  n > 0
  x = 3
  x > 0
val pass : int{ _ > 0 } -> int{ _ > 0 } = <fun>
|}]

(* Weakening is just an intro from a carrier binder: obligation
   [n > 0], with the binder's own fact [n > 1] in context. *)
let weaken (n : {v:int | v > 1}) : {v:int | v > 0} = n
[%%expect{|
Line 1, characters 53-54: vox VC:
  goal: n > 0
  hypotheses:
  n > 1
  x = 3
  x > 0
val weaken : int{ _ > 1 } -> int{ _ > 0 } = <fun>
|}]

(* A refined bool parameter binds at [bool] and conditions on it
   directly; the path fact is the variable itself. *)
let cond (c : {v:bool | v}) : int = if c then 1 else 0
[%%expect{|
val cond : bool{ _ } -> int = <fun>
|}]

(* The DESIGN.md div/safe example, with no explicit intro or elim:
   [b] binds at the skeleton (binders as facts) so [a / b] is plain
   arithmetic; the call [div 100 x] discharges the parameter's
   CONTRACT at [x] under the path fact [0 < x].  The contract rule
   owns argument positions (the domain is stripped before the
   argument is typed, so the implicit intro cannot fire there):
   exactly one obligation. *)
let div (a : int) (b : {v:int | not (v = 0)}) : int = a / b
[%%expect{|
val div : int -> int{ not (_ = 0) } -> int = <fun>
|}]

let safe (x : int) : int = if 0 < x then div 100 x else 0
[%%expect{|
Line 1, characters 49-50: vox VC:
  goal: not (x = 0)
  hypotheses:
  0 < x
  x#2 = 3
  x#2 > 0
val safe : int -> int = <fun>
|}]

(* An APPLICATION at a different refinement is re-refined inline: the
   call's own instantiated result refinement is selfified at the
   node's name and hypothesizes the obligation -- the unpack that
   [let q = f () in q] used to spell.  (Here it is too weak: v > 0
   does not give v > 1, and the obligation fails closed.) *)
let bad (f : unit -> {v:int | v > 0}) : {v:int | v > 1} = f ()
[%%expect{|
Line 1, characters 58-62: vox VC:
  goal: *unknown9* > 1
  hypotheses:
  *unknown9* > 0
  x = 3
  x > 0
val bad : (unit -> int{ _ > 0 }) -> int{ _ > 1 } = <fun>
|}]

(* Joins are order-insensitive under binders as facts: [b] binds at
   the skeleton, so [if c then b else 0] and [if c then 0 else b]
   (errors.ml) both join at plain [int], with no obligation.  The
   old order-sensitivity only arose from package-typed binders. *)
let join_rev (b : {v:int | v > 0}) (c : bool) = if c then b else 0
[%%expect{|
val join_rev : int{ _ > 0 } -> bool -> int = <fun>
|}]

(* Synthesis mode is unchanged: with no refined type expected from
   context, an implicit intro never fires; [refine_ 3] synthesizes the
   exact refinement, which the binder then unpacks.  (The binder
   EQUATION [n = 3] would carry the same fact for a plain [let n = 3]
   -- the explicit spelling remains for emphasis.) *)
let named () : {v:int | v = 3} =
  let refine_ n = refine_ 3 in
  n
[%%expect{|
Line 3, characters 2-3: vox VC:
  goal: n = 3
  hypotheses:
  n = 3
  x = 3
  x > 0
val named : unit -> int{ _ = 3 } = <fun>
|}]

(* Implicit intros propagate through [exclave_]: its typing rule
   checks the body against the expected type, so the intros land at
   the branch LEAVES, under their path facts -- not on the exclave as
   a whole (which would name a fresh unknown). *)
let epropagate : (c : bool) -> int{ _ >= 1 } @ local =
  fun c -> exclave_ (if c then 1 else 2)
[%%expect{|
Line 2, characters 31-32: vox VC:
  goal: 1 >= 1
  hypotheses:
  c
  x = 3
  x > 0
Line 2, characters 38-39: vox VC:
  goal: 2 >= 1
  hypotheses:
  not c
  x = 3
  x > 0
val epropagate : bool -> int{ _ >= 1 } @ local = <fun>
|}]
