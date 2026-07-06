(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: the [@@vox.lemma] export PATH is observable under [-dump-vc]: a
   body the v2 translator covers exports via "structural translation";
   a shape it does not cover routes to the v1 tactic "fallback".  Both
   remain fail-closed at the solver. *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len (l : ilist) : int =
  match l with Nil -> 0 | Cons (_, t) -> 1 + len t
[%%expect{|
type ilist = Nil | Cons of int * ilist
val len : ilist -> int = <fun>
|}]

(* Structural: match + self-call -> v2 proof term. *)
let rec lemma_nonneg (l : ilist) : unit{ len l >= 0 } =
  match l with Nil -> () | Cons (_, t) -> lemma_nonneg t
[@@vox.lemma]
[%%expect{|
vox: [@vox.lemma] lemma_nonneg exported via structural translation
Line 2, characters 22-24: vox VC:
  goal: len l >= 0
  hypotheses:
  l = Nil
Line 2, characters 42-56: vox VC:
  goal: len l >= 0
  hypotheses:
  len t >= 0
  l = Cons (*vox-wild*, t)
val lemma_nonneg : (l : ilist) -> unit{ len l >= 0 } = <fun>
|}]

(* A call to a non-lemma helper is outside the translator fragment ->
   v1 fallback. *)
let helper (x : ilist) : unit = ignore x
[%%expect{|
val helper : ilist -> unit = <fun>
|}]

let rec lemma_fb (l : ilist) : unit{ len l >= 0 } =
  match l with Nil -> helper l | Cons (_, t) -> lemma_fb t
[@@vox.lemma]
[%%expect{|
vox: [@vox.lemma] lemma_fb exported via fallback translation
Line 2, characters 22-30: vox VC:
  goal: len l >= 0
  hypotheses:
  l = Nil
Line 2, characters 48-58: vox VC:
  goal: len l >= 0
  hypotheses:
  len t >= 0
  l = Cons (*vox-wild*, t)
val lemma_fb : (l : ilist) -> unit{ len l >= 0 } = <fun>
|}]
