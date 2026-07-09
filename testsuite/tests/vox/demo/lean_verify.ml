(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "vc_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: first contact with vox.  Real verification through
   Lean 4's [grind]: every obligation below is actually proved (this
   test fails to compile if any proof fails).  has-lean.sh locates the
   solver (VOX_LEAN, PATH, or a pinned copy) and skips the test when
   there is none.

   There are no intro or elim forms anywhere in this file.  Binders
   bind at their skeletons with their refinements as facts, a plain
   [let] of a translatable expression contributes its defining
   equation, annotations and calls introduce proof obligations
   implicitly, and contract parameters are discharged at each
   application. *)

(* Arithmetic through a dependent userland operation (linear: grind's
   integer reasoning; nonlinear facts like x*x >= 0 are beyond it).
   The call's refined result is unpacked by the [let] that names
   it. *)
let double_pos (x : {v:int | v > 0}) : {v:int | v > 1} =
  let d = Vc_lib.add x x in
  d

(* Cross-module refined value: the fact travels via the .cmi and is
   unpacked by a plain local [let]. *)
let from_lib : {v:int | v > 0} =
  let p = Vc_lib.pos in
  p

(* Path facts + dependent comparison (DESIGN.md flagship shape).
   Reflection makes both obligations trivial -- (x < y) = (x < y) and
   0 = 0 -- so these userland operations are PROVED, not assumed. *)
let lt (x : int) (y : int) : {z:bool | z = (x < y)} = x < y

let zero : {v:int | v = 0} = 0

let div (a : int) (b : {v:int | not (v = 0)}) : int = a / b

(* [zero]'s toplevel binder equation [zero = 0], the binder equation
   [c = (zero < x)] from [lt]'s dependent result, and the path fact
   [c] together discharge [div]'s contract at [x]. *)
let safe (x : int) : int =
  let c = lt zero x in
  if c then div 100 x else 0

(* Weakening is an implicit introduction from the binder's fact. *)
let weaken_strengthen (n : {v:int | v > 1}) : {v:int | v > 0} = n

(* Both path polarities: the then-branch uses the fact [c], the
   else-branch the fact [not c]. *)
let branch_facts (a : int) (b : int) : {v:bool | v || not v} =
  let c = lt a b in
  if c then c else true

(* Reflection without userland operations: a plain [let] of a
   translatable expression names it exactly ([c = (0 < x)]), and a
   translatable compound condition contributes itself as the path
   fact. *)
let safe_reflect (x : int) : int =
  let c = 0 < x in
  if c then div 100 x else 0

let safe_direct (x : int) : int =
  if 0 < x then div 100 x else 0

(* Checking position: the goal is (x + 1) > x, proved by grind. *)
let bump (x : int) : {v:int | v > x} = x + 1

(* Exact binder equations compose: d = x + x and x > 0 prove d > 1. *)
let double_reflect (x : {v:int | v > 0}) : {v:int | v > 1} =
  let d = x + x in
  d
