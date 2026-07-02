(* TEST
 flags = "-dump-vc -vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: VC generation display test.  Compiled with -vox-dry-run, so VCs
   are printed but not sent to the solver; the expected output lives in
   vc_gen.compilers.reference (update with [make promote-one
   TEST=vox/vc_gen.ml]). *)

(* Intro on a literal: one VC with no hypotheses. *)
let x : {v:int | v > 0} = refine_ 3

(* Toplevel binder facts accumulate for later items. *)
let b : {v:bool | v} = refine_ true

(* assume_ skips the obligation and is flagged ASSUMED. *)
let a : {v:int | v >= 0} = assume_ 5

(* Elimination: no VC. *)
let weaken (d : {v:int | not (v = 0)}) : int = (d :> int)

(* Coercions are transparent to naming: the goal is about [n], and the
   hypothesis from [n]'s own binder discharges it. *)
let reuse (n : {v:int | not (v = 0)}) : {v:int | not (v = 0)} =
  refine_ (n :> int)

(* Unpacking: [w] gets [x]'s refinement as a fact. *)
let unpack : {v:int | v > 0} =
  let refine_ w = x in
  refine_ w

(* Path facts: each branch is checked under the condition / its
   negation. *)
let branch (c : bool) : {v:bool | v || not v} =
  if c then refine_ c else refine_ false
