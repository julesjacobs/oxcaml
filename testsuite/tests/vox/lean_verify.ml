(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/has-lean.sh";
 modules = "vc_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Real verification through Lean 4's [grind]: every obligation below
   is actually proved (this test fails to compile if any proof fails).
   has-lean.sh locates the solver (VOX_LEAN, PATH, or a pinned copy)
   and skips the test when there is none. *)

(* Arithmetic through a dependent userland operation (linear: grind's
   integer reasoning; nonlinear facts like x*x >= 0 are beyond it). *)
let double_pos (x : {v:int | v > 0}) : {v:int | v > 1} =
  let refine_ x' = x in
  let refine_ d = Vc_lib.add x' x' in
  refine_ d

(* Cross-module refined value: the fact travels via the .cmi. *)
let from_lib : {v:int | v > 0} =
  let refine_ p = Vc_lib.pos in
  refine_ p

(* Path facts + dependent comparison (DESIGN.md flagship shape). *)
let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)

let zero : {v:int | v = 0} = assume_ 0

let div (a : int) (b : {v:int | not (v = 0)}) : int =
  a / (let refine_ b = b in b)

let safe (x : int) : int =
  let refine_ z = zero in
  let refine_ c = lt z x in
  if c then div 100 (refine_ x) else 0

(* Coercions are transparent to logical naming. *)
let weaken_strengthen (n : {v:int | v > 1}) : {v:int | v > 0} =
  refine_ (n :> int)

(* Both path polarities: the then-branch uses the fact [c], the
   else-branch the fact [not c]. *)
let branch_facts (a : int) (b : int) : {v:bool | v || not v} =
  let refine_ c = lt a b in
  if c then refine_ c else refine_ true

(* Reflection: refine_ on compound int/bool expressions.  Synthesis
   position gives the exact refinement {v | v = e'}; checking position
   reflects e' into the goal; compound if-conditions become path facts
   directly.  The flagship shape again, with no userland operations at
   all. *)
let safe_reflect (x : int) : int =
  let c = refine_ (0 < x) in
  if (c :> bool) then div 100 (refine_ x) else 0

let safe_direct (x : int) : int =
  if 0 < x then div 100 (refine_ x) else 0

(* Checking position: the goal is (x + 1) > x, proved by grind. *)
let bump : (x : int) -> {v:int | v > x} =
  fun x -> refine_ (x + 1)

(* Exact refinements compose through binder facts: d = x + x and
   x > 0 prove d > 1. *)
let double_reflect (x : {v:int | v > 0}) : {v:int | v > 1} =
  let refine_ x' = x in
  let d = refine_ (x' + x') in
  refine_ (d :> int)
