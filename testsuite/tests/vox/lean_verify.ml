(* TEST
 flags = "-vox-solver lean -vox-solver-path /nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean";
 modules = "vc_lib.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Real verification through Lean 4's [grind]: every obligation below
   is actually proved (this test fails to compile if any proof fails).
   The solver path is this machine's nix store lean4; adjust or use
   -vox-solver-path/VOX_LEAN elsewhere. *)

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
