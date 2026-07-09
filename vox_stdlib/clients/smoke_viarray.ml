(* Smoke client for Viarray. length/get exercise the built-in theory; for_all /
   any / mem exercise the WP-4 read-only queries -- the reflected bool result
   carries the window predicate ia_all_from / ia_ex_from / ia_mem_from over the
   whole array. Predicates are call-site lambdas; goals name a block abbrev (a
   lambda may not appear in refinement text). The step/done laws are load-bearing
   at the .ml seal (the loop proof); deleting either breaks viarray.ml. Verifies
   against viarray.cmi + VoxSig_Viarray.olean only. *)

[@@@warning "-6-32-26-27"]

let length_nonneg (a : int iarray) : int{ 0 <= _ } = Viarray.length a

let get_reads (a : int iarray) (i : int{ 0 <= _ && _ < Iarray.length a })
  : int{ _ = a.(i) } = Viarray.get a i

[%%vox.lean {lean|
@[grind, expose] abbrev pNn : Int -> Prop := fun x => x >= 0
@[grind, expose] abbrev pGt5 : Int -> Prop := fun x => x > 5
|lean}]

(* for_all: result equals the all-window predicate over [0, length). *)
let forall_nn (a : int iarray) : bool{ _ = ia_all_from pNn a 0 (Iarray.length a) } =
  Viarray.for_all (fun x -> x >= 0) (fun x -> x >= 0) a

(* any: result equals the exists-window predicate. *)
let any_gt5 (a : int iarray) : bool{ _ = ia_ex_from pGt5 a 0 (Iarray.length a) } =
  Viarray.any (fun x -> x > 5) (fun x -> x > 5) a

(* mem: result equals the membership-window predicate for the value x. *)
let mem_x (x : int) (a : int iarray)
  : bool{ _ = ia_mem_from x a 0 (Iarray.length a) } = Viarray.mem x a
