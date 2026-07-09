(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* Module-level values in WRITTEN predicates: an unapplied qualified
   identifier resolves to [Pglobal] -- stamp-free and .cmi-stable --
   and the value's own refinement arrives as a global fact wherever
   the predicate is used. *)
module M = struct
  let cap : {v:int | v = 10} = 10
end
[%%expect{|
Line 2, characters 31-33: vox VC:
  goal: 10 = 10
  hypotheses: <none>
module M : sig val cap : int{ _ = 10 } end
|}]

let ok : int{ _ <= M.cap } = 3
[%%expect{|
Line 1, characters 29-30: vox VC:
  goal: 3 <= M.cap
  hypotheses:
  M.cap = 10
val ok : int{ _ <= M.cap } = 3
|}]

(* The predicate TRAVELS through a signature: facts follow paths. *)
module N : sig
  val small : int{ _ <= M.cap }
end = struct
  let small : int{ _ <= M.cap } = 7
end
[%%expect{|
Line 4, characters 34-35: vox VC:
  goal: 7 <= M.cap
  hypotheses:
  ok = 3
  ok <= M.cap
  M.cap = 10
module N : sig val small : int{ _ <= M.cap } end
|}]

(* A client's obligation about [s] discharges from the chain
   s = N.small, N.small <= M.cap, M.cap = 10. *)
let use : int{ _ <= M.cap } =
  let s = N.small in
  s
[%%expect{|
Line 3, characters 2-3: vox VC:
  goal: s <= M.cap
  hypotheses:
  s = N.small
  ok = 3
  ok <= M.cap
  N.small <= M.cap
  M.cap = 10
val use : int{ _ <= M.cap } = 7
|}]
