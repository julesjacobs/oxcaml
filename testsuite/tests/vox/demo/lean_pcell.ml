(* TEST
 flags = "-vox-solver lean -vox-prelude pcell_spec.lean";
 script = "sh ${test_source_directory}/../has-lean.sh";
 readonly_files = "pcell_spec.lean";
 modules = "pcell_lib.mli pcell_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Interior mutability via unique separation tokens (pcell_lib): every
   obligation below is really proved through Lean's [grind].  The mode
   checker enforces the linear token discipline; the negative probes
   (duplication, stale reuse, forged and cross-cell tokens) live in
   lean_pcell_fail.ml. *)

open Pcell_lib

(* Two cells' tokens live at once -- the point of the pair-returning
   [alloc].  Swap the cells' contents through interleaved reads and
   writes and prove the final sum. *)
let swap_sum : (a : int) -> (b : int) -> int{ _ = a + b } =
  fun a b ->
  let refine_ p = alloc a in
  let refine_ q = alloc b in
  let { cell = c1; tok = t1 } = p in
  let { cell = c2; tok = t2 } = q in
  let t1' = (refine_ t1 : itoken{ tid _ = cid c1 && cts _ = a }) in
  let t2' = (refine_ t2 : itoken{ tid _ = cid c2 && cts _ = b }) in
  let (r1p, t1a) = read c1 a t1' in
  let (r2p, t2a) = read c2 b t2' in
  let refine_ r1 = r1p in                     (* r1 = a *)
  let refine_ r2 = r2p in                     (* r2 = b *)
  let t1b = write c1 a r2 t1a in              (* cell1 := r2 *)
  let t2b = write c2 b r1 t2a in              (* cell2 := r1 *)
  let (s1p, u1) = read c1 r2 t1b in
  let (s2p, u2) = read c2 r1 t2b in
  ignore u1; ignore u2;
  let refine_ s1 = s1p in                     (* s1 = r2 = b *)
  let refine_ s2 = s2p in                     (* s2 = r1 = a *)
  (refine_ ((s1 :> int) + (s2 :> int)) : int{ _ = a + b })

(* Token threading in a helper that never mentions the pair type: its
   solver input must stay valid (regression for prelude injection).
   Returning the new token at [cts _ = k + 1] rather than the rigid
   [cts _ = v1] needs a strip-and-reprove: a refine_ EXPRESSION
   expects its subject at the bare skeleton type, and the refine_
   PATTERN that strips [t2] also registers its fact (cts t2b = v1),
   which the re-proof needs. *)
let bump_via : (c : icell) -> (k : int) ->
  itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k + 1 } * itoken{ tid _ = cid c && cts _ = k + 1 }) @ unique =
  fun c k t ->
  let (rp, t1) = read c k t in
  let refine_ r = rp in
  let refine_ v1 = (refine_ ((r :> int) + 1) : int{ _ = k + 1 }) in
  let t2 = write c k v1 t1 in
  let refine_ t2b = t2 in
  let t2' = (refine_ t2b : itoken{ tid _ = cid c && cts _ = k + 1 }) in
  ((refine_ (v1 :> int) : int{ _ = k + 1 }), t2')

(* Drive the helper from a fresh allocation: proves alloc + bump
   compose (result n + 1 from initial contents n). *)
let bump : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let refine_ p = alloc n in
  let { cell = c; tok = t } = p in
  let t' = (refine_ t : itoken{ tid _ = cid c && cts _ = n }) in
  let (rp, u) = bump_via c n t' in
  ignore u;
  let refine_ r = rp in
  (refine_ (r :> int) : int{ _ = n + 1 })
