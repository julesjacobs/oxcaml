(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
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
  let (r1p, t1a0) = read c1 a t1 in
  let (r2p, t2a0) = read c2 b t2 in
  let refine_ r1 = r1p in                     (* r1 = a *)
  let refine_ r2 = r2p in                     (* r2 = b *)
  let refine_ t1a = t1a0 in
  let refine_ t2a = t2a0 in
  let refine_ t1b = write c1 a r2 t1a in      (* cell1 := r2 *)
  let refine_ t2b = write c2 b r1 t2a in      (* cell2 := r1 *)
  let (s1p, u1) = read c1 r2 t1b in
  let (s2p, u2) = read c2 r1 t2b in
  ignore u1; ignore u2;
  let refine_ s1 = s1p in                     (* s1 = r2 = b *)
  let refine_ s2 = s2p in                     (* s2 = r1 = a *)
  refine_ (s1 + s2)

(* Token threading in a helper that never mentions the pair type: its
   solver input must stay valid (regression for prelude injection).
   The incoming token is a contract parameter, so the body holds it at
   the bare skeleton with its facts; returning the new token at
   [cts _ = k + 1] rather than the rigid [cts _ = v1] is a
   re-proof at the result package's type from the unpacked fact
   (cts t2 = v1) and v1 = k + 1. *)
let bump_via : (c : icell) -> (k : int) ->
  itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k + 1 } * itoken{ tid _ = cid c && cts _ = k + 1 }) @ unique =
  fun c k t ->
  let (rp, t1) = read c k t in
  let refine_ r = rp in
  let refine_ v1 = refine_ (r + 1) in
  let refine_ t1u = t1 in
  let refine_ t2 = write c k v1 t1u in
  ((refine_ v1 : int{ _ = k + 1 }),
   (refine_ t2 : itoken{ tid _ = cid c && cts _ = k + 1 }))

(* Drive the helper from a fresh allocation: proves alloc + bump
   compose (result n + 1 from initial contents n). *)
let bump : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let refine_ p = alloc n in
  let { cell = c; tok = t } = p in
  let (rp, u) = bump_via c n t in
  ignore u;
  let refine_ r = rp in
  refine_ r
