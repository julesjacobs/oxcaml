(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pcell_lib.mli ../lib/pcell_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Interior mutability via unique separation tokens (pcell_lib): every
   obligation below is really proved through Lean's [grind].  The mode
   checker enforces the linear token discipline; the negative probes
   (duplication, stale reuse, forged and cross-cell tokens) live in
   lean_pcell_fail.ml.  No intro or elim forms anywhere: tokens and
   read results bind at their skeletons with their ownership facts,
   and are passed bare to the next contract. *)

open Pcell_lib

(* Two cells' tokens live at once -- the point of the pair-returning
   [alloc].  Swap the cells' contents through interleaved reads and
   writes and prove the final sum. *)
let swap_sum : (a : int) -> (b : int) -> int{ _ = a + b } =
  fun a b ->
  let p = alloc a in
  let q = alloc b in
  let (c1, t1) = p in
  let (c2, t2) = q in
  let (r1, t1a) = read c1 a t1 in             (* r1 = a *)
  let (r2, t2a) = read c2 b t2 in             (* r2 = b *)
  let t1b = write c1 a r2 t1a in              (* cell1 := r2 *)
  let t2b = write c2 b r1 t2a in              (* cell2 := r1 *)
  let (s1, u1) = read c1 r2 t1b in            (* s1 = r2 = b *)
  let (s2, u2) = read c2 r1 t2b in            (* s2 = r1 = a *)
  ignore u1; ignore u2;
  s1 + s2

(* Token threading in a helper that never mentions the pair type: its
   solver input must stay valid (regression for prelude injection).
   The incoming token is a contract parameter, so the body holds it at
   the bare skeleton with its facts; the result components are
   introduced at the annotated pair type -- returning the new token at
   [cts _ = k + 1] rather than [cts _ = v1] is a re-proof from the
   facts [cts t2 = v1] and [v1 = r + 1] and [r = k]. *)
let bump_via : (c : icell) -> (k : int) ->
  itoken{ tid _ = cid c && cts _ = k } @ unique ->
  (int{ _ = k + 1 } * itoken{ tid _ = cid c && cts _ = k + 1 }) @ unique =
  fun c k t ->
  let (r, t1) = read c k t in
  let v1 = r + 1 in
  let t2 = write c k v1 t1 in
  (v1, t2)

(* Drive the helper from a fresh allocation: proves alloc + bump
   compose (result n + 1 from initial contents n). *)
let bump : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let p = alloc n in
  let (c, t) = p in
  let (r, u) = bump_via c n t in
  ignore u;
  r
