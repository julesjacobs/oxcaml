(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/Vint.mli ../lib/Vint.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: a client of the vox_stdlib [Vint] module (the graduation of
   demo/reflectbits -- min / max / abs, reflected, with the full bound /
   cases algebra proved as theorems, never assumed).  [lib/Vint.{mli,ml}]
   is a pinned snapshot of [vox_stdlib/Vint.{mli,ml}] -- the stdlib lives
   outside the testsuite tree, so a demo consumes it by snapshotting the
   module (and its transitive deps, in dependency order) into ../lib and
   listing them in the [modules] stanza; the harness compiles them in
   sequence so each .mli/.ml pair yields the .cmi + VoxSig_*.olean the
   next consumes.  Vint is a LEAF (no deps), so a single pair suffices.
   Every law below arrives through Vint's .cmi / VoxSig with no local
   block and no prelude flag. *)

open Vint

(* Reflected calls are definitionally exact. *)
let m : int{ _ = vi_min 3 5 } = imin 3 5
let x : int{ _ = vi_max 3 5 } = imax 3 5
let a : int{ _ = vi_abs (-4) } = iabs (-4)

(* The shipped bound laws fire ambiently (grind_pattern), no lemma call. *)
let lo (p : int) (q : int) : int{ _ <= p } = imin p q      (* vi_min_le_left *)
let hi (p : int) (q : int) : int{ p <= _ } = imax p q      (* vi_max_ge_left *)
let nn (p : int) : int{ 0 <= _ } = iabs p                  (* vi_abs_nonneg *)

(* Clamp: the header's selling point.  [imax lo (imin hi x)] lands in
   [lo, hi] -- the COMBINED bound is discharged by the min/max CASES laws
   (vi_max_cases / vi_min_le_left), so no dedicated [clamp] primitive is
   needed.  This is the op an index-clamp (cf. lean_binsearch) consumes. *)
let clamp (lo : int) (hi : int{ lo <= _ }) (v : int) : int{ lo <= _ && _ <= hi } =
  imax lo (imin hi v)
