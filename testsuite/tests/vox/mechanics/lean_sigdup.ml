(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "dupspec_a.mli dupspec_a.ml dupspec_b.mli dupspec_b.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Two imported interfaces export the SAME spec-function name with
   different meanings.  Splicing both preludes must fail (duplicate
   definition), attributed to the colliding unit's block -- silently
   preferring one interpretation would be unsound.  Spec functions are
   not yet unit-namespaced; this pins the fails-closed behavior.  (The
   obligation applies a spec function; prelude text only reaches
   solver inputs that use one.) *)

let _ = Dupspec_b.v

let a : Dupspec_a.t{ dup_spec _ = 0 } = Dupspec_a.A
