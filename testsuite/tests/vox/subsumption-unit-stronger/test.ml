(* TEST
 has-z3;
 readonly_files = "stronger.mli stronger.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "stronger.mli";
 ocamlc.byte;
 module = "stronger.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
 file = "stronger.cmo";
 file-exists;
*)

(* [stage 1 + 3] A compilation unit whose implementation is stronger than its
   interface and which has no proofs other than the inclusion obligations
   (all of stronger.ml is trusted external declarations).  Vox_vc.generate
   must start the solver for the interface alone.
   Intended: compiles silently (test.compilers.reference is empty) and
   produces stronger.cmo.
   Currently: rejected at typing,
     File "stronger.ml", line 1:
     Error: The implementation stronger.ml
            does not match the interface stronger.cmi:
            Values do not match:
              external f : (x : int) -> {r : int | r = x} = "%identity"
            is not included in
              val f : (x : int) -> {r : int | r >= x}
            ...
            Type {r : int | r = x} is not compatible with type {r : int | r >= x} *)
