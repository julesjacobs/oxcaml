(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml wasm_u32.ml wasm_word_memory.ml hmc_tagged_cell.ml hmc_u32_index.ml hmc_linear_bytes.ml hmc_linear_bounds.ml hmc_linear_preservation.ml";
 readonly_files = "hmc_linear_memory_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.opt-build-env; ocamlc.opt; run-expect; check-program-output; }
*)
module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation;;
[%%expect{|
module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
|}]

let changed_suffix (u : unit) = ghost_ (
  let payload = B.Byte (9, B.End) in
  let before = B.Byte (1, B.Byte (2, B.End)) in
  let after = B.Byte (9, B.Byte (3, B.End)) in
  L.overlay_def payload before after;
  L.overlay_def B.End (B.Byte (2, B.End)) (B.Byte (3, B.End));
  let proof : {u : unit | L.overlay payload before after} = refine_ () in proof);;
[%%expect{|
Line 7, characters 60-70:
7 |   let proof : {u : unit | L.overlay payload before after} = refine_ () in proof);;
                                                                ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let overlapping_read (u : unit) = ghost_ (
  let boundary : W.limb = 2 in
  let before = B.Byte (1, B.Byte (2, B.End)) in
  let after = B.Byte (1, B.Byte (9, B.End)) in
  P.equal_prefix_def boundary before after;
  P.equal_prefix_def 1 (B.Byte (2, B.End)) (B.Byte (9, B.End));
  let proof : {u : unit | P.equal_prefix boundary before after} = refine_ () in proof);;
[%%expect{|
Line 7, characters 66-76:
7 |   let proof : {u : unit | P.equal_prefix boundary before after} = refine_ () in proof);;
                                                                      ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let beyond_endpoint (u : unit) = ghost_ (
  let address : W.limb = 1 in
  L.fits_at_def B.End address B.End; L.drop_def B.End address;
  let proof : {u : unit | L.fits_at B.End address B.End} = refine_ () in proof);;
[%%expect{|
Line 4, characters 59-69:
4 |   let proof : {u : unit | L.fits_at B.End address B.End} = refine_ () in proof);;
                                                               ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrapped_extent (u : unit) = ghost_ (
  let start : W.limb = 4294967295 and stop : W.limb = 0 in
  Bounds.range_def (D.S D.Z) start stop;
  let proof : {u : unit | Bounds.range (D.S D.Z) start stop} = refine_ () in proof);;
[%%expect{|
Line 4, characters 63-73:
4 |   let proof : {u : unit | Bounds.range (D.S D.Z) start stop} = refine_ () in proof);;
                                                                   ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
