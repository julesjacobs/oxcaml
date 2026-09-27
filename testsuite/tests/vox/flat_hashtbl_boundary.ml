(* TEST
 has-z3;
 ocamlrunparam += ",l=262144";
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_table_model.ml";
 all_modules += " vox_table_model_proofs.ml vox_table_bits.ml";
 all_modules += " vox_table_probe.ml vox_table_wrap.ml vox_table_mask.ml";
 all_modules += " vox_table_map.ml vox_table_invariant.ml";
 all_modules += " vox_table_initial.ml vox_table_update_proofs.ml";
 all_modules += " vox_table_insert_proofs.ml vox_table_migration_proofs.ml";
 all_modules += " vox_table_read_proofs.ml vox_table_search_spec.ml";
 all_modules += " vox_table_stop_proof.ml pref.mli pref.ml ghost_pref.mli";
 all_modules += " ghost_pref.ml vox_table_storage.mli vox_table_storage.ml";
 all_modules += " vox_table_search.ml vox_table_mutation.ml";
 all_modules += " vox_table_coverage.ml vox_table_occupancy.ml";
 all_modules += " vox_table_vacancy_progress.ml vox_table_vacancy.ml";
 all_modules += " vox_table_insert.ml vox_table_migrate.ml";
 all_modules += " vox_table_resize.ml vox_table_implementation.ml";
 all_modules += " vox_table_bindings.ml vox_table_bindings_bridge.ml";
 all_modules += " vox_verified_flat_hashtbl.mli";
 all_modules += " vox_verified_flat_hashtbl.ml";
 set lib = "";
 set here = "${test_source_directory}";
 {
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   compile_only = "true";
   ocamlc.opt;
   src = "${lib}/pref.cmi ${lib}/ghost_pref.cmi";
   src += " ${lib}/vox_verified_flat_hashtbl.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
   compiler_directory_suffix = ".public";
   readonly_files = "flat_hashtbl_public.ml flat_hashtbl_boundary.ml";
   readonly_files += " emitted_code.ml flat_hashtbl_boundary_check.ml";
   readonly_files += " flat_hashtbl_stale.ml flat_hashtbl_unowned.ml";
   readonly_files += " flat_hashtbl_reused.ml";
   setup-ocamlc.opt-build-env;
   copy;
   flags += " -dlambda";
   compiler_output2 = "${lib}.public/client.lambda";
   all_modules = "flat_hashtbl_public.ml";
   ocamlc.opt;
   flags = "-extension refinement_types";
   compiler_output2 = "${lib}.public/link.output";
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_table_model";
   binary_modules += " ${lib}/vox_table_model_proofs ${lib}/vox_table_bits";
   binary_modules += " ${lib}/vox_table_probe ${lib}/vox_table_wrap";
   binary_modules += " ${lib}/vox_table_mask ${lib}/vox_table_map";
   binary_modules += " ${lib}/vox_table_invariant ${lib}/vox_table_initial";
   binary_modules += " ${lib}/vox_table_update_proofs";
   binary_modules += " ${lib}/vox_table_insert_proofs";
   binary_modules += " ${lib}/vox_table_migration_proofs";
   binary_modules += " ${lib}/vox_table_read_proofs";
   binary_modules += " ${lib}/vox_table_search_spec";
   binary_modules += " ${lib}/vox_table_stop_proof";
   binary_modules += " ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/vox_table_storage ${lib}/vox_table_search";
   binary_modules += " ${lib}/vox_table_mutation ${lib}/vox_table_coverage";
   binary_modules += " ${lib}/vox_table_occupancy";
   binary_modules += " ${lib}/vox_table_vacancy_progress";
   binary_modules += " ${lib}/vox_table_vacancy ${lib}/vox_table_insert";
   binary_modules += " ${lib}/vox_table_migrate ${lib}/vox_table_resize";
   binary_modules += " ${lib}/vox_table_implementation";
   binary_modules += " ${lib}/vox_table_bindings";
   binary_modules += " ${lib}/vox_table_bindings_bridge";
   binary_modules += " ${lib}/vox_verified_flat_hashtbl";
   all_modules = "flat_hashtbl_public.ml";
   program = "${lib}.public/client.exe";
   ocamlc.opt;
   check-ocamlc.opt-output;
   output = "${lib}.public/client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/flat_hashtbl_boundary.client.reference";
   run;
   check-program-output;
   unset binary_modules;
   compile_only = "true";
   flags = "";
   compiler_output2 = "${lib}.public/stale.output";
   compiler_reference2 = "${here}/flat_hashtbl_stale.compilers.reference";
   all_modules = "flat_hashtbl_stale.ml";
   ocamlc_opt_exit_status = "2";
   ocamlc.opt;
   check-ocamlc.opt-output;
   compiler_output2 = "${lib}.public/missing_ownership.output";
   compiler_reference2 = "${here}/flat_hashtbl_unowned.compilers.reference";
   all_modules = "flat_hashtbl_unowned.ml";
   ocamlc_opt_exit_status = "2";
   ocamlc.opt;
   check-ocamlc.opt-output;
   compiler_output2 = "${lib}.public/reused_token.output";
   compiler_reference2 = "${here}/flat_hashtbl_reused.compilers.reference";
   all_modules = "flat_hashtbl_reused.ml";
   ocamlc_opt_exit_status = "2";
   ocamlc.opt;
   check-ocamlc.opt-output;
   ocamlc_opt_exit_status = "0";
   compile_only = "false";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml flat_hashtbl_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "byte ${lib}.public";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/flat_hashtbl_boundary.checks-byte.reference";
   run;
   check-program-output;
   flags = "-extension refinement_types";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_table_model";
   binary_modules += " ${lib}/vox_table_model_proofs ${lib}/vox_table_bits";
   binary_modules += " ${lib}/vox_table_probe ${lib}/vox_table_wrap";
   binary_modules += " ${lib}/vox_table_mask ${lib}/vox_table_map";
   binary_modules += " ${lib}/vox_table_invariant ${lib}/vox_table_initial";
   binary_modules += " ${lib}/vox_table_update_proofs";
   binary_modules += " ${lib}/vox_table_insert_proofs";
   binary_modules += " ${lib}/vox_table_migration_proofs";
   binary_modules += " ${lib}/vox_table_read_proofs";
   binary_modules += " ${lib}/vox_table_search_spec";
   binary_modules += " ${lib}/vox_table_stop_proof";
   binary_modules += " ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/vox_table_storage ${lib}/vox_table_search";
   binary_modules += " ${lib}/vox_table_mutation ${lib}/vox_table_coverage";
   binary_modules += " ${lib}/vox_table_occupancy";
   binary_modules += " ${lib}/vox_table_vacancy_progress";
   binary_modules += " ${lib}/vox_table_vacancy ${lib}/vox_table_insert";
   binary_modules += " ${lib}/vox_table_migrate ${lib}/vox_table_resize";
   binary_modules += " ${lib}/vox_table_implementation";
   binary_modules += " ${lib}/vox_table_bindings";
   binary_modules += " ${lib}/vox_table_bindings_bridge";
   binary_modules += " ${lib}/vox_verified_flat_hashtbl flat_hashtbl_public";
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   compile_only = "true";
   ocamlopt.opt;
   src = "${lib}/pref.cmi ${lib}/ghost_pref.cmi";
   src += " ${lib}/vox_verified_flat_hashtbl.cmi";
   src += " ${lib}/vox_sequence.cmx ${lib}/vox_table_model.cmx";
   src += " ${lib}/vox_table_model_proofs.cmx";
   src += " ${lib}/vox_table_bits.cmx ${lib}/vox_table_probe.cmx";
   src += " ${lib}/vox_table_wrap.cmx ${lib}/vox_table_mask.cmx";
   src += " ${lib}/vox_table_map.cmx ${lib}/vox_table_invariant.cmx";
   src += " ${lib}/vox_table_initial.cmx";
   src += " ${lib}/vox_table_update_proofs.cmx";
   src += " ${lib}/vox_table_insert_proofs.cmx";
   src += " ${lib}/vox_table_migration_proofs.cmx";
   src += " ${lib}/vox_table_read_proofs.cmx";
   src += " ${lib}/vox_table_search_spec.cmx";
   src += " ${lib}/vox_table_stop_proof.cmx ${lib}/pref.cmx";
   src += " ${lib}/ghost_pref.cmx ${lib}/vox_table_storage.cmx";
   src += " ${lib}/vox_table_search.cmx ${lib}/vox_table_mutation.cmx";
   src += " ${lib}/vox_table_coverage.cmx";
   src += " ${lib}/vox_table_occupancy.cmx";
   src += " ${lib}/vox_table_vacancy_progress.cmx";
   src += " ${lib}/vox_table_vacancy.cmx ${lib}/vox_table_insert.cmx";
   src += " ${lib}/vox_table_migrate.cmx ${lib}/vox_table_resize.cmx";
   src += " ${lib}/vox_table_implementation.cmx";
   src += " ${lib}/vox_table_bindings.cmx";
   src += " ${lib}/vox_table_bindings_bridge.cmx";
   src += " ${lib}/vox_verified_flat_hashtbl.cmx";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.public/";
   compiler_directory_suffix = ".public";
   readonly_files = "flat_hashtbl_public.ml flat_hashtbl_boundary.ml";
   readonly_files += " emitted_code.ml flat_hashtbl_boundary_check.ml";
   readonly_files += " flat_hashtbl_stale.ml flat_hashtbl_unowned.ml";
   readonly_files += " flat_hashtbl_reused.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   flags += " -dlambda";
   compiler_output2 = "${lib}.public/client.lambda";
   all_modules = "flat_hashtbl_public.ml";
   ocamlopt.opt;
   flags = "-extension refinement_types -dcmm";
   compiler_output2 = "${lib}.public/client.cmm";
   ocamlopt.opt;
   flags += " -O3";
   compiler_output2 = "${lib}.public/client-O3.cmm";
   ocamlopt.opt;
   flags = "-extension refinement_types";
   compiler_output2 = "${lib}.public/link.output";
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_table_model";
   binary_modules += " ${lib}/vox_table_model_proofs ${lib}/vox_table_bits";
   binary_modules += " ${lib}/vox_table_probe ${lib}/vox_table_wrap";
   binary_modules += " ${lib}/vox_table_mask ${lib}/vox_table_map";
   binary_modules += " ${lib}/vox_table_invariant ${lib}/vox_table_initial";
   binary_modules += " ${lib}/vox_table_update_proofs";
   binary_modules += " ${lib}/vox_table_insert_proofs";
   binary_modules += " ${lib}/vox_table_migration_proofs";
   binary_modules += " ${lib}/vox_table_read_proofs";
   binary_modules += " ${lib}/vox_table_search_spec";
   binary_modules += " ${lib}/vox_table_stop_proof";
   binary_modules += " ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/vox_table_storage ${lib}/vox_table_search";
   binary_modules += " ${lib}/vox_table_mutation ${lib}/vox_table_coverage";
   binary_modules += " ${lib}/vox_table_occupancy";
   binary_modules += " ${lib}/vox_table_vacancy_progress";
   binary_modules += " ${lib}/vox_table_vacancy ${lib}/vox_table_insert";
   binary_modules += " ${lib}/vox_table_migrate ${lib}/vox_table_resize";
   binary_modules += " ${lib}/vox_table_implementation";
   binary_modules += " ${lib}/vox_table_bindings";
   binary_modules += " ${lib}/vox_table_bindings_bridge";
   binary_modules += " ${lib}/vox_verified_flat_hashtbl";
   all_modules = "flat_hashtbl_public.ml";
   program = "${lib}.public/client.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   output = "${lib}.public/client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/flat_hashtbl_boundary.client.reference";
   run;
   check-program-output;
   unset binary_modules;
   compile_only = "true";
   flags = "";
   compiler_output2 = "${lib}.public/stale.output";
   compiler_reference2 = "${here}/flat_hashtbl_stale.compilers.reference";
   all_modules = "flat_hashtbl_stale.ml";
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   compiler_output2 = "${lib}.public/missing_ownership.output";
   compiler_reference2 = "${here}/flat_hashtbl_unowned.compilers.reference";
   all_modules = "flat_hashtbl_unowned.ml";
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   compiler_output2 = "${lib}.public/reused_token.output";
   compiler_reference2 = "${here}/flat_hashtbl_reused.compilers.reference";
   all_modules = "flat_hashtbl_reused.ml";
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   ocamlopt_opt_exit_status = "0";
   compiler_directory_suffix = ".vacancy";
   readonly_files = "vox_table_vacancy.ml emitted_code.ml";
   readonly_files += " flat_hashtbl_boundary_check.ml";
   setup-ocamlopt.opt-build-env;
   flags = "-extension refinement_types -I ${lib} -dcmm";
   compiler_output2 = "${lib}.public/vacancy.cmm";
   all_modules = "vox_table_vacancy.ml";
   ocamlopt.opt;
   flags = "";
   compile_only = "false";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml flat_hashtbl_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "native ${lib}.public";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/flat_hashtbl_boundary.checks-native.reference";
   run;
   check-program-output;
 }
*)

(* The flat hash table's boundary. With each compiler, the library is
   compiled and flat_hashtbl_public.ml is compiled with only the Pref,
   Ghost_pref and Vox_verified_flat_hashtbl interfaces, linked and run.
   flat_hashtbl_boundary_check.ml checks the client's Lambda (and, natively,
   its Cmm at the default level and at -O3, and the Cmm of the vacancy scan)
   for proof code. Three misuses are compiled without the refinement
   extension and must still be rejected. The phrases below are rejected
   against the public interfaces and the client. *)

(* A positive control. *)
let length_after_replace () =
  let module V = Flat_hashtbl_public.V in
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let u = V.replace r.#table r.#view 1 84 r.#token in
  V.length r.#table u.#view (borrow_ u.#token);;
[%%expect{|
val length_after_replace : unit -> int = <fun>
|}]

(* A key type whose equality is not reflexive, and one whose hash does not
   respect equality, are refused by the laws Key requires. *)
module False_reflexivity = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = false
  let (reflexive @ total) (x : int) : {u : unit | equal x x} =
    equal_def x x; ()
end;;
[%%expect{|
Line 5, characters 19-21:
5 |     equal_def x x; ()
                       ^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 4, characters 50-59:
4 |   let (reflexive @ total) (x : int) : {u : unit | equal x x} =
                                                      ^^^^^^^^^
  The refinement is stated here.
|}]

module Inconsistent_hash = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = true
  let[@def] (hash @ total) (x : int) = x
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    equal_def x y; hash_def x; hash_def y; ()
end;;
[%%expect{|
Line 7, characters 43-45:
7 |     equal_def x y; hash_def x; hash_def y; ()
                                               ^^
Error: Refinement could not be proved (counterexample: x = 0, y = -1)
Line 6, characters 18-52:
6 |       {u : unit | not (equal x y) || hash x = hash y} =
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module V = Flat_hashtbl_public.V;;
[%%expect{|
module V = Flat_hashtbl_public.V
|}]

(* The map is abstract, and the implementation is hidden. *)
let f : int V.Map.t = [];;
[%%expect{|
Line 1, characters 22-24:
1 | let f : int V.Map.t = [];;
                          ^^
Error: The constructor "[]" has type "'a list"
       but an expression was expected of type
         "int V.Map.t" =
           "int Vox_verified_flat_hashtbl.Make(Flat_hashtbl_public.Key).Map.t"
|}]

let f = V.Map.Assoc.lookup;;
[%%expect{|
Line 1, characters 8-19:
1 | let f = V.Map.Assoc.lookup;;
            ^^^^^^^^^^^
Error: Unbound module "V.Map.Assoc"
|}]

let f = V.Bridge.compact;;
[%%expect{|
Line 1, characters 8-16:
1 | let f = V.Bridge.compact;;
            ^^^^^^^^
Error: Unbound module "V.Bridge"
|}]

module Hidden = V.Spec;;
[%%expect{|
Line 1, characters 16-22:
1 | module Hidden = V.Spec;;
                    ^^^^^^
Error: Unbound module "V.Spec"
|}]

module Hidden = V.Impl;;
[%%expect{|
Line 1, characters 16-22:
1 | module Hidden = V.Impl;;
                    ^^^^^^
Error: Unbound module "V.Impl"
|}]

let f = V.Map.empty_same;;
[%%expect{|
Line 1, characters 8-24:
1 | let f = V.Map.empty_same;;
            ^^^^^^^^^^^^^^^^
Error: Unbound value "V.Map.empty_same"
|}]

let f (v : int V.view) = v.storage;;
[%%expect{|
Line 1, characters 27-34:
1 | let f (v : int V.view) = v.storage;;
                               ^^^^^^^
Error: Unbound record field "storage"
|}]

(* Ownership: a stale view, a lookup without ownership and a reused
   token. *)
let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.#table r.#view 1 84 r.#token in
  V.find_opt r.#table r.#view 1 (borrow_ changed.#token);;
[%%expect{|
Line 4, characters 41-55:
4 |   V.find_opt r.#table r.#view 1 (borrow_ changed.#token);;
                                             ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_verified_flat_hashtbl.mli", line 146, characters 6-61:
  The refinement is stated here.
|}]

let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let empty = Ghost_pref.empty () in
  V.find_opt r.#table r.#view 1 (borrow_ empty);;
[%%expect{|
Line 4, characters 41-46:
4 |   V.find_opt r.#table r.#view 1 (borrow_ empty);;
                                             ^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_verified_flat_hashtbl.mli", line 146, characters 6-61:
  The refinement is stated here.
|}]

let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.#table r.#view 1 84 r.#token in
  V.replace r.#table changed.#view 2 90 r.#token;;
[%%expect{|
Line 4, characters 40-48:
4 |   V.replace r.#table changed.#view 2 90 r.#token;;
                                            ^^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 48-56:
3 |   let changed = V.replace r.#table r.#view 1 84 r.#token in
                                                    ^^^^^^^^

|}]

(* A false claim about a lookup: 84 is stored, 85 is claimed. *)
let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.#table r.#view 1 84 r.#token in
  let value : {v : int | v = 85} =
    V.find r.#table changed.#view 1 (borrow_ changed.#token) in value;;
[%%expect{|
Line 5, characters 4-60:
5 |     V.find r.#table changed.#view 1 (borrow_ changed.#token) in value;;
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 25-31:
4 |   let value : {v : int | v = 85} =
                             ^^^^^^
  The refinement is stated here.
|}]
