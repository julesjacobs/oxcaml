(* TEST
 has-z3;
 set base = "-opaque -principal -extension refinement_types";
 set dump = "-drawlambda -dcanonical-ids";
 set lib = "";
 readonly_files = "dfa_semantics.ml regex_semantics.ml";
 readonly_files += " dfa_equivalence_proof.ml regex_core.ml";
 readonly_files += " regex_dfa_bridge_core.ml dfa_equivalence_core.mli";
 readonly_files += " dfa_equivalence_core.ml regex_language.mli";
 readonly_files += " regex_language.ml";
 {
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   flags = "${base} ${dump}";
   compiler_output2 = "${lib}/dfa_semantics.lambda";
   module = "dfa_semantics.ml";
   ocamlc.opt;
   flags = "${base} ${dump}";
   compiler_output2 = "${lib}/regex_semantics.lambda";
   module = "regex_semantics.ml";
   ocamlc.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   compiler_output2 = "${lib}/dfa_equivalence_proof.lambda";
   module = "dfa_equivalence_proof.ml";
   ocamlc.opt;
   flags = "${base} ${dump} -open Regex_semantics";
   compiler_output2 = "${lib}/regex_core.lambda";
   module = "regex_core.ml";
   ocamlc.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof -open Regex_semantics";
   flags += " -open Regex_core";
   compiler_output2 = "${lib}/regex_dfa_bridge_core.lambda";
   module = "regex_dfa_bridge_core.ml";
   ocamlc.opt;
   flags = "${base} -open Dfa_semantics";
   module = "dfa_equivalence_core.mli";
   ocamlc.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof";
   compiler_output2 = "${lib}/dfa_equivalence_core.lambda";
   module = "dfa_equivalence_core.ml";
   ocamlc.opt;
   flags = "${base} -open Dfa_semantics -open Regex_semantics";
   module = "regex_language.mli";
   ocamlc.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof -open Regex_semantics";
   flags += " -open Regex_core -open Regex_dfa_bridge_core";
   compiler_output2 = "${lib}/regex_language.lambda";
   module = "regex_language.ml";
   ocamlc.opt;
   unset module;
   flags = "${base}";
   compiler_output2 = "${lib}/ocamlc.opt.output";
   src = "${lib}/dfa_semantics.cmi ${lib}/dfa_equivalence_core.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.dfa/";
   compiler_directory_suffix = ".dfa";
   all_modules = "dfa_public_client.ml";
   readonly_files = "dfa_public_client.ml";
   setup-ocamlc.opt-build-env;
   copy;
   compile_only = "true";
   ocamlc.opt;
   src = "${lib}/dfa_semantics.cmi ${lib}/regex_semantics.cmi";
   src += " ${lib}/dfa_equivalence_core.cmi";
   src += " ${lib}/regex_language.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
   compiler_directory_suffix = ".public";
   all_modules = "regex_public_client.ml";
   readonly_files = "regex_public_client.ml dfa_boundary.ml";
   readonly_files += " emitted_code.ml dfa_boundary_check.ml";
   setup-ocamlc.opt-build-env;
   copy;
   ocamlc.opt;
   compile_only = "false";
   all_modules = "";
   binary_modules = "${lib}/dfa_semantics ${lib}/regex_semantics";
   binary_modules += " ${lib}/dfa_equivalence_proof ${lib}/regex_core";
   binary_modules += " ${lib}/regex_dfa_bridge_core";
   binary_modules += " ${lib}/dfa_equivalence_core ${lib}/regex_language";
   binary_modules += " ${lib}.dfa/dfa_public_client regex_public_client";
   program = "${lib}.public/clients.exe";
   ocamlc.opt;
   check-ocamlc.opt-output;
   output = "${lib}.public/clients.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/dfa_boundary.clients.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml dfa_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/dfa_boundary.checks.reference";
   run;
   check-program-output;
   flags = "-principal -extension refinement_types";
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   flags = "${base} ${dump}";
   compiler_output2 = "${lib}/dfa_semantics.lambda";
   module = "dfa_semantics.ml";
   ocamlopt.opt;
   flags = "${base} ${dump}";
   compiler_output2 = "${lib}/regex_semantics.lambda";
   module = "regex_semantics.ml";
   ocamlopt.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   compiler_output2 = "${lib}/dfa_equivalence_proof.lambda";
   module = "dfa_equivalence_proof.ml";
   ocamlopt.opt;
   flags = "${base} ${dump} -open Regex_semantics";
   compiler_output2 = "${lib}/regex_core.lambda";
   module = "regex_core.ml";
   ocamlopt.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof -open Regex_semantics";
   flags += " -open Regex_core";
   compiler_output2 = "${lib}/regex_dfa_bridge_core.lambda";
   module = "regex_dfa_bridge_core.ml";
   ocamlopt.opt;
   flags = "${base} -open Dfa_semantics";
   module = "dfa_equivalence_core.mli";
   ocamlopt.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof";
   compiler_output2 = "${lib}/dfa_equivalence_core.lambda";
   module = "dfa_equivalence_core.ml";
   ocamlopt.opt;
   flags = "${base} -open Dfa_semantics -open Regex_semantics";
   module = "regex_language.mli";
   ocamlopt.opt;
   flags = "${base} ${dump} -open Dfa_semantics";
   flags += " -open Dfa_equivalence_proof -open Regex_semantics";
   flags += " -open Regex_core -open Regex_dfa_bridge_core";
   compiler_output2 = "${lib}/regex_language.lambda";
   module = "regex_language.ml";
   ocamlopt.opt;
   unset module;
   flags = "${base}";
   compiler_output2 = "${lib}/ocamlopt.opt.output";
   src = "${lib}/dfa_semantics.cmi ${lib}/dfa_equivalence_core.cmi";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.dfa/";
   compiler_directory_suffix = ".dfa";
   all_modules = "dfa_public_client.ml";
   readonly_files = "dfa_public_client.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   compile_only = "true";
   ocamlopt.opt;
   src = "${lib}/dfa_semantics.cmi ${lib}/regex_semantics.cmi";
   src += " ${lib}/dfa_equivalence_core.cmi";
   src += " ${lib}/regex_language.cmi";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.public/";
   compiler_directory_suffix = ".public";
   all_modules = "regex_public_client.ml";
   readonly_files = "regex_public_client.ml dfa_boundary.ml";
   readonly_files += " emitted_code.ml dfa_boundary_check.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   ocamlopt.opt;
   compile_only = "false";
   all_modules = "";
   binary_modules = "${lib}/dfa_semantics ${lib}/regex_semantics";
   binary_modules += " ${lib}/dfa_equivalence_proof ${lib}/regex_core";
   binary_modules += " ${lib}/regex_dfa_bridge_core";
   binary_modules += " ${lib}/dfa_equivalence_core ${lib}/regex_language";
   binary_modules += " ${lib}.dfa/dfa_public_client regex_public_client";
   program = "${lib}.public/clients.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   output = "${lib}.public/clients.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/dfa_boundary.clients.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml dfa_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/dfa_boundary.checks.reference";
   run;
   check-program-output;
 }
*)

(* The boundary of the DFA and regex demos. With each compiler, the DFA and
   regex units are compiled with -drawlambda. dfa_public_client.ml is
   compiled with only the Dfa_semantics and Dfa_equivalence_core
   interfaces, and regex_public_client.ml with those and the
   Regex_semantics and Regex_language interfaces; both are linked and run.
   dfa_boundary_check.ml checks the Lambda: [state_size] counts rows
   directly, [compare] and [reduce] do not reach the certificate-building
   functions, and the public proof functions make no calls. The phrases
   below are rejected against the public interfaces. *)

(* Each unit holds a module of the same name. *)
open Dfa_semantics
open Regex_semantics
open Regex_language
open Dfa_equivalence_core;;
[%%expect{|
|}]

(* A positive control. *)
let (same @ total) (m : Dfa_semantics.machine) (word : int list) :
    {u : unit | Dfa_semantics.run m word === Dfa_semantics.run m word} =
  let u = () in u;;
[%%expect{|
val same :
  (m : Dfa_semantics.Dfa_semantics.machine) ->
  (word : int list) ->
  {u : unit
    | (Dfa_semantics.Dfa_semantics.run m word) ===
        (Dfa_semantics.Dfa_semantics.run m word)} =
  <fun>
|}]

(* The certificate check, a definition lemma and the proof module are
   hidden. *)
let hidden = Dfa_equivalence.check_reduction;;
[%%expect{|
Line 1, characters 13-44:
1 | let hidden = Dfa_equivalence.check_reduction;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Dfa_equivalence.check_reduction"
|}]

let hidden = Dfa_equivalence.compare_def;;
[%%expect{|
Line 1, characters 13-40:
1 | let hidden = Dfa_equivalence.compare_def;;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value "Dfa_equivalence.compare_def"
|}]

module Hidden = Dfa_equivalence_proof.Dfa_proof;;
[%%expect{|
Line 1, characters 16-37:
1 | module Hidden = Dfa_equivalence_proof.Dfa_proof;;
                    ^^^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Dfa_equivalence_proof"
|}]

(* Two machines need not agree on a word. *)
let (false_equality @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (word : int list) :
    {u : unit | Dfa_semantics.run left word === Dfa_semantics.run right word} =
  let u = () in u;;
[%%expect{|
Line 4, characters 16-17:
4 |   let u = () in u;;
                    ^
Error: Refinement could not be proved (counterexample)
Line 3, characters 16-76:
3 |     {u : unit | Dfa_semantics.run left word === Dfa_semantics.run right word} =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Lowering a regex to a DFA may fail. *)
let (lowering_success @ total) (root : Regex_semantics.t) :
    {u : unit | match Regex_language.lower root with
      None -> false | Some _ -> true} =
  let u = () in u;;
[%%expect{|
Line 4, characters 16-17:
4 |   let u = () in u;;
                    ^
Error: Refinement could not be proved (counterexample)
Lines 2-3, characters 16-36:
2 | ................match Regex_language.lower root with
3 |       None -> false | Some _ -> true...
  The refinement is stated here.
|}]
