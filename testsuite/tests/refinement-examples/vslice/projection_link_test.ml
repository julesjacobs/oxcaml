(* TEST
 readonly_files = "vslice_model.mli vslice_model.ml vslice.mli \
                   vslice_runtime_impl.ml projection_link_test.reference";
 flags = "-extension-universe alpha";
 {
   setup-ocamlc.byte-build-env;
   script = "cp vslice_runtime_impl.ml vslice.ml";
   script;
   all_modules = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml \
                  projection_link_test.ml";
   ocamlc.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   script = "cp vslice_runtime_impl.ml vslice.ml";
   script;
   all_modules = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml \
                  projection_link_test.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
*)

let () =
  let array = Vslice.make ~n:2 ~value:7 in
  assert (Vslice.contents array = [7; 7]);
  print_endline "projection-link-ok"
