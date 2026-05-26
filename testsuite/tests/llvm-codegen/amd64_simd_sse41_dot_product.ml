(* TEST
 flags = "-extension layouts_alpha -extension simd_beta -extension small_numbers";
 readonly_files = "amd64_simd_sse41_dot_product.sh";
 setup-ocamlc.byte-build-env;
 script = "sh ${test_source_directory}/amd64_simd_sse41_dot_product.sh";
 check-ocamlc.byte-output;
*)
