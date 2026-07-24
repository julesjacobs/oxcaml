(* TEST
 readonly_files = "sequence_result_span_check.py";
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc-json vcs.json -c";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/sequence_result_span_check.py vcs.json";
 script;
*)

let returned_leaf () : unit{ true } =
  print_int 0;
  ignore 0;
  ()

let requires_true (_ : unit{ true }) = ()

let nested_let_site () =
  requires_true
    (let value = 0 in
     print_int value;
     ())

let nested_open_site () =
  requires_true
    (let open Stdlib in
     print_int 0;
     ())

let application_site () : unit{ true } =
  Fun.id
    (let value = 0 in
     print_int value;
     ())
