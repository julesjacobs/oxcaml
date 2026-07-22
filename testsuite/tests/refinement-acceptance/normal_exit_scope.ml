(* TEST
 readonly_files = "normal_exit_scope_check.py";
 setup-ocamlc.byte-build-env;
 flags = "-keywords 5.3 -vox-dump-vc-json vcs.json -c";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/normal_exit_scope_check.py vcs.json";
 script;
*)

let stable_let_summary () =
  let result =
    let scope_leak_sentinel = 7 in
    scope_leak_sentinel
  in
  (result : int{ _ = 7 })
