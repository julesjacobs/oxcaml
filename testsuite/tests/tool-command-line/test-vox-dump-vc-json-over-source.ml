(* TEST
 setup-ocamlc.byte-build-env;
 script = "cp ${test_source_directory}/test-vox-dump-vc-json-over-source.ml victim.ml";
 script;
 all_modules = "victim.ml";
 compile_only = "true";
 flags = "-vox-dump-vc-json victim.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
 script = "grep -q 'this file must survive' victim.ml";
 script;
*)

(*
  -vox-dump-vc-json takes a destination and truncates it. Every neighbouring
  vox flag takes no argument, so the source is easy to write where the
  destination belongs, and the source is then destroyed with no diagnostic.

  The refusal is only half the fix, and the missing half is the whole point.
  The dump is written from an at_exit handler, so a guard that refuses and then
  exits runs that handler on the way out, opens the destination and truncates
  it -- printing the refusal and destroying the file anyway. That is what the
  first version of this guard did, in every invocation shape including with no
  source file at all, and it destroyed an earlier version of THIS test through
  the hard link the harness keeps to its sources.

  So the test asserts both halves: the refusal is printed, AND the destination
  still has its contents afterwards. The second script action is the one that
  matters; without it this test passed against a compiler that ate the file.

  The destination is a copy rather than this file itself, so that a regression
  cannot destroy the test that detects it.
*)

let () = print_string "this file must survive being named as a dump destination\n"
