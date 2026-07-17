(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "vc_dump.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/vc_dump.reference";
 check-ocamlc.byte-output;
*)

let positive (x : int{ _ > 0 }) = x
let annotation = (3 : int{ _ >= 3 })
let contract = positive 1
let branch y = if y > 0 then positive y else 0
