(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "dependent_arrow_unrelated.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_unrelated.reference";
 check-ocamlc.byte-output;
 flags = "-principal -vox-dump-vc -c";
 compiler_output = "dependent_arrow_unrelated.principal.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_unrelated.reference";
 check-ocamlc.byte-output;
*)

let consume ~(x : int)
    ~(witness : (q:int -> unit{ q = x }) @ total) =
  ignore witness

let unrelated ~(x : int) ~(other : int)
    ~(witness : (q:int -> unit{ q = other }) @ total) =
  consume ~x ~witness
