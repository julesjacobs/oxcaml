(* TEST
 readonly_files = "assume_shared_ppx.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/assume_shared_ppx.exe";
 all_modules = "assume_shared_ppx.ml";
 ocamlc.byte;
 module = "assume_shared_locations.ml";
 flags = "-vox-dump-vc -c -ppx ${program}";
 compiler_output = "assume_shared_locations.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/assume_shared_locations.reference";
 check-ocamlc.byte-output;
*)

(* One [assume] must admit ONE obligation.

   This file is compiled through a mapper that gives every expression in it
   the same location record, which is what a generator does whenever it omits
   [~loc].  Two obligations then share a location, and anything that
   identified an admission by its location would admit both -- so [h] would
   be accepted with a result of [int{ _ > 100 }] on the strength of an
   admitted [int{ _ > 0 }], and [needs_big] would be reached with 5.

   What the dump has to show is the [_ > 100] obligation still being raised.
   Its absence is the failure this test exists for, and a version of this
   test written without the mapper would pass whatever the identity was:
   from ordinary source the same shape is rejected before a second mark
   exists at all. *)

let (needs_positive @ total) (n : int{ _ > 0 }) = n

let (needs_big @ total) (n : int{ _ > 100 }) = n

let h (y : int) : int{ _ > 100 } = needs_positive (assume y : int{ _ > 0 })

let () = print_int (needs_big (h 5))
