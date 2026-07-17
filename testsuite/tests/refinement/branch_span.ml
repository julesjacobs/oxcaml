(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "branch_span.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/branch_span.reference";
 check-ocamlc.byte-output;
*)

(* A marked [if] splits its result obligation across the two branches.  Each
   branch obligation must carry a TIGHT location covering only its own branch
   expression (the then-expression span vs the else-expression span), not the
   shared whole-[if]/annotation span -- otherwise a caret in one branch would
   surface the other branch's obligation in the IDE pane. *)
let clamp (n : int) = (if n > 0 then n else 100 : int{ _ >= 0 })
