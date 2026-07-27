(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "assume_obligations.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/assume_obligations.reference";
 check-ocamlc.byte-output;
*)

(* The same annotation, admitted and proved, so the difference between them
   is a difference of obligations rather than of wording.

   Admitted, the annotation raises NO obligation and its statement arrives
   as a hypothesis of what follows.  Proved, the annotation raises its own
   obligation with nothing to discharge it -- [y] is an arbitrary integer --
   and only then supplies the same hypothesis.

   Both sites also raise the call's own contract obligation, which is what
   the hypothesis is there for.  A change that admitted the annotation but
   dropped the fact would leave that obligation with an empty hypothesis
   list, and a change that lost the admission would put the annotation
   obligation back; the two together pin the meaning down. *)

let (needs_positive @ total) (n : int{ _ > 0 }) = n

let admitted (y : int) = needs_positive (assume y : int{ _ > 0 })

let proved (y : int) = needs_positive (y : int{ _ > 0 })
