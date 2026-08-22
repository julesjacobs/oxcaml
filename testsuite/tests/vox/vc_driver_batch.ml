(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-backend nonsense";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The batch route: every other -vox-backend fixture is an expect test and
   exercises only the Topcommon hook, so this one drives the
   Compile_common hook with a real ocamlc action.  The unit is unrefined —
   zero obligations — and still fails at selection: the plan is validated
   before any obligation is consulted, and the error reports at the unit's
   file-level location (no fabricated [_none_] header). *)

let f x = x
