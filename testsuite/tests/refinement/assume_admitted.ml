(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "assume_admitted.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/assume_admitted.reference";
 check-ocamlc.byte-output;
*)

(* [assume] admits the obligation a site would have raised instead of
   proving it.  Three things are checked here, and the reference file is
   where the third one lives.

   The predicate is never written at the call.  It is read from whatever
   refined type the site imposes, so a law already stated as a result
   annotation is admitted without being restated -- which is the point,
   since two copies of a statement drift apart.

   The statement becomes a fact.  What follows may rely on it, so an
   obligation that is not provable on its own becomes provable next to an
   admission of what it needs.

   Every admission is reported, per site and counted per unit, whether or
   not anything was discharged.  That report is not a warning and cannot be
   turned off: an admission nobody has to look at defeats the only thing
   keeping an admitted proof honest. *)

type vec

external length : vec @ logical -> int @@ total = "%identity"

(* A law over an opaque total function.  There is nothing to prove it with
   and no way to execute it, and it is admitted all the same: the shapes one
   most wants to admit are exactly the ones that do not run.  The statement
   is written once, in the result annotation, and the body says only that it
   is taken on trust. *)
let (length_nonneg_law @ total) (v : vec) : unit{ length v >= 0 } =
  assume ()

(* The same, at a carrier the model does not describe. *)
let (string_law @ total) (s : string) : unit{ s = s } = assume ()

let (needs_positive @ total) (n : int{ _ > 0 }) = n

(* The admitted statement is a fact.  [y] is an arbitrary integer, so the
   annotation is not provable; admitting it discharges the call's
   precondition rather than leaving it unmet.  The proved counterpart of
   this line is in assume_obligations.ml, which shows the two side by
   side. *)
let admitted (y : int) = needs_positive (assume y : int{ _ > 0 })
