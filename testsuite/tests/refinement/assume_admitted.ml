(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "assume_admitted.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/assume_admitted.reference";
 check-ocamlc.byte-output;
*)

(* [assume] admits the obligation a site would have raised instead of
   proving it.  Four things are checked here, and the reference file is
   where the last two live.

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
   keeping an admitted proof honest.

   And each site is reported with its tier.  A predicate that can be run is
   run, so that the lemma can be refuted by running the program; one that
   cannot is admitted just the same.  Which of the two a site gets is never
   the difference between compiling and not. *)

type vec

external length : vec @ logical -> int @@ total = "%identity"

(* A law over an opaque total function.  There is nothing to PROVE it with,
   and the statement is written once, in the result annotation, with the
   body saying only that it is taken on trust.  It can still be RUN: the
   function is total, so the model's reading of it as a function is one the
   machine keeps, and the comparison is at [int].  This is the widening, and
   it is what makes a law over an abstract type falsifiable at all. *)
let (length_nonneg_law @ total) (v : vec) : unit{ length v >= 0 } =
  assume ()

(* The other tier.  Equality at [string] is not the equality the backends
   reason about, so there is nothing here whose run would tell you about the
   statement, and no check is emitted. *)
let (string_law @ total) (s : string) : unit{ s = s } = assume ()

let (needs_positive @ total) (n : int{ _ > 0 }) = n

(* The admitted statement is a fact.  [y] is an arbitrary integer, so the
   annotation is not provable; admitting it discharges the call's
   precondition rather than leaving it unmet.  The proved counterpart of
   this line is in assume_obligations.ml, which shows the two side by
   side. *)
let admitted (y : int) = needs_positive (assume y : int{ _ > 0 })
