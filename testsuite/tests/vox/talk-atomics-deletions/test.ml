(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml";
 readonly_files = "run.sh spawn_client.ml stale_read.ml dup.ml diverge.ml confuse.ml";
 arguments = "${test_source_directory}/run.sh ${ocamlc_opt} ${ocamlsrcdir}/stdlib ${ocamlrun} ${test_source_directory}/../../../../verification/library";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run;
   check-program-output;
 }
*)

(* Talk, section 4c ("atomic invariants: what keeps them sound"), the
   restriction table: delete one restriction of Verified_atomic (or of
   Unique_cell.Slot.take) and see what breaks.

   The weakened interfaces are WEAKENED COPIES, never the library. run.sh
   derives each one from the library's current source by a single textual
   substitution, prints the lines it changed (so the reference records
   exactly what was deleted), compiles the client against the real
   interface and against the copy, and runs some of the weakened builds.
   Deriving the copies, instead of storing them, keeps each one equal to
   today's interface minus one restriction; if the interface changes so
   that a substitution no longer applies, the change counts in the
   reference show it.

   Rows (test.reference):
   1. [@ local contended] on the handle: nothing breaks; the two-domain
      client compiles either way.
   2. [@@ portable] on the operations: the two-domain client no longer
      compiles ("The value bump is nonportable").
   3. [@ unique] on the caller token: a read after release is a uniqueness
      error; with the copy it compiles and prints
      "refinement {v | v = 5} violated: read 6". The investigation ran this
      with two domains (a multidomain build); here one domain runs the same
      interleaving deterministically.
   4. [@ unique] on the transition results: returning the invariant's token
      twice is a uniqueness error; with the copy it compiles.
   5. [@ total] on the transition: a diverging transition is rejected; with
      the copy the lock "acquires" although its compare-and-set failed.
   6. Slot.take's ghost premise: taking from an empty slot is a refinement
      error; with the copy it compiles, and [()] is returned as a [string]:
      the program crashes. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
