(* Fork-join for loans.  Each task is a once-closure that CONSUMES
   the loans it captures and reports through its result -- typically
   the refined unit of a prophecy resolution, which is global and
   survives the join.  Passing two closures over DISJOINT loans (the
   two halves of a split) is data-race-free by the mode discipline
   alone: each task holds exactly one half, and the same loan cannot
   be captured twice.  The closures are local (loans are local), and
   fork-join is structured: both tasks complete before the caller's
   region continues, so lending local state across the fork is
   region-sound.

   TRUSTED: the implementation runs the first task on another domain
   (degrading to sequential execution on a single-domain runtime);
   the signature is what makes that safe. *)

val fork_join2 :
  (unit -> 'a @ unique) @ once local ->
  (unit -> 'b @ unique) @ once local ->
  ('a * 'b) @ unique
