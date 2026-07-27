(* TEST
 {
   flags = "-keywords 5.3 -vox-backend z3";
   expect;
 } {
   flags = "-keywords 5.3 -vox-backend lean";
   expect;
 } {
   flags = "-keywords 5.3 -vox-backend oxsmt";
   expect;
 }
*)

(* Polymorphic comparison observes [Bigint.t]'s private runtime
   representation, not its mathematical order.  It must therefore not become
   a refinement fact.

   What this pins is the refusal, on each of the three backends.  Which
   refusal it is has already moved once -- the goal used to reach the solver
   as a term it could not take and came back [solver-error], and now it is
   taken and comes back [not-proved] -- and either way nothing is proved
   from the primitive comparison.  A change in the spelling is worth
   reading; it is not by itself a change in what this file is about. *)
let primitive_order_is_not_a_bigint_law
    (left : Bigint.t @ logical)
    (right : Bigint.t @ logical) =
  if left < right
  then ()
  else ((): unit{ Bigint.ge left right })
;;

[%%expect{|
Line 6, characters 7-41:
6 |   else ((): unit{ Bigint.ge left right })
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]
