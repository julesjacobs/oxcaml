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
   a refinement fact. *)
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
Error: Refinement verification failed (solver-error)
|}]
