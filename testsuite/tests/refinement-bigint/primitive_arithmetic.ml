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

(* A low-level machine-integer primitive can be given a misleading external
   type.  Its result must not receive the mathematical semantics of
   [Bigint.add]. *)
external primitive_add
  : Bigint.t -> Bigint.t -> Bigint.t @@ total
  = "%addint"

let primitive_add_is_not_bigint_add
    (left : Bigint.t @ logical)
    (right : Bigint.t @ logical) =
  if Bigint.equal (primitive_add left right) (Bigint.add left right)
  then ()
  else ((): unit{ false })
;;

[%%expect{|
external primitive_add : Bigint.t -> Bigint.t -> Bigint.t = "%addint"
Line 10, characters 7-26:
10 |   else ((): unit{ false })
            ^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (solver-error)
|}]
