(* TEST
 expect;
*)

(* A user function whose source name resembles arithmetic remains opaque. *)
let add _ _ = 0

let false_if_add_were_builtin =
  (0 : int{ add 1 2 = 3 })
[%%expect {|
val add : 'a -> 'b -> int = <fun>
Line 4, characters 2-26:
4 |   (0 : int{ add 1 2 = 3 })
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* The resolved Stdlib primitive still receives its arithmetic meaning. *)
let real_addition =
  (3 : int{ _ = 1 + 2 })
[%%expect {|
val real_addition : int{ (app[Stdlib!.=] _ (app[Stdlib!.+] 1 2)) } = 3
|}]

