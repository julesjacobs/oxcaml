(* TEST
 flags = "-vox-backend lean";
 expect;
*)

(* A user function whose source name resembles arithmetic is an ordinary
   (partial) function, not one of the comparison/arithmetic primitives admitted
   inside a predicate.  A predicate is checked at [total], so calling [add] is
   rejected at totality (before any verification obligation), where previously
   the stub let the opaque call through to an unprovable obligation. *)
let add _ _ = 0

let false_if_add_were_builtin =
  (0 : int{ add 1 2 = 3 })
[%%expect {|
val add : 'a -> 'b -> int = <fun>
Line 4, characters 12-15:
4 |   (0 : int{ add 1 2 = 3 })
                ^^^
Error: The value "add" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 4, characters 7-25).
|}]

(* The resolved Stdlib primitive still receives its arithmetic meaning. *)
let real_addition =
  (3 : int{ _ = 1 + 2 })
[%%expect {|
val real_addition : int{ _ = 1 + 2 } = 3
|}]

