(* TEST
 expect;
*)

let counter = ref 0
let next () =
  let n = !counter in
  incr counter;
  n

let equal_pair (x : int) (y : int{ _ = x }) = x, y

let bad = equal_pair (next ()) (next ())
[%%expect {|
val counter : int ref = {contents = 0}
val next : unit -> int = <fun>
val equal_pair : int -> int{ _ = x } -> int * int = <fun>
Line 9, characters 31-40:
9 | let bad = equal_pair (next ()) (next ())
                                   ^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let[@vox.def] pure_succ x = x + 1
let good = equal_pair (pure_succ 0) (pure_succ 0)
[%%expect {|
val pure_succ : int -> int = <fun>
val pure_succ_def : int @ total -> unit{ pure_succ x = x + 1 } = <fun>
val good : int * int = (1, 1)
|}]

(* Aliases are classified by the resolved binding, not by surface syntax. *)
let pure_alias = pure_succ
let good_alias = equal_pair (pure_alias 0) (pure_alias 0)
let impure_alias = next
let bad_alias = equal_pair (impure_alias ()) (impure_alias ())
[%%expect {|
val pure_alias : int -> int = <fun>
val good_alias : int * int = (1, 1)
val impure_alias : unit -> int = <fun>
Line 4, characters 45-62:
4 | let bad_alias = equal_pair (impure_alias ()) (impure_alias ())
                                                 ^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Qualified paths retain the identity of the resolved module member. *)
module Pure_path = struct
  let succ @ total = fun x -> x + 1
end

module Impure_path = struct
  let next = next
end

let good_path = equal_pair (Pure_path.succ 0) (Pure_path.succ 0)
let bad_path = equal_pair (Impure_path.next ()) (Impure_path.next ())
[%%expect {|
module Pure_path : sig val succ : int -> int end
module Impure_path : sig val next : unit -> int end
val good_path : int * int = (1, 1)
Line 10, characters 48-69:
10 | let bad_path = equal_pair (Impure_path.next ()) (Impure_path.next ())
                                                     ^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A total partial application remains a total callee. *)
let good_partial =
  let add @ total = fun x y -> x + y in
  let increment @ total = add 1 in
  equal_pair (increment 0) (increment 0)
[%%expect {|
val good_partial : int * int = (1, 1)
|}]

let bad_partial =
  let impure_add delta () = next () + delta in
  let impure_increment = impure_add 1 in
  equal_pair (impure_increment ()) (impure_increment ())
[%%expect {|
Line 4, characters 35-56:
4 |   equal_pair (impure_increment ()) (impure_increment ())
                                       ^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Totality also applies to supported non-identifier heads. *)
let good_lambda =
  equal_pair
    (((fun x -> x + 1) : _ @ total) 0)
    (((fun x -> x + 1) : _ @ total) 0)

let bad_lambda =
  equal_pair
    (((fun () -> next ()) : _ @ partial) ())
    (((fun () -> next ()) : _ @ partial) ())
[%%expect {|
val good_lambda : int * int = (1, 1)
Line 9, characters 4-44:
9 |     (((fun () -> next ()) : _ @ partial) ())
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]
