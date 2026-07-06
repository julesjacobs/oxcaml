(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox Milestone 0 (positives).  A call that never returns normally has
   result refinement [false] (Rule 2: parametricity on the callee's
   declared scheme, or a raising primitive by name), and a sequence
   threads its LHS's result refinement to the continuation (Rule 1),
   exactly as [let () = e1 in ...] does.  Each obligation below is a
   ground fact chosen so it verifies ONLY if the intended fact reached
   it. *)

(* (1) A mid-body raise makes the continuation vacuous: the [false] it
   contributes through the sequence discharges [0 = 1]. *)
let mid_raise () : int =
  raise Not_found;
  let refine_ r = (0 : int{ _ = 1 }) in
  r
[%%expect{|
Line 2, characters 2-17:
2 |   raise Not_found;
      ^^^^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val mid_raise : unit -> int = <fun>
|}]

(* (2) A raising branch drops out of the join: the [if] collapses to its
   surviving branch, so [x] selfifies to the else value [0]. *)
let if_raise (b : bool) : int =
  let x = if b then raise Not_found else 0 in
  let refine_ r = (x : int{ _ = 0 }) in
  r
[%%expect{|
val if_raise : bool -> int = <fun>
|}]

(* (3) A sequence threads a non-raise postcondition: [ensure_pos x]'s
   result refinement [x > 0] survives the [;]. *)
let ensure_pos (x : int) : unit{ x > 0 } = assume_unchecked_ ()
[%%expect{|
val ensure_pos : (x : int) -> unit{ x > 0 } = <fun>
|}]

let use_post (x : int) : int =
  ensure_pos x;
  let refine_ r = (x : int{ _ > 0 }) in
  r
[%%expect{|
Line 2, characters 2-14:
2 |   ensure_pos x;
      ^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

val use_post : int -> int = <fun>
|}]

(* (4) A user [let rec loop () : 'a] is classified bottom with no
   annotation: its scheme result variable occurs in no argument. *)
let rec loop () : 'a = loop ()
[%%expect{|
val loop : unit -> 'a = <fun>
|}]

let after_loop () : int =
  let _ = loop () in
  let refine_ r = (0 : int{ _ = 1 }) in
  r
[%%expect{|
val after_loop : unit -> int = <fun>
|}]

(* (5) [failwith] is classified bottom through its scheme [string -> 'a]
   (an ordinary value, caught by the scheme branch -- not a primitive). *)
let after_failwith () : int =
  failwith "boom";
  let refine_ r = (0 : int{ _ = 1 }) in
  r
[%%expect{|
Line 2, characters 2-17:
2 |   failwith "boom";
      ^^^^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val after_failwith : unit -> int = <fun>
|}]

(* (8) Tail-position raise is unchanged: it produces no obligation. *)
let tail_raise () : int{ _ = 0 } = raise Not_found
[%%expect{|
val tail_raise : unit -> int{ _ = 0 } = <fun>
|}]
