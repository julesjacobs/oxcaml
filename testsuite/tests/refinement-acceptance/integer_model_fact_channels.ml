(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: runtime observations of a machine integer   *)
(*                                                                *)
(* Each case observes one simple fact and then asks for a         *)
(* consequence that holds only of a 63-bit machine integer, so a  *)
(* channel that carried the observation under some other integer  *)
(* meaning would fail here.  Asking back for the proposition just *)
(* observed would pass whatever the encoding said, which is why   *)
(* the requested consequence is always a different formula.       *)
(* ============================================================= *)

(* A branch condition. *)
let branch (x : int) =
  if x = max_int then ignore (() : unit{ x + 1 = min_int })
[%%expect {|
val branch : int -> unit = <fun>
|}]

(* A match guard. *)
let guard (x : int) =
  match x with
  | _ when x = max_int -> ignore (() : unit{ x + 1 = min_int })
  | _ -> ()
[%%expect {|
val guard : int -> unit = <fun>
|}]

(* An ordinary assertion. *)
let asserted (x : int) =
  assert (x = max_int);
  ignore (() : unit{ x + 1 = min_int })
[%%expect {|
val asserted : int -> unit = <fun>
|}]

(* A loop condition, which holds throughout the body. *)
let looped (x : int) =
  while x = max_int do
    ignore (() : unit{ x + 1 = min_int })
  done
[%%expect {|
val looped : int -> unit = <fun>
|}]

(* A refined parameter. *)
let parameter (x : int{ _ = max_int }) = (x + 1 : int{ _ = min_int })
[%%expect {|
val parameter : int{ _ = max_int } -> int{ _ = min_int } = <fun>
|}]

(* Each channel is load-bearing: without the observation the same
   consequence does not follow. *)
let unobserved (x : int) = ignore (() : unit{ x + 1 = min_int })
[%%expect {|
Line 1, characters 34-64:
1 | let unobserved (x : int) = ignore (() : unit{ x + 1 = min_int })
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* And the observation does not license a consequence that is false of a
   machine integer.  At the maximum the successor wraps, so it is not
   above its predecessor. *)
let branch_successor_grows (x : int) =
  if x = max_int then ignore (() : unit{ x + 1 > x })
[%%expect {|
Line 2, characters 29-53:
2 |   if x = max_int then ignore (() : unit{ x + 1 > x })
                                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A guard's observation stays inside its own arm. *)
let guard_other_arm (x : int) =
  match x with
  | _ when x = max_int -> ()
  | _ -> ignore (() : unit{ x + 1 = min_int })
[%%expect {|
Line 4, characters 16-46:
4 |   | _ -> ignore (() : unit{ x + 1 = min_int })
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]
