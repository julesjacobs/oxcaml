(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — Family 1 (crossing abuse: erasing an arrow from a type's surface
   to escape the (Boundary) capture rule).

   (Boundary): a total closure's arrow-typed CAPTURES must be total. Types that cross
   totality (spec: "types containing no arrows cross") are exempt. So if a partial or
   diverging function can be hidden inside a value whose type has no surface arrow,
   capturing it into a total closure would not be constrained, and — since there is NO
   application-site rule — unpacking and applying it inside total code would run a
   partial/diverging function in total code.

   These MUST be rejected (either the capture is constrained, or the crossing correctly
   inherits non-crossing from the function-typed field). Acceptance of the
   divergence-carrying ones is a soundness finding. *)

let expects_total (f @ total) = f

(* B1: GADT existential hides the arrow. Pack a DIVERGING function, capture the pack in a
   total closure, unpack and apply -> a total value that diverges. *)
type packed = Pack : (unit -> unit) -> packed

let diverge () =
  while true do
    ()
  done
;;

let p = Pack diverge

let run () =
  match p with
  | Pack f -> f ()
;;

let escaped = expects_total run

[%%expect
  {|
val expects_total : 'a @ total -> 'a = <fun>
type packed = Pack : (unit -> unit) -> packed
val diverge : unit -> 'a = <fun>
val p : packed = Pack <fun>
val run : unit -> unit = <fun>
Line 20, characters 28-31:
20 | let escaped = expects_total run
                                 ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* B2: same, but force totality directly with an annotation on [run]. *)
let (run2 @ total) () =
  match p with
  | Pack f -> f ()
;;

[%%expect
  {|
Line 2, characters 8-9:
2 |   match p with
            ^
Error: The value "p" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 19-18
         which is expected to be "total".
|}]

(* B3: record with a function field (no surface arrow on the record type). *)
type box = { call : unit -> unit }

let b =
  { call =
      (fun () ->
        while true do
          ()
        done)
  }
;;

let (run3 @ total) () = b.call ()

[%%expect
  {|
type box = { call : unit -> unit; }
val b : box = {call = <fun>}
Line 12, characters 24-25:
12 | let (run3 @ total) () = b.call ()
                             ^
Error: The value "b" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 12, characters 19-33
         which is expected to be "total".
|}]

(* B4: existential pack of a partial (effectful) function, applied in total. *)
type packed_int = PackI : (unit -> int) -> packed_int

let cell = ref 0

let eff () =
  incr cell;
  !cell
;;

let pe = PackI eff

let (run4 @ total) () =
  match pe with
  | PackI f -> f ()
;;

[%%expect
  {|
type packed_int = PackI : (unit -> int) -> packed_int
val cell : int ref = {contents = 0}
val eff : unit -> int = <fun>
val pe : packed_int = PackI <fun>
Line 13, characters 8-10:
13 |   match pe with
             ^^
Error: The value "pe" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 12-14, characters 19-19
         which is expected to be "total".
|}]
