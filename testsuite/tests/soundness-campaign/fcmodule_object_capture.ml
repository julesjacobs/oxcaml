(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — extension A (first-class modules & objects as totality capture
   vehicles).

   (Boundary): a total closure's CAPTURES whose types do not cross totality must be total.
   The crossing-arrow sweep confirmed GADT existentials and records with function fields
   correctly inherit NON-crossing from their function-typed contents, so a
   partial/diverging function packed inside them cannot be captured freely by a total
   closure. Here we test the two remaining vehicles that pack functions behind a surface
   with no top-level arrow: first-class modules [(module S)] and object types
   [< m : ... >].

   If [(module S)] / an object type crosses totality despite containing a function-typed
   member, a partial/diverging member could ride into a total closure and be invoked there
   (no application-site rule). Acceptance of a divergence-carrying capture is a soundness
   finding.

   RESULT (no finding): both vehicles are sound. A [(module S)] value and an object value
   carrying a partial/diverging member are themselves [partial], and capturing them in a
   total closure is rejected via the closure-lock path (E1..E5 all reject). Totality-
   crossing correctly inherits non-crossing from function-typed module fields and object
   methods, matching the GADT/record result from crossing_arrow_laundering.ml. *)

let expects_total (f @ total) = f

(* E1: first-class module packing a diverging function; capture the packed value in a
   total closure, unpack, invoke. *)
module type S = sig
  val f : unit -> unit
end

let m =
  (module struct
    let f () =
      while true do
        ()
      done
    ;;
  end : S)
;;

let run () =
  let module M = (val m) in
  M.f ()
;;

let e1 = expects_total run

[%%expect
  {|
val expects_total : 'a @ total -> 'a = <fun>
module type S = sig val f : unit -> unit end
val m : (module S) = <module>
val run : unit -> unit = <fun>
Line 24, characters 23-26:
24 | let e1 = expects_total run
                            ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* E2: same, forcing totality directly with an annotation. *)
let (run2 @ total) () =
  let module M = (val m) in
  M.f ()
;;

[%%expect
  {|
Line 2, characters 22-23:
2 |   let module M = (val m) in
                          ^
Error: The value "m" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 19-8
         which is expected to be "total".
|}]

(* E3: object with a diverging method; capture in a total closure, invoke. *)
let o =
  object
    method f () =
      while true do
        ()
      done
  end
;;

let run3 () = o#f ()
let e3 = expects_total run3

[%%expect
  {|
val o : < f : unit -> 'a > = <obj>
val run3 : unit -> 'a = <fun>
Line 11, characters 23-27:
11 | let e3 = expects_total run3
                            ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* E4: same, forcing totality directly. *)
let (run4 @ total) () = o#f ()

[%%expect
  {|
Line 1, characters 24-25:
1 | let (run4 @ total) () = o#f ()
                            ^
Error: The value "o" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 19-30
         which is expected to be "total".
|}]

(* E5: first-class module packing a partial (effectful) value, invoked in total. *)
module type SE = sig
  val g : unit -> int
end

let cell = ref 0

let me =
  (module struct
    let g () =
      incr cell;
      !cell
    ;;
  end : SE)
;;

let (run5 @ total) () =
  let module M = (val me) in
  M.g ()
;;

[%%expect
  {|
module type SE = sig val g : unit -> int end
val cell : int ref = {contents = 0}
val me : (module SE) = <module>
Line 17, characters 22-24:
17 |   let module M = (val me) in
                           ^^
Error: The value "me" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 16-18, characters 19-8
         which is expected to be "total".
|}]
