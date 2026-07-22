(* TEST
 expect;
*)

(* Aggregate children do not have a language-level evaluation order.  Each
   child must therefore prove its own obligations from the aggregate's entry
   facts.  Facts established by all normally returning children become
   available only after the complete aggregate returns. *)

module Aggregate_facts : sig
  val p1 : bool
  val p2 : bool
  val p3 : bool
  val p4 : bool
  val p5 : bool
  val p6 : bool
  val p7 : bool
  val p8 : bool
  val p9 : bool
  val p10 : bool
  val p11 : bool
  val p12 : bool
  val p13 : bool
  val p14 : bool
  val p15 : bool
  val p16 : bool
  val p17 : bool
  val p18 : bool
  val law1 : unit{ p1 = true }
  val law2 : unit{ p2 = true }
  val law3 : unit{ p3 = true }
  val law4 : unit{ p4 = true }
  val law5 : unit{ p5 = true }
  val law6 : unit{ p6 = true }
  val law7 : unit{ p7 = true }
  val law8 : unit{ p8 = true }
  val law9 : unit{ p9 = true }
  val law10 : unit{ p10 = true }
  val law11 : unit{ p11 = true }
  val law12 : unit{ p12 = true }
  val law13 : unit{ p13 = true }
  val law14 : unit{ p14 = true }
  val law15 : unit{ p15 = true }
  val law16 : unit{ p16 = true }
  val law17 : unit{ p17 = true }
  val law18 : unit{ p18 = true }

  type mutable_record = { mutable contents : unit }
  val mutable_record : unit -> mutable_record{ p6 = true }

  type update_record = { first : unit; second : unit }
  val base_record : unit -> update_record{ p7 = true }
end = struct
  let p1 = true
  let p2 = true
  let p3 = true
  let p4 = true
  let p5 = true
  let p6 = true
  let p7 = true
  let p8 = true
  let p9 = true
  let p10 = true
  let p11 = true
  let p12 = true
  let p13 = true
  let p14 = true
  let p15 = true
  let p16 = true
  let p17 = true
  let p18 = true
  let law1 = (() : unit{ p1 = true })
  let law2 = (() : unit{ p2 = true })
  let law3 = (() : unit{ p3 = true })
  let law4 = (() : unit{ p4 = true })
  let law5 = (() : unit{ p5 = true })
  let law6 = (() : unit{ p6 = true })
  let law7 = (() : unit{ p7 = true })
  let law8 = (() : unit{ p8 = true })
  let law9 = (() : unit{ p9 = true })
  let law10 = (() : unit{ p10 = true })
  let law11 = (() : unit{ p11 = true })
  let law12 = (() : unit{ p12 = true })
  let law13 = (() : unit{ p13 = true })
  let law14 = (() : unit{ p14 = true })
  let law15 = (() : unit{ p15 = true })
  let law16 = (() : unit{ p16 = true })
  let law17 = (() : unit{ p17 = true })
  let law18 = (() : unit{ p18 = true })

  type mutable_record = { mutable contents : unit }
  let mutable_record_value = { contents = () }
  let mutable_record () =
    (mutable_record_value : mutable_record{ p6 = true })

  type update_record = { first : unit; second : unit }
  let base_record_value = { first = (); second = () }
  let base_record () =
    (base_record_value : update_record{ p7 = true })
end

type aggregate_pair = Aggregate_pair of unit * unit

type aggregate_record =
  { first : unit;
    second : unit;
  }
[%%expect {|
module Aggregate_facts :
  sig
    val p1 : bool
    val p2 : bool
    val p3 : bool
    val p4 : bool
    val p5 : bool
    val p6 : bool
    val p7 : bool
    val p8 : bool
    val p9 : bool
    val p10 : bool
    val p11 : bool
    val p12 : bool
    val p13 : bool
    val p14 : bool
    val p15 : bool
    val p16 : bool
    val p17 : bool
    val p18 : bool
    val law1 : unit{ p1 = true }
    val law2 : unit{ p2 = true }
    val law3 : unit{ p3 = true }
    val law4 : unit{ p4 = true }
    val law5 : unit{ p5 = true }
    val law6 : unit{ p6 = true }
    val law7 : unit{ p7 = true }
    val law8 : unit{ p8 = true }
    val law9 : unit{ p9 = true }
    val law10 : unit{ p10 = true }
    val law11 : unit{ p11 = true }
    val law12 : unit{ p12 = true }
    val law13 : unit{ p13 = true }
    val law14 : unit{ p14 = true }
    val law15 : unit{ p15 = true }
    val law16 : unit{ p16 = true }
    val law17 : unit{ p17 = true }
    val law18 : unit{ p18 = true }
    type mutable_record = { mutable contents : unit; }
    val mutable_record : unit -> mutable_record{ p6 = true }
    type update_record = { first : unit; second : unit; }
    val base_record : unit -> update_record{ p7 = true }
  end
type aggregate_pair = Aggregate_pair of unit * unit
type aggregate_record = { first : unit; second : unit; }
|}]

let tuple_no_left_to_right_proof =
  ( Aggregate_facts.law1,
    (() : unit{ Aggregate_facts.p1 = true }) )
[%%expect {|
Line 3, characters 4-44:
3 |     (() : unit{ Aggregate_facts.p1 = true }) )
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let tuple_no_right_to_left_proof =
  ( (() : unit{ Aggregate_facts.p2 = true }),
    Aggregate_facts.law2 )
[%%expect {|
Line 2, characters 4-44:
2 |   ( (() : unit{ Aggregate_facts.p2 = true }),
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let constructor_no_cross_child_proof =
  Aggregate_pair
    ( Aggregate_facts.law3,
      (() : unit{ Aggregate_facts.p3 = true }) )
[%%expect {|
Line 4, characters 6-46:
4 |       (() : unit{ Aggregate_facts.p3 = true }) )
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let record_no_cross_field_proof =
  { first = Aggregate_facts.law4;
    second = (() : unit{ Aggregate_facts.p4 = true });
  }
[%%expect {|
Line 3, characters 13-53:
3 |     second = (() : unit{ Aggregate_facts.p4 = true });
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let array_no_cross_element_proof =
  [| Aggregate_facts.law5;
     (() : unit{ Aggregate_facts.p5 = true });
  |]
[%%expect {|
Line 3, characters 5-45:
3 |      (() : unit{ Aggregate_facts.p5 = true });
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let setfield_no_operand_proof () =
  ((Aggregate_facts.mutable_record () : Aggregate_facts.mutable_record)).contents <-
    (() : unit{ Aggregate_facts.p6 = true })
[%%expect {|
Line 3, characters 4-44:
3 |     (() : unit{ Aggregate_facts.p6 = true })
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let record_update_no_base_to_field_proof =
  { (Aggregate_facts.base_record () : Aggregate_facts.update_record) with
    second = (() : unit{ Aggregate_facts.p7 = true });
  }
[%%expect {|
Line 3, characters 13-53:
3 |     second = (() : unit{ Aggregate_facts.p7 = true });
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let nested_aggregate_no_cross_proof =
  ( (Aggregate_facts.law8, Aggregate_facts.law9),
    (() : unit{
      Aggregate_facts.p8 = true && Aggregate_facts.p9 = true
    }) )
[%%expect {|
Lines 3-5, characters 4-6:
3 | ....(() : unit{
4 |       Aggregate_facts.p8 = true && Aggregate_facts.p9 = true
5 |     })..
Error: Refinement verification failed (not-proved)
|}]

let aggregate_normal_exit_merge () =
  let _ =
    (Aggregate_facts.law12, Aggregate_facts.law13)
  in
  (() : unit{
    Aggregate_facts.p12 = true && Aggregate_facts.p13 = true
  })
[%%expect {|
val aggregate_normal_exit_merge :
  unit -> unit{ Aggregate_facts.p12 = true && Aggregate_facts.p13 = true } =
  <fun>
|}]

let mutation_child_does_not_receive_sibling_proof () =
  let cell = ref false in
  ( Aggregate_facts.law10,
    (cell := true; (() : unit{ Aggregate_facts.p10 = true })) )
[%%expect {|
Line 4, characters 19-60:
4 |     (cell := true; (() : unit{ Aggregate_facts.p10 = true })) )
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let aggregate_merge_survives_unrelated_mutation () =
  let cell = ref false in
  let _ = (Aggregate_facts.law14, (cell := true)) in
  (() : unit{ Aggregate_facts.p14 = true })
[%%expect {|
val aggregate_merge_survives_unrelated_mutation :
  unit -> unit{ Aggregate_facts.p14 = true } = <fun>
|}]

let ( let+ ) value continuation = continuation value
let ( and+ ) left right = left, right
[%%expect {|
val ( let+ ) : 'a -> ('a -> 'b) -> 'b = <fun>
val ( and+ ) : 'a -> 'b -> 'a * 'b = <fun>
|}]

let letop_no_cross_operand_proof =
  let+ () = Aggregate_facts.law11
  and+ () = (() : unit{ Aggregate_facts.p11 = true }) in
  ()
[%%expect {|
Line 3, characters 12-53:
3 |   and+ () = (() : unit{ Aggregate_facts.p11 = true }) in
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let letop_body_sees_all_completed_operands =
  let+ () = Aggregate_facts.law15
  and+ () = Aggregate_facts.law16 in
  (() : unit{
    Aggregate_facts.p15 = true && Aggregate_facts.p16 = true
  })
[%%expect {|
val letop_body_sees_all_completed_operands :
  unit{ Aggregate_facts.p15 = true && Aggregate_facts.p16 = true } = ()
|}]

let tuple_children_use_own_evidence =
  ( (Aggregate_facts.law17 : unit{ Aggregate_facts.p17 = true }),
    (Aggregate_facts.law18 : unit{ Aggregate_facts.p18 = true }) )
[%%expect {|
val tuple_children_use_own_evidence :
  unit{ Aggregate_facts.p17 = true } * unit{ Aggregate_facts.p18 = true } =
  ((), ())
|}]
