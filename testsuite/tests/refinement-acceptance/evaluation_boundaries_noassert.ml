(* TEST
 flags += "-noassert";
 expect;
*)

module Make_fact () : sig
  val p : bool
  val law : unit{ p = true }
end = struct
  let p = true
  let law = (() : unit{ p = true })
end
[%%expect {|
module Make_fact :
  functor () -> sig val p : bool val law : unit{ p = true } end
|}]

let disabled_assert_condition_fact_does_not_escape () =
  let module F = Make_fact () in
  assert (ignore F.law; true);
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]
