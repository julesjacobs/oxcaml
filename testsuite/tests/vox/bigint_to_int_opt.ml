(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

let round_trip (x : int) :
    {u : unit | Bigint.to_int_opt (Bigint.of_int x) === Some x} = ();;
[%%expect{|
val round_trip :
  (x : int) ->
  {u : unit | (Bigint.to_int_opt (Bigint.of_int x)) === (Some x)} = <fun>
|}]

let bounds () :
    {u : unit | Bigint.to_int_opt (Bigint.of_int max_int) === Some max_int
      && Bigint.to_int_opt (Bigint.add (Bigint.of_int max_int) Bigint.one)
         === None
      && Bigint.to_int_opt (Bigint.sub (Bigint.of_int min_int) Bigint.one)
         === None} = ();;
[%%expect{|
val bounds :
  unit ->
  {u : unit
    | ((Bigint.to_int_opt (Bigint.of_int max_int)) === (Some max_int)) &&
        (((Bigint.to_int_opt (Bigint.add (Bigint.of_int max_int) Bigint.one))
            === None)
           &&
           ((Bigint.to_int_opt
               (Bigint.sub (Bigint.of_int min_int) Bigint.one))
              === None))} =
  <fun>
|}]

let (checked @ total) (b : Bigint.t) :
    {r : int option | match r with
      | None -> Bigint.( < ) b (Bigint.of_int min_int)
                || Bigint.( > ) b (Bigint.of_int max_int)
      | Some x -> Bigint.of_int x === b} = Bigint.to_int_opt b;;
[%%expect{|
val checked :
  (b : Bigint.t) ->
  {r : int option
    | match r with
      | None ->
          (Bigint.(<) b (Bigint.of_int min_int)) ||
            (Bigint.(>) b (Bigint.of_int max_int))
      | Some x -> (Bigint.of_int x) === b} =
  <fun>
|}]

let wrong (b : Bigint.t) : {u : unit | not (Bigint.to_int_opt b === None)} =
  ();;
[%%expect{|
Line 2, characters 2-4:
2 |   ();;
      ^^
Error: Refinement could not be proved (counterexample: b = 4611686018427387904Z)
Line 1, characters 39-73:
1 | let wrong (b : Bigint.t) : {u : unit | not (Bigint.to_int_opt b === None)} =
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
