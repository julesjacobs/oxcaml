open Dependent_constructors

let (ordered @ total) (Bounded {lower; value}) : {b : bool | b} =
  lower <= value

let (minimum_is_nonpositive @ total) (Minimum {value; optimality}) :
    {b : bool | b} =
  ghost_ (optimality 0);
  value <= 0

let () =
  assert (ordered (make 2 5));
  assert (minimum_is_nonpositive (minimum ()))

let (tag @ total) = function
  | First r -> ghost_ r.proof; 1
  | Payload _ -> 3
  | Second r -> ghost_ r.proof; 2

let () =
  assert (tag (first ()) = 1);
  assert (tag (second ()) = 2);
  assert (tag (Payload 9) = 3);
  assert (tag (copy (first ())) = 1);
  assert (tag (copy (second ())) = 2);
  assert (tag (copy (Payload 9)) = 3)
