(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
*)

let (zero @ total) (x : int) : {r : int | r = 0} =
  let result = x * 0 in refine_ result

let (one @ total) (x : int) : {r : int | r = x} =
  let result = 1 * x in refine_ result

let (negate @ total) (x : int) : {r : int | r = -x} =
  let result = x * -1 in refine_ result

let (twice @ total) (x : int) : {r : int | r = x + x} =
  let result = x * 2 in refine_ result

let (zero_in_bits @ total) (x : int) (y : {n : int | n = 0}) :
    {r : int | r = 0} =
  let refine_ y = y in
  let result = (x land -1) * y in refine_ result

let () =
  List.iter (fun x ->
    assert (zero x = 0);
    assert (one x = x);
    assert (negate x = -x);
    assert (twice x = x + x);
    let value = 0 in
    let y : {n : int | n = 0} = refine_ value in
    assert (zero_in_bits x y = 0))
    [0; 1; -1; min_int; max_int]
