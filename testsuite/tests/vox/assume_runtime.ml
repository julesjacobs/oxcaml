(* TEST
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
 { flags += " -noassert"; bytecode; }
 { flags += " -noassert"; native; }
*)

type nonnegative = {n : int | n >= 0}

let () =
  let evaluations = ref 0 in
  let input = incr evaluations; 42 in
  let checked : nonnegative = assume_ input in
  let refine_ result = checked in
  if result <> 42 || !evaluations <> 1 then
    failwith "assume_ changed its input or evaluated it again";
  let input = -1 in
  match (assume_ input : nonnegative) with
  | _ -> failwith "assume_ omitted its runtime check"
  | exception Assert_failure _ -> print_endline "runtime check preserved"
