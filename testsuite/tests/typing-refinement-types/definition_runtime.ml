(* TEST
 modules = "assume_stubs.c";
 flags = "-extension refinement_types -noassert";
 { bytecode; }
 { native; }
*)

external counted : int -> bool @@ total = "caml_assume_counted"
external calls : unit -> int = "caml_assume_predicate_calls"

let[@def] f x = counted x

let () =
  let x = 0 in
  let before = calls () in
  let refine_ proof = f_def x in
  if calls () <> before then failwith "lemma evaluated the definition";
  ignore (f x);
  if calls () <> before + 1 then failwith "definition did not run"
