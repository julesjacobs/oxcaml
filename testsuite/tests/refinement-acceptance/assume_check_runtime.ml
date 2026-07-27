(* TEST
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/assume_check_runtime.byte";
 all_modules = "assume_check_runtime.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* An admitted lemma that is false, and the run that says so.

   Nothing here can catch the lemma at compile time: it is admitted, so the
   solver is never asked about it, and that is the whole point of admitting.
   What is left is to run the program, and a check that trips is a
   counterexample -- a concrete value rather than a solver model.

   The unchecked law next to it is the other tier.  Its predicate is over a
   carrier the model does not describe, so there is nothing to run, and it
   is admitted just the same.  The two together are what "executability
   decides the tier, not whether the program compiles" means. *)

let (positive_law @ total) (n : int) : unit{ n > 0 } = assume ()

let (string_law @ total) (s : string) : unit{ s = s } = assume ()

let attempt name f =
  match f () with
  | () -> Printf.printf "%s: no check ran\n" name
  | exception Assert_failure _ -> Printf.printf "%s: refuted by the run\n" name

let () =
  attempt "positive_law at 1" (fun () -> positive_law 1);
  attempt "positive_law at -1" (fun () -> positive_law (-1));
  attempt "string_law" (fun () -> string_law "anything")
