(* TEST
 setup-ocamlc.byte-build-env;
 flags += "-noassert";
 program = "${test_build_directory}/assume_check_noassert.byte";
 all_modules = "assume_check_noassert.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The same program as assume_check_runtime.ml, built with the checks off.

   Falsifying a lemma by running it is a development affordance, so it
   belongs on the switch that turns development affordances off, and the
   false law here goes unremarked.  That is correct rather than regrettable:
   the obligation was admitted, never guaranteed, and what remains in a
   production build is the identity function and an admitted fact with
   nothing checking it.  Which is why the mark, and not the check, is the
   part of this feature that must not be silenceable. *)

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
