(* TEST
 readonly_files = "partial_recursion_interface.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types";
 module = "partial_recursion_interface.mli";
 ocamlc.opt;
 module = "partial_recursion_interface.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

(* Without [@ total] here, the function is partial because this recursive
   call does not decrease; the interface requires totality, so the error
   points at the call, as [@ total] on the definition would. *)
let rec length (l : int list) : int =
  match l with
  | [] -> 0
  | _ :: _ -> 1 + length l
