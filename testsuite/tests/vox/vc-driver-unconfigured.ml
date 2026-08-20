(* TEST
 flags = "-vox-backend z3 -vox-z3 false";
 expect;
*)

(* An unusable solver configuration fails once, at selection: availability
   is checked when the backend is chosen, not per obligation
   (design-docs/vc-generation.md, driver fixtures).  [false] is a command
   that exists everywhere, exits 1 and prints nothing, so the message is
   machine-independent. *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
File "_none_", line 1:
Error: z3 backend: solver command false failed (exit code 1):
|}]
