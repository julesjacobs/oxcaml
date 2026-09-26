(* TEST
 flags = "-extension refinement_types";
 expect;
*)

#rectypes;;
[%%expect{|
File "_none_", line 1:
Error: The #rectypes directive cannot be used with the refinement_types extension
|}]
