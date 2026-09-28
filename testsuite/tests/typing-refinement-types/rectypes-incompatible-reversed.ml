(* TEST
 flags = "-rectypes -extension refinement_types";
 expect;
*)

let () = ();;
[%%expect{|
File "_none_", line 1:
Error: The -rectypes option cannot be used with the refinement_types extension
|}]
