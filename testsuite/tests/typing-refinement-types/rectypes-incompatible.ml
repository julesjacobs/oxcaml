(* TEST
 flags = "-extension refinement_types -rectypes";
 expect;
*)

let () = ();;
[%%expect{|
File "_none_", line 1:
Error: The -rectypes option cannot be used with the refinement_types extension
|}]
