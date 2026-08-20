(* TEST
 flags = "-vox-backend nonsense";
 expect;
*)

(* An unknown backend name fails once, at selection, with the message
   listing the valid names — before any obligation is consulted
   (design-docs/vc-generation.md, driver fixtures). *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
Line 1:
Error: unknown vox backend nonsense (valid backends: printing, z3; or none to typecheck only)
|}]
