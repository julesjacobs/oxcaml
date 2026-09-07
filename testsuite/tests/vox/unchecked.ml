(* TEST
 flags = "-extension refinement_types";
 expect;
*)

let currently_unchecked x : {n : int | false} = refine_ x;;
[%%expect{|
val currently_unchecked : int @ total -> {n : int | false} = <fun>
|}]
