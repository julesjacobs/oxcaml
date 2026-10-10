(* TEST
 flags = "-extension refinement_types -vox-library";
 expect;
*)

(* A unit of the verified library declares its trusted externals without a
   warning: they are part of the trusted base. *)
external trust_total : 'a -> 'a @ total = "%identity"
external positive : int -> {x : int | x > 0} @@ total = "%identity";;
[%%expect{|
external trust_total : 'a -> 'a @ total = "%identity"
external positive : int -> {x : int | x > 0} = "%identity"
|}]
