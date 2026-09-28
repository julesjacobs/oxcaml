(* TEST
 has-z3;
 flags = "-extension refinement_types -no-ikinds";
 expect;
*)

type total_function = { f : unit -> unit | true };;
[%%expect{|
type total_function = {f : unit -> unit | true}
|}]

let wrapped : total_function = let raw = fun () -> () in refine_ raw;;
[%%expect{|
val wrapped : total_function = <fun>
|}]

let (crossed @ total) = wrapped;;
[%%expect{|
val crossed : unit -> unit = <fun>
|}]

let (crossed_stateless @ stateless) = wrapped
let (crossed_portable @ portable) = wrapped;;
[%%expect{|
val crossed_stateless : unit -> unit = <fun>
val crossed_portable : unit -> unit = <fun>
|}]
