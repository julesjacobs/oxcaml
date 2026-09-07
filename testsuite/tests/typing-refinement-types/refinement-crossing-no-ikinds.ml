(* TEST
 flags = "-extension refinement_types -no-ikinds";
 expect;
*)

type total_function = { f : unit -> unit | true };;
[%%expect{|
type total_function = {f : unit -> unit | true}
|}]

let wrapped : total_function = refine_ (fun () -> ());;
[%%expect{|
val wrapped : total_function = <fun>
|}]

let (crossed @ total) = wrapped;;
[%%expect{|
val crossed : total_function = <fun>
|}]

let (crossed_stateless @ stateless) = wrapped
let (crossed_portable @ portable) = wrapped;;
[%%expect{|
val crossed_stateless : total_function = <fun>
val crossed_portable : total_function = <fun>
|}]
