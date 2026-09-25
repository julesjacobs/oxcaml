(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let use_portable (x @ portable) = ()
let id x = x
[%%expect{|
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let compose f g x = f (g x)
[%%expect{|
val compose :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global] ->
  ('c @ [> 'o] -> 'a @ [< 'n & global]) @ [< global] ->
  'c @ [< 'o] -> 'b @ [> 'm | dynamic] = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let x = compose id id x in
  let y = compose id id y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]
