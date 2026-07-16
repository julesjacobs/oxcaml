(* TEST
 {
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
 }{
   setup-ocamlopt.byte-build-env;
   compile_only = "true";
   ocamlopt.byte;
 }
*)

let refined_id = ((fun x -> x) : (int -> int){ true })
let applied = refined_id 1

let refined_ignore = (ignore : (int -> unit){ true })
let () = refined_ignore applied
