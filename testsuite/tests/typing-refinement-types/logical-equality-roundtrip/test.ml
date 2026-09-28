(* TEST
 modules = "producer.ml";
 flags = "-extension refinement_types";
 has-z3;
 { bytecode; }
 { native; }
*)

module Alias = Producer
module Same = Alias

let roundtrip () : {r : Same.t | r === Same.zero} = Same.same ()

let () = ignore (roundtrip ())
