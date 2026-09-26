(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Inside a refinement predicate, a type ascription is only logical: the
   refinement of the ascribed type is dropped. *)

type 'a nonempty = {l : 'a list | not (l === [])};;
[%%expect{|
type 'a nonempty = {l : 'a list | not (l === [])}
|}]

let (local @ total) () :
  {u : unit | let l = [1] in (l : int nonempty) === [1]} =
  ();;
[%%expect{|
val local : unit -> {u : unit | let l = [1] in (l : int list) === [1]} =
  <fun>
|}]

let (argument @ total) (x : int list) : {u : unit | (x : int nonempty) === x} =
  ();;
[%%expect{|
val argument : (x : int list) -> {u : unit | (x : int list) === x} = <fun>
|}]
