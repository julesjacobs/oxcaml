(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* An unused binding of a refined unit still warns (26 or 27), with a hint
   that the fact holds without the name. *)

module M = struct
  let (lemma @ total) (x : int) : {u : unit | x + 0 = x} = ()

  let bound (x : int) : {r : int | r = x} =
    let law = lemma x in
    x + 0

  let statement (x : int) : {r : int | r = x} =
    lemma x;
    x + 0

  let plain (x : int) =
    let n = x + 1 in
    x
end
[%%expect{|
Line 5, characters 8-11:
5 |     let law = lemma x in
            ^^^
Warning 26 [unused-var]: unused variable "law".
  Hint: the binding is unnecessary, because the fact in its
  refined type holds without the name; a statement such as "lemma x;" suffices.

Line 13, characters 8-9:
13 |     let n = x + 1 in
             ^
Warning 26 [unused-var]: unused variable "n".

module M :
  sig
    val lemma : (x : int) -> {u : unit | (x + 0) = x}
    val bound : (x : int) -> {r : int | r = x}
    val statement : (x : int) -> {r : int | r = x}
    val plain : int -> int
  end
|}]
