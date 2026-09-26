(* TEST
 has-z3;
 flags = "-extension refinement_types -strict-sequence -w +10 -warn-error +10";
 expect;
*)

module Proof : sig
  val value : int -> int @@ total
  val lemma : (x : int) -> {u : unit | value x >= 0} @@ total
end = struct
  let[@def] value (x : int) = if x < 0 then 0 else x
  let (lemma @ total) (x : int) : {u : unit | value x >= 0} =
    value_def x;
    let u = () in refine_ u
end

let (erased @ total) x : {y : int | y >= 0} =
  let y = Proof.value x in
  ghost_ (Proof.lemma x);
  refine_ y

let (retained @ total) x : {y : int | y >= 0} =
  let y = Proof.value x in
  Proof.lemma x;
  refine_ y

let nested : {p : {u : unit | true} | true} =
  let u = () in
  let p : {u : unit | true} = refine_ u in
  refine_ p

let discard_nested () = ghost_ nested; ()
;;
[%%expect{|
module Proof :
  sig
    val value : int -> int @@ total
    val lemma : (x : int) -> {u : unit | (value x) >= 0} @@ total
  end
val erased : int @ total -> {y : int | y >= 0} = <fun>
val retained : int -> {y : int | y >= 0} = <fun>
val nested : {p : {u : unit | true} | true} = ()
val discard_nested : unit -> unit = <fun>
|}]

let nonunit (x : {n : int | true}) = ghost_ x; ();;
[%%expect{|
Line 1, characters 44-45:
1 | let nonunit (x : {n : int | true}) = ghost_ x; ();;
                                                ^
Error: This expression has type "int" but an expression was expected of type
         "unit"
       because it is in the left-hand side of a sequence
|}]

let unknown (x : 'a) = ghost_ x; ();;
[%%expect{|
val unknown : unit @ total -> unit = <fun>
|}]

let too_early x : {y : int | y >= 0} =
  let y = Proof.value x in
  let result : {y : int | y >= 0} = refine_ y in
  ghost_ (Proof.lemma x);
  result;;
[%%expect{|
Line 3, characters 36-45:
3 |   let result : {y : int | y >= 0} = refine_ y in
                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let (ordinary @ total) () : {u : unit | true} =
  ();
  ghost_ (());
  let u = () in refine_ u;;
[%%expect{|
val ordinary : unit -> {u : unit | true} = <fun>
|}]

let (use_nested @ total) x : {y : int | y >= 0} =
  let y = Proof.value x in
  ghost_ (
    let p = Proof.lemma x in
    (refine_ p : {p : {u : unit | Proof.value x >= 0} | true}));
  refine_ y;;
[%%expect{|
val use_nested : int @ total -> {y : int | y >= 0} = <fun>
|}]
