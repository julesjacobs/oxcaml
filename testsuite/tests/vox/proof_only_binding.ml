(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* A local value computed at run time but used only in ghost code
   (warning 226, on by default). *)

module M = struct
  let (witness @ total) (a : int) (b : int) : int = a * b

  let (lemma @ total) (x : int) (w : int) : {u : unit | w = w} @ ghost =
    ghost_ ()

  (* The rsa pattern: a Bezout witness used only by proofs. *)
  let proof_only (a : int) (b : int) =
    let w = witness a b in
    ghost_ (lemma a w);
    a + b

  let in_predicate (a : int) (b : int) : {r : int | r = a + b} =
    let w = witness a b in
    (() : {u : unit | w = w});
    a + b

  (* Not reported. *)
  let erased (a : int) (b : int) =
    let w = ghost_ (witness a b) in
    ghost_ (lemma a w);
    a + b

  let used (a : int) (b : int) =
    let w = witness a b in
    ghost_ (lemma a w);
    w

  let alias (a : int) =
    let w = a in
    ghost_ (lemma a w);
    a

  let effectful (a : int) =
    let w = (print_int a; a) in
    ghost_ (lemma a w);
    a

  let checked (a : int) (b : int) : {r : int | r >= 0} =
    let w = witness a b in
    let r = a * a in
    let checked : {r : int | r >= 0 && w = w} = assume_ r in
    checked

  let annotated_real (a : int) (b : int) =
    let (w @ real) = witness a b in
    ghost_ (lemma a w);
    a + b

  let _underscore (a : int) (b : int) =
    let _w = witness a b in
    ghost_ (lemma a _w);
    a
end
[%%expect{|
Line 9, characters 8-9:
9 |     let w = witness a b in
            ^
Warning 226 [proof-only-binding]: "w" is computed at run time but used only in ghost code.
  Wrap its definition in "ghost_ (...)" to erase it.

Line 14, characters 8-9:
14 |     let w = witness a b in
             ^
Warning 226 [proof-only-binding]: "w" is computed at run time but used only in ghost code.
  Wrap its definition in "ghost_ (...)" to erase it.

module M :
  sig
    val witness : int -> int -> int
    val lemma : int -> (w : int) -> {u : unit | w = w} @ ghost
    val proof_only : int -> int -> int
    val in_predicate : (a : int) -> (b : int) -> {r : int | r = (a + b)}
    val erased : int -> int -> int
    val used : int -> int -> int
    val alias : int -> int
    val effectful : int -> int
    val checked : int -> int -> {r : int | r >= 0}
    val annotated_real : int -> int -> int
    val _underscore : int -> int -> int
  end
|}]
