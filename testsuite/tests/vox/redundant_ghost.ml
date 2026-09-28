(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* Inside ghost code [ghost_] changes nothing (warning 225). Predicates are
   not ghost code in this sense: generated definition equations and printed
   interfaces contain [ghost_]. *)

module M = struct
  let (lemma @ total) (x : int) : {u : unit | x + 0 = x} @ ghost =
    ghost_ ()

  let (proof @ total) (x : int) : {u : unit | x + 0 = x} @ ghost = ghost_ (
    ghost_ (lemma x);
    ghost_ (ghost_ (lemma x));
    lemma x)

  let real (x : int) =
    ghost_ (lemma x);
    x

  let (predicate @ total) (x : int) : {y : int | y === ghost_ x} = x

  let[@def] (defined @ total) (x : int) : int @ ghost = ghost_ (x + 1)
end
[%%expect{|
Line 6, characters 4-20:
6 |     ghost_ (lemma x);
        ^^^^^^^^^^^^^^^^
Warning 225 [redundant-ghost]: This "ghost_" is redundant: the enclosing code is already ghost.

Line 7, characters 4-29:
7 |     ghost_ (ghost_ (lemma x));
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 225 [redundant-ghost]: This "ghost_" is redundant: the enclosing code is already ghost.

Line 7, characters 11-29:
7 |     ghost_ (ghost_ (lemma x));
               ^^^^^^^^^^^^^^^^^^
Warning 225 [redundant-ghost]: This "ghost_" is redundant: the enclosing code is already ghost.

module M :
  sig
    val lemma : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val proof : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val real : int -> int
    val predicate : (x : int) -> {y : int | y === (ghost_ x)}
    val defined : int -> int @ ghost
    val defined_def :
      (x : int) -> {u : unit | (defined x) === (ghost_ (x + 1) : int)}
  end
|}]
