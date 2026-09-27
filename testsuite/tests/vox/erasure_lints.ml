(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* [ghost_ e] is the only construct that erases. A total function whose
   result is ghost still runs its body unless the body is [ghost_ (...)]
   (warning 223), and real code that calls a total function only to discard
   its ghost result still runs the call (warning 224). Neither warning
   changes the meaning of the program. *)

module Bodies = struct
  let (lemma @ total) (x : int) : {u : unit | x + 0 = x} @ ghost =
    ghost_ ()

  let (unit_body @ total) (x : int) : {u : unit | x + 0 = x} @ ghost = ()

  let (runs @ total) (x : int) : {u : unit | x + 0 = x} @ ghost =
    let y = x + 1 in
    ghost_ (lemma y; lemma x)

  let (erased @ total) (x : int) : {u : unit | x + 0 = x} @ ghost =
    ghost_ (let y = x + 1 in lemma y; lemma x)

  let (curried @ total) : (x : int) -> {u : unit | x + 0 = x} @ ghost =
    fun x -> lemma x

  let (cases @ total) : int list -> int @ ghost = function
    | [] -> ghost_ 0
    | x :: _ -> x + 1

  let (returns_parameter @ total) (x : int @ ghost) = x

  (* Partial functions cannot be erased: [ghost_] requires totality. *)
  let rec loops (x : int) : {u : unit | false} @ ghost = loops x

  let[@warning "-unerased-ghost-body"] (silenced @ total) (x : int)
      : {u : unit | x + 0 = x} @ ghost =
    lemma x
end
[%%expect{|
Line 7, characters 7-11:
7 |   let (runs @ total) (x : int) : {u : unit | x + 0 = x} @ ghost =
           ^^^^
Warning 223 [unerased-ghost-body]: This function's result is ghost, but its body is not wrapped in
  "ghost_", so the body is computed when the function is called and its
  value may be thrown away. Wrap the body in "ghost_ (...)" to erase it.

Line 14, characters 7-14:
14 |   let (curried @ total) : (x : int) -> {u : unit | x + 0 = x} @ ghost =
            ^^^^^^^
Warning 223 [unerased-ghost-body]: This function's result is ghost, but its body is not wrapped in
  "ghost_", so the body is computed when the function is called and its
  value may be thrown away. Wrap the body in "ghost_ (...)" to erase it.

Line 17, characters 7-12:
17 |   let (cases @ total) : int list -> int @ ghost = function
            ^^^^^
Warning 223 [unerased-ghost-body]: This function's result is ghost, but its body is not wrapped in
  "ghost_", so the body is computed when the function is called and its
  value may be thrown away. Wrap the body in "ghost_ (...)" to erase it.

module Bodies :
  sig
    val lemma : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val unit_body : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val runs : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val erased : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val curried : (x : int) -> {u : unit | (x + 0) = x} @ ghost
    val cases : int list -> int @ ghost
    val returns_parameter : int @ ghost -> int @ ghost
    val loops : int -> {u : unit | false} @ ghost
    val silenced : (x : int) -> {u : unit | (x + 0) = x} @ ghost
  end
|}]

module Calls = struct
  open Bodies

  let statement (x : int) =
    lemma x;
    x + 1

  let wildcard (x : int) =
    let _ = lemma x in
    x + 1

  let arguments (x : int) (l : int list) =
    lemma (x + 1);
    lemma (List.length l);
    x

  let erased (x : int) =
    ghost_ (lemma x);
    x + 1

  (* Not reported: the callee is partial, or [ghost_] would not accept the
     argument. *)
  let partial (x : int) =
    loops x;
    x

  let effectful_argument (x : int) =
    lemma (print_int x; x);
    x

  let in_ghost_code (x : int) = ghost_ (lemma x; lemma x)
end
[%%expect{|
Line 5, characters 4-11:
5 |     lemma x;
        ^^^^^^^
Warning 224 [unerased-ghost-call]: This call is evaluated at run time only to discard its ghost
  result. Wrap the call in "ghost_ (...)" to erase it.

Line 9, characters 12-19:
9 |     let _ = lemma x in
                ^^^^^^^
Warning 224 [unerased-ghost-call]: This call is evaluated at run time only to discard its ghost
  result. Wrap the call in "ghost_ (...)" to erase it.

Line 13, characters 4-17:
13 |     lemma (x + 1);
         ^^^^^^^^^^^^^
Warning 224 [unerased-ghost-call]: This call is evaluated at run time only to discard its ghost
  result. Wrap the call in "ghost_ (...)" to erase it.

Line 14, characters 4-25:
14 |     lemma (List.length l);
         ^^^^^^^^^^^^^^^^^^^^^
Warning 224 [unerased-ghost-call]: This call is evaluated at run time only to discard its ghost
  result. Wrap the call in "ghost_ (...)" to erase it.

module Calls :
  sig
    val statement : int -> int
    val wildcard : int -> int
    val arguments : int -> int list -> int
    val erased : int -> int
    val partial : int -> int
    val effectful_argument : int -> int
    val in_ghost_code : int -> unit @ ghost
  end
|}]

(* A unique argument cannot move under [ghost_], which makes the values it
   captures aliased. *)
module Unique = struct
  type token = { n : int @@ ghost }

  let (consume @ total) (t : token @ unique) : token @ unique ghost =
    ghost_ { n = 0 }

  let tokens (t : token @ unique) =
    let _ = consume t in
    ()
end
[%%expect{|
module Unique :
  sig
    type token = { n : int @@ ghost; }
    val consume : token @ unique -> token @ unique ghost
    val tokens : token @ unique -> unit
  end
|}]

(* [ghost_] captures values through a total lock, so a call that captures a
   partial function is not reported. *)
module Captures = struct
  let (lemma_f @ total) (f : int -> int) : {u : unit | true} @ ghost =
    ghost_ ()

  let partial_callback (f @ partial) =
    lemma_f f;
    ()

  let total_callback (f @ total) =
    lemma_f f;
    ()
end
[%%expect{|
Line 10, characters 4-13:
10 |     lemma_f f;
         ^^^^^^^^^
Warning 224 [unerased-ghost-call]: This call is evaluated at run time only to discard its ghost
  result. Wrap the call in "ghost_ (...)" to erase it.

module Captures :
  sig
    val lemma_f : (int -> int) -> {u : unit | true} @ ghost
    val partial_callback : (int -> int) -> unit
    val total_callback : (int -> int) @ total -> unit
  end
|}]
