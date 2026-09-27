(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml";
 readonly_files = "talk_heap_frame.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, section 4d ("shared mutable state with permission tokens"), from
   the investigation's e8_frame.ml. A token's meaning is an immutable finite
   map, and framing is [put]'s pointwise meaning: a callee with an exact
   postcondition frames automatically, and a callee with a weak one frames
   only if the caller splits off what it wants preserved. Without the split
   the proof fails. Also the "presenting limits" example of normal-return
   contracts (e7_partial.ml): a function may promise a heap and raise. *)

#load "pref.cmo";;

module H = Pref.Heap;;
[%%expect{|
module H = Pref.Heap
|}]

(* Exact postcondition: the new heap is the old one updated at p. *)
let set5 (p : int Pref.t) (t : {t : int Pref.token | H.mem (Pref.own t) p}) :
    {u : int Pref.token | Pref.own u === H.put (Pref.own t) p 5} =
  Pref.write p 5 t;;
[%%expect{|
val set5 :
  (p : int Pref.t) ->
  (t : {t : int Pref.token | H.mem (Pref.own t) p}) @ unique ->
  {u : int Pref.token | (Pref.own u) === (H.put (Pref.own t) p 5)} = <fun>
|}]

(* 1. Framing from the exact equation: q is not p (allocation freshness), so
      at (put h p 5) q = at h q by the pointwise expansion. *)
let whole () : {x : int | x = 15} =
  let a = Pref.alloc 1 (Pref.empty ()) in
  let b = Pref.alloc 10 a.state in
  let p = a.value and q = b.value in
  let t = set5 p b.state in
  Pref.read p (borrow_ t) + Pref.read q (borrow_ t);;
[%%expect{|
val whole : unit -> {x : int | x = 15} = <fun>
|}]

(* A callee with only a membership postcondition. *)
let scribble (p : int Pref.t) (t : {t : int Pref.token | H.mem (Pref.own t) p}) :
    {u : int Pref.token | H.mem (Pref.own u) p} =
  Pref.write p 7 t;;
[%%expect{|
val scribble :
  (p : int Pref.t) ->
  {t : int Pref.token | H.mem (Pref.own t) p} @ unique ->
  {u : int Pref.token | H.mem (Pref.own u) p} = <fun>
|}]

(* 2. Framing by ownership: q's cell is split off and never passed, so the
      remaining token is the same variable, with the same heap term. *)
let split_off () : {x : int | x = 10} =
  let a = Pref.alloc 1 (Pref.empty ()) in
  let b = Pref.alloc 10 a.state in
  let p = a.value and q = b.value in
  let sel = ghost_ (H.put (H.empty ()) q 0) in
  let parts = Pref.split sel b.state in
  let kept = parts.#left in
  let _mine = scribble p parts.#right in
  Pref.read q (borrow_ kept);;
[%%expect{|
val split_off : unit -> {x : int | x = 10} = <fun>
|}]

(* 3. The same call without the split: the weak contract loses q, and the
      caller cannot even show that q is still owned. *)
let no_split () : {x : int | x = 10} =
  let a = Pref.alloc 1 (Pref.empty ()) in
  let b = Pref.alloc 10 a.state in
  let p = a.value and q = b.value in
  let t = scribble p b.state in
  Pref.read q (borrow_ t);;
[%%expect{|
Line 6, characters 23-24:
6 |   Pref.read q (borrow_ t);;
                           ^
Error: Refinement could not be proved (counterexample)
File "pref.mli", line 139, characters 23-41:
  The refinement is stated here.
|}]

(* The two accepted programs, run: 15 and 10, as verified. *)
let results = (whole (), split_off ());;
[%%expect{|
val results : int * int = (15, 10)
|}]

(* Contracts describe normal return: raising satisfies any postcondition, so
   [fake] promises 99, writes 7, and raises; the caller never sees the heap
   it promised, and the token is lost. *)
let fake (p : int Pref.t) (t : {t : int Pref.token | H.mem (Pref.own t) p}) :
    {u : int Pref.token | Pref.own u === H.put (Pref.own t) p 99} =
  let _ = Pref.write p 7 t in
  raise (Failure "never returns");;
[%%expect{|
val fake :
  (p : int Pref.t) ->
  (t : {t : int Pref.token | H.mem (Pref.own t) p}) @ unique ->
  {u : int Pref.token | (Pref.own u) === (H.put (Pref.own t) p 99)} = <fun>
|}]
