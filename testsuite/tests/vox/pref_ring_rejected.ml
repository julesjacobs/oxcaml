(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml";
 readonly_files = "pref_ring_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

(* Load the implementation so that the accepted phrase below can be
   evaluated. *)
#load "pref.cmo";;
#load "pref_ring.cmo";;

open Pref_ring

(* [remove] refuses to unlink the sentinel: the token below has every
   other fact [remove] requires, so the call fails only on
   [not (n === sentinel)]. *)
module Remove_sentinel = struct
  let bad : (s : node) @ immutable -> (left : node) @ immutable ->
      (right : node) @ immutable ->
      (t : {t : node option Pref.token | present (Pref.own t) left
        && present (Pref.own t) s && present (Pref.own t) right
        && H.at (Pref.own t) left.next === Some (Some s)
        && H.at (Pref.own t) s.prev === Some (Some left)
        && H.at (Pref.own t) s.next === Some (Some right)
        && H.at (Pref.own t) right.prev === Some (Some s)}) @ unique ->
      node option Pref.token @ unique = fun s left right t ->
    let t = remove s left s right t in t
end;;
[%%expect{|
Line 16, characters 34-35:
16 |     let t = remove s left s right t in t
                                       ^
Error: Refinement could not be proved (counterexample)
File "pref_ring.mli", line 169, characters 9-29:
  The refinement is stated here.
|}, Principal{|
Line 9, characters 59-60:
9 |       (t : {t : node option Pref.token | present (Pref.own t) left
                                                               ^
Error: The value "t" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

(* The same call with a sentinel other than the removed node is accepted. *)
module Remove_other = struct
  let good : (sentinel : node) @ immutable -> (s : node) @ immutable ->
      (left : node) @ immutable -> (right : node) @ immutable ->
      (t : {t : node option Pref.token | present (Pref.own t) left
        && present (Pref.own t) s && present (Pref.own t) right
        && not (s === sentinel)
        && H.at (Pref.own t) left.next === Some (Some s)
        && H.at (Pref.own t) s.prev === Some (Some left)
        && H.at (Pref.own t) s.next === Some (Some right)
        && H.at (Pref.own t) right.prev === Some (Some s)}) @ unique ->
      node option Pref.token @ unique = fun sentinel s left right t ->
    let t = remove sentinel left s right t in t
end;;
[%%expect{|
module Remove_other :
  sig
    val good :
      (sentinel : Pref_ring.node) @ immutable ->
      (s : Pref_ring.node) @ immutable ->
      (left : Pref_ring.node) @ immutable ->
      (right : Pref_ring.node) @ immutable ->
      {t : Pref_ring.node option Pref.token
        | (Pref_ring.present (Pref.own t) left) &&
            ((Pref_ring.present (Pref.own t) s) &&
               ((Pref_ring.present (Pref.own t) right) &&
                  ((not (s === sentinel)) &&
                     (((Pref_ring.H.at (Pref.own t) left.Pref_ring.next) ===
                         (Some (Some s)))
                        &&
                        (((Pref_ring.H.at (Pref.own t) s.Pref_ring.prev) ===
                            (Some (Some left)))
                           &&
                           (((Pref_ring.H.at (Pref.own t) s.Pref_ring.next)
                               === (Some (Some right)))
                              &&
                              ((Pref_ring.H.at (Pref.own t)
                                  right.Pref_ring.prev)
                                 === (Some (Some s)))))))))} @ unique ->
      Pref_ring.node option Pref.token @ unique
  end
|}, Principal{|
Line 4, characters 59-60:
4 |       (t : {t : node option Pref.token | present (Pref.own t) left
                                                               ^
Error: The value "t" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

module Missing_backward_update = struct
  let bad (left : node @ immutable) (right : node @ immutable)
      (t : {t : node option Pref.token | H.mem (Pref.own t) left.next
        && H.mem (Pref.own t) right.prev} @ unique) :
      {r : node option Pref.token | Pref.own r === connected (Pref.own t) left right} @ unique =
    let before = ghost_ (Pref.own (borrow_ t)) in
    let definition = ghost_ (connected_def before left right) in
    let p = left.next in
    let v = Some right in
    let t : {t : node option Pref.token | H.mem (Pref.own t) p} = t in
    let t = Pref.write p v t in
    t
end;;
[%%expect{|
Line 7, characters 8-18:
7 |     let definition = ghost_ (connected_def before left right) in
            ^^^^^^^^^^
Warning 26 [unused-var]: unused variable "definition".

Line 12, characters 4-5:
12 |     t
         ^
Error: Refinement could not be proved (counterexample)
Line 5, characters 36-84:
5 |       {r : node option Pref.token | Pref.own r === connected (Pref.own t) left right} @ unique =
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}, Principal{|
Line 3, characters 57-58:
3 |       (t : {t : node option Pref.token | H.mem (Pref.own t) left.next
                                                             ^
Error: The value "t" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

module No_reversal = struct
  let bad (ns : node list @ immutable)
      (t : {t : node option Pref.token | owns (Pref.own t) ns} @ unique) :
      {r : node option Pref.token | Pref.own r === flipped_all (Pref.own t) ns} @ unique =
    t
end;;
[%%expect{|
Line 5, characters 4-5:
5 |     t
        ^
Error: Refinement could not be proved (counterexample)
Line 4, characters 36-78:
4 |       {r : node option Pref.token | Pref.own r === flipped_all (Pref.own t) ns} @ unique =
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}, Principal{|
Line 3, characters 56-57:
3 |       (t : {t : node option Pref.token | owns (Pref.own t) ns} @ unique) :
                                                            ^
Error: The value "t" has type "Pref_ring.node option Pref.token"
       but an expression was expected of type "'a Pref.token"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

(* The v3 structures gate checked this outside ocamltest: private proof
   helpers are hidden by the interface. *)
let hidden = Pref_ring.owns_put;;
[%%expect{|
Line 1, characters 13-31:
1 | let hidden = Pref_ring.owns_put;;
                 ^^^^^^^^^^^^^^^^^^
Error: Unbound value "Pref_ring.owns_put"
Hint:   Did you mean "Pref_ring.owns_def"?
|}]
