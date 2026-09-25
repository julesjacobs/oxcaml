(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_ring.mli pref_ring.ml";
 readonly_files = "pref_ring_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

open Pref_ring

module Remove_sentinel = struct
  let bad : (s : node) @ immutable -> (left : node) @ immutable ->
      (right : node) @ immutable ->
      (t : {t : Pref.token | present (Pref.own t) left
        && present (Pref.own t) s && present (Pref.own t) right
        && H.at (Pref.own t) left.next === Some (Some s)
        && H.at (Pref.own t) s.prev === Some (Some left)
        && H.at (Pref.own t) s.next === Some (Some right)
        && H.at (Pref.own t) right.prev === Some (Some s)}) @ unique ->
      Pref.token @ unique = fun s left right t ->
    let t : {t : Pref.token | present (Pref.own t) left
      && present (Pref.own t) s && present (Pref.own t) right
      && not (s === s)
      && H.at (Pref.own t) left.next === Some (Some s)
      && H.at (Pref.own t) s.prev === Some (Some left)
      && H.at (Pref.own t) s.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some s)} = t in
    let t = remove s left s right t in t
end;;
[%%expect{|
Line 19, characters 59-60:
19 |       && H.at (Pref.own t) right.prev === Some (Some s)} = t in
                                                                ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 8, characters 29-38:
8 |         && H.at (Pref.own t) left.next === Some (Some s)
                                 ^^^^^^^^^
Error: The field access "left.next" has type "Pref_ring.node option Pref.t"
       but an expression was expected of type "'a Pref.t"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

module Missing_backward_update = struct
  let bad (left : node @ immutable) (right : node @ immutable)
      (t : {t : Pref.token | H.mem (Pref.own t) left.next
        && H.mem (Pref.own t) right.prev} @ unique) :
      {r : Pref.token | Pref.own r === connected (Pref.own t) left right} @ unique =
    let before = ghost_ (Pref.own (borrow_ t)) in
    let definition = ghost_ (connected_def before left right) in
    let p = left.next in
    let v = Some right in
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
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
|}, Principal{|
Line 3, characters 48-57:
3 |       (t : {t : Pref.token | H.mem (Pref.own t) left.next
                                                    ^^^^^^^^^
Error: The field access "left.next" has type "Pref_ring.node option Pref.t"
       but an expression was expected of type "'a Pref.t"
       The kind of Pref_ring.node option is
           immutable_data with Pref_ring.node
         because it's a boxed variant type.
       But the kind of Pref_ring.node option must be a subkind of
           immutable_data.
|}]

module No_reversal = struct
  let bad (ns : node list @ immutable)
      (t : {t : Pref.token | owns (Pref.own t) ns} @ unique) :
      {r : Pref.token | Pref.own r === flipped_all (Pref.own t) ns} @ unique =
    t
end;;
[%%expect{|
Line 5, characters 4-5:
5 |     t
        ^
Error: Refinement could not be proved (counterexample)
|}, Principal{|
Line 5, characters 4-5:
5 |     t
        ^
Error: Refinement could not be proved (counterexample)
|}]
