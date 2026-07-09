(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Companion to scope_selfcapture_annotation.ml.  With the self-capture
   fixed, the two directions the bug damaged now behave correctly:

   [dual] is the COMPLETENESS dual -- a legitimate "successor of the old
   x".  The inner annotation [int{ _ = x + 1 }] resolves [x] to the
   outer LOCAL [x] (= 7), so the obligation is [8 = 7 + 1], which holds.
   Before the fix, self-capture turned it into the unsatisfiable
   [x = x + 1] and REJECTED it.

   [self_named] shows the self-name convenience is PRESERVED where it is
   unambiguous: with no outer binding of [x] in scope, [x] in the
   annotation still denotes the value being defined (an alias for [_]),
   so [int{ _ = x }] is the tautology and holds. *)

let dual () =
  let x : int{ _ = 7 } = 7 in
  let x : int{ _ = x + 1 } = x + 1 in
  x

let x : int{ _ = x } = 5
