(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/via_set.mli ../lib/via_set.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* STAGE 3: honest [via] across a MODULE BOUNDARY.  via_set.mli declares
   [type t : value refines (iset)] and specs in pure set vocabulary;
   via_set.ml implements [t] as [tree{ bst _ } via (elems : iset)] and
   PROVES those specs (no [assume_unchecked_]) by unpacking the image
   binder to its tree with [refine_].  The .mli never mentions [elems].
   A client binds [Via_set.t] at the set sort (the [refines] kind gives
   the image dsort), imports ISet + mem/ins from the unit's VoxSig, and
   proves a membership fact THROUGH the abstraction with no view of the
   representation. *)

open Via_set

(* [add x s] gives [t = ins x s]; [member x] on it reads
   [mem x (ins x s)] -- congruence through the exported model closes
   the goal.  [t] is opaque (the set sort); no tree in sight. *)
let roundtrip : (x : int) -> (s : t) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let u = add x s in
    member x u
