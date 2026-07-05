(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/via_set.mli ../lib/via_set.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* STAGE 3: [via] across a MODULE BOUNDARY.  via_set.mli declares
   [type t : value refines (iset)] and specs in pure set vocabulary;
   via_set.ml implements [t] as [tree{ bst _ } via (elems : iset)].
   The .mli never mentions the tree or [elems] -- the boundary
   reconciliation (ctype.ml vox_flatten_view) relates the manifest's
   flattened [via] form to the abstract [refines] claim, so the impl
   type-checks against the interface with no leak.  A client binds
   [Via_set.t] at the set sort ISet (through the [refines] kind, dsort
   returns the image) and proves a membership fact THROUGH the
   abstraction, importing ISet + mem/ins/card from the unit's VoxSig
   and never seeing the representation. *)

open Via_set

(* [add x s] gives [t = ins x s]; [member x] on it reads
   [mem x (ins x s)] -- congruence through the exported model closes
   the goal.  [t] is opaque (the set sort); no tree in sight. *)
let roundtrip : (x : int) -> (s : t) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let u = add x s in
    member x u

(* [card] exposes an Int observable: [card (ins x s) = 1 + card s]
   follows from the exported model (unfolding [ins]/[card]). *)
let grows : (x : int) -> (s : t) -> int{ _ = card s + 1 } =
  fun x s ->
    let u = add x s in
    card u
