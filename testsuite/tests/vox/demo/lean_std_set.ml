(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/voption.mli ../lib/voption.ml ../lib/Vlist.mli ../lib/Vlist.ml ../lib/Vset_bst.mli ../lib/Vset_bst.ml ../lib/Vset.mli ../lib/Vset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: a client of the vox_stdlib [Vset] finite-set face, the stdlib
   upgrade of demo/lean_oset.  Where lean_oset's [Oset] is FULLY opaque
   (its type has its own uninterpreted sort, membership justified by bare
   axioms), [Vset] is via-abstract with EXTENSIONAL specs: [mem] agrees,
   pointwise, with [vs_addspec] / [vs_isempty].  It also ships a
   cross-module eliminator [elements] into the stdlib's own [Vlist] that
   lean_oset had no equivalent of.

   The [modules] stanza shows the demo-consumes-stdlib recipe at its
   heaviest: [Vset] depends on [Vset_bst], [Vlist] and (transitively)
   [Voption], so all four are snapshotted from [vox_stdlib/] into ../lib
   and listed in DEPENDENCY ORDER; the harness compiles them in sequence,
   each .mli/.ml pair yielding the .cmi + VoxSig_*.olean the next needs.
   ([Vset] and [Vlist] both bind a [mem] / [empty], so this client opens
   only [Vset] and qualifies [Vlist.mem] to keep the names unambiguous.) *)

open Vset

(* Concrete membership -- the same shape as demo/lean_oset's [found] /
   [absent], now over the stdlib set. *)
let found : bool{ _ = true } =
  let s1 = add 2 (empty ()) in
  let s2 = add 1 s1 in
  mem 1 s2

let absent : bool{ _ = false } =
  let s1 = add 2 (empty ()) in
  let s2 = add 1 s1 in
  mem 3 s2

(* Cross-module edge (no lean_oset equivalent): [elements] enumerates the
   set into a [Vlist], membership preserved (vs_elements_spec + ll_mem).
   After adding x, x is found in the enumerated list -- one nested
   expression, a depth-3 fact thread, no let-bind. *)
let roundtrip (x : int) (s : Vset.t) : bool{ _ = true } =
  Vlist.mem x (elements (add x s))

(* [add] yields a superset: the relational vocabulary [vs_subset] consumed
   directly as a goal, with no client-side quantifier (F-3). *)
let superset (x : int) (s : Vset.t) : Vset.t{ vs_subset s _ } = add x s
