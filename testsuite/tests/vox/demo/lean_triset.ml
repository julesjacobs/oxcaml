(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ptrie.mli ../lib/ptrie.ml ../lib/triset.mli ../lib/triset.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the Patricia-trie set FACE.  [Triset.t] is opaque -- a set
   (Lean [ISet]) to the client; the trie and its Patricia invariant
   never leave the unit.  The client reasons purely in set vocabulary
   ([smem]/[addspec]), exactly as it would against the BST in
   lib/via_set: the two representations are interchangeable behind
   [refines (iset)].  Every membership fact is discharged through the
   exported set model -- with no bit, prefix, or tree in sight. *)

open Triset

(* [x] is a member of [add x s] -- proved from [addspec] alone. *)
let roundtrip : (x : int) -> (s : t) -> bool{ _ } =
  fun x s ->
    let u = add x s in
    mem x u

(* Build a set on the compiler's own trie and read off memberships in
   set vocabulary: 5 and 1 are in, 7 is not. *)
let built : bool{ _ } * bool{ _ } * bool{ not _ } =
  let e = empty () in
  let s1 = add 5 e in
  let s2 = add 1 s1 in
  let has5 = mem 5 s2 in
  let has1 = mem 1 s2 in
  let has7 = mem 7 s2 in
  (has5, has1, has7)
