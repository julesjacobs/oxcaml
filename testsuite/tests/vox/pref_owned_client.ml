(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_list.mli pref_list.ml pref_tree.mli pref_tree.ml pref_owned_client.ml";
 { bytecode; }
*)
module L = Pref_list
module T = Pref_tree

let () =
  let list = L.Owned.of_list [1; 2; 3] in
  let before = ghost_ (L.Owned.model (borrow_ list)) in
  let nodes = L.Owned.observe (borrow_ list) in
  let reversed : {s : L.Owned.t |
    L.Owned.model s === L.rev_append before L.Nil} = L.Owned.reverse list in
  let actual = L.Owned.observe (borrow_ reversed) in
  assert (actual = List.rev nodes);
  let raw : {b : L.built | b.model === L.rev_append before L.Nil &&
    L.valid b.model && b.pointer === L.root b.model &&
    Pref.own b.state === L.heap b.model} = L.Owned.release reversed in
  let list = L.Owned.adopt raw in
  let _nodes = L.Owned.observe (borrow_ list) in
  let left = T.Owned.branch 2 (T.Owned.leaf 4) (T.Owned.empty ()) in
  let tree = T.Owned.branch 1 left (T.Owned.leaf 3) in
  let before = ghost_ (T.Owned.model (borrow_ tree)) in
  let _original = T.Owned.observe (borrow_ tree) in
  let mirrored : {s : T.Owned.t | T.Owned.model s === T.flipped before} =
    T.Owned.mirror tree in
  let actual = T.Owned.observe (borrow_ mirrored) in
  assert (actual = T.Fork (1, T.Fork (3, T.Tip, T.Tip),
    T.Fork (2, T.Tip, T.Fork (4, T.Tip, T.Tip))));
  let raw : {b : T.built | b.model === T.flipped before &&
    T.valid b.model && b.pointer === T.root b.model &&
    Pref.own b.state === T.heap b.model} = T.Owned.release mirrored in
  let tree = T.Owned.adopt raw in
  let _shape = T.Owned.observe (borrow_ tree) in
  ()
