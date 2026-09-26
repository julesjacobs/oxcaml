(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_tree.mli pref_tree.ml pref_tree_client.ml";
 { bytecode; }
 { native; }
*)
open Pref_tree

let () =
  let four = 4 in
  let two = 2 in
  let three = 3 in
  let one = 1 in
  let ll = leaf four in
  ghost_ (let view = borrow_ ll in
    (() : {u : unit | shape_of view.model === Fork (four, Tip, Tip)}));
  let lr = empty () in
  let lr : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = lr in
  let ll : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = ll in
  let l = branch two ll lr in
  let r = leaf three in
  let l : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = l in
  let r : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = r in
  let tree = branch one l r in
  let pointer = tree.pointer in
  let model = tree.model in
  let t = tree.state in
  (* A typed token owns cells of one payload type. An unrelated [int] cell
     therefore lives in its own token, which the tree operations never see. *)
  let counter = Pref.alloc 42 (Pref.empty ()) in
  let counter_cell = counter.value in
  let counter_token = counter.state in
  (* The frame passed through [mirror_with_frame] is an unrelated cell of the
     tree's own payload type [node option]. *)
  let frame_token = Pref.empty () in
  let spare : node option = None in
  let extra = Pref.alloc spare frame_token in
  let unrelated = extra.value in
  let frame_token = extra.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let t = Pref.join t frame_token in
  let t : {t : node option Pref.token | valid model && root model === pointer
    && H.disjoint (heap model) frame
    && Pref.own t === H.union (heap model) frame} = t in
  let t = mirror_with_frame pointer model frame t in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let result : {v : node option | v === None} =
    let b = borrow_ t in
    let b : {b : node option Pref.token | H.mem (Pref.own b) unrelated} = b in
    let result = Pref.read unrelated b in
    result in
  assert (result = None);
  let flipped_model = ghost_ (flipped model) in
  let selection = ghost_ (heap flipped_model) in
  let _proof = ghost_ (
    let _a = root_flipped model in
    let _b = H.partition_law selection frame in
    let u = () in
    let proof : {u : unit | root flipped_model === pointer
      && H.restrict after selection === selection} = u in proof) in
  let parts = Pref.split selection t in
  let tree_token = parts.#left in
  let tree_token : {t : node option Pref.token | valid flipped_model
    && root flipped_model === pointer
    && Pref.own t === heap flipped_model} = tree_token in
  let borrowed : {result : shape | result === shape_of flipped_model} =
    observe_read pointer flipped_model (borrow_ tree_token) in
  let actual : {result : shape | result === shape_of flipped_model} =
    observe pointer flipped_model tree_token in
  assert (borrowed = actual);
  assert (actual = Fork (1, Fork (3, Tip, Tip),
    Fork (2, Tip, Fork (4, Tip, Tip))));
  let counter_value : {v : int | v = 42} =
    Pref.read counter_cell (borrow_ counter_token) in
  assert (counter_value = 42)
