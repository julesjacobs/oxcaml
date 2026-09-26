(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_list.mli pref_list.ml pref_list_client.ml";
 { bytecode; }
 { native; }
*)
open Pref_list

let check values =
  let b = of_list values in
  ghost_ (let view = borrow_ b in
    (() : {u : unit | contents view.model === values}));
  let pointer = b.pointer in
  let xs = b.model in
  let t = b.state in
  let t : {t : node option Pref.token | valid xs && root xs === pointer
    && Pref.own t === heap xs} = t in
  let borrowed : {result : node list | result === nodes xs} =
    observe_read pointer xs (borrow_ t) in
  let original = observe pointer xs t in
  assert (borrowed = original.nodes);
  let original_nodes = original.nodes in
  let t = original.state in
  (* A typed token owns cells of one payload type. An unrelated [int] cell
     therefore lives in its own token, which the list operations never see. *)
  let counter = Pref.alloc 42 (Pref.empty ()) in
  let counter_cell = counter.value in
  let counter_token = counter.state in
  (* The frame passed through [reverse] is an unrelated cell of the list's
     own payload type [node option]. *)
  let frame_token = Pref.empty () in
  let spare : node option = None in
  let extra = Pref.alloc spare frame_token in
  let unrelated = extra.value in
  let frame_token = extra.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let u = () in
  let _frame_value : {u : unit | H.mem frame unrelated
    && H.at frame unrelated === Some None} = u in
  let t = Pref.join t frame_token in
  let t : {t : node option Pref.token | valid xs && root xs === pointer
    && H.disjoint (heap xs) frame
    && Pref.own t === H.union (heap xs) frame} = t in
  let reversed = reverse pointer xs frame t in
  let pointer = reversed.pointer in
  let t = reversed.state in
  let ys = ghost_ (rev_append xs Nil) in
  let selection = ghost_ (heap ys) in
  let _proof = ghost_ (H.partition_law selection frame) in
  let parts = Pref.split selection t in
  let list = parts.#left in
  let frame_token = parts.#right in
  let untouched : {v : node option | v === None} =
    let b = borrow_ frame_token in
    let b : {b : node option Pref.token | H.mem (Pref.own b) unrelated} = b in
    let v = Pref.read unrelated b in
    v in
  assert (untouched = None);
  let list : {t : node option Pref.token | valid ys && root ys === pointer
    && Pref.own t === heap ys} = list in
  let observed = observe pointer ys list in
  let reversed_nodes = observed.nodes in
  let state = observed.state in
  assert (List.map (fun n -> n.value) reversed_nodes = List.rev values);
  assert (List.for_all2 ( == ) reversed_nodes (List.rev original_nodes));
  let t = Pref.join state frame_token in
  let t : {t : node option Pref.token | valid ys && root ys === pointer
    && H.disjoint (heap ys) frame
    && Pref.own t === H.union (heap ys) frame} = t in
  let restored = reverse pointer ys frame t in
  let pointer = restored.pointer in
  let t = restored.state in
  let zs = ghost_ (rev_append ys Nil) in
  let selection = ghost_ (heap zs) in
  let _proof = ghost_ (H.partition_law selection frame) in
  let parts = Pref.split selection t in
  let list = parts.#left in
  let list : {t : node option Pref.token | valid zs && root zs === pointer
    && Pref.own t === heap zs} = list in
  let observed = observe pointer zs list in
  assert (List.for_all2 ( == ) observed.nodes original_nodes);
  let counter_value : {v : int | v = 42} =
    Pref.read counter_cell (borrow_ counter_token) in
  assert (counter_value = 42)

let () =
  List.iter check [[]; [7]; [1; 2]; [1; 2; 3; 4]; [7; 7; 7]];
  check (List.init 1000 Fun.id)
