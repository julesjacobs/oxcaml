(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_partition.ml";
 { bytecode; }
*)
module P = Vox_partition
module Laws = Vox_partition

let () =
  let _ = ghost_ (
  let empty : int P.bindings = [] in
  Laws.empty_model_law 0; Laws.empty_model_law 1; Laws.empty_model_law 2;
  let first = P.add_singleton empty 0 in
  Laws.add_singleton_law empty 0 0;
  Laws.add_singleton_law empty 0 2;
  Laws.add_singleton_valid empty 0;
  let second = P.add_singleton first 1 in
  Laws.add_singleton_law first 1 0;
  Laws.add_singleton_law first 1 1;
  Laws.add_singleton_law first 1 2;
  Laws.add_singleton_law empty 0 1;
  Laws.add_singleton_valid first 1;
  let merged = P.merge_classes second 0 1 0 in
  Laws.merge_classes_law second 0 1 0 0;
  Laws.merge_classes_law second 0 1 0 1;
  Laws.merge_classes_law second 0 1 0 2;
  Laws.merge_classes_valid second 0 1 0;
  let _ : {u : unit | P.representative merged 0 = 0 && P.representative merged 1 = 0 &&
    P.valid merged && P.size merged = 2Z && not (P.contains merged 2)} = () in ()) in
  ()

let () =
  let _ = ghost_ (
    let p0 : int P.bindings = [] in
    P.empty_model_law 0; P.empty_model_law 1; P.empty_model_law 2;
    let p1 = P.add_singleton p0 0 in
    P.add_singleton_valid p0 0;
    P.add_singleton_law p0 0 0; P.add_singleton_law p0 0 1;
    P.add_singleton_law p0 0 2;
    let p2 = P.add_singleton p1 1 in
    P.add_singleton_valid p1 1;
    P.add_singleton_law p1 1 0; P.add_singleton_law p1 1 1;
    P.add_singleton_law p1 1 2;
    let p3 = P.add_singleton p2 2 in
    P.add_singleton_valid p2 2;
    P.add_singleton_law p2 2 0; P.add_singleton_law p2 2 1;
    P.add_singleton_law p2 2 2;
    let before = P.merge_classes p3 0 1 0 in
    P.merge_classes_valid p3 0 1 0;
    P.merge_classes_law p3 0 1 0 0;
    P.merge_classes_law p3 0 1 0 1;
    P.merge_classes_law p3 0 1 0 2;
    P.connected_def before 0 1; P.connected_def before 0 2;
    P.joined_member_intro before 0 2 1;
    let after = P.merge_classes before 0 2 1 in
    P.joined_law before after 0 2 1 0;
    P.joined_law before after 0 2 1 1;
    P.joined_law before after 0 2 1 2;
    P.connected_def before 0 0; P.connected_def before 2 2;
    P.connected_def before 1 0;
    let _ : {u : unit | P.joined before after 0 2 1 &&
      P.representative before 0 = 0 && P.representative before 2 = 2 &&
      P.representative after 0 = 1 && P.representative after 2 = 1} = () in
    P.joined_intro after 0 2 1;
    let unchanged = P.merge_classes after 0 2 1 in
    P.joined_law after unchanged 0 2 1 0;
    P.connected_def after 0 2;
    let _ : {u : unit | P.same after unchanged} = () in
    ()) in
  ()

let () =
  let _ = ghost_ (
    let p : int P.bindings = [(0, 0); (1, 1)] in
    let q : int P.bindings = [(1, 1); (0, 0)] in
    P.lookup_def p 0; P.lookup_def p 1;
    P.lookup_def [(1, 1)] 1;
    P.lookup_def q 0; P.lookup_def q 1;
    P.lookup_def [(0, 0)] 0;
    P.size_def p; P.size_def [(1, 1)];
    P.size_def q; P.size_def [(0, 0)]; P.size_def ([] : int P.bindings);
    P.agree_def p q p; P.agree_def p q [(1, 1)]; P.agree_def p q [];
    P.agree_def p q q; P.agree_def p q [(0, 0)];
    P.same_def p q;
    let _ : {u : unit | P.same p q && not (p === q)} = () in
    ()) in
  ()
