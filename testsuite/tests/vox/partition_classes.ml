(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_partition.ml vox_partition_classes.ml vox_partition_classes_proof.ml vox_partition_classes_bridge.ml vox_partition_transport_proof.ml vox_partition_classes_group.ml";
 { bytecode; }
*)
module P = Vox_partition_classes
module L = Vox_partition_classes_proof

let (snapshot_representative @ total)
    (p : int P.t) (x : int) :
    {u : unit | if P.contains p x then
      P.connected p x (P.representative p x) else true} @ ghost = ghost_ (
  L.representative_law p x;
  P.connected_def p x (P.representative p x); ())

let (union_existing_class @ total)
    (before : int P.t) (after : int P.t) (x : int) (y : int) (r : int) :
    {u : unit | if P.joined before after x y r && P.connected before x y
      then P.same before after && r === P.representative before x
      else true} @ ghost = ghost_ (
  L.joined_law before after x y r x;
  L.same_law before after x;
  P.connected_def before x x; ())

module C = Vox_partition_classes
module B = Vox_partition
module Bridge = Vox_partition_classes_bridge

let (union_equivalence @ total)
    (before : int C.t) (after : int C.t) (x : int) (y : int) (r : int) :
    {u : unit | C.joined before after x y r =
      B.joined (Bridge.flatten before) (Bridge.flatten after) x y r} @ ghost =
  ghost_ (Bridge.flatten_joined before after x y r)

let (snapshot_equivalence @ total) (p : int C.t) (x : int) :
    {u : unit | B.valid (Bridge.flatten p) &&
      B.size (Bridge.flatten p) = C.size p &&
      B.contains (Bridge.flatten p) x = C.contains p x &&
      B.representative (Bridge.flatten p) x === C.representative p x} @ ghost =
  ghost_ (
    Bridge.flatten_valid p; Bridge.flatten_size p;
    Bridge.flatten_observation p x)

let () =
  let _ = ghost_ (
    C.members_def ([] : int C.classes); C.distinct_def ([] : int list);
    let _ : int C.t = [] in
    let singleton = [(7, [])] in
    C.members_def singleton; C.append_def ([] : int list) [];
    C.distinct_def [7]; C.mem_def [] 7;
    let _ : int C.t = singleton in
    let duplicate = [(7, [7])] in
    C.members_def duplicate; C.append_def [7] []; C.append_def ([] : int list) [];
    C.distinct_def [7; 7]; C.mem_def [7] 7;
    let _ : {u : unit | not (C.distinct (C.members duplicate))} = () in
    ()) in ()

module Group = Vox_partition_classes_group

let (grouped_snapshot @ total) (p : int B.t) : int C.t @ immutable ghost = ghost_ (
  Group.group_valid p; Group.group p)

let (grouped_observations @ total) (p : int B.t) (x : int) :
    {u : unit | C.size (Group.group p) = B.size p &&
      C.contains (Group.group p) x = B.contains p x &&
      C.representative (Group.group p) x === B.representative p x} @ ghost =
  ghost_ (
    Group.group_size p; Group.group_contains p x; Group.group_root p x)

let[@def] original (u : unit @ immutable) = ghost_ ([(1, [2; 3]); (4, [5]); (6, [])])
let[@def] reordered (u : unit @ immutable) = ghost_ ([(6, []); (4, [5]); (1, [3; 2])])
let[@def] merged (u : unit @ immutable) = ghost_ ([(2, [1; 3; 4; 5]); (6, [])])

let (original_lookup @ total) (q : int) :
    {u : unit | C.lookup (original ()) q ===
      (if q = 1 || q = 2 || q = 3 then Some 1
       else if q = 4 || q = 5 then Some 4
       else if q = 6 then Some 6 else None)} @ ghost = ghost_ (
  original_def ();
  C.lookup_def [(1, [2; 3]); (4, [5]); (6, [])] q;
  C.lookup_def [(4, [5]); (6, [])] q; C.lookup_def [(6, [])] q;
  C.lookup_def [] q;
  C.mem_def [2; 3] q; C.mem_def [3] q; C.mem_def [5] q; C.mem_def [] q; ())

let (reordered_lookup @ total) (q : int) :
    {u : unit | C.lookup (reordered ()) q === C.lookup (original ()) q}
    @ ghost = ghost_ (
  reordered_def (); original_lookup q;
  C.lookup_def [(6, []); (4, [5]); (1, [3; 2])] q;
  C.lookup_def [(4, [5]); (1, [3; 2])] q; C.lookup_def [(1, [3; 2])] q;
  C.lookup_def [] q;
  C.mem_def [3; 2] q; C.mem_def [2] q; C.mem_def [5] q; C.mem_def [] q; ())

let (merged_lookup @ total) (q : int) :
    {u : unit | C.lookup (merged ()) q ===
      (if q = 1 || q = 2 || q = 3 || q = 4 || q = 5 then Some 2
       else if q = 6 then Some 6 else None)} @ ghost = ghost_ (
  merged_def ();
  C.lookup_def [(2, [1; 3; 4; 5]); (6, [])] q;
  C.lookup_def [(6, [])] q; C.lookup_def [] q;
  C.mem_def [1; 3; 4; 5] q; C.mem_def [3; 4; 5] q;
  C.mem_def [4; 5] q; C.mem_def [5] q; C.mem_def [] q; ())

let (fixture_sizes @ total) () :
    {u : unit | C.size (original ()) = 6Z && C.size (reordered ()) = 6Z &&
      C.size (merged ()) = 6Z} @ ghost = ghost_ (
  original_def (); reordered_def (); merged_def ();
  C.size_def (original ()); C.size_def (reordered ()); C.size_def (merged ());
  C.members_def [(1, [2; 3]); (4, [5]); (6, [])];
  C.members_def [(4, [5]); (6, [])]; C.members_def [(6, [])];
  C.members_def [(6, []); (4, [5]); (1, [3; 2])];
  C.members_def [(4, [5]); (1, [3; 2])]; C.members_def [(1, [3; 2])];
  C.members_def [(2, [1; 3; 4; 5]); (6, [])]; C.members_def ([] : int C.classes);
  C.append_def ([] : int list) []; C.append_def [] [4; 5; 1; 3; 2];
  C.append_def [2; 3] [4; 5; 6]; C.append_def [3] [4; 5; 6];
  C.append_def [5] [6]; C.append_def [] [6]; C.append_def [] [4; 5; 6];
  C.append_def [5] [1; 3; 2]; C.append_def [] [1; 3; 2];
  C.append_def [3; 2] []; C.append_def [2] [];
  C.append_def [1; 3; 4; 5] [6]; C.append_def [3; 4; 5] [6];
  C.append_def [4; 5] [6];
  C.length_def [1; 2; 3; 4; 5; 6]; C.length_def [2; 3; 4; 5; 6];
  C.length_def [3; 4; 5; 6]; C.length_def [4; 5; 6];
  C.length_def [5; 6]; C.length_def [6]; C.length_def ([] : int list);
  C.length_def [6; 4; 5; 1; 3; 2]; C.length_def [4; 5; 1; 3; 2];
  C.length_def [5; 1; 3; 2]; C.length_def [1; 3; 2];
  C.length_def [3; 2]; C.length_def [2];
  C.length_def [2; 1; 3; 4; 5; 6]; C.length_def [1; 3; 4; 5; 6]; ())

let rec (reordering_agree @ total) (queries : int list @ immutable) :
    {u : unit | C.agree (original ()) (reordered ()) queries} @ ghost = ghost_ (
  C.agree_def (original ()) (reordered ()) queries;
  match queries with [] -> () | q :: rest -> reordered_lookup q; reordering_agree rest)

let (class_and_member_reordering @ total) () :
    {u : unit | C.same (original ()) (reordered ())} @ ghost = ghost_ (
  fixture_sizes (); reordering_agree (C.members (original ()));
  reordering_agree (C.members (reordered ()));
  C.same_def (original ()) (reordered ()); ())

let (original_observation @ total) (q : int) :
    {u : unit | C.contains (original ()) q = (1 <= q && q <= 6) &&
      C.representative (original ()) q ===
        (if q = 1 || q = 2 || q = 3 then 1 else if q = 4 || q = 5 then 4 else q)}
    @ ghost = ghost_ (
  original_lookup q; C.contains_def (original ()) q;
  C.representative_def (original ()) q; ())

let rec (member_merge_queries @ total) (queries : int list @ immutable) :
    {u : unit | C.merged (original ()) (merged ()) 1 4 2 queries} @ ghost = ghost_ (
  C.merged_def (original ()) (merged ()) 1 4 2 queries;
  match queries with
  | [] -> ()
  | q :: rest ->
      original_lookup q; merged_lookup q;
      original_observation q; original_observation 1; original_observation 4;
      C.connected_def (original ()) q 1; C.connected_def (original ()) q 4;
      member_merge_queries rest)

let (member_representative @ total) () :
    {u : unit | C.joined (original ()) (merged ()) 1 4 2} @ ghost = ghost_ (
  fixture_sizes (); original_observation 1; original_observation 2;
  original_observation 4; C.connected_def (original ()) 1 2;
  C.connected_def (original ()) 4 2; C.connected_def (original ()) 1 4;
  member_merge_queries (C.members (original ()));
  member_merge_queries (C.members (merged ()));
  C.joined_def (original ()) (merged ()) 1 4 2; ())

let (no_op_representative @ total) () :
    {u : unit | not (C.joined (original ()) (original ()) 1 3 2)} @ ghost = ghost_ (
  original_observation 1; original_observation 2; original_observation 3;
  C.connected_def (original ()) 1 3;
  C.joined_def (original ()) (original ()) 1 3 2; ())

let (cannot_absorb_third_class @ total) () :
    {u : unit | not (C.joined (original ()) [(2, [1; 3; 4; 5; 6])] 1 4 2)}
    @ ghost = ghost_ (
  let after = [(2, [1; 3; 4; 5; 6])] in
  Vox_partition_classes_proof.joined_law (original ()) after 1 4 2 6;
  original_observation 1; original_observation 4; original_observation 6;
  C.connected_def (original ()) 6 1; C.connected_def (original ()) 6 4;
  C.representative_def after 6; C.lookup_def after 6;
  C.mem_def [1; 3; 4; 5; 6] 6; C.mem_def [3; 4; 5; 6] 6;
  C.mem_def [4; 5; 6] 6; C.mem_def [5; 6] 6; C.mem_def [6] 6; ())

let (cannot_drop_member @ total) () :
    {u : unit | not (C.joined (original ()) [(2, [1; 4; 5]); (6, [])] 1 4 2)}
    @ ghost = ghost_ (
  let after = [(2, [1; 4; 5]); (6, [])] in
  Vox_partition_classes_proof.joined_law (original ()) after 1 4 2 3;
  original_observation 3; C.contains_def after 3; C.lookup_def after 3;
  C.lookup_def [(6, [])] 3; C.lookup_def [] 3;
  C.mem_def [1; 4; 5] 3; C.mem_def [4; 5] 3; C.mem_def [5] 3;
  C.mem_def [] 3; ())

let (cannot_add_member @ total) () :
    {u : unit | not (C.joined (original ()) [(2, [1; 3; 4; 5; 7]); (6, [])] 1 4 2)}
    @ ghost = ghost_ (
  let after = [(2, [1; 3; 4; 5; 7]); (6, [])] in
  Vox_partition_classes_proof.joined_law (original ()) after 1 4 2 7;
  original_observation 7; C.contains_def after 7; C.lookup_def after 7;
  C.mem_def [1; 3; 4; 5; 7] 7; C.mem_def [3; 4; 5; 7] 7;
  C.mem_def [4; 5; 7] 7; C.mem_def [5; 7] 7; C.mem_def [7] 7; ())

let (unrelated_representative @ total)
    (before : int C.t) (after : int C.t) (x : int) (y : int) (r : int) (q : int) :
    {u : unit | if C.joined before after x y r &&
      not (C.connected before q x) && not (C.connected before q y) then
      C.representative after q === C.representative before q else true} @ ghost =
    ghost_ (Vox_partition_classes_proof.joined_law before after x y r q)
