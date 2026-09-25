(* TEST
 has-z3;
 readonly_files = "dfa_equivalence_core.ml";
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

#use "dfa_equivalence_core.ml";;
[%%expect{|
module Dfa_equivalence :
  sig
    type row = (int * int) list * int
    type raw = int * (int * bool * row) list
    type machine : value mod immutable
    type relation = (int * int) list
    type decision = Equal of relation | Different of int list | Limit
    [@@inductive]
    type reduction_certificate =
        relation * (int * int list) list * (int * int * int list) list
    val valid : machine -> bool @@ total
    val of_raw : raw @ total -> machine option @ total @@ total
    val of_raw_valid :
      (raw : raw) ->
      {u : unit
        | match of_raw raw with
          | None -> true
          | Some machine -> valid machine}
      @@ total
    val reject_all : machine @@ total
    val run : machine -> int list -> bool @@ total
    val run_from : machine -> int -> int list -> bool @@ total
    val final : machine -> int -> bool @@ total
    val step : machine -> int -> int -> int @@ total
    val raw_final : raw -> int -> bool @@ total
    val raw_step : raw -> int -> int -> int @@ total
    val raw_view : (int * bool * row) list -> int -> bool * row @@ total
    val raw_has_key : (int * bool * row) list -> int -> bool @@ total
    val raw_unique_keys : (int * bool * row) list -> bool @@ total
    val raw_unique_cons :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      {u : unit
        | (raw_unique_keys ((key, accepting, row) :: rest)) ===
            ((not (raw_has_key rest key)) && (raw_unique_keys rest))}
      @@ total
    val raw_valid : raw -> bool @@ total
    val of_raw_raw_valid :
      (raw : raw) ->
      {u : unit
        | match of_raw raw with | None -> true | Some _ -> raw_valid raw}
      @@ total
    val raw_has_key_empty :
      (state : int) -> {u : unit | (raw_has_key [] state) === false} @@ total
    val raw_has_key_cons :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      (state : int) ->
      {u : unit
        | (raw_has_key ((key, accepting, row) :: rest) state) ===
            ((key = state) || (raw_has_key rest state))}
      @@ total
    val raw_unique_head :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      {u : unit
        | if raw_unique_keys ((key, accepting, row) :: rest)
          then not (raw_has_key rest key)
          else true}
      @@ total
    val raw_tail_key_distinct :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      (state : int) ->
      {u : unit
        | if
            (raw_unique_keys ((key, accepting, row) :: rest)) &&
              (raw_has_key rest state)
          then state <> key
          else true}
      @@ total
    val raw_unique_tail :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      {u : unit
        | if raw_unique_keys ((key, accepting, row) :: rest)
          then raw_unique_keys rest
          else true}
      @@ total
    val raw_valid_unique :
      (raw : raw) ->
      {u : unit
        | match raw with
          | (_, table) ->
              if raw_valid raw then raw_unique_keys table else true}
      @@ total
    val raw_view_empty :
      (state : int) -> {u : unit | (raw_view [] state) === (false, ([], 0))}
      @@ total
    val raw_view_cons :
      (key : int) ->
      (accepting : bool) ->
      (row : row) ->
      (rest : (int * bool * row) list) ->
      (state : int) ->
      {u : unit
        | (raw_view ((key, accepting, row) :: rest) state) ===
            (if key = state then (accepting, row) else raw_view rest state)}
      @@ total
    val raw_row_step : row -> int -> int @@ total
    val raw_edge_step : (int * int) list -> int -> int -> int @@ total
    val raw_edge_step_empty :
      (fallback : int) ->
      (letter : int) ->
      {u : unit | (raw_edge_step [] fallback letter) === fallback} @@ total
    val raw_edge_step_cons :
      (label : int) ->
      (target : int) ->
      (rest : (int * int) list) ->
      (fallback : int) ->
      (letter : int) ->
      {u : unit
        | (raw_edge_step ((label, target) :: rest) fallback letter) ===
            (if label = letter
             then target
             else raw_edge_step rest fallback letter)}
      @@ total
    val raw_final_view :
      (raw : raw) ->
      (state : int) ->
      {u : unit
        | match raw with
          | (_, table) ->
              (match raw_view table state with
               | (accepting, _) -> (raw_final raw state) === accepting)}
      @@ total
    val raw_step_view :
      (raw : raw) ->
      (state : int) ->
      (letter : int) ->
      {u : unit
        | match raw with
          | (_, table) ->
              (match raw_view table state with
               | (_, row) ->
                   (raw_step raw state letter) === (raw_row_step row letter))}
      @@ total
    val raw_row_step_edges :
      (edges : (int * int) list) ->
      (fallback : int) ->
      (letter : int) ->
      {u : unit
        | (raw_row_step (edges, fallback) letter) ===
            (raw_edge_step edges fallback letter)}
      @@ total
    val raw_run : raw -> int list -> bool @@ total
    val raw_run_from : raw -> int -> int list -> bool @@ total
    val raw_run_initial :
      (raw : raw) ->
      (word : int list) ->
      {u : unit
        | match raw with
          | (initial, _) ->
              (raw_run raw word) === (raw_run_from raw initial word)}
      @@ total
    val raw_run_from_empty :
      (raw : raw) ->
      (state : int) ->
      {u : unit | (raw_run_from raw state []) === (raw_final raw state)} @@
      total
    val raw_run_from_letter :
      (raw : raw) ->
      (state : int) ->
      (letter : int) ->
      (suffix : int list) ->
      {u : unit
        | (raw_run_from raw state (letter :: suffix)) ===
            (raw_run_from raw (raw_step raw state letter) suffix)}
      @@ total
    val of_raw_run :
      (raw : raw) ->
      (machine : machine) ->
      (word : int list) ->
      {u : unit
        | if (of_raw raw) === (Some machine)
          then (run machine word) === (raw_run raw word)
          else true}
      @@ total
    val of_raw_final :
      (raw : raw) ->
      (machine : machine) ->
      (state : int) ->
      {u : unit
        | if (of_raw raw) === (Some machine)
          then (final machine state) === (raw_final raw state)
          else true}
      @@ total
    val of_raw_step :
      (raw : raw) ->
      (machine : machine) ->
      (state : int) ->
      (letter : int) ->
      {u : unit
        | if (of_raw raw) === (Some machine)
          then (step machine state letter) === (raw_step raw state letter)
          else true}
      @@ total
    val run_from_empty :
      (machine : machine) ->
      (state : int) ->
      {u : unit | (run_from machine state []) === (final machine state)} @@
      total
    val run_from_letter :
      (machine : machine) ->
      (state : int) ->
      (letter : int) ->
      (suffix : int list) ->
      {u : unit
        | (run_from machine state (letter :: suffix)) ===
            (run_from machine (step machine state letter) suffix)}
      @@ total
    val reached : machine -> int list -> int @@ total
    val state_count : machine -> int @@ total
    val state_size : machine -> Bigint.t @@ total
    val has_state : machine -> int -> bool @@ total
    val reached_valid :
      (machine : machine) ->
      (word : int list) ->
      {u : unit
        | if valid machine
          then has_state machine (reached machine word)
          else true}
      @@ total
    val check : machine -> machine -> relation -> bool @@ total
    val valid_decision : machine -> machine -> decision -> bool @@ total
    val labels_bounded : machine -> bool @@ total
    val diagnose_comparison :
      (left : machine) ->
      (right : machine) ->
      (limit : int) ->
      {decision : decision
        | (valid_decision left right decision) &&
            (if
               (valid left) &&
                 ((valid right) &&
                    ((labels_bounded left) &&
                       ((labels_bounded right) &&
                          ((0 < limit) &&
                             ((limit <= 65536) &&
                                ((Bigint.compare
                                    (Bigint.mul (state_size left)
                                       (state_size right))
                                    (Bigint.of_int limit))
                                   <= 0))))))
             then
               match decision with
               | Limit -> false
               | Equal _ | Different _ -> true
             else true)}
      @@ total
    type comparison = Equivalent | Inequivalent | Comparison_limit
    [@@inductive]
    val compare : machine -> machine -> int -> comparison @ total @@ total
    val compare_complete :
      (left : machine) ->
      (right : machine) ->
      (limit : int) ->
      {u : unit
        | if
            (valid left) &&
              ((valid right) &&
                 ((labels_bounded left) &&
                    ((labels_bounded right) &&
                       ((0 < limit) &&
                          ((limit <= 65536) &&
                             ((Bigint.compare
                                 (Bigint.mul (state_size left)
                                    (state_size right)) (Bigint.of_int limit))
                                <= 0))))))
          then
            match compare left right limit with
            | Comparison_limit -> false
            | Equivalent | Inequivalent -> true
          else true}
      @@ total
    val compare_equal :
      (left : machine) ->
      (right : machine) ->
      (limit : int) ->
      (word : int list) ->
      {u : unit
        | if (compare left right limit) === Equivalent
          then (run left word) === (run right word)
          else true}
      @@ total
    val comparison_witness :
      (left : machine) ->
      (right : machine) ->
      (limit : int) ->
      {witness : int list Ghost.t
        | if (compare left right limit) === Inequivalent
          then
            (run left witness.Ghost.ghost) <> (run right witness.Ghost.ghost)
          else true}
      @@ total
    val decision_correct :
      (left : machine) ->
      (right : machine) ->
      (decision : decision) ->
      (word : int list) ->
      {u : unit
        | if valid_decision left right decision
          then
            match decision with
            | Equal _ -> (run left word) === (run right word)
            | Different witness -> (run left witness) <> (run right witness)
            | Limit -> true
          else true}
      @@ total
    val check_reduction : machine -> machine -> reduction_certificate -> bool
      @@ total
    val reduction_preserves :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (word : int list) ->
      {u : unit
        | if check_reduction source candidate certificate
          then (run source word) === (run candidate word)
          else true}
      @@ total
    val access_for :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (state : int) ->
      {word' : int list option
        | if
            (check_reduction source candidate certificate) &&
              (has_state candidate state)
          then
            match word' with
            | None -> false
            | Some word -> (reached candidate word) === state
          else true}
      @@ total
    val separating_for :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (p : int) ->
      (q : int) ->
      {word' : int list option
        | if
            (check_reduction source candidate certificate) &&
              ((has_state candidate p) &&
                 ((has_state candidate q) && (p <> q)))
          then
            match word' with
            | None -> false
            | Some word ->
                (run_from candidate p word) <> (run_from candidate q word)
          else true}
      @@ total
    val access_image : reduction_certificate -> machine -> int -> int option
      @@ total
    val image_valid :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (other : machine) ->
      (state : int) ->
      {u : unit
        | if
            (check_reduction source candidate certificate) &&
              ((has_state candidate state) && (valid other))
          then
            match access_image certificate other state with
            | None -> false
            | Some image -> has_state other image
          else true}
      @@ total
    val images_distinct :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (other : machine) ->
      (relation : relation) ->
      (p : int) ->
      (q : int) ->
      {u : unit
        | if
            (check_reduction source candidate certificate) &&
              ((check candidate other relation) &&
                 ((has_state candidate p) &&
                    ((has_state candidate q) && (p <> q))))
          then
            match ((access_image certificate other p),
                    (access_image certificate other q))
            with
            | (Some left, Some right) -> left <> right
            | _ -> false
          else true}
      @@ total
    val minimum_count :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (other : machine) ->
      (relation : relation) ->
      {u : unit
        | if
            (check_reduction source candidate certificate) &&
              ((check candidate other relation) && (valid other))
          then
            (Bigint.compare (state_size candidate) (state_size other)) <= 0
          else true}
      @@ total
    val minimum_count_semantic :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (other : machine) ->
      ((word : int list) ->
       {u : unit | (run candidate word) === (run other word)}) @ total ->
      {u : unit
        | if (check_reduction source candidate certificate) && (valid other)
          then
            (Bigint.compare (state_size candidate) (state_size other)) <= 0
          else true}
      @@ total
    val minimum_count_source_semantic :
      (source : machine) ->
      (candidate : machine) ->
      (certificate : reduction_certificate) ->
      (other : machine) ->
      ((word : int list) ->
       {u : unit | (run source word) === (run other word)}) @ total ->
      {u : unit
        | if (check_reduction source candidate certificate) && (valid other)
          then
            (Bigint.compare (state_size candidate) (state_size other)) <= 0
          else true}
      @@ total
    val diagnose_reduction :
      (source : machine) ->
      (limit : int) ->
      {result : (machine * reduction_certificate) option
        | (if
             (valid source) &&
               ((labels_bounded source) &&
                  ((0 < limit) &&
                     ((limit <= 64) &&
                        ((Bigint.compare (state_size source)
                            (Bigint.of_int limit))
                           <= 0))))
           then match result with | None -> false | Some _ -> true
           else true) &&
            (match result with
             | None -> true
             | Some (candidate, certificate) ->
                 check_reduction source candidate certificate)}
      @@ total
    val reduce : machine -> int -> machine option @ total @@ total
    val reduce_complete :
      (source : machine) ->
      (limit : int) ->
      {u : unit
        | if
            (valid source) &&
              ((labels_bounded source) &&
                 ((0 < limit) &&
                    ((limit <= 64) &&
                       ((Bigint.compare (state_size source)
                           (Bigint.of_int limit))
                          <= 0))))
          then
            match reduce source limit with
            | None -> false
            | Some candidate -> valid candidate
          else true}
      @@ total
    val reduce_preserves :
      (source : machine) ->
      (limit : int) ->
      (word : int list) ->
      {u : unit
        | let result = reduce source limit in
          match result with
          | None -> true
          | Some candidate -> (run source word) === (run candidate word)}
      @@ total
    val reduce_minimum :
      (source : machine) ->
      (limit : int) ->
      (other : machine) ->
      ((word : int list) ->
       {u : unit | (run source word) === (run other word)}) @ total ->
      {u : unit
        | let result = reduce source limit in
          match result with
          | None -> true
          | Some candidate ->
              if valid other
              then
                (Bigint.compare (state_size candidate) (state_size other)) <=
                  0
              else true}
      @@ total
    val check_agrees :
      (left : machine) ->
      (right : machine) ->
      (relation : relation) ->
      (word : int list) ->
      {u : unit
        | if check left right relation
          then (run left word) === (run right word)
          else true}
      @@ total
  end
|}]

let () =
  let open Dfa_equivalence in
  assert (valid reject_all);
  let singleton = reject_all in
  assert (compare singleton singleton 1 = Equivalent);
  assert (compare singleton singleton 0 = Comparison_limit);
  assert (compare singleton singleton (-1) = Comparison_limit);
  assert (compare singleton singleton 65_537 = Equivalent);
  let budget = 1 in
  let refine_ comparison = diagnose_comparison singleton singleton budget in
  (match comparison with
   | Equal _ -> ()
   | Different _ | Limit -> assert false);
  let refine_ minimized = diagnose_reduction singleton budget in
  (match minimized with
   | Some (reduced, certificate) ->
     assert (check_reduction singleton reduced certificate);
     assert (state_count reduced = 1)
   | None -> assert false);
  let (load @ total) (raw @ total) : machine @ total =
    match of_raw raw with Some machine -> machine | None -> reject_all in
  let (left @ total) = load (0,
    [0, true, ([(0, 0)], 1);
     1, false, ([], 1)]) in
  let (right @ total) = load (10,
    [10, true, ([(0, 10)], 20);
     20, false, ([], 20)]) in
  let relation = [0, 10; 1, 20] in
  assert (check left right relation);
  let comparison_budget = 4 in
  let refine_ comparison = diagnose_comparison left right comparison_budget in
  (match comparison with
   | Equal certificate -> assert (check left right certificate)
   | Different _ | Limit -> assert false);
  assert (compare left right comparison_budget = Equivalent);
  let exact_budget = 2 in
  let refine_ comparison = diagnose_comparison left right exact_budget in
  (match comparison with
   | Equal certificate -> assert (check left right certificate)
   | Different _ | Limit -> assert false);
  assert (compare left right exact_budget = Equivalent);
  let limited_budget = 1 in
  let refine_ comparison = diagnose_comparison left right limited_budget in
  (match comparison with
   | Limit -> ()
   | Equal _ | Different _ -> assert false);
  assert (compare left right limited_budget = Comparison_limit);
  List.iter (fun word ->
    assert (run left word = run right word))
    [[]; [0]; [0; 0]; [1]; [min_int]; [max_int]; [0; max_int]];
  let wrong_default = load (10,
    [10, true, ([(0, 10)], 10)]) in
  assert (not (check left wrong_default [0, 10; 1, 10]));
  assert (run left [max_int] <> run wrong_default [max_int]);
  let refine_ comparison = diagnose_comparison left wrong_default comparison_budget in
  (match comparison with
   | Different word -> assert (run left word <> run wrong_default word)
   | Equal _ | Limit -> assert false);
  assert (compare left wrong_default comparison_budget = Inequivalent);
  let clone = load (0,
    [0, true, ([(0, 2)], 1);
     1, false, ([], 1);
     2, true, ([(0, 2)], 1)]) in
  let reduction =
    ([0, 0; 1, 1; 2, 0], [0, []; 1, [1]], [0, 1, []]) in
  let p = 0 in
  let q = 1 in
  assert (check_reduction clone left reduction);
  let witness = [0; 0; max_int] in
  ghost_ (reduction_preserves clone left reduction witness);
  let refine_ access = access_for clone left reduction q in
  (match access with
   | Some word -> assert (reached left word = 1)
   | None -> assert false);
  let refine_ separation = separating_for clone left reduction p q in
  (match separation with
   | Some word -> assert (run_from left 0 word <> run_from left 1 word)
   | None -> assert false);
  ghost_ (images_distinct clone left reduction right relation p q);
  ghost_ (minimum_count clone left reduction right relation);
  assert (Bigint.compare (state_size left) (state_size right) = 0);
  assert (access_image reduction right p = Some 10);
  assert (access_image reduction right q = Some 20);
  let limit = 4 in
  let refine_ reduced_clone = diagnose_reduction clone limit in
  (match reduced_clone with
   | None -> assert false
   | Some (reduced, certificate) ->
     assert (check_reduction clone reduced certificate);
     assert (state_count reduced = 2);
     List.iter (fun word -> assert (run clone word = run reduced word))
       [[]; [0]; [1]; [0; 1]; [max_int]]);
  let exact_budget = 3 in
  let refine_ reduced_clone = diagnose_reduction clone exact_budget in
  (match reduced_clone with
   | Some (reduced, certificate) ->
     assert (check_reduction clone reduced certificate);
     assert (state_count reduced = 2)
   | None -> assert false);
  let limit = 1 in
  let refine_ limited = diagnose_reduction clone limit in
  assert (limited = None);
  let all_accept = load (0,
    [0, true, ([(min_int, 1)], 1);
     1, true, ([], 1)]) in
  let limit = 4 in
  let refine_ reduced_accept = diagnose_reduction all_accept limit in
  (match reduced_accept with
   | None -> assert false
   | Some (reduced, certificate) ->
     assert (check_reduction all_accept reduced certificate);
     assert (state_count reduced = 1);
     List.iter (fun word -> assert (run reduced word))
       [[]; [min_int]; [max_int]; [min_int; max_int]]);
  assert (not (check_reduction clone left
    ([0, 0; 1, 1; 2, 0], [0, []; 1, [1]], [])));
  assert (not (check_reduction clone left
    ([0, 0; 1, 1; 2, 0], [0, []; 1, [0]], [0, 1, []])));
  assert (not (check_reduction wrong_default left reduction));
  assert (of_raw (0, []) = None);
  assert (of_raw (0, [1, false, ([], 1)]) = None);
  assert (of_raw (0, [0, false, ([(0, 1)], 0)]) = None);
  assert (of_raw (0, [0, false, ([(0, 0); (0, 0)], 0)]) = None);
  assert (of_raw (0, [0, false, ([], 0); 0, true, ([], 0)]) = None)
;;
[%%expect{|
|}]

let () =
  let open Dfa_equivalence in
  let raw = 0,
    [0, false, ([1, 1; 2, 5], 4);
     1, false, ([1, 2], 4);
     2, false, ([1, 3], 4);
     3, true, ([], 4);
     4, false, ([], 4);
     5, false, ([1, 3], 4)] in
  let source = match of_raw raw with
    | Some source -> source
    | None -> assert false in
  let limit = 8 in
  let refine_ result = diagnose_reduction source limit in
  match result with
  | None -> assert false
  | Some (reduced, certificate) ->
    assert (check_reduction source reduced certificate);
    assert (state_count reduced = 5);
    List.iter (fun word -> assert (run source word = run reduced word))
      [[]; [1]; [1; 1]; [1; 1; 1]; [2]; [2; 1]; [2; 1; 1];
       [max_int]; [1; max_int]]
;;
[%%expect{|
|}]

let () =
  let open Dfa_equivalence in
  let first = List.init 33 (fun label -> label, 1) in
  let second = List.init 33 (fun label -> label + 33, 0) in
  let raw = 0, [0, false, (first, 0); 1, true, (second, 1)] in
  let source = match of_raw raw with
    | Some source -> source
    | None -> assert false in
  let limit = 2 in
  let refine_ result = diagnose_reduction source limit in
  match result with
  | None -> assert false
  | Some (reduced, certificate) ->
    assert (check_reduction source reduced certificate);
    assert (state_count reduced = 2)
;;
[%%expect{|
|}]

let () =
  let open Dfa_equivalence in
  let raw = 0, [0, false, ([7, 1; 9, 2], 0);
                1, true, ([], 1); 2, true, ([], 2)] in
  let source = match of_raw raw with Some value -> value | None -> assert false in
  let limit = 3 in
  let result = reduce source limit in
  (match result with
   | None -> assert false
   | Some candidate ->
     assert (valid candidate);
     assert (state_count candidate = 2);
     List.iter (fun word -> assert (run source word = run candidate word))
       [[]; [7]; [9]; [0; 7]; [7; min_int]; [max_int]]);
  let too_small = 1 in
  let result = reduce source too_small in
  assert (result = None)
;;
[%%expect{|
|}]
