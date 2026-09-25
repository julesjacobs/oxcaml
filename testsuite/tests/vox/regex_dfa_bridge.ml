(* TEST
 has-z3;
 readonly_files = "regex_core.ml dfa_equivalence_core.ml regex_dfa_bridge_core.ml";
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }
*)

#use "regex_core.ml";;
[%%expect{|
module Regex :
  sig
    type t =
        Empty
      | Epsilon
      | Symbol of int
      | Alt of t * t
      | Seq of t * t
      | Star of t
    [@@inductive]
    module Membership :
      sig
        type evidence =
            Epsilon_match
          | Symbol_match of int
          | Alt_left of evidence
          | Alt_right of evidence
          | Seq_match of evidence * evidence
          | Star_empty
          | Star_step of evidence * evidence
        [@@inductive]
        val word : evidence -> int list @@ total
        val valid : t -> evidence -> bool @@ total
      end
    val alt : t -> t -> t @@ total
    val nullable : t -> bool @@ total
    val derive : int -> t -> t @@ total
    val matches : t -> int list -> bool @@ total
    val sound :
      (r : t) ->
      (s : int list) ->
      {p : Membership.evidence
        | if matches r s
          then (Membership.valid r p) && ((Membership.word p) === s)
          else true}
      @@ total
    val complete :
      (r : t) ->
      (s : int list) ->
      (p : Membership.evidence) ->
      {u : unit
        | if (Membership.valid r p) && ((Membership.word p) === s)
          then matches r s
          else true}
      @@ total
    val recognize :
      (r : t) ->
      (s : int list) ->
      {result : Membership.evidence option
        | match result with
          | None -> (matches r s) === false
          | Some p ->
              (matches r s) &&
                ((Membership.valid r p) && ((Membership.word p) === s))}
      @@ total
    module Dfa :
      sig
        type automaton : value mod immutable
        type state = t list
        val compile : t @ total -> automaton @ total @@ total
        val run : automaton -> int list -> bool @@ total
        val initial : automaton -> state @@ total
        val states : automaton -> state list @@ total
        val contains_in : state -> state list -> bool @@ total
        val contains_in_empty :
          (source : state) -> {u : unit | (contains_in source []) === false}
          @@ total
        val contains_state : automaton -> state -> bool @@ total
        val contains_state_equation :
          (dfa : automaton) ->
          (source : state) ->
          {u : unit
            | (contains_state dfa source) ===
                (contains_in source (states dfa))}
          @@ total
        val output : automaton -> state -> bool @@ total
        val next : automaton -> state -> int -> state @@ total
        val compiled_next_member :
          (root : t) ->
          (source : state) ->
          (letter : int) ->
          {u : unit
            | if contains_state (compile root) source
              then
                contains_state (compile root)
                  (next (compile root) source letter)
              else true}
          @@ total
        val compiled_initial_member :
          (root : t) ->
          {u : unit | contains_state (compile root) (initial (compile root))}
          @@ total
        val compiled_empty_member :
          (root : t) -> {u : unit | contains_state (compile root) []} @@
          total
        val default : automaton -> state -> state @@ total
        val default_empty :
          (dfa : automaton) ->
          (source : state) -> {u : unit | (default dfa source) === []} @@
          total
        val labels : automaton -> state -> int list @@ total
        val label_member : int -> int list -> bool @@ total
        val label_member_empty :
          (letter : int) -> {u : unit | (label_member letter []) === false}
          @@ total
        val label_member_cons :
          (letter : int) ->
          (head : int) ->
          (rest : int list) ->
          {u : unit
            | (label_member letter (head :: rest)) ===
                ((letter = head) || (label_member letter rest))}
          @@ total
        val next_outside_labels :
          (dfa : automaton) ->
          (source : state) ->
          (letter : int) ->
          {u : unit
            | if not (label_member letter (labels dfa source))
              then (next dfa source letter) === []
              else true}
          @@ total
        val run_from : automaton -> state -> int list -> bool @@ total
        val same_state : state -> state -> bool @@ total
        val contains_in_cons :
          (source : state) ->
          (head : state) ->
          (rest : state list) ->
          {u : unit
            | (contains_in source (head :: rest)) ===
                ((same_state source head) || (contains_in source rest))}
          @@ total
        val same_state_correct :
          (left : state) ->
          (right : state) ->
          {u : unit | (same_state left right) === (left === right)} @@ total
        val run_from_empty :
          (dfa : automaton) ->
          (state : state) ->
          {u : unit | (run_from dfa state []) === (output dfa state)} @@
          total
        val run_from_letter :
          (dfa : automaton) ->
          (state : state) ->
          (letter : int) ->
          (suffix : int list) ->
          {u : unit
            | (run_from dfa state (letter :: suffix)) ===
                (run_from dfa (next dfa state letter) suffix)}
          @@ total
        val run_initial :
          (dfa : automaton) ->
          (word : int list) ->
          {u : unit | (run dfa word) === (run_from dfa (initial dfa) word)}
          @@ total
        val universe_size : t -> int @@ total
        val state_count : automaton -> int @@ total
        val correct :
          (root : t) ->
          (s : int list) ->
          {u : unit | (run (compile root) s) === (matches root s)} @@ total
        val sound :
          (root : t) ->
          (s : int list) ->
          {p : Membership.evidence
            | if run (compile root) s
              then (Membership.valid root p) && ((Membership.word p) === s)
              else true}
          @@ total
        val complete :
          (root : t) ->
          (s : int list) ->
          (p : Membership.evidence) ->
          {u : unit
            | if (Membership.valid root p) && ((Membership.word p) === s)
              then run (compile root) s
              else true}
          @@ total
      end
  end
|}]

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

#use "regex_dfa_bridge_core.ml";;
[%%expect{|
module Regex_dfa_bridge :
  sig
    val lower : Regex.Dfa.automaton -> Dfa_equivalence.machine option @@
      total
    val lower_compiled_matches :
      (root : Regex.t) ->
      (word : int list) ->
      {u : unit
        | match lower (Regex.Dfa.compile root) with
          | None -> true
          | Some machine ->
              (Dfa_equivalence.run machine word) ===
                (Regex.matches root word)}
      @@ total
  end
|}]

let () =
  let open Regex in
  let root = Alt (Symbol 1, Seq (Symbol 1, Symbol 2)) in
  let source = Dfa.compile root in
  match Regex_dfa_bridge.lower source with
  | None -> assert false
  | Some indexed ->
    assert (Dfa_equivalence.valid indexed);
    List.iter (fun word ->
      assert (Dfa.run source word = Dfa_equivalence.run indexed word))
      [[]; [1]; [1; 2]; [1; 1]; [2]; [min_int]; [max_int]]
;;
[%%expect{|
|}]

let () =
  let open Regex in
  let alphabet = Alt (Symbol 0, Symbol 1) in
  let suffix = Seq (alphabet, Seq (alphabet, Seq (alphabet, alphabet))) in
  let lookback = Seq (Star alphabet, Seq (Symbol 0, suffix)) in
  let source = Dfa.compile lookback in
  assert (Dfa.state_count source = 4096);
  match Regex_dfa_bridge.lower source with
  | None -> assert false
  | Some indexed ->
    assert (Dfa_equivalence.valid indexed);
    List.iter (fun word ->
      assert (Dfa.run source word = Dfa_equivalence.run indexed word))
      [[]; [0]; [1]; [0; 0; 0; 0; 0];
       [1; 0; 1; 0; 1; 0]; [max_int; 0; 1; 0; 1]]
;;
[%%expect{|
|}]
