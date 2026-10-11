open Dfa_semantics
open Dfa_equivalence_core
open Regex_semantics
open Regex_language

let (minimized_regex @ total) (root : Regex_semantics.t) (limit : int)
    (word : int list) :
    {u : unit | match Regex_language.lower root with None -> true | Some source ->
      match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
        Dfa_semantics.run reduced word === Regex_language.matches root word} =
  let _proof = ghost_ (
    Regex_language.lower_matches root word;
    let source = Regex_language.lower root in
    (match source with
     | None -> ()
     | Some machine -> Dfa_equivalence.reduce_preserves machine limit word; ())
    : {u : unit | match Regex_language.lower root with None -> true | Some source ->
        match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
          Dfa_semantics.run reduced word === Regex_language.matches root word}) in
  ()

let (lowered_reduction_finishes @ total) (root : Regex_semantics.t)
    (limit : int) :
    {u : unit | match Regex_language.lower root with None -> true | Some source ->
      if Dfa_semantics.labels_bounded source && 0 < limit && limit <= 64 &&
        Bigint.compare (Dfa_semantics.state_size source)
          (Bigint.of_int limit) <= 0 then
        match Dfa_equivalence.reduce source limit with
        | None -> false | Some reduced -> Dfa_semantics.valid reduced
      else true} =
  let _proof = ghost_ (
    Regex_language.lower_valid root;
    (match Regex_language.lower root with
     | None -> ()
     | Some source -> Dfa_equivalence.reduce_complete source limit; ())
    : {u : unit | match Regex_language.lower root with None -> true | Some source ->
        if Dfa_semantics.labels_bounded source && 0 < limit && limit <= 64 &&
          Bigint.compare (Dfa_semantics.state_size source)
            (Bigint.of_int limit) <= 0 then
          match Dfa_equivalence.reduce source limit with
          | None -> false | Some reduced -> Dfa_semantics.valid reduced
        else true}) in
  ()

let (minimized_no_larger @ total) (root : Regex_semantics.t) (limit : int) :
    {u : unit | match Regex_language.lower root with None -> true | Some source ->
      match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
        Bigint.compare (Dfa_semantics.state_size reduced)
          (Dfa_semantics.state_size source) <= 0} =
  let _proof = ghost_ (
    Regex_language.lower_valid root;
    (match Regex_language.lower root with
     | None -> ()
     | Some source ->
       let (agreement @ total) (word : int list) :
           {u : unit | Dfa_semantics.run source word ===
             Dfa_semantics.run source word} =
         () in
       Dfa_equivalence.reduce_minimum source limit source agreement; ())
    : {u : unit | match Regex_language.lower root with None -> true | Some source ->
        match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
          Bigint.compare (Dfa_semantics.state_size reduced)
            (Dfa_semantics.state_size source) <= 0}) in
  ()

let (membership_implies_matching @ total) (root : Regex_semantics.t)
    (proof : Regex_semantics.Membership.evidence) (word : int list) :
    {u : unit | if Regex_semantics.Membership.valid root proof &&
      Regex_semantics.Membership.word proof === word then
      Regex_language.matches root word else true} =
  ghost_ (Regex_language.complete root word proof);
  ()

let (epsilon_matches @ total) () :
    {u : unit | Regex_language.matches Regex_semantics.Epsilon []} =
  let root = Regex_semantics.Epsilon in
  let word : int list = [] in
  let proof = Regex_semantics.Membership.Epsilon_match in
  ghost_ (Regex_semantics.Membership.valid_def root proof);
  ghost_ (Regex_semantics.Membership.word_def proof);
  ghost_ (Regex_language.complete root word proof);
  ()

let () =
  match Regex_language.lower (Regex_semantics.Symbol 7) with
  | None -> assert false
  | Some machine ->
    assert (Dfa_semantics.run machine [7]);
    assert (not (Dfa_semantics.run machine []))

let () =
  let literal = Regex_semantics.Symbol 7 in
  let repeated = Regex_semantics.Alt (literal, literal) in
  assert (literal <> repeated);
  List.iter (fun word ->
    assert (Regex_language.matches literal word = Regex_language.matches repeated word))
    [[]; [7]; [8]; [7; 7]]
