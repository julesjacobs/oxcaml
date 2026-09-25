open Dfa_semantics
open Dfa_equivalence_core
open Regex_semantics
open Regex_language

let (equal_words @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (limit : int) (word : int list) :
    {u : unit | if Dfa_equivalence.compare left right limit === Dfa_equivalence.Equivalent then
      Dfa_semantics.run left word === Dfa_semantics.run right word else true} =
  ghost_ (Dfa_equivalence.compare_equal left right limit word);
  let u = () in refine_ u

let (comparison_finishes @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (limit : int) :
    {u : unit | if Dfa_semantics.valid left && Dfa_semantics.valid right &&
      Dfa_semantics.labels_bounded left && Dfa_semantics.labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (Dfa_semantics.state_size left)
        (Dfa_semantics.state_size right)) (Bigint.of_int limit) <= 0 then
      match Dfa_equivalence.compare left right limit with
      | Dfa_equivalence.Comparison_limit -> false
      | Dfa_equivalence.Equivalent | Dfa_equivalence.Inequivalent -> true
      else true} =
  ghost_ (Dfa_equivalence.compare_complete left right limit);
  let u = () in refine_ u

let (distinguishing_word @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (limit : int) :
    {word : int list Ghost.t |
      if Dfa_equivalence.compare left right limit === Dfa_equivalence.Inequivalent then
        Dfa_semantics.run left word.ghost <> Dfa_semantics.run right word.ghost
      else true} =
  let refine_ word = Dfa_equivalence.comparison_witness left right limit in
  refine_ word

let (minimum @ total) (source : Dfa_semantics.machine) (limit : int)
    (other : Dfa_semantics.machine)
    (agreement : ((word : int list) ->
      {u : unit | Dfa_semantics.run source word === Dfa_semantics.run other word}) @ total) :
    {u : unit | match Dfa_equivalence.reduce source limit with
      | None -> true
      | Some reduced -> if Dfa_semantics.valid other then
        Bigint.compare (Dfa_semantics.state_size reduced)
          (Dfa_semantics.state_size other) <= 0 else true} =
  ghost_ (Dfa_equivalence.reduce_minimum source limit other agreement);
  let u = () in refine_ u

let (reduction_finishes @ total) (source : Dfa_semantics.machine) (limit : int) :
    {u : unit | if Dfa_semantics.valid source && Dfa_semantics.labels_bounded source &&
      0 < limit && limit <= 64 &&
      Bigint.compare (Dfa_semantics.state_size source) (Bigint.of_int limit) <= 0 then
      match Dfa_equivalence.reduce source limit with
      | None -> false | Some reduced -> Dfa_semantics.valid reduced
      else true} =
  ghost_ (Dfa_equivalence.reduce_complete source limit);
  let u = () in refine_ u

let (minimized_regex @ total) (root : Regex_semantics.t) (limit : int)
    (word : int list) :
    {u : unit | match Regex_language.lower root with None -> true | Some source ->
      match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
        Dfa_semantics.run reduced word === Regex_language.matches root word} =
  let refine_ _proof = ghost_ (
    ghost_ (Regex_language.lower_matches root word);
    let source = Regex_language.lower root in
    let u = () in
    (match source with None -> refine_ u | Some machine ->
      ghost_ (Dfa_equivalence.reduce_preserves machine limit word);
      refine_ u)
    : {u : unit | match Regex_language.lower root with None -> true | Some source ->
        match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
          Dfa_semantics.run reduced word === Regex_language.matches root word}) in
  let u = () in refine_ u

let (membership_implies_matching @ total) (root : Regex_semantics.t)
    (proof : Regex_semantics.Membership.evidence) (word : int list) :
    {u : unit | if Regex_semantics.Membership.valid root proof &&
      Regex_semantics.Membership.word proof === word then
      Regex_language.matches root word else true} =
  ghost_ (Regex_language.complete root word proof);
  let u = () in refine_ u

let (epsilon_matches @ total) () :
    {u : unit | Regex_language.matches Regex_semantics.Epsilon []} =
  let root = Regex_semantics.Epsilon in
  let word : int list = [] in
  let proof = Regex_semantics.Membership.Epsilon_match in
  ghost_ (Regex_semantics.Membership.valid_def root proof);
  ghost_ (Regex_semantics.Membership.word_def proof);
  ghost_ (Regex_language.complete root word proof);
  let u = () in refine_ u

let () =
  let source : Dfa_semantics.machine = 0, [0, false, ([], 0)] in
  assert (Dfa_equivalence.compare source source 1 = Dfa_equivalence.Equivalent);
  (match Dfa_equivalence.reduce source 1 with
   | None -> assert false
   | Some reduced -> assert (not (Dfa_semantics.run reduced [7])));
  match Regex_language.lower (Regex_semantics.Symbol 7) with
  | None -> assert false
  | Some machine ->
    assert (Dfa_semantics.run machine [7]);
    assert (not (Dfa_semantics.run machine []))
