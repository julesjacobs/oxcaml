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
    ghost_ (Regex_language.lower_matches root word);
    let source = Regex_language.lower root in
    let u = () in
    (match source with None -> u | Some machine ->
      ghost_ (Dfa_equivalence.reduce_preserves machine limit word);
      u)
    : {u : unit | match Regex_language.lower root with None -> true | Some source ->
        match Dfa_equivalence.reduce source limit with None -> true | Some reduced ->
          Dfa_semantics.run reduced word === Regex_language.matches root word}) in
  let u = () in u

let (membership_implies_matching @ total) (root : Regex_semantics.t)
    (proof : Regex_semantics.Membership.evidence) (word : int list) :
    {u : unit | if Regex_semantics.Membership.valid root proof &&
      Regex_semantics.Membership.word proof === word then
      Regex_language.matches root word else true} =
  ghost_ (Regex_language.complete root word proof);
  let u = () in u

let (epsilon_matches @ total) () :
    {u : unit | Regex_language.matches Regex_semantics.Epsilon []} =
  let root = Regex_semantics.Epsilon in
  let word : int list = [] in
  let proof = Regex_semantics.Membership.Epsilon_match in
  ghost_ (Regex_semantics.Membership.valid_def root proof);
  ghost_ (Regex_semantics.Membership.word_def proof);
  ghost_ (Regex_language.complete root word proof);
  let u = () in u

let () =
  match Regex_language.lower (Regex_semantics.Symbol 7) with
  | None -> assert false
  | Some machine ->
    assert (Dfa_semantics.run machine [7]);
    assert (not (Dfa_semantics.run machine []))
