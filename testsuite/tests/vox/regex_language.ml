module Regex_language : sig
  open Regex_semantics
  val matches : t -> int list -> bool @@ total
  val sound : (root : t) -> (word : int list) ->
    {proof : Membership.evidence Ghost.t | if matches root word then
      Membership.valid root proof.ghost && Membership.word proof.ghost === word else true} @@ total
  val complete : (root : t) -> (word : int list) -> (proof : Membership.evidence) ->
    {u : unit | if Membership.valid root proof && Membership.word proof === word then
      matches root word else true} @@ total
  val lower : t @ total -> Dfa_semantics.machine option @@ total
  val lower_matches : (root : t) -> (word : int list) ->
    {u : unit | match lower root with None -> true | Some machine ->
      Dfa_semantics.run machine word === matches root word} @@ total
end = struct
  open Regex_semantics
  let[@def] matches root word = Regex.matches root word
  let (sound @ total) (root : t) (word : int list) :
    {proof : Membership.evidence Ghost.t | if matches root word then
      Membership.valid root proof.ghost && Membership.word proof.ghost === word else true} =
    let refine_ proof = ghost_ (
      ghost_ (matches_def root word);
      let refine_ proof = Regex.sound root word in
      ghost_ (Regex.membership_valid root proof);
      ghost_ (Regex.membership_word proof);
      refine_ proof
      : {proof : Membership.evidence | if matches root word then
        Membership.valid root proof && Membership.word proof === word else true}) in
    let result : Membership.evidence Ghost.t = { ghost = proof } in refine_ result
  let (complete @ total) (root : t) (word : int list) (proof : Membership.evidence) :
    {u : unit | if Membership.valid root proof && Membership.word proof === word then
      matches root word else true} =
    let refine_ _proof = ghost_ (
      ghost_ (matches_def root word);
      ghost_ (Regex.membership_valid root proof);
      ghost_ (Regex.membership_word proof);
      ghost_ (Regex.complete root word proof);
      let u = () in refine_ u
      : {u : unit | if Membership.valid root proof && Membership.word proof === word then
        matches root word else true}) in
    let u = () in refine_ u
  let[@def] (lower @ total) (root : t @ total) =
    let dfa = Regex.Dfa.compile root in
    Regex_dfa_bridge.lower dfa
  let (lower_matches @ total) (root : t) (word : int list) :
    {u : unit | match lower root with None -> true | Some machine ->
      Dfa_semantics.run machine word === matches root word} =
    let refine_ _proof = ghost_ (
      ghost_ (lower_def root);
      ghost_ (matches_def root word);
      ghost_ (Regex_dfa_bridge.lower_compiled_matches root word);
      let u = () in refine_ u
      : {u : unit | match lower root with None -> true | Some machine ->
        Dfa_semantics.run machine word === matches root word}) in
    let u = () in refine_ u
end;;
