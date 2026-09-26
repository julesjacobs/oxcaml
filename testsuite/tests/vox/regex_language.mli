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
end
