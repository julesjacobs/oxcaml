module Dfa_equivalence : sig
  type machine = Dfa_semantics.machine
  open Dfa_semantics
  type comparison = Equivalent | Inequivalent | Comparison_limit
  [@@inductive]
  val compare : machine -> machine -> int -> comparison @ total @@ total
  val compare_complete : (left : machine) -> (right : machine) -> (limit : int) ->
    {u : unit | if valid left && valid right && labels_bounded left && labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (state_size left) (state_size right))
        (Bigint.of_int limit) <= 0 then
      (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true} @@ total
  val compare_equal : (left : machine) -> (right : machine) -> (limit : int) ->
    (word : int list) ->
    {u : unit | if compare left right limit === Equivalent then
      run left word === run right word else true} @@ total
  val comparison_witness : (left : machine) -> (right : machine) -> (limit : int) ->
    {witness : int list Ghost.t | if compare left right limit === Inequivalent then
      run left witness.ghost <> run right witness.ghost else true} @@ total
  val reduce : machine -> int -> machine option @ total @@ total
  val reduce_complete : (source : machine) -> (limit : int) ->
    {u : unit | if valid source && labels_bounded source &&
      0 < limit && limit <= 64 &&
      Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
      match reduce source limit with None -> false | Some candidate -> valid candidate
      else true} @@ total
  val reduce_preserves : (source : machine) -> (limit : int) ->
    (word : int list) ->
    {u : unit | let result = reduce source limit in
      match result with None -> true | Some candidate ->
        run source word === run candidate word} @@ total
  val reduce_minimum : (source : machine) -> (limit : int) ->
    (other : machine) ->
    (agreement : ((word : int list) ->
      {u : unit | run source word === run other word})) @ total ->
    {u : unit | let result = reduce source limit in
      match result with None -> true | Some candidate ->
        if valid other then
          Bigint.compare (state_size candidate) (state_size other) <= 0
        else true} @@ total
end
