open Dfa_semantics
open Dfa_equivalence_core

let (equal_words @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (limit : int) (word : int list) :
    {u : unit | if Dfa_equivalence.compare left right limit === Dfa_equivalence.Equivalent then
      Dfa_semantics.run left word === Dfa_semantics.run right word else true} =
  ghost_ (Dfa_equivalence.compare_equal left right limit word);
  let u = () in u

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
  let u = () in u

let (distinguishing_word @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (limit : int) :
    {word : int list Ghost.t |
      if Dfa_equivalence.compare left right limit === Dfa_equivalence.Inequivalent then
        Dfa_semantics.run left word.ghost <> Dfa_semantics.run right word.ghost
      else true} =
  let word = Dfa_equivalence.comparison_witness left right limit in
  word

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
  let u = () in u

let (reduction_finishes @ total) (source : Dfa_semantics.machine) (limit : int) :
    {u : unit | if Dfa_semantics.valid source && Dfa_semantics.labels_bounded source &&
      0 < limit && limit <= 64 &&
      Bigint.compare (Dfa_semantics.state_size source) (Bigint.of_int limit) <= 0 then
      match Dfa_equivalence.reduce source limit with
      | None -> false | Some reduced -> Dfa_semantics.valid reduced
      else true} =
  ghost_ (Dfa_equivalence.reduce_complete source limit);
  let u = () in u


let () =
  let source : Dfa_semantics.machine = 0, [0, false, ([], 0)] in
  assert (Dfa_equivalence.compare source source 1 = Dfa_equivalence.Equivalent);
  (match Dfa_equivalence.reduce source 1 with
   | None -> assert false
   | Some reduced -> assert (not (Dfa_semantics.run reduced [7])))
