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
end = struct
  type machine = Dfa_semantics.machine
  open Dfa_semantics
  type comparison = Dfa_proof.comparison = Equivalent | Inequivalent | Comparison_limit
  [@@inductive]
  let[@def] (compare @ total) (left : machine) (right : machine) (limit : int) : comparison @ total =
    Dfa_proof.compare left right limit
  let[@def] (reduce @ total) (source : machine) (limit : int) : machine option @ total =
    Dfa_proof.reduce source limit
  let (compare_complete @ total) (left : machine) (right : machine) (limit : int) :
    {u : unit | if valid left && valid right && labels_bounded left && labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (state_size left) (state_size right))
        (Bigint.of_int limit) <= 0 then
      (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true} =
    let _proof = ghost_ (
      ghost_ (compare_def left right limit);
      ghost_ (Dfa_proof.compare_complete left right limit);
      let u = () in u
      : {u : unit | if valid left && valid right && labels_bounded left && labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (state_size left) (state_size right))
        (Bigint.of_int limit) <= 0 then
      (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true}) in
    let u = () in u
  let (compare_equal @ total) (left : machine) (right : machine) (limit : int)
      (word : int list) :
    {u : unit | if compare left right limit === Equivalent then
      run left word === run right word else true} =
    let _proof = ghost_ (
      ghost_ (compare_def left right limit);
      ghost_ (Dfa_proof.compare_equal left right limit word);
      let u = () in u
      : {u : unit | if compare left right limit === Equivalent then
      run left word === run right word else true}) in
    let u = () in u
  let (comparison_witness @ total) (left : machine) (right : machine) (limit : int) :
    {witness : int list Ghost.t | if compare left right limit === Inequivalent then
      run left witness.ghost <> run right witness.ghost else true} =
    let witness = ghost_ (
      ghost_ (compare_def left right limit);
      let witness = Dfa_proof.comparison_witness left right limit in
      let word = witness.Ghost.ghost in
      word
      : {word : int list | if compare left right limit === Inequivalent then
          run left word <> run right word else true}) in
    let result : int list Ghost.t = { ghost = witness } in result
  let (reduce_complete @ total) (source : machine) (limit : int) :
      {u : unit | if valid source && labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match reduce source limit with None -> false | Some candidate -> valid candidate
        else true} =
    let _proof = ghost_ (
      ghost_ (reduce_def source limit);
      ghost_ (Dfa_proof.reduce_complete source limit);
      let u = () in u
      : {u : unit | if valid source && labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match reduce source limit with None -> false | Some candidate -> valid candidate
        else true}) in
    let u = () in u
  let (reduce_preserves @ total) (source : machine) (limit : int)
      (word : int list) :
      {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          run source word === run candidate word} =
    let _proof = ghost_ (
      ghost_ (reduce_def source limit);
      ghost_ (Dfa_proof.reduce_preserves source limit word);
      let u = () in u
      : {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          run source word === run candidate word}) in
    let u = () in u
  let (reduce_minimum @ total) (source : machine) (limit : int)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run source word === run other word}) @ total) :
      {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          if valid other then
            Bigint.compare (state_size candidate) (state_size other) <= 0
          else true} =
    let _proof = ghost_ (
      ghost_ (reduce_def source limit);
      ghost_ (Dfa_proof.reduce_minimum source limit other agreement);
      let u = () in u
      : {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          if valid other then
            Bigint.compare (state_size candidate) (state_size other) <= 0
          else true}) in
    let u = () in u
end;;
