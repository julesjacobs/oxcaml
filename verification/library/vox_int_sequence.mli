open Vox_sequence


type multiset : immutable_data

val element : int list -> Bigint.t -> int @@ total
val element_def : (values : int list) -> (index : Bigint.t) ->
  {u : unit | element values index ===
    (match at values index with Some value -> value | None -> 0)} @@ total
val accepts : int -> int -> bool -> bool @@ total
val accepts_def : (value : int) -> (bound : int) -> (lower : bool) ->
  {u : unit | accepts value bound lower ===
    (if lower then value <= bound else bound <= value)} @@ total
val all : int list -> int -> bool -> bool @@ total
val all_le : int list -> int -> bool @@ total
val all_ge : int list -> int -> bool @@ total
val range : int list -> int -> bool -> Bigint.t -> Bigint.t -> bool @@ total
val sorted : int list -> bool @@ total
val all_def : (values : int list) -> (bound : int) -> (lower : bool) ->
  {u : unit | all values bound lower ===
    (match values with [] -> true | head :: tail ->
      accepts head bound lower && all tail bound lower)} @@ total
val all_le_def : (values : int list) -> (bound : int) ->
  {u : unit | all_le values bound === all values bound true} @@ total
val all_ge_def : (values : int list) -> (bound : int) ->
  {u : unit | all_ge values bound === all values bound false} @@ total
val range_def : (values : int list) -> (bound : int) -> (lower : bool) ->
  (first : Bigint.t) -> (past : Bigint.t) ->
  {u : unit | range values bound lower first past ===
    (if past <= 0Z then true else match values with
     | [] -> true
     | head :: tail -> (first > 0Z || accepts head bound lower)
       && range tail bound lower
         (if first > 0Z then Bigint.sub first 1Z else 0Z)
         (Bigint.sub past 1Z))} @@ total
val sorted_def : (values : int list) ->
  {u : unit | sorted values ===
    (match values with [] -> true | head :: tail ->
      all tail head false && sorted tail)} @@ total
val bag : int list -> multiset @@ total
val permutation : int list -> int list -> bool @ ghost @@ total
val permutation_def : (left : int list) -> (right : int list) ->
  {u : unit | permutation left right ===
    ghost_ (bag left === bag right)} @@ total
val count : int list -> int -> Bigint.t @@ total
val count_def : (values : int list) -> (target : int) ->
  {u : unit | count values target ===
    (match values with [] -> 0Z | head :: tail ->
      Bigint.add (if head = target then 1Z else 0Z) (count tail target))} @@
        total

val element_at : (values : int list) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values then
      at values index === Some (element values index) else true} @@ total

val swap_equation : (values : int list) ->
    (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values then
      swap values first second ===
        set (set values first (element values second)) second (element values
          first)
      else true} @@ total

val element_set : (values : int list) ->
    (index : Bigint.t) -> (query : Bigint.t) -> (value : int) ->
    {u : unit | if 0Z <= index && index < length values
      && 0Z <= query && query < length values then
      element (set values index value) query ===
        (if index = query then value else element values query)
      else true} @@ total

val range_empty : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && past <= first then
      range values bound lower first past else true} @@ total

val range_shrink : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    (new_first : Bigint.t) -> (new_past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= new_first && new_past <= past
      && range values bound lower first past then
      range values bound lower new_first new_past else true} @@ total

val range_get : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) -> (index :
      Bigint.t) ->
    {u : unit | if 0Z <= first && first <= index && index < past
      && index < length values && range values bound lower first past then
      accepts (element values index) bound lower else true} @@ total

val range_grow : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= past && past < length values
      && range values bound lower first past
      && accepts (element values past) bound lower then
      range values bound lower first ((Bigint.add past 1Z)) else true} @@ total

val range_set : (values : int list) -> (bound : int) ->
    (lower : bool) -> (first : Bigint.t) -> (past : Bigint.t) ->
    (index : Bigint.t) -> (value : int) ->
    {u : unit | if 0Z <= first && 0Z <= index
      && range values bound lower first past
      && (index < first || past <= index || accepts value bound lower) then
      range (set values index value) bound lower first past else true} @@ total

val element_equation : (values : int list) -> (index : Bigint.t) ->
    {u : unit | element values index ===
      (match values with [] -> 0 | head :: tail ->
        if index = 0Z then head else element tail ((Bigint.sub index 1Z)))} @@
          total

val permutation_swap : (values : int list) ->
    (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values then
      permutation values (swap values first second) else true} @@ total

val permutation_refl : (values : int list) ->
    {u : unit | permutation values values} @@ total

val permutation_trans : (first : int list) -> (second : int list) ->
    (third : int list) ->
    {u : unit | if permutation first second && permutation second third then
      permutation first third else true} @@ total

val element_swap : (values : int list) -> (first : Bigint.t) ->
    (second : Bigint.t) -> (query : Bigint.t) ->
    {u : unit | if 0Z <= first && first < length values
      && 0Z <= second && second < length values
      && 0Z <= query && query < length values then
      element (swap values first second) query ===
        (if query = first then element values second
         else if query = second then element values first else element values
           query)
      else true} @@ total

val range_sub : (values : int list) -> (bound : int) -> (lower : bool) ->
    (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if 0Z <= first && first <= past && past <= length values then
      range values bound lower first past === all (sub values first past) bound
        lower
      else true} @@ total

val all_append : (left : int list) -> (right : int list) ->
    (bound : int) -> (lower : bool) ->
    {u : unit | all (append left right) bound lower ===
      (all left bound lower && all right bound lower)} @@ total

val all_permutation : (before : int list) -> (after : int list) ->
    (bound : int) -> (lower : bool) ->
    {u : unit | if permutation before after then
      all before bound lower === all after bound lower else true} @@ total

val all_weaken : (values : int list) -> (old_bound : int) ->
    (new_bound : int) -> (lower : bool) ->
    {u : unit | if (if lower then old_bound <= new_bound else new_bound <=
      old_bound)
      && all values old_bound lower then all values new_bound lower else true}
        @@ total

val sorted_short : (values : int list) ->
    {u : unit | if length values <= 1Z then sorted values else true} @@ total

val sorted_glue : (left : int list) -> (pivot : int) -> (right : int list) ->
    {u : unit | if sorted left && sorted right
      && all left pivot true && all right pivot false then
      sorted (append left (pivot :: right)) else true} @@ total

val permutation_append : (left : int list) -> (right : int list) ->
    (new_left : int list) -> (new_right : int list) ->
    {u : unit | if permutation left new_left && permutation right new_right then
      permutation (append left right) (append new_left new_right) else true} @@
        total

val sub_one : (values : int list) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values then
      sub values index ((Bigint.add index 1Z)) === [element values index] else
        true} @@
        total

val permutation_count : (before : int list) -> (after : int list) -> (target :
  int) ->
    {u : unit | if permutation before after then count before target === count
      after target
      else true} @@ total

val multiplicity : multiset -> int -> Bigint.t @@ total

val count_nonnegative : (values : int list) -> (target : int) ->
    {u : unit | 0Z <= count values target} @@ total

val count_extensional : (left : int list) -> (right : int list) ->
    ((target : int) ->
      {u : unit | count left target === count right target}) @ total ->
    {u : unit | permutation left right} @@ total

val bag_multiplicity : (values : int list) -> (target : int) ->
    {u : unit | multiplicity (bag values) target === count values target} @@
      total

val count_append : (left : int list) -> (right : int list) ->
    (target : int) ->
    {u : unit | count (append left right) target ===
      Bigint.add (count left target) (count right target)} @@ total

val permutation_rotate : (left : int list) -> (right : int list) ->
    {u : unit | permutation (append left right) (append right left)} @@ total
