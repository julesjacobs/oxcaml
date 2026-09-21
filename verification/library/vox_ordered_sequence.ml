module type Order = sig
  type elt : immutable_data mod total
  val le : elt @ immutable -> elt @ immutable -> bool @ ghost @@ total
  val reflexive : (x : elt) @ immutable -> {u : unit | le x x}
    @ ghost @@ total
  val totality : (x : elt) @ immutable -> (y : elt) @ immutable ->
    {u : unit | le x y || le y x} @ ghost @@ total
  val transitive : (x : elt) @ immutable -> (y : elt) @ immutable ->
    (z : elt) @ immutable ->
    {u : unit | if le x y && le y z then le x z else true} @ ghost @@ total
end

module Make (O : Order) = struct
  module S = Vox_sequence

  let[@def] rec count (values : O.elt list @ immutable)
      (target : O.elt @ immutable) = ghost_ (
    match values with
    | [] -> 0Z
    | head :: tail -> Bigint.add (if head === target then 1Z else 0Z)
        (count tail target))

  let[@def] rec same_counts (keys : O.elt list @ immutable)
      (left : O.elt list @ immutable) (right : O.elt list @ immutable) =
    ghost_ (match keys with
    | [] -> true
    | head :: tail -> count left head = count right head &&
        same_counts tail left right)

  let[@def] permutation (left : O.elt list @ immutable)
      (right : O.elt list @ immutable) = ghost_ (
    same_counts left left right && same_counts right left right)

  let[@def] rec all (values : O.elt list @ immutable)
      (bound : O.elt @ immutable) = ghost_ (match values with
    | [] -> true
    | head :: tail -> O.le bound head && all tail bound)

  let[@def] rec sorted (values : O.elt list @ immutable) = ghost_ (
    match values with [] -> true | head :: tail -> all tail head && sorted tail)

  let rec (count_nonnegative @ total) : (values : O.elt list) @ immutable ->
      (target : O.elt) @ immutable -> {u : unit | 0Z <= count values target}
      @ ghost = fun values target -> ghost_ (
    count_def values target;
    (match values with [] -> () | _ :: tail -> count_nonnegative tail target);
    let u = () in u)

  let rec (count_append @ total) : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable -> (target : O.elt) @ immutable ->
      {u : unit | count (S.append left right) target =
        Bigint.add (count left target) (count right target)} @ ghost =
      fun left right target -> ghost_ (
    S.append_def left right;
    count_def left target;
    count_def (S.append left right) target;
    (match left with [] -> () | _ :: tail -> count_append tail right target);
    let u = () in u)

  let rec (same_elim @ total) : (keys : O.elt list) @ immutable ->
      (left : O.elt list) @ immutable -> (right : O.elt list) @ immutable ->
      (target : O.elt) @ immutable ->
      {u : unit | if same_counts keys left right && count keys target > 0Z
        then count left target = count right target else true} @ ghost =
      fun keys left right target -> ghost_ (
    same_counts_def keys left right;
    count_def keys target;
    (match keys with [] -> () | _ :: tail -> same_elim tail left right target);
    let u = () in u)

  let (permutation_count @ total) : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable -> (target : O.elt) @ immutable ->
      {u : unit | if permutation left right then
        count left target = count right target else true} @ ghost =
      fun left right target -> ghost_ (
    permutation_def left right;
    count_nonnegative left target;
    count_nonnegative right target;
    same_elim left left right target;
    same_elim right left right target;
    let u = () in u)

  let rec (same_intro @ total) : (keys : O.elt list) @ immutable ->
      (left : O.elt list) @ immutable -> (right : O.elt list) @ immutable ->
      ((target : O.elt) @ immutable ->
        {u : unit | count left target = count right target} @ ghost)
        @ total ghost -> {u : unit | same_counts keys left right} @ ghost =
      fun keys left right proof -> ghost_ (
    same_counts_def keys left right;
    (match keys with [] -> () | head :: tail ->
      proof head; same_intro tail left right proof);
    let u = () in u)

  let (count_extensional @ total) : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable ->
      ((target : O.elt) @ immutable ->
        {u : unit | count left target = count right target} @ ghost)
        @ total ghost -> {u : unit | permutation left right} @ ghost =
      fun left right proof -> ghost_ (
    same_intro left left right proof;
    same_intro right left right proof;
    permutation_def left right;
    let u = () in u)

  let (permutation_refl @ total) (values : O.elt list @ immutable) :
      {u : unit | permutation values values} @ ghost = ghost_ (
    count_extensional values values (fun _target -> let u = () in u))

  let (permutation_trans @ total) : (first : O.elt list) @ immutable ->
      (second : O.elt list) @ immutable -> (third : O.elt list) @ immutable ->
      {u : unit | if permutation first second && permutation second third
        then permutation first third else true} @ ghost =
      fun first second third -> ghost_ (
    if permutation first second && permutation second third then
      count_extensional first third (fun target ->
        permutation_count first second target;
        permutation_count second third target;
        let u = () in u);
    let u = () in u)

  let (permutation_append @ total) : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable -> (new_left : O.elt list) @ immutable ->
      (new_right : O.elt list) @ immutable ->
      {u : unit | if permutation left new_left && permutation right new_right
        then permutation (S.append left right) (S.append new_left new_right)
        else true} @ ghost = fun left right new_left new_right -> ghost_ (
    if permutation left new_left && permutation right new_right then
      count_extensional (S.append left right) (S.append new_left new_right)
        (fun target ->
          count_append left right target;
          count_append new_left new_right target;
          permutation_count left new_left target;
          permutation_count right new_right target;
          let u = () in u);
    let u = () in u)

  let (permutation_rotate @ total) (left : O.elt list @ immutable)
      (right : O.elt list @ immutable) :
      {u : unit | permutation (S.append left right) (S.append right left)}
      @ ghost = ghost_ (
    count_extensional (S.append left right) (S.append right left) (fun target ->
      count_append left right target;
      count_append right left target;
      let u = () in u))

  let rec (all_member @ total) : (values : O.elt list) @ immutable ->
      (bound : O.elt) @ immutable -> (target : O.elt) @ immutable ->
      {u : unit | if all values bound && count values target > 0Z then
        O.le bound target else true} @ ghost = fun values bound target ->
    ghost_ (
      all_def values bound;
      count_def values target;
      (match values with [] -> () | _ :: tail -> all_member tail bound target);
      let u = () in u)

  let rec (all_if_counts @ total) : (before : O.elt list) @ immutable ->
      (after : O.elt list) @ immutable -> (bound : O.elt) @ immutable ->
      ((target : O.elt) @ immutable ->
        {u : unit | count after target <= count before target} @ ghost)
        @ total ghost ->
      {u : unit | if all before bound then all after bound else true} @ ghost =
      fun before after bound proof -> ghost_ (
    all_def after bound;
    (match after with [] -> () | head :: tail ->
      proof head;
      count_def after head;
      count_nonnegative tail head;
      all_member before bound head;
      all_if_counts before tail bound (fun target ->
        proof target;
        count_def after target;
        let u = () in u));
    let u = () in u)

  let (all_permutation @ total) : (before : O.elt list) @ immutable ->
      (after : O.elt list) @ immutable -> (bound : O.elt) @ immutable ->
      {u : unit | if permutation before after && all before bound then
        all after bound else true} @ ghost = fun before after bound -> ghost_ (
    if permutation before after then
      all_if_counts before after bound (fun target ->
        permutation_count before after target;
        let u = () in u);
    let u = () in u)

  let rec (all_append @ total) : (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable -> (bound : O.elt) @ immutable ->
      {u : unit | all (S.append left right) bound =
        (all left bound && all right bound)} @ ghost = fun left right bound ->
    ghost_ (
      S.append_def left right;
      all_def (S.append left right) bound;
      all_def left bound;
      (match left with [] -> () | _ :: tail -> all_append tail right bound);
      let u = () in u)

  let rec (all_weaken @ total) : (values : O.elt list) @ immutable ->
      (old_bound : O.elt) @ immutable -> (new_bound : O.elt) @ immutable ->
      {u : unit | if O.le new_bound old_bound && all values old_bound then
        all values new_bound else true} @ ghost =
      fun values old_bound new_bound ->
    ghost_ (
      all_def values old_bound;
      all_def values new_bound;
      (match values with [] -> () | head :: tail ->
        O.transitive new_bound old_bound head;
        all_weaken tail old_bound new_bound);
      let u = () in u)

  let (sorted_short @ total) (values : O.elt list @ immutable) :
      {u : unit | if S.length values <= 1Z then sorted values else true}
      @ ghost = ghost_ (
    S.length_def values;
    sorted_def values;
    (match values with [] -> () | head :: tail ->
      S.length_def tail;
      sorted_def tail;
      match tail with [] -> all_def tail head
      | _ :: _ -> ());
    let u = () in u)
end
