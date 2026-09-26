module S = Vox_sequence
module Make (O : Vox_ordered_sequence.Order)
    (P : module type of Vox_ordered_sequence.Make (O)) = struct

  let (head @ total) (value : O.elt @ immutable) (tail : O.elt list @ immutable)
      (other : O.elt list @ immutable)
      (merged : O.elt list @ immutable) :
      {u : unit | if P.sorted (value :: tail) && P.all other value &&
        P.sorted merged && P.permutation (S.append tail other) merged then
        P.sorted (value :: merged) &&
        P.permutation (S.append (value :: tail) other) (value :: merged)
        else true} @ ghost = ghost_ (
    let before = value :: tail in
    let after = value :: merged in
    let rest = S.append tail other in
    let singleton = [value] in
    let nil : O.elt list = [] in
    P.sorted_def before;
    P.sorted_def after;
    P.all_append tail other value;
    P.all_permutation rest merged value;
    P.permutation_refl singleton;
    P.permutation_append singleton rest singleton merged;
    S.append_def singleton rest;
    S.append_def singleton merged;
    S.append_def nil rest;
    S.append_def nil merged;
    S.append_def before other;
    ())

  let (lower @ total) (first : O.elt @ immutable) (second : O.elt @ immutable)
      (tail : O.elt list @ immutable) :
      {u : unit | if O.le first second && P.sorted (second :: tail) then
        P.all (second :: tail) first else true} @ ghost = ghost_ (
    let values = second :: tail in
    P.sorted_def values;
    P.all_weaken tail second first;
    P.all_def values first;
    ())
  let (split_step @ total) (x : O.elt @ immutable) (y : O.elt @ immutable)
      (tail : O.elt list @ immutable) (left : O.elt list @ immutable)
      (right : O.elt list @ immutable) :
      {u : unit | if P.permutation tail (S.append left right) then
        P.permutation (x :: y :: tail) (S.append (x :: left) (y :: right))
        else true} @ ghost = ghost_ (
    let rest = S.append left right in
    if P.permutation tail rest then (
      let values = x :: y :: tail in
      let new_left = x :: left in
      let new_right = y :: right in
      let joined = S.append new_left new_right in
      P.count_extensional values joined (fun target ->
        P.count_def values target;
        P.count_def (y :: tail) target;
        P.permutation_count tail rest target;
        P.count_def new_left target;
        P.count_def new_right target;
        P.count_append new_left new_right target;
        P.count_append left right target;
        ()));
    ())

  let (right_head @ total) (x : O.elt @ immutable) (xs : O.elt list @ immutable)
      (y : O.elt @ immutable) (ys : O.elt list @ immutable)
      (values : O.elt list @ immutable) :
      {u : unit | if P.sorted (x :: xs) && P.sorted (y :: ys) && O.le y x &&
        P.sorted values && P.permutation (S.append (x :: xs) ys) values then
        P.sorted (y :: values) &&
        P.permutation (S.append (x :: xs) (y :: ys)) (y :: values)
        else true} @ ghost = ghost_ (
    let left = x :: xs in
    let right = y :: ys in
    let rest = S.append ys left in
    let other = S.append left ys in
    let swapped = S.append right left in
    let original = S.append left right in
    P.permutation_rotate ys left;
    P.permutation_trans rest other values;
    lower y x xs;
    head y ys left values;
    P.permutation_rotate left right;
    P.permutation_trans original swapped (y :: values);
    ())

end
