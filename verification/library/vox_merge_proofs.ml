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
end
