open Oxsmt_core

type owner =
  | A
  | B
  | Both

let arithmetic_sort sort = Sort.equal sort Sort.real

let require_real where (term : Term.t) =
  if not (arithmetic_sort term.sort)
  then
    raise
      (Combine.Combination_unsound
         (where ^ ": order atom belongs to a different arithmetic theory"))
;;

let owner term =
  match Theory_view.atom term with
  | Theory_view.Le_zero arg ->
    require_real "Uflra_router.owner" arg;
    B
  | Theory_view.Predicate _ | Theory_view.Bool_lit _ -> A
  | Theory_view.Equality (x, _) ->
    if arithmetic_sort x.sort then Both else A
;;

let assert_to term ~positive:_ =
  match Theory_view.atom term with
  | Theory_view.Le_zero arg ->
    require_real "Uflra_router.assert_to" arg;
    B
  | Theory_view.Predicate _ | Theory_view.Bool_lit _ -> A
  | Theory_view.Equality (x, _) ->
    if arithmetic_sort x.sort then Both else A
;;

let equality_split ctx x y =
  if not (arithmetic_sort x.Term.sort && arithmetic_sort y.Term.sort)
  then
    raise
      (Combine.Combination_unsound
         "Uflra_router.equality_split: non-Real shared term");
  [ Context.eq ctx x y; Context.lt ctx x y; Context.gt ctx x y ]
;;
