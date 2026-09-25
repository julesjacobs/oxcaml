(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_ordered_sequence.ml vox_credits.mli vox_credits.ml vox_merge_proofs.ml vox_sort_cost.ml vox_merge_sort.mli vox_merge_sort.ml merge_sort.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module O = struct
  type elt = int
  let[@def] le (x : int) (y : int) = ghost_ (x <= y)
  let (reflexive @ total) (x : int) : {u : unit | le x x} @ ghost =
    ghost_ (le_def x x; let u = () in refine_ u)
  let (totality @ total) (x : int) (y : int) :
      {u : unit | le x y || le y x} @ ghost = ghost_ (
    le_def x y; le_def y x; let u = () in refine_ u)
  let (transitive @ total) (x : int) (y : int) (z : int) :
      {u : unit | if le x y && le y z then le x z else true} @ ghost =
    ghost_ (le_def x y; le_def y z; le_def x z;
      let u = () in refine_ u)
end
module C = Vox_credits.Make ()
module Make_compare (C : Vox_credits.S) = struct
  type result = #{ before : bool; state : C.token @@ ghost }
  let (compare @ total) : (left : int) -> (right : int) ->
      (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
      {r : result | let refine_ token = token in
        r.#before = O.le left right &&
        C.credits r.#state = C.credits token - 1} @ unique =
      fun left right token ->
    ghost_ (O.le_def left right);
    let refine_ state = C.tick token in
    let result = #{ before = left <= right; state } in
    refine_ result
end
module Compare = Make_compare (C)
module Sort = Vox_merge_sort.Make (O) (C) (Compare)

let check_budget (values : int list) amount target =
  let certified : {n : int | n >= 0 &&
    Vox_sort_cost.budget (Vox_sequence.length values) <= Bigint.of_int n} =
    assume_ amount in
  let refine_ certified = certified in
  let certified : int = certified in
  let initial : {n : int | n >= 0} = refine_ certified in
  let refine_ token = C.Budget.create initial in
  let input : {t : C.token |
    Vox_sort_cost.budget (Vox_sequence.length values) <=
      Bigint.of_int (C.credits t)} = refine_ token in
  let before = ghost_ (C.credits (borrow_ input)) in
  let refine_ result = Sort.sort values input in
  let #{ Sort.values = sorted; state } = result in
  ghost_ (
    let u = () in
    let refine_ checked = (refine_ u : {u : unit |
      0 <= C.credits state && C.credits state <= before &&
      Bigint.of_int (C.credits state) >=
        Bigint.sub (Bigint.of_int before)
          (Vox_sort_cost.budget (Vox_sequence.length values))}) in
    Sort.P.permutation_count values sorted target;
    let u = () in
    (refine_ u : {u : unit |
      Sort.P.count values target = Sort.P.count sorted target}));
  assert (sorted = List.sort Stdlib.compare values)

let check values =
  let size = List.length values in
  let rec height n = if n <= 1 then 0 else 1 + height ((n + 1) / 2) in
  let bound = size * height size in
  List.iter (fun amount -> check_budget values amount 7)
    [bound; bound + 1; max_int]

let (two @ total) : (x : int) -> (y : int) -> (z : int) ->
    (token : {t : C.token | C.credits t >= 2}) @ unique total ghost ->
    {r : Compare.result | let refine_ token = token in
      r.#before = (O.le x y && O.le y z) &&
      C.credits r.#state = C.credits token - 2} @ unique =
    fun x y z token ->
  let refine_ token = token in
  let first : {t : C.token | C.credits t > 0} = refine_ token in
  let refine_ a = Compare.compare x y first in
  let #{ Compare.before = ab; state } = a in
  let second : {t : C.token | C.credits t > 0} = refine_ state in
  let refine_ b = Compare.compare y z second in
  let #{ Compare.before = bc; state } = b in
  let result = #{ Compare.before = ab && bc; state } in
  refine_ result

let () =
  let amount = 2 in
  let initial : {n : int | n >= 0} = refine_ amount in
  let refine_ token = C.Budget.create initial in
  let input : {t : C.token | C.credits t >= 2} = refine_ token in
  let refine_ result = two 3 2 1 input in
  let #{ Compare.before; state } = result in
  ghost_ (let u = () in
    (refine_ u : {u : unit | C.credits state = 0}));
  assert (not before)

let () =
  List.iter check
    [[]; [0]; [2; 1]; [1; 1]; [3; 1; 2]; [5; 4; 3; 2; 1];
     [min_int; max_int; min_int; 0; -1; max_int]];
  for size = 0 to 128 do
    check (List.init size (fun i -> (i * 37 + size) mod 17 - 8))
  done

module Ranked = struct
  type elt = { rank : int; payload : int }
  let[@def] le (x : elt) (y : elt) = ghost_ (x.rank <= y.rank)
  let (reflexive @ total) (x : elt) : {u : unit | le x x} @ ghost =
    ghost_ (le_def x x; let u = () in refine_ u)
  let (totality @ total) (x : elt) (y : elt) :
      {u : unit | le x y || le y x} @ ghost = ghost_ (
    le_def x y; le_def y x; let u = () in refine_ u)
  let (transitive @ total) (x : elt) (y : elt) (z : elt) :
      {u : unit | if le x y && le y z then le x z else true} @ ghost =
    ghost_ (le_def x y; le_def y z; le_def x z;
      let u = () in refine_ u)
end
module Make_rank_compare (C : Vox_credits.S) = struct
  type result = #{ before : bool; state : C.token @@ ghost }
  let (compare @ total) : (left : Ranked.elt) -> (right : Ranked.elt) ->
      (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
      {r : result | let refine_ token = token in
        r.#before = Ranked.le left right &&
        C.credits r.#state = C.credits token - 1} @ unique =
      fun left right token ->
    ghost_ (Ranked.le_def left right);
    let refine_ state = C.tick token in
    let result = #{ before = left.rank <= right.rank; state } in
    refine_ result
end
module Rank_compare = Make_rank_compare (C)
module Rank_sort = Vox_merge_sort.Make (Ranked) (C) (Rank_compare)

let check_ranked (values : Ranked.elt list) =
  let size = List.length values in
  let amount = size * size in
  let certified : {n : int | n >= 0 &&
    Vox_sort_cost.budget (Vox_sequence.length values) <= Bigint.of_int n} =
    assume_ amount in
  let refine_ certified = certified in
  let certified : int = certified in
  let initial : {n : int | n >= 0} = refine_ certified in
  let refine_ token = C.Budget.create initial in
  let input : {t : C.token |
    Vox_sort_cost.budget (Vox_sequence.length values) <=
      Bigint.of_int (C.credits t)} = refine_ token in
  let refine_ result = Rank_sort.sort values input in
  let #{ Rank_sort.values = sorted; state = _ } = result in
  let ranks = List.map (fun (r : Ranked.elt) -> r.rank) in
  assert (ranks sorted = List.sort Stdlib.compare (ranks values));
  assert (List.sort Stdlib.compare sorted = List.sort Stdlib.compare values)

let () =
  check_ranked
    [{ rank = 2; payload = 10 }; { rank = 1; payload = 20 };
     { rank = 2; payload = 30 }; { rank = 1; payload = 40 };
     { rank = 1; payload = 20 }]
