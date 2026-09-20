module S = Vox_sequence

module Make (O : Vox_ordered_sequence.Order) (C : Vox_credits.S) (Compare : sig
  type result = #{ before : bool; state : C.token @@ ghost }
  val compare : (left : O.elt) @ immutable -> (right : O.elt) @ immutable ->
    (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
    {r : result | let refine_ token = token in
      r.#before = (O.le left right) &&
      C.credits r.#state = C.credits token - 1} @ unique @@ total
end) = struct
  module P = Vox_ordered_sequence.Make (O)
  module Proof = Vox_merge_proofs.Make (O) (P)
  type partition = { left : O.elt list; right : O.elt list }

  let rec (split @ total) : (values : O.elt list) @ immutable ->
      {p : partition |
        P.permutation values (S.append p.left p.right) &&
        S.length p.left = Bigint.div (Bigint.add (S.length values) 1Z) 2Z &&
        S.length p.right = Bigint.div (S.length values) 2Z} = fun values ->
    ghost_ (S.length_def values);
    match values with
    | [] ->
      let result = { left = []; right = [] } in
      ghost_ (S.append_def values values; P.permutation_refl values);
      refine_ result
    | [x] ->
      let nil : O.elt list = [] in
      let result = { left = [x]; right = nil } in
      ghost_ (S.length_def nil; S.append_nil values;
        P.permutation_refl values);
      refine_ result
    | x :: y :: tail ->
      let refine_ parts = split tail in
      let left = x :: parts.left in
      let right = y :: parts.right in
      let result = { left; right } in
      ghost_ (
        let rest = S.append parts.left parts.right in
        let joined = S.append left right in
        S.length_def (y :: tail);
        S.length_def left;
        S.length_def right;
        P.count_extensional values joined (fun target ->
          P.count_def values target;
          P.count_def (y :: tail) target;
          P.permutation_count tail rest target;
          P.count_append parts.left parts.right target;
          P.count_append left right target;
          P.count_def left target;
          P.count_def right target;
          let u = () in refine_ u));
      refine_ result

  type result = #{ values : O.elt list @@ aliased; state : C.token @@ ghost }

  let rec (merge @ total) : (size : Bigint.t) @ ghost ->
      (left : O.elt list) @ immutable ->
      (right : O.elt list) @ immutable ->
      (token : {t : C.token |
        size = Bigint.add (S.length left) (S.length right) &&
        P.sorted left && P.sorted right &&
        Bigint.of_int (C.credits t) >=
          Bigint.add (S.length left) (S.length right)}) @ unique total ghost ->
      {r : result | let refine_ token = token in
        P.sorted r.#values && P.permutation (S.append left right) r.#values &&
        S.length r.#values = Bigint.add (S.length left) (S.length right) &&
        0 <= C.credits r.#state && C.credits r.#state <= C.credits token &&
        Bigint.of_int (C.credits r.#state) >=
          Bigint.sub (Bigint.of_int (C.credits token))
            (Bigint.add (S.length left) (S.length right))} @ unique =
      fun size left right token ->
    let refine_ token = token in
    ghost_ (S.length_def left; S.length_def right);
    match left, right with
    | [], _ ->
      ghost_ (S.append_def left right; P.permutation_refl right);
      let result = #{ values = right; state = token } in
      refine_ result
    | _, [] ->
      ghost_ (S.append_nil left; P.permutation_refl left);
      let result = #{ values = left; state = token } in
      refine_ result
    | x :: xs, y :: ys ->
      ghost_ (P.sorted_def left; P.sorted_def right; O.totality x y);
      let available : {t : C.token | C.credits t > 0} = refine_ token in
      let refine_ compared = Compare.compare x y available in
      let #{ Compare.before; state } = compared in
      if before then (
        let next_size = ghost_ (Bigint.sub size 1Z) in
        let next : {t : C.token | next_size =
          Bigint.add (S.length xs) (S.length right) &&
          P.sorted xs && P.sorted right &&
          Bigint.of_int (C.credits t) >=
            Bigint.add (S.length xs) (S.length right)} = refine_ state in
        let refine_ merged = merge next_size xs right next in
        let #{ values; state } = merged in
        let output = x :: values in
        ghost_ (Proof.lower x y ys;
          Proof.head x xs right values;
          S.length_def output);
        let result = #{ values = output; state } in
        refine_ result)
      else (
        let next_size = ghost_ (Bigint.sub size 1Z) in
        let next : {t : C.token | next_size =
          Bigint.add (S.length left) (S.length ys) &&
          P.sorted left && P.sorted ys &&
          Bigint.of_int (C.credits t) >=
            Bigint.add (S.length left) (S.length ys)} = refine_ state in
        let refine_ merged = merge next_size left ys next in
        let #{ values; state } = merged in
        let output = y :: values in
        ghost_ (
          let rest = S.append ys left in
          let other = S.append left ys in
          let swapped = S.append right left in
          let original = S.append left right in
          P.permutation_rotate ys left;
          P.permutation_trans rest other values;
          Proof.lower y x xs;
          Proof.head y ys left values;
          P.permutation_rotate left right;
          P.permutation_trans original swapped output;
          S.length_def output);
        let result = #{ values = output; state } in
        refine_ result)
  [@@decreases size]

  let rec (sort_at_depth @ total) : (size : Bigint.t) @ ghost ->
      (depth : Bigint.t) @ ghost -> (values : O.elt list) @ immutable ->
      (token : {t : C.token | size = S.length values && 0Z <= depth &&
        size <= Vox_sort_cost.power depth &&
        Bigint.mul size depth <= Bigint.of_int (C.credits t)})
        @ unique total ghost ->
      {r : result | let refine_ token = token in
        P.sorted r.#values && P.permutation values r.#values &&
        S.length r.#values = size &&
        0 <= C.credits r.#state && C.credits r.#state <= C.credits token &&
        Bigint.of_int (C.credits r.#state) >=
          Bigint.sub (Bigint.of_int (C.credits token)) (Bigint.mul size depth)}
        @ unique = fun size depth values token ->
    let refine_ token = token in
    ghost_ (S.length_def values; Vox_sort_cost.power_def depth);
    match values with
    | [] | [_] ->
      ghost_ (P.sorted_short values; P.permutation_refl values);
      let result = #{ values; state = token } in
      refine_ result
    | _ :: (_ :: _ as tail) ->
      ghost_ (S.length_def tail);
      let refine_ halves = split values in
      let left_size = ghost_ (S.length halves.left) in
      let right_size = ghost_ (S.length halves.right) in
      let next_depth = ghost_ (Bigint.sub depth 1Z) in
      let capacity = ghost_ (C.credits (borrow_ token)) in
      let left_budget = ghost_ (Bigint.mul left_size next_depth) in
      let bounded : {n : Bigint.t | 0Z <= n &&
        n <= Bigint.of_int capacity} = refine_ left_budget in
      let refine_ amount =
        ghost_ (Vox_sort_cost.bounded_int capacity bounded) in
      let amount : int = amount in
      let available : {t : C.token | 0 <= amount && amount <= C.credits t} =
        refine_ token in
      let refine_ parts = C.split amount available in
      let { C.left = left_state; right = right_state } = parts in
      let left_token : {t : C.token | left_size = S.length halves.left &&
        0Z <= next_depth && left_size <= Vox_sort_cost.power next_depth &&
        Bigint.mul left_size next_depth <= Bigint.of_int (C.credits t)} =
        refine_ left_state in
      let refine_ left_result =
        sort_at_depth left_size next_depth halves.left left_token in
      let #{ values = sorted_left; state = left_state } = left_result in
      let right_token : {t : C.token | right_size = S.length halves.right &&
        0Z <= next_depth && right_size <= Vox_sort_cost.power next_depth &&
        Bigint.mul right_size next_depth <= Bigint.of_int (C.credits t)} =
        refine_ right_state in
      let refine_ right_result =
        sort_at_depth right_size next_depth halves.right right_token in
      let #{ values = sorted_right; state = right_state } = right_result in
      let joinable : {t : C.token | 0 <= C.credits left_state &&
        0 <= C.credits t && 0 <= C.credits left_state + C.credits t} =
        refine_ right_state in
      let refine_ combined = C.merge left_state joinable in
      let merge_token : {t : C.token |
        size = Bigint.add (S.length sorted_left) (S.length sorted_right) &&
        P.sorted sorted_left && P.sorted sorted_right &&
        Bigint.of_int (C.credits t) >=
          Bigint.add (S.length sorted_left) (S.length sorted_right)} =
        refine_ combined in
      let refine_ merged = merge size sorted_left sorted_right merge_token in
      let #{ values = output; state } = merged in
      ghost_ (
        let original = S.append halves.left halves.right in
        let sorted = S.append sorted_left sorted_right in
        P.permutation_append halves.left halves.right sorted_left sorted_right;
        P.permutation_trans values original sorted;
        P.permutation_trans values sorted output);
      let result = #{ values = output; state } in
      refine_ result
  [@@decreases size]

  let (sort @ total) : (values : O.elt list) @ immutable ->
      (token : {t : C.token | Vox_sort_cost.budget (S.length values) <=
        Bigint.of_int (C.credits t)}) @ unique total ghost ->
      {r : result | let refine_ token = token in
        P.sorted r.#values && P.permutation values r.#values &&
        S.length r.#values = S.length values &&
        0 <= C.credits r.#state && C.credits r.#state <= C.credits token &&
        Bigint.of_int (C.credits r.#state) >=
          Bigint.sub (Bigint.of_int (C.credits token))
            (Vox_sort_cost.budget (S.length values))} @ unique =
      fun values token ->
    let refine_ token = token in
    let size = ghost_ (S.length values) in
    let depth = ghost_ (Vox_sort_cost.height size) in
    ghost_ (Vox_sort_cost.height_bound size; Vox_sort_cost.budget_def size);
    let input : {t : C.token | size = S.length values && 0Z <= depth &&
      size <= Vox_sort_cost.power depth &&
      Bigint.mul size depth <= Bigint.of_int (C.credits t)} = refine_ token in
    let refine_ result = sort_at_depth size depth values input in
    refine_ result

end
