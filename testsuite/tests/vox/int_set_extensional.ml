(* TEST
 flags = "-extension refinement_types";
 has-z3;
 timeout = "900";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
*)

type repr = Nil | Cons of int * repr [@@inductive]

let[@def] rec (all_greater @ total) lower xs =
  match xs with
  | Nil -> true
  | Cons (head, tail) -> lower < head && all_greater lower tail

let[@def] rec (valid @ total) xs =
  match xs with
  | Nil -> true
  | Cons (head, tail) -> all_greater head tail && valid tail

type t = {xs : repr | valid xs}

let[@def] rec (lookup_repr @ total) element xs =
  match xs with
  | Nil -> false
  | Cons (head, tail) -> element = head || lookup_repr element tail

let[@def] (lookup @ total) element (set : t) =
  let refine_ xs = set in
  lookup_repr element xs

let rec (lookup_below @ total) :
    (lower : int) ->
    (element : int) ->
    (xs : repr) ->
    {u : unit |
      if element <= lower && all_greater lower xs
      then lookup_repr element xs === false
      else true} @ immutable contended =
  fun lower element xs ->
  let u = () in
  if element <= lower && all_greater lower xs then
    match xs with
    | Nil ->
      let refine_ lookup_equation = lookup_repr_def element xs in
      refine_ u
    | Cons (_, tail) ->
      let refine_ order_equation = all_greater_def lower xs in
      let refine_ lookup_equation = lookup_repr_def element xs in
      let refine_ induction = lookup_below lower element tail in
      refine_ u
  else refine_ u

let rec (extensional_repr @ total) :
    (left : repr) ->
    (right : repr) ->
    ((element : int) ->
      {u : unit |
        lookup_repr element left === lookup_repr element right})
      @ total ->
    {u : unit |
      if valid left && valid right then left === right else true}
      @ immutable contended =
  fun left right premise ->
  let u = () in
  if valid left && valid right then
    let refine_ left_valid = valid_def left in
    let refine_ right_valid = valid_def right in
    match left with
    | Nil ->
      (match right with
       | Nil -> refine_ u
       | Cons (right_head, _) ->
         let refine_ same_lookup = premise right_head in
         let refine_ left_lookup = lookup_repr_def right_head left in
         let refine_ right_lookup = lookup_repr_def right_head right in
         refine_ u)
    | Cons (left_head, left_tail) ->
      match right with
      | Nil ->
        let refine_ same_lookup = premise left_head in
        let refine_ left_lookup = lookup_repr_def left_head left in
        let refine_ right_lookup = lookup_repr_def left_head right in
        refine_ u
      | Cons (right_head, right_tail) ->
        let refine_ left_lookup = premise left_head in
        let refine_ right_lookup = premise right_head in
        let refine_ left_at_left = lookup_repr_def left_head left in
        let refine_ right_at_left = lookup_repr_def left_head right in
        let refine_ left_at_right = lookup_repr_def right_head left in
        let refine_ right_at_right = lookup_repr_def right_head right in
        if left_head < right_head then
          let refine_ absent =
            lookup_below right_head left_head right_tail
          in
          refine_ u
        else if right_head < left_head then
          let refine_ absent =
            lookup_below left_head right_head left_tail
          in
          refine_ u
        else
          let (tail_premise @ total) :
              (element : int) ->
              {u : unit |
                lookup_repr element left_tail
                === lookup_repr element right_tail} =
            fun element ->
            let refine_ same_lookup = premise element in
            let refine_ left_equation = lookup_repr_def element left in
            let refine_ right_equation = lookup_repr_def element right in
            if element = left_head then
              let refine_ left_absent =
                lookup_below left_head element left_tail
              in
              let refine_ right_absent =
                lookup_below right_head element right_tail
              in
              refine_ u
            else refine_ u
          in
          let refine_ tails_equal =
            extensional_repr left_tail right_tail tail_premise
          in
          refine_ u
  else refine_ u

let (extensional @ total) :
    (left : t) ->
    (right : t) ->
    ((element : int) ->
      {u : unit | lookup element left === lookup element right})
      @ total ->
    {u : unit | left === right} @ immutable contended =
  fun left right premise ->
  let refine_ xs = left in
  let refine_ ys = right in
  let (repr_premise @ total) :
      (element : int) ->
      {u : unit |
        lookup_repr element xs === lookup_repr element ys} =
    fun element ->
    let refine_ same_lookup = premise element in
    let refine_ left_lookup = lookup_def element left in
    let refine_ right_lookup = lookup_def element right in
    let u = () in
    refine_ u
  in
  let refine_ same_repr = extensional_repr xs ys repr_premise in
  let u = () in
  refine_ u
