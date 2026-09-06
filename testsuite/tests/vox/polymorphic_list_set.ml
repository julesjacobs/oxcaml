module Make (Element : Polymorphic_set_intf.Ordered) = struct
  module Element = Element

  type elt = Element.t
  type repr = Nil | Cons of elt * repr [@@inductive]

  let[@def] (equivalent @ total) left right =
    Element.compare left right = 0

  let[@def] (less @ total) left right =
    Element.compare left right < 0

  let (equivalent_reflexive @ total) (x : elt) :
      {u : unit | equivalent x x === true} =
    let refine_ order = Element.compare_reflexive x in
    let refine_ equation = equivalent_def x x in
    let u = () in
    refine_ u

  let (equivalent_symmetric @ total) :
      (x : elt) ->
      (y : elt) ->
      {u : unit |
        if equivalent x y then equivalent y x else true}
        @ immutable contended =
    fun x y ->
    let u = () in
    if equivalent x y then
      let refine_ premise = equivalent_def x y in
      let refine_ forward = Element.compare_reverse x y in
      let refine_ backward = Element.compare_reverse y x in
      let refine_ result = equivalent_def y x in
      refine_ u
    else refine_ u

  let (equivalent_transitive @ total) :
      (x : elt) ->
      (y : elt) ->
      (z : elt) ->
      {u : unit |
        if equivalent x y && equivalent y z
        then equivalent x z
        else true} @ immutable contended =
    fun x y z ->
    let u = () in
    if equivalent x y && equivalent y z then
      let refine_ left = equivalent_def x y in
      let refine_ right = equivalent_def y z in
      let refine_ left_symmetric = equivalent_symmetric x y in
      let refine_ right_symmetric = equivalent_symmetric y z in
      let refine_ left_reverse = equivalent_def y x in
      let refine_ right_reverse = equivalent_def z y in
      let refine_ forward = Element.compare_transitive x y z in
      let refine_ backward = Element.compare_transitive z y x in
      let refine_ outer_reverse = Element.compare_reverse x z in
      let refine_ outer_reverse_back = Element.compare_reverse z x in
      let refine_ result = equivalent_def x z in
      refine_ u
    else refine_ u

  let (equivalent_congruent @ total) :
      (probe : elt) ->
      (left : elt) ->
      (right : elt) ->
      {u : unit |
        if equivalent left right
        then equivalent probe left === equivalent probe right
        else true} @ immutable contended =
    fun probe left right ->
    let u = () in
    if equivalent left right then
      let refine_ symmetric = equivalent_symmetric left right in
      if equivalent probe left then
        let refine_ forward =
          equivalent_transitive probe left right
        in
        refine_ u
      else if equivalent probe right then
        let refine_ backward =
          equivalent_transitive probe right left
        in
        refine_ u
      else refine_ u
    else refine_ u

  let (less_transitive @ total) :
      (x : elt) ->
      (y : elt) ->
      (z : elt) ->
      {u : unit |
        if less x y && less y z then less x z else true}
        @ immutable contended =
    fun x y z ->
    let u = () in
    if less x y && less y z then
      let refine_ left = less_def x y in
      let refine_ right = less_def y z in
      let refine_ forward = Element.compare_transitive x y z in
      let refine_ first_reverse = Element.compare_reverse y x in
      let refine_ outer_reverse = Element.compare_reverse z x in
      let refine_ backward = Element.compare_transitive y z x in
      let refine_ result = less_def x z in
      refine_ u
    else refine_ u

  let (le_less_transitive @ total) :
      (x : elt) ->
      (y : elt) ->
      (z : elt) ->
      {u : unit |
        if Element.compare x y <= 0 && less y z
        then less x z
        else true} @ immutable contended =
    fun x y z ->
    let u = () in
    if Element.compare x y <= 0 && less y z then
      let refine_ strict = less_def y z in
      let refine_ forward = Element.compare_transitive x y z in
      let refine_ outer_reverse = Element.compare_reverse z x in
      let refine_ strict_reverse = Element.compare_reverse z y in
      let refine_ backward = Element.compare_transitive z x y in
      let refine_ result = less_def x z in
      refine_ u
    else refine_ u

  let (reverse_less @ total) :
      (x : elt) ->
      (y : elt) ->
      {u : unit |
        if Element.compare x y > 0 then less y x else true}
        @ immutable contended =
    fun x y ->
    let u = () in
    if Element.compare x y > 0 then
      let refine_ forward = Element.compare_reverse x y in
      let refine_ backward = Element.compare_reverse y x in
      let refine_ result = less_def y x in
      refine_ u
    else refine_ u

  let (not_less_equivalent @ total) :
      (x : elt) ->
      (y : elt) ->
      {u : unit |
        if less x y = false && less y x = false
        then equivalent x y
        else true} @ immutable contended =
    fun x y ->
    let u = () in
    if less x y = false && less y x = false then
      let refine_ left = less_def x y in
      let refine_ right = less_def y x in
      let refine_ reverse = Element.compare_reverse x y in
      let refine_ result = equivalent_def x y in
      refine_ u
    else refine_ u

  let[@def] rec (all_greater @ total) lower xs =
    match xs with
    | Nil -> true
    | Cons (head, tail) ->
      less lower head && all_greater lower tail

  let[@def] rec (valid @ total) xs =
    match xs with
    | Nil -> true
    | Cons (head, tail) -> all_greater head tail && valid tail

  type t = {xs : repr | valid xs}

  let (empty @ total) : t =
    let xs = Nil in
    let refine_ equation = ghost_ (valid_def xs) in
    refine_ xs

  let[@def] rec (lookup_repr @ total) element xs =
    match xs with
    | Nil -> false
    | Cons (head, tail) ->
      equivalent element head || lookup_repr element tail

  let[@def] (lookup @ total) element (set : t) =
    let refine_ xs = set in
    lookup_repr element xs

  let[@def] rec (equal_repr @ total) left right =
    match left with
    | Nil ->
      (match right with
       | Nil -> true
       | Cons _ -> false)
    | Cons (left_head, left_tail) ->
      match right with
      | Nil -> false
      | Cons (right_head, right_tail) ->
        equivalent left_head right_head
        && equal_repr left_tail right_tail

  let rec (equal_repr_lookup @ total) :
      (element : elt) ->
      (left : repr) ->
      (right : repr) ->
      {u : unit |
        if equal_repr left right
        then lookup_repr element left === lookup_repr element right
        else true} @ immutable contended =
    fun element left right ->
    let u = () in
    if equal_repr left right then
      let refine_ equality = equal_repr_def left right in
      let refine_ left_lookup = lookup_repr_def element left in
      let refine_ right_lookup = lookup_repr_def element right in
      match left with
      | Nil -> refine_ u
      | Cons (left_head, left_tail) ->
        match right with
        | Nil -> refine_ u
        | Cons (right_head, right_tail) ->
          let refine_ heads =
            equivalent_congruent element left_head right_head
          in
          let refine_ induction =
            equal_repr_lookup element left_tail right_tail
          in
          refine_ u
    else refine_ u

  let rec (equal_repr_reflexive @ total) :
      (xs : repr) ->
      {u : unit | equal_repr xs xs === true} @ immutable contended =
    fun xs ->
    let refine_ equation = equal_repr_def xs xs in
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      let refine_ head_equal = equivalent_reflexive head in
      let refine_ induction = equal_repr_reflexive tail in
      let u = () in
      refine_ u

  let[@def] rec (add_repr @ total) element xs =
    match xs with
    | Nil -> Cons (element, Nil)
    | Cons (head, tail) ->
      if equivalent element head then xs
      else if less element head then Cons (element, xs)
      else Cons (head, add_repr element tail)

  let rec (lookup_add_repr @ total) :
      (element : elt) ->
      (added : elt) ->
      (xs : repr) ->
      {u : unit |
        lookup_repr element (add_repr added xs)
        === (equivalent element added || lookup_repr element xs)}
        @ immutable contended =
    fun element added xs ->
    let result = add_repr added xs in
    let refine_ add_equation = add_repr_def added xs in
    let refine_ result_equation = lookup_repr_def element result in
    let refine_ input_equation = lookup_repr_def element xs in
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      if equivalent added head then
        let refine_ heads = equivalent_congruent element added head in
        let u = () in
        refine_ u
      else if less added head then
        let u = () in
        refine_ u
      else
        let refine_ induction = lookup_add_repr element added tail in
        let u = () in
        refine_ u

  let rec (all_greater_transitive @ total) :
      (lower : elt) ->
      (middle : elt) ->
      (xs : repr) ->
      {u : unit |
        if less lower middle && all_greater middle xs
        then all_greater lower xs
        else true} @ immutable contended =
    fun lower middle xs ->
    let u = () in
    if less lower middle && all_greater middle xs then
      match xs with
      | Nil ->
        let refine_ lower_equation = all_greater_def lower xs in
        refine_ u
      | Cons (head, tail) ->
        let refine_ middle_equation = all_greater_def middle xs in
        let refine_ order = less_transitive lower middle head in
        let refine_ lower_equation = all_greater_def lower xs in
        let refine_ induction = all_greater_transitive lower middle tail in
        refine_ u
    else refine_ u

  let rec (add_preserves_lower @ total) :
      (lower : elt) ->
      (element : elt) ->
      (xs : repr) ->
      {u : unit |
        if less lower element && all_greater lower xs
        then all_greater lower (add_repr element xs)
        else true} @ immutable contended =
    fun lower element xs ->
    let u = () in
    if less lower element && all_greater lower xs then
      let result = add_repr element xs in
      let refine_ add_equation = add_repr_def element xs in
      match xs with
      | Nil ->
        let refine_ result_equation = all_greater_def lower result in
        refine_ u
      | Cons (head, tail) ->
        let refine_ input_equation = all_greater_def lower xs in
        if equivalent element head then refine_ u
        else if less element head then
          let refine_ result_equation = all_greater_def lower result in
          refine_ u
        else
          let refine_ induction = add_preserves_lower lower element tail in
          let refine_ result_equation = all_greater_def lower result in
          refine_ u
    else refine_ u

  let rec (add_valid @ total) :
      (element : elt) ->
      (xs : repr) ->
      {u : unit |
        if valid xs then valid (add_repr element xs) else true}
        @ immutable contended =
    fun element xs ->
    let u = () in
    if valid xs then
      let result = add_repr element xs in
      let refine_ add_equation = add_repr_def element xs in
      match xs with
      | Nil ->
        let refine_ result_equation = valid_def result in
        let refine_ greater_equation = all_greater_def element xs in
        refine_ u
      | Cons (head, tail) ->
        let refine_ input_equation = valid_def xs in
        let refine_ equivalent_equation = equivalent_def element head in
        let refine_ less_equation = less_def element head in
        if equivalent element head then refine_ u
        else if less element head then
          let refine_ transitive =
            all_greater_transitive element head tail
          in
          let refine_ result_equation = valid_def result in
          let refine_ greater_equation = all_greater_def element xs in
          refine_ u
        else
          let refine_ reversed = reverse_less element head in
          let refine_ lower = add_preserves_lower head element tail in
          let refine_ induction = add_valid element tail in
          let refine_ result_equation = valid_def result in
          refine_ u
    else refine_ u

  let[@def] (add @ total) :
      elt -> t -> t @ immutable contended =
    fun element set ->
    let refine_ xs = set in
    let (result @ total) = (add_repr element xs : repr @ total) in
    let refine_ invariant = ghost_ (add_valid element xs) in
    refine_ result

  let (lookup_empty @ total) element :
      {u : unit | lookup element empty === false} =
    let empty_set = empty in
    let refine_ xs = empty_set in
    let refine_ lookup_equation = lookup_def element empty_set in
    let refine_ repr_equation = lookup_repr_def element xs in
    let u = () in
    refine_ u

  let (lookup_add @ total) :
      (element : elt) ->
      (added : elt) ->
      (set : t) ->
      {u : unit |
        lookup element (add added set)
        ===
        (Element.compare element added = 0 || lookup element set)}
        @ immutable contended =
    fun element added set ->
    let refine_ xs = set in
    let result = add added set in
    let refine_ add_equation = add_def added set in
    let refine_ ys = result in
    let expected = add_repr added xs in
    let refine_ reflexive = equal_repr_reflexive expected in
    let refine_ same_result = equal_repr_lookup element ys expected in
    let refine_ induction = lookup_add_repr element added xs in
    let refine_ equivalent_equation = equivalent_def element added in
    let refine_ result_equation = lookup_def element result in
    let refine_ input_equation = lookup_def element set in
    let u = () in
    refine_ u

  let[@def] rec (size_repr @ total) xs =
    match xs with
    | Nil -> 0Z
    | Cons (_, tail) -> Bigint.add 1Z (size_repr tail)

  let rec (size_nonnegative @ total) :
      (xs : repr) ->
      {u : unit | size_repr xs >= 0Z} @ immutable contended =
    fun xs ->
    let refine_ equation = size_repr_def xs in
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (_, tail) ->
      let refine_ induction = size_nonnegative tail in
      let u = () in
      refine_ u

  let[@def] rec (union_repr @ total) left right =
    match left with
    | Nil -> right
    | Cons (head, tail) -> union_repr tail (add_repr head right)

  let rec (union_valid @ total) :
      (left : repr) ->
      (right : repr) ->
      {u : unit |
        if valid left && valid right
        then valid (union_repr left right)
        else true} @ immutable contended =
    fun left right ->
    let u = () in
    if valid left && valid right then
      let refine_ union_equation = union_repr_def left right in
      match left with
      | Nil -> refine_ u
      | Cons (head, tail) ->
        let added = add_repr head right in
        let refine_ input_equation = valid_def left in
        let refine_ added_valid = add_valid head right in
        let refine_ induction = union_valid tail added in
        refine_ u
    else refine_ u

  let rec (lookup_union_repr @ total) :
      (element : elt) ->
      (left : repr) ->
      (right : repr) ->
      {u : unit |
        lookup_repr element (union_repr left right)
        === (lookup_repr element left || lookup_repr element right)}
        @ immutable contended =
    fun element left right ->
    let result = union_repr left right in
    let refine_ union_equation = union_repr_def left right in
    let refine_ result_equation = lookup_repr_def element result in
    let refine_ left_equation = lookup_repr_def element left in
    match left with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      let added = add_repr head right in
      let refine_ added_equation = lookup_add_repr element head right in
      let refine_ induction = lookup_union_repr element tail added in
      let u = () in
      refine_ u

  let[@def] (union @ total) :
      t -> t -> t @ immutable contended =
    fun left right ->
    let refine_ xs = left in
    let refine_ ys = right in
    let (result @ total) = (union_repr xs ys : repr @ total) in
    let refine_ invariant = ghost_ (union_valid xs ys) in
    refine_ result

  let (lookup_union @ total) :
      (element : elt) ->
      (left : t) ->
      (right : t) ->
      {u : unit |
        lookup element (union left right)
        === (lookup element left || lookup element right)}
        @ immutable contended =
    fun element left right ->
    let refine_ xs = left in
    let refine_ ys = right in
    let result = union left right in
    let refine_ union_equation = union_def left right in
    let refine_ zs = result in
    let expected = union_repr xs ys in
    let refine_ reflexive = equal_repr_reflexive expected in
    let refine_ same_result = equal_repr_lookup element zs expected in
    let refine_ induction = lookup_union_repr element xs ys in
    let refine_ result_equation = lookup_def element result in
    let refine_ left_equation = lookup_def element left in
    let refine_ right_equation = lookup_def element right in
    let u = () in
    refine_ u
  let rec (size_zero_repr @ total) :
      (xs : repr) ->
      {u : unit |
        (size_repr xs === 0Z) === equal_repr xs Nil}
        @ immutable contended =
    fun xs ->
    let nil = Nil in
    let refine_ size_equation = size_repr_def xs in
    let refine_ equal_equation = equal_repr_def xs nil in
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (_, tail) ->
      let refine_ nonnegative = size_nonnegative tail in
      let u = () in
      refine_ u

  let[@def] (size @ total) (set : t) =
    let refine_ xs = set in
    size_repr xs

  let[@def] (equal @ total) (left : t) (right : t) =
    let refine_ xs = left in
    let refine_ ys = right in
    equal_repr xs ys

  let (size_zero @ total) (set : t) :
      {u : unit | (size set === 0Z) === equal set empty} =
    let empty_set = empty in
    let refine_ xs = set in
    let refine_ empty_repr = empty_set in
    let refine_ size_equation = size_def set in
    let refine_ equal_equation = equal_def set empty_set in
    let refine_ induction = size_zero_repr xs in
    let u = () in
    refine_ u

  let (equal_lookup @ total) :
      (left : t) ->
      (right : t) ->
      (element : elt) ->
      {u : unit |
        if equal left right
        then lookup element left === lookup element right
        else true} @ immutable contended =
    fun left right element ->
    let u = () in
    if equal left right then
      let refine_ xs = left in
      let refine_ ys = right in
      let refine_ equality = equal_def left right in
      let refine_ same_lookup = equal_repr_lookup element xs ys in
      let refine_ left_lookup = lookup_def element left in
      let refine_ right_lookup = lookup_def element right in
      refine_ u
    else refine_ u

  let rec (lookup_below @ total) :
      (lower : elt) ->
      (element : elt) ->
      (xs : repr) ->
      {u : unit |
        if Element.compare element lower <= 0 && all_greater lower xs
        then lookup_repr element xs === false
        else true} @ immutable contended =
    fun lower element xs ->
    let u = () in
    if Element.compare element lower <= 0 && all_greater lower xs then
      match xs with
      | Nil ->
        let refine_ lookup_equation = lookup_repr_def element xs in
        refine_ u
      | Cons (head, tail) ->
        let refine_ order_equation = all_greater_def lower xs in
        let refine_ below = le_less_transitive element lower head in
        let refine_ comparison = less_def element head in
        let refine_ equivalence = equivalent_def element head in
        let refine_ lookup_equation = lookup_repr_def element xs in
        let refine_ induction = lookup_below lower element tail in
        refine_ u
    else refine_ u

  let rec (extensional_repr @ total) :
      (left : repr) ->
      (right : repr) ->
      ((element : elt) ->
        {u : unit |
          lookup_repr element left === lookup_repr element right})
        @ total ->
      {u : unit |
        if valid left && valid right
        then equal_repr left right
        else true} @ immutable contended =
    fun left right premise ->
    let u = () in
    if valid left && valid right then
      let refine_ left_valid = valid_def left in
      let refine_ right_valid = valid_def right in
      match left with
      | Nil ->
        (match right with
         | Nil ->
           let refine_ equality = equal_repr_def left right in
           refine_ u
         | Cons (right_head, _) ->
           let refine_ reflexive = equivalent_reflexive right_head in
           let refine_ same_lookup = premise right_head in
           let refine_ left_lookup = lookup_repr_def right_head left in
           let refine_ right_lookup = lookup_repr_def right_head right in
           refine_ u)
      | Cons (left_head, left_tail) ->
        match right with
        | Nil ->
          let refine_ reflexive = equivalent_reflexive left_head in
          let refine_ same_lookup = premise left_head in
          let refine_ left_lookup = lookup_repr_def left_head left in
          let refine_ right_lookup = lookup_repr_def left_head right in
          refine_ u
        | Cons (right_head, right_tail) ->
          let refine_ left_lookup = premise left_head in
          let refine_ right_lookup = premise right_head in
          let refine_ left_reflexive = equivalent_reflexive left_head in
          let refine_ right_reflexive = equivalent_reflexive right_head in
          let refine_ left_at_left = lookup_repr_def left_head left in
          let refine_ right_at_left = lookup_repr_def left_head right in
          let refine_ left_at_right = lookup_repr_def right_head left in
          let refine_ right_at_right = lookup_repr_def right_head right in
          if less left_head right_head then
            let refine_ order = less_def left_head right_head in
            let refine_ not_equivalent =
              equivalent_def left_head right_head
            in
            let refine_ absent =
              lookup_below right_head left_head right_tail
            in
            refine_ u
          else if less right_head left_head then
            let refine_ order = less_def right_head left_head in
            let refine_ not_equivalent =
              equivalent_def right_head left_head
            in
            let refine_ absent =
              lookup_below left_head right_head left_tail
            in
            refine_ u
          else
            let refine_ heads =
              not_less_equivalent left_head right_head
            in
            let (tail_premise @ total) :
                (element : elt) ->
                {u : unit |
                  lookup_repr element left_tail
                  === lookup_repr element right_tail} =
              fun element ->
              let refine_ same_lookup = premise element in
              let refine_ left_equation = lookup_repr_def element left in
              let refine_ right_equation = lookup_repr_def element right in
              let refine_ head_congruence =
                equivalent_congruent element left_head right_head
              in
              if equivalent element left_head then
                let refine_ element_left =
                  equivalent_def element left_head
                in
                let refine_ left_absent =
                  lookup_below left_head element left_tail
                in
                let refine_ element_right =
                  equivalent_transitive element left_head right_head
                in
                let refine_ element_right_equation =
                  equivalent_def element right_head
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
            let refine_ equality = equal_repr_def left right in
            refine_ u
    else refine_ u

  let (extensional @ total) :
      (left : t) ->
      (right : t) ->
      ((element : elt) ->
        {u : unit | lookup element left === lookup element right})
        @ total ->
      {u : unit | equal left right === true} @ immutable contended =
    fun left right premise ->
    let refine_ xs = left in
    let refine_ ys = right in
    let (repr_premise @ total) :
        (element : elt) ->
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
    let refine_ equality = equal_def left right in
    let u = () in
    refine_ u
end
