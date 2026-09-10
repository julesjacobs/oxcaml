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
    Element.compare_reflexive x;
    equivalent_def x x;
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
      (equivalent_def x y;
      Element.compare_reverse x y;
      Element.compare_reverse y x;
      equivalent_def y x;
      refine_ u)
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
      (equivalent_def x y;
      equivalent_def y z;
      equivalent_symmetric x y;
      equivalent_symmetric y z;
      equivalent_def y x;
      equivalent_def z y;
      Element.compare_transitive x y z;
      Element.compare_transitive z y x;
      Element.compare_reverse x z;
      Element.compare_reverse z x;
      equivalent_def x z;
      refine_ u)
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
      (equivalent_symmetric left right;
      if equivalent probe left then
        (equivalent_transitive probe left right;
        refine_ u)
      else if equivalent probe right then
        (equivalent_transitive probe right left;
        refine_ u)
      else refine_ u)
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
      (less_def x y;
      less_def y z;
      Element.compare_transitive x y z;
      Element.compare_reverse y x;
      Element.compare_reverse z x;
      Element.compare_transitive y z x;
      less_def x z;
      refine_ u)
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
      (less_def y z;
      Element.compare_transitive x y z;
      Element.compare_reverse z x;
      Element.compare_reverse z y;
      Element.compare_transitive z x y;
      less_def x z;
      refine_ u)
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
      (Element.compare_reverse x y;
      Element.compare_reverse y x;
      less_def y x;
      refine_ u)
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
      (less_def x y;
      less_def y x;
      Element.compare_reverse x y;
      equivalent_def x y;
      refine_ u)
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
    valid_def xs;
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
      (equal_repr_def left right;
      lookup_repr_def element left;
      lookup_repr_def element right;
      match left with
      | Nil -> refine_ u
      | Cons (left_head, left_tail) ->
        match right with
        | Nil -> refine_ u
        | Cons (right_head, right_tail) ->
          equivalent_congruent element left_head right_head;
          equal_repr_lookup element left_tail right_tail;
          refine_ u)
    else refine_ u

  let rec (equal_repr_reflexive @ total) :
      (xs : repr) ->
      {u : unit | equal_repr xs xs === true} @ immutable contended =
    fun xs ->
    equal_repr_def xs xs;
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      equivalent_reflexive head;
      equal_repr_reflexive tail;
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
    add_repr_def added xs;
    lookup_repr_def element result;
    lookup_repr_def element xs;
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      if equivalent added head then
        (equivalent_congruent element added head;
        let u = () in
        refine_ u)
      else if less added head then
        let u = () in
        refine_ u
      else
        (lookup_add_repr element added tail;
        let u = () in
        refine_ u)

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
        all_greater_def lower xs;
        refine_ u
      | Cons (head, tail) ->
        all_greater_def middle xs;
        less_transitive lower middle head;
        all_greater_def lower xs;
        all_greater_transitive lower middle tail;
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
      add_repr_def element xs;
      match xs with
      | Nil ->
        all_greater_def lower result;
        refine_ u
      | Cons (head, tail) ->
        all_greater_def lower xs;
        if equivalent element head then refine_ u
        else if less element head then
          (all_greater_def lower result;
          refine_ u)
        else
          (add_preserves_lower lower element tail;
          all_greater_def lower result;
          refine_ u)
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
      add_repr_def element xs;
      match xs with
      | Nil ->
        valid_def result;
        all_greater_def element xs;
        refine_ u
      | Cons (head, tail) ->
        valid_def xs;
        equivalent_def element head;
        less_def element head;
        if equivalent element head then refine_ u
        else if less element head then
          (all_greater_transitive element head tail;
          valid_def result;
          all_greater_def element xs;
          refine_ u)
        else
          (reverse_less element head;
          add_preserves_lower head element tail;
          add_valid element tail;
          valid_def result;
          refine_ u)
    else refine_ u

  let[@def] (add @ total) :
      elt -> t -> t @ immutable contended =
    fun element set ->
    let refine_ xs = set in
    let (result @ total) = (add_repr element xs : repr @ total) in
    add_valid element xs;
    refine_ result

  let (lookup_empty @ total) element :
      {u : unit | lookup element empty === false} =
    let empty_set = empty in
    let refine_ xs = empty_set in
    lookup_def element empty_set;
    lookup_repr_def element xs;
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
    add_def added set;
    let refine_ ys = result in
    let expected = add_repr added xs in
    equal_repr_reflexive expected;
    equal_repr_lookup element ys expected;
    lookup_add_repr element added xs;
    equivalent_def element added;
    lookup_def element result;
    lookup_def element set;
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
    size_repr_def xs;
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (_, tail) ->
      size_nonnegative tail;
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
      (union_repr_def left right;
      match left with
      | Nil -> refine_ u
      | Cons (head, tail) ->
        let added = add_repr head right in
        valid_def left;
        add_valid head right;
        union_valid tail added;
        refine_ u)
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
    union_repr_def left right;
    lookup_repr_def element result;
    lookup_repr_def element left;
    match left with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (head, tail) ->
      let added = add_repr head right in
      lookup_add_repr element head right;
      lookup_union_repr element tail added;
      let u = () in
      refine_ u

  let[@def] (union @ total) :
      t -> t -> t @ immutable contended =
    fun left right ->
    let refine_ xs = left in
    let refine_ ys = right in
    let (result @ total) = (union_repr xs ys : repr @ total) in
    union_valid xs ys;
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
    union_def left right;
    let refine_ zs = result in
    let expected = union_repr xs ys in
    equal_repr_reflexive expected;
    equal_repr_lookup element zs expected;
    lookup_union_repr element xs ys;
    lookup_def element result;
    lookup_def element left;
    lookup_def element right;
    let u = () in
    refine_ u
  let rec (size_zero_repr @ total) :
      (xs : repr) ->
      {u : unit |
        (size_repr xs === 0Z) === equal_repr xs Nil}
        @ immutable contended =
    fun xs ->
    let nil = Nil in
    size_repr_def xs;
    equal_repr_def xs nil;
    match xs with
    | Nil ->
      let u = () in
      refine_ u
    | Cons (_, tail) ->
      size_nonnegative tail;
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
    size_def set;
    equal_def set empty_set;
    size_zero_repr xs;
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
      equal_def left right;
      equal_repr_lookup element xs ys;
      lookup_def element left;
      lookup_def element right;
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
        lookup_repr_def element xs;
        refine_ u
      | Cons (head, tail) ->
        all_greater_def lower xs;
        le_less_transitive element lower head;
        less_def element head;
        equivalent_def element head;
        lookup_repr_def element xs;
        lookup_below lower element tail;
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
      (valid_def left;
      valid_def right;
      match left with
      | Nil ->
        (match right with
         | Nil ->
           equal_repr_def left right;
           refine_ u
         | Cons (right_head, _) ->
           equivalent_reflexive right_head;
           let refine_ same_lookup = premise right_head in
           lookup_repr_def right_head left;
           lookup_repr_def right_head right;
           refine_ u)
      | Cons (left_head, left_tail) ->
        match right with
        | Nil ->
          equivalent_reflexive left_head;
          let refine_ same_lookup = premise left_head in
          lookup_repr_def left_head left;
          lookup_repr_def left_head right;
          refine_ u
        | Cons (right_head, right_tail) ->
          let refine_ left_lookup = premise left_head in
          let refine_ right_lookup = premise right_head in
          equivalent_reflexive left_head;
          equivalent_reflexive right_head;
          lookup_repr_def left_head left;
          lookup_repr_def left_head right;
          lookup_repr_def right_head left;
          lookup_repr_def right_head right;
          if less left_head right_head then
            (less_def left_head right_head;
            equivalent_def left_head right_head;
            lookup_below right_head left_head right_tail;
            refine_ u)
          else if less right_head left_head then
            (less_def right_head left_head;
            equivalent_def right_head left_head;
            lookup_below left_head right_head left_tail;
            refine_ u)
          else
            (not_less_equivalent left_head right_head;
            let (tail_premise @ total) :
                (element : elt) ->
                {u : unit |
                  lookup_repr element left_tail
                  === lookup_repr element right_tail} =
              fun element ->
              let refine_ same_lookup = premise element in
              lookup_repr_def element left;
              lookup_repr_def element right;
              equivalent_congruent element left_head right_head;
              if equivalent element left_head then
                (equivalent_def element left_head;
                lookup_below left_head element left_tail;
                equivalent_transitive element left_head right_head;
                equivalent_def element right_head;
                lookup_below right_head element right_tail;
                refine_ u)
              else refine_ u
            in
            extensional_repr left_tail right_tail tail_premise;
            equal_repr_def left right;
            refine_ u))
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
      lookup_def element left;
      lookup_def element right;
      let u = () in
      refine_ u
    in
    extensional_repr xs ys repr_premise;
    equal_def left right;
    let u = () in
    refine_ u
end
