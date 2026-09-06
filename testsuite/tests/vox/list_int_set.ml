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

    let (empty @ total) : t =
      let xs = Nil in
      let refine_ equation = ghost_ (valid_def xs) in
      refine_ xs

    let[@def] rec (lookup_repr @ total) element xs =
      match xs with
      | Nil -> false
      | Cons (head, tail) ->
        element = head || lookup_repr element tail

    let[@def] (lookup @ total) element (set : t) =
      let refine_ xs = set in
      lookup_repr element xs

    let[@def] rec (add_repr @ total) element xs =
      match xs with
      | Nil -> Cons (element, Nil)
      | Cons (head, tail) ->
        if element = head then xs
        else if element < head then Cons (element, xs)
        else Cons (head, add_repr element tail)

    let[@def] rec (same_repr @ total) left right =
      match left with
      | Nil ->
        (match right with
         | Nil -> true
         | Cons _ -> false)
      | Cons (left_head, left_tail) ->
        match right with
        | Nil -> false
        | Cons (right_head, right_tail) ->
          left_head = right_head && same_repr left_tail right_tail

    let rec (same_repr_reflexive @ total) :
        (xs : repr) ->
        {u : unit | same_repr xs xs === true} @ immutable contended =
      fun xs ->
      let refine_ equation = same_repr_def xs xs in
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let refine_ induction = same_repr_reflexive tail in
        let u = () in
        refine_ u

    let rec (same_repr_equal @ total) :
        (left : repr) ->
        (right : repr) ->
        {u : unit |
          if same_repr left right then left === right else true}
          @ immutable contended =
      fun left right ->
      let u = () in
      if same_repr left right then
        let refine_ equation = same_repr_def left right in
        match left with
        | Nil -> refine_ u
        | Cons (_, left_tail) ->
          match right with
          | Nil -> refine_ u
          | Cons (_, right_tail) ->
            let refine_ induction = same_repr_equal left_tail right_tail in
            refine_ u
      else refine_ u

    let rec (lookup_add_repr @ total) :
        (element : int) ->
        (added : int) ->
        (xs : repr) ->
        {u : unit |
          lookup_repr element (add_repr added xs)
          === (element = added || lookup_repr element xs)}
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
      | Cons (_, tail) ->
        let refine_ induction = lookup_add_repr element added tail in
        let u = () in
        refine_ u

    let rec (all_greater_transitive @ total) :
        (lower : int) ->
        (middle : int) ->
        (xs : repr) ->
        {u : unit |
          if lower < middle && all_greater middle xs
          then all_greater lower xs
          else true} @ immutable contended =
      fun lower middle xs ->
      let u = () in
      if lower < middle && all_greater middle xs then
        match xs with
        | Nil ->
          let refine_ lower_equation = all_greater_def lower xs in
          refine_ u
        | Cons (_, tail) ->
          let refine_ middle_equation = all_greater_def middle xs in
          let refine_ lower_equation = all_greater_def lower xs in
          let refine_ induction = all_greater_transitive lower middle tail in
          refine_ u
      else refine_ u

    let rec (add_preserves_lower @ total) :
        (lower : int) ->
        (element : int) ->
        (xs : repr) ->
        {u : unit |
          if lower < element && all_greater lower xs
          then all_greater lower (add_repr element xs)
          else true} @ immutable contended =
      fun lower element xs ->
      let u = () in
      if lower < element && all_greater lower xs then
        let result = add_repr element xs in
        let refine_ add_equation = add_repr_def element xs in
        match xs with
        | Nil ->
          let refine_ result_equation = all_greater_def lower result in
          refine_ u
        | Cons (head, tail) ->
          let refine_ input_equation = all_greater_def lower xs in
          if element = head then refine_ u
          else if element < head then
            let refine_ result_equation = all_greater_def lower result in
            refine_ u
          else
            let refine_ induction = add_preserves_lower lower element tail in
            let refine_ result_equation = all_greater_def lower result in
            refine_ u
      else refine_ u

    let rec (add_valid @ total) :
        (element : int) ->
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
          if element = head then refine_ u
          else if element < head then
            let refine_ transitive =
              all_greater_transitive element head tail
            in
            let refine_ result_equation = valid_def result in
            let refine_ greater_equation = all_greater_def element xs in
            refine_ u
          else
            let refine_ lower = add_preserves_lower head element tail in
            let refine_ induction = add_valid element tail in
            let refine_ result_equation = valid_def result in
            refine_ u
      else refine_ u

    let[@def] (add @ total) :
        int -> t -> t @ immutable contended =
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
        (element : int) ->
        (added : int) ->
        (set : t) ->
        {u : unit |
          lookup element (add added set)
          === (element = added || lookup element set)}
          @ immutable contended =
      fun element added set ->
      let refine_ xs = set in
      let result = add added set in
      let refine_ add_equation = add_def added set in
      let refine_ ys = result in
      let expected = add_repr added xs in
      let refine_ reflexive = same_repr_reflexive expected in
      let refine_ same_result = same_repr_equal ys expected in
      let refine_ induction = lookup_add_repr element added xs in
      let refine_ result_equation = lookup_def element result in
      let refine_ input_equation = lookup_def element set in
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
        (element : int) ->
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
        (element : int) ->
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
      let refine_ reflexive = same_repr_reflexive expected in
      let refine_ same_result = same_repr_equal zs expected in
      let refine_ induction = lookup_union_repr element xs ys in
      let refine_ result_equation = lookup_def element result in
      let refine_ left_equation = lookup_def element left in
      let refine_ right_equation = lookup_def element right in
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

    let rec (size_zero_repr @ total) :
        (xs : repr) ->
        {u : unit |
          (size_repr xs === 0Z) === (xs === Nil)} @ immutable contended =
      fun xs ->
      let refine_ equation = size_repr_def xs in
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

    let (size_zero @ total) (set : t) :
        {u : unit |
          (size set === 0Z) === (set === empty)} =
      let empty_set = empty in
      let refine_ xs = set in
      let refine_ empty_repr = empty_set in
      let refine_ size_equation = size_def set in
      let refine_ induction = size_zero_repr xs in
      let u = () in
      refine_ u

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
