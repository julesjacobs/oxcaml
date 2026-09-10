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
      valid_def xs;
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
      same_repr_def xs xs;
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        same_repr_reflexive tail;
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
        (same_repr_def left right;
        match left with
        | Nil -> refine_ u
        | Cons (_, left_tail) ->
          match right with
          | Nil -> refine_ u
          | Cons (_, right_tail) ->
            same_repr_equal left_tail right_tail;
            refine_ u)
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
      add_repr_def added xs;
      lookup_repr_def element result;
      lookup_repr_def element xs;
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        lookup_add_repr element added tail;
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
          all_greater_def lower xs;
          refine_ u
        | Cons (_, tail) ->
          all_greater_def middle xs;
          all_greater_def lower xs;
          all_greater_transitive lower middle tail;
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
        add_repr_def element xs;
        match xs with
        | Nil ->
          all_greater_def lower result;
          refine_ u
        | Cons (head, tail) ->
          all_greater_def lower xs;
          if element = head then refine_ u
          else if element < head then
            (all_greater_def lower result;
            refine_ u)
          else
            (add_preserves_lower lower element tail;
            all_greater_def lower result;
            refine_ u)
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
        add_repr_def element xs;
        match xs with
        | Nil ->
          valid_def result;
          all_greater_def element xs;
          refine_ u
        | Cons (head, tail) ->
          valid_def xs;
          if element = head then refine_ u
          else if element < head then
            (all_greater_transitive element head tail;
            valid_def result;
            all_greater_def element xs;
            refine_ u)
          else
            (add_preserves_lower head element tail;
            add_valid element tail;
            valid_def result;
            refine_ u)
      else refine_ u

    let[@def] (add @ total) :
        int -> t -> t @ immutable contended =
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
      add_def added set;
      let refine_ ys = result in
      let expected = add_repr added xs in
      same_repr_reflexive expected;
      same_repr_equal ys expected;
      lookup_add_repr element added xs;
      lookup_def element result;
      lookup_def element set;
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
        (element : int) ->
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
      union_def left right;
      let refine_ zs = result in
      let expected = union_repr xs ys in
      same_repr_reflexive expected;
      same_repr_equal zs expected;
      lookup_union_repr element xs ys;
      lookup_def element result;
      lookup_def element left;
      lookup_def element right;
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

    let rec (size_zero_repr @ total) :
        (xs : repr) ->
        {u : unit |
          (size_repr xs === 0Z) === (xs === Nil)} @ immutable contended =
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

    let[@def] (size @ total) (set : t) =
      let refine_ xs = set in
      size_repr xs

    let (size_zero @ total) (set : t) :
        {u : unit |
          (size set === 0Z) === (set === empty)} =
      let empty_set = empty in
      let refine_ xs = set in
      let refine_ empty_repr = empty_set in
      size_def set;
      size_zero_repr xs;
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
          lookup_repr_def element xs;
          refine_ u
        | Cons (_, tail) ->
          all_greater_def lower xs;
          lookup_repr_def element xs;
          lookup_below lower element tail;
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
        (valid_def left;
        valid_def right;
        match left with
        | Nil ->
          (match right with
           | Nil -> refine_ u
           | Cons (right_head, _) ->
             let refine_ same_lookup = premise right_head in
             lookup_repr_def right_head left;
             lookup_repr_def right_head right;
             refine_ u)
        | Cons (left_head, left_tail) ->
          match right with
          | Nil ->
            let refine_ same_lookup = premise left_head in
            lookup_repr_def left_head left;
            lookup_repr_def left_head right;
            refine_ u
          | Cons (right_head, right_tail) ->
            let refine_ left_lookup = premise left_head in
            let refine_ right_lookup = premise right_head in
            lookup_repr_def left_head left;
            lookup_repr_def left_head right;
            lookup_repr_def right_head left;
            lookup_repr_def right_head right;
            if left_head < right_head then
              (lookup_below right_head left_head right_tail;
              refine_ u)
            else if right_head < left_head then
              (lookup_below left_head right_head left_tail;
              refine_ u)
            else
              let (tail_premise @ total) :
                  (element : int) ->
                  {u : unit |
                    lookup_repr element left_tail
                    === lookup_repr element right_tail} =
                fun element ->
                let refine_ same_lookup = premise element in
                lookup_repr_def element left;
                lookup_repr_def element right;
                if element = left_head then
                  (lookup_below left_head element left_tail;
                  lookup_below right_head element right_tail;
                  refine_ u)
                else refine_ u
              in
              extensional_repr left_tail right_tail tail_premise;
              refine_ u)
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
        lookup_def element left;
        lookup_def element right;
        let u = () in
        refine_ u
      in
      extensional_repr xs ys repr_premise;
      let u = () in
      refine_ u
