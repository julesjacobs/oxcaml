(* TEST
 flags = "-extension refinement_types";
 has-z3;
 timeout = "120";
 all_modules = "avl_sets.mli avl_sets.ml";
 { bytecode; }
 { flags += " -principal"; bytecode; }
*)

module List_set = struct
    type repr = Nil | Cons of int * repr [@@inductive]

    let[@def] rec (append @ total) left right =
      match left with
      | Nil -> right
      | Cons (head, tail) -> Cons (head, append tail right)

    let[@def] rec (all_less @ total) upper xs =
      match xs with
      | Nil -> true
      | Cons (head, tail) -> head < upper && all_less upper tail

    let[@def] rec (all_greater @ total) lower xs =
      match xs with
      | Nil -> true
      | Cons (head, tail) -> lower < head && all_greater lower tail

    let[@def] rec (add_repr @ total) element xs =
      match xs with
      | Nil -> Cons (element, Nil)
      | Cons (head, tail) ->
        if element = head then xs
        else if element < head then Cons (element, xs)
        else Cons (head, add_repr element tail)

    let[@def] rec (lookup_repr @ total) element xs =
      match xs with
      | Nil -> false
      | Cons (head, tail) -> element = head || lookup_repr element tail

    let[@def] rec (union_repr @ total) left right =
      match left with
      | Nil -> right
      | Cons (head, tail) -> union_repr tail (add_repr head right)

    let[@def] rec (size_repr @ total) xs =
      match xs with
      | Nil -> 0Z
      | Cons (_, tail) -> Bigint.add 1Z (size_repr tail)
end

module List_proofs = struct
    open List_set

    let rec (append_associative @ total) :
        (left : repr) ->
        (middle : repr) ->
        (right : repr) ->
        {u : unit |
          append (append left middle) right
          === append left (append middle right)} @ immutable contended =
      fun left middle right ->
      let left_middle = append left middle in
      let middle_right = append middle right in
      let refine_ left_equation = append_def left middle in
      let refine_ result_equation = append_def left_middle right in
      let refine_ right_equation = append_def left middle_right in
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let refine_ induction = append_associative tail middle right in
        let u = () in
        refine_ u

    let rec (all_less_append @ total) :
        (upper : int) ->
        (left : repr) ->
        (right : repr) ->
        {u : unit |
          all_less upper (append left right)
          === (all_less upper left && all_less upper right)}
          @ immutable contended =
      fun upper left right ->
      let result = append left right in
      let refine_ append_equation = append_def left right in
      let refine_ result_bound = all_less_def upper result in
      let refine_ left_bound = all_less_def upper left in
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let refine_ induction = all_less_append upper tail right in
        let u = () in
        refine_ u

    let rec (all_greater_append @ total) :
        (lower : int) ->
        (left : repr) ->
        (right : repr) ->
        {u : unit |
          all_greater lower (append left right)
          === (all_greater lower left && all_greater lower right)}
          @ immutable contended =
      fun lower left right ->
      let result = append left right in
      let refine_ append_equation = append_def left right in
      let refine_ result_bound = all_greater_def lower result in
      let refine_ left_bound = all_greater_def lower left in
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let refine_ induction = all_greater_append lower tail right in
        let u = () in
        refine_ u

    let rec (lookup_append @ total) :
        (element : int) ->
        (left : repr) ->
        (right : repr) ->
        {u : unit |
          lookup_repr element (append left right)
          === (lookup_repr element left || lookup_repr element right)}
          @ immutable contended =
      fun element left right ->
      let appended = append left right in
      let refine_ append_equation = append_def left right in
      let refine_ appended_lookup = lookup_repr_def element appended in
      let refine_ left_lookup = lookup_repr_def element left in
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        let refine_ induction = lookup_append element tail right in
        let u = () in
        refine_ u

    let rec (lookup_above @ total) :
        (element : int) ->
        (upper : int) ->
        (xs : repr) ->
        {u : unit |
          if upper <= element && all_less upper xs
          then lookup_repr element xs === false
          else true} @ immutable contended =
      fun element upper xs ->
      let u = () in
      if upper <= element && all_less upper xs then
        let refine_ bound = all_less_def upper xs in
        let refine_ lookup = lookup_repr_def element xs in
        match xs with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          let refine_ induction = lookup_above element upper tail in
          refine_ u
      else refine_ u

    let rec (lookup_below @ total) :
        (element : int) ->
        (lower : int) ->
        (xs : repr) ->
        {u : unit |
          if element <= lower && all_greater lower xs
          then lookup_repr element xs === false
          else true} @ immutable contended =
      fun element lower xs ->
      let u = () in
      if element <= lower && all_greater lower xs then
        let refine_ bound = all_greater_def lower xs in
        let refine_ lookup = lookup_repr_def element xs in
        match xs with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          let refine_ induction = lookup_below element lower tail in
          refine_ u
      else refine_ u

    let rec (add_left @ total) :
        (element : int) ->
        (left : repr) ->
        (pivot : int) ->
        (right : repr) ->
        {u : unit |
          if element < pivot
          then
            add_repr element (append left (Cons (pivot, right)))
            === append (add_repr element left) (Cons (pivot, right))
          else true} @ immutable contended =
      fun element left pivot right ->
      let u = () in
      if element < pivot then
        let suffix = Cons (pivot, right) in
        let whole = append left suffix in
        let added_left = add_repr element left in
        let refine_ whole_equation = append_def left suffix in
        let refine_ whole_add = add_repr_def element whole in
        let refine_ left_add = add_repr_def element left in
        let refine_ result_equation = append_def added_left suffix in
        match left with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          let refine_ induction = add_left element tail pivot right in
          refine_ u
      else refine_ u

    let rec (add_right @ total) :
        (element : int) ->
        (left : repr) ->
        (pivot : int) ->
        (right : repr) ->
        {u : unit |
          if pivot < element && all_less pivot left
          then
            add_repr element (append left (Cons (pivot, right)))
            === append left (Cons (pivot, add_repr element right))
          else true} @ immutable contended =
      fun element left pivot right ->
      let u = () in
      if pivot < element && all_less pivot left then
        let suffix = Cons (pivot, right) in
        let whole = append left suffix in
        let added_right = add_repr element right in
        let result_suffix = Cons (pivot, added_right) in
        let refine_ bound_equation = all_less_def pivot left in
        let refine_ whole_equation = append_def left suffix in
        let refine_ whole_add = add_repr_def element whole in
        let refine_ result_equation = append_def left result_suffix in
        match left with
        | Nil ->
          let refine_ right_add = add_repr_def element right in
          refine_ u
        | Cons (_, tail) ->
          let refine_ induction = add_right element tail pivot right in
          refine_ u
      else refine_ u

    let rec (add_at_pivot @ total) :
        (left : repr) ->
        (pivot : int) ->
        (right : repr) ->
        {u : unit |
          if all_less pivot left
          then
            add_repr pivot (append left (Cons (pivot, right)))
            === append left (Cons (pivot, right))
          else true} @ immutable contended =
      fun left pivot right ->
      let u = () in
      if all_less pivot left then
        let suffix = Cons (pivot, right) in
        let whole = append left suffix in
        let refine_ bound_equation = all_less_def pivot left in
        let refine_ whole_equation = append_def left suffix in
        let refine_ add_equation = add_repr_def pivot whole in
        match left with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          let refine_ induction = add_at_pivot tail pivot right in
          refine_ u
      else refine_ u
end

module Core = struct
type tree = Empty | Node of tree * int * tree * Bigint.t [@@inductive]

    let[@def] (maximum @ total) left right =
      if Bigint.compare left right >= 0 then left else right

    let[@def] (height @ total) tree =
      match tree with
      | Empty -> 0Z
      | Node (_, _, _, cached_height) -> cached_height

    let[@def] rec (all_less @ total) upper tree =
      match tree with
      | Empty -> true
      | Node (left, value, right, _) ->
        value < upper && all_less upper left && all_less upper right

    let[@def] rec (all_greater @ total) lower tree =
      match tree with
      | Empty -> true
      | Node (left, value, right, _) ->
        lower < value && all_greater lower left && all_greater lower right

    let[@def] rec (valid @ total) tree =
      match tree with
      | Empty -> true
      | Node (left, value, right, cached_height) ->
        all_less value left
        && all_greater value right
        && Bigint.compare (height left) (Bigint.add (height right) 1Z) <= 0
        && Bigint.compare (height right) (Bigint.add (height left) 1Z) <= 0
        && cached_height
           = Bigint.add 1Z (maximum (height left) (height right))
        && valid left
        && valid right

    type t = tree

    let[@def] rec (elements @ total) tree =
      match tree with
      | Empty -> List_set.Nil
      | Node (left, value, right, _) ->
        List_set.append (elements left)
          (List_set.Cons (value, elements right))

    let (empty @ total) : t = Empty

    let[@def] rec (lookup_tree @ total) element tree =
      match tree with
      | Empty -> false
      | Node (left, value, right, _) ->
        if element = value then true
        else if element < value then lookup_tree element left
        else lookup_tree element right

    let[@def] (make_node @ total) left value right =
      let left_height = height left in
      let right_height = height right in
      let cached_height =
        Bigint.add 1Z (maximum left_height right_height)
      in
      Node (left, value, right, cached_height)

    let[@def] (balance @ total) left value right =
      let left_height = height left in
      let right_height = height right in
      if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
        match left with
        | Empty ->
          make_node left value right
        | Node (left_left, left_value, left_right, _) ->
          if Bigint.compare (height left_left) (height left_right) >= 0 then
            let new_right = make_node left_right value right in
            make_node left_left left_value new_right
          else
            match left_right with
            | Empty ->
              make_node left value right
            | Node (middle_left, middle_value, middle_right, _) ->
              let new_left = make_node left_left left_value middle_left in
              let new_right = make_node middle_right value right in
              make_node new_left middle_value new_right
      else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0 then
        match right with
        | Empty ->
          make_node left value right
        | Node (right_left, right_value, right_right, _) ->
          if Bigint.compare (height right_right) (height right_left) >= 0 then
            let new_left = make_node left value right_left in
            make_node new_left right_value right_right
          else
            match right_left with
            | Empty ->
              make_node left value right
            | Node (middle_left, middle_value, middle_right, _) ->
              let new_left = make_node left value middle_left in
              let new_right = make_node middle_right right_value right_right in
              make_node new_left middle_value new_right
      else make_node left value right

    let[@def] rec (add_tree @ total) element tree =
      match tree with
      | Empty -> Node (Empty, element, Empty, 1Z)
      | Node (left, value, right, _) ->
        if element = value then tree
        else if element < value then
          let new_left = add_tree element left in
          balance new_left value right
        else
          let new_right = add_tree element right in
          balance left value new_right

    let () =
      let one_element = 1 in
      let two_element = 2 in
      let three_element = 3 in
      let tree =
        add_tree three_element
          (add_tree two_element (add_tree one_element empty))
      in
      let right_rotation =
        add_tree one_element
          (add_tree two_element (add_tree three_element empty))
      in
      let left_right_rotation =
        add_tree two_element
          (add_tree one_element (add_tree three_element empty))
      in
      let right_left_rotation =
        add_tree two_element
          (add_tree three_element (add_tree one_element empty))
      in
      let tree_elements = elements tree in
      Format.printf
        "height = %s; valid = %b; members = %b,%b,%b; size = %s@."
        (Bigint.to_string (height tree))
        (valid tree)
        (lookup_tree one_element tree) (lookup_tree two_element tree)
        (lookup_tree three_element tree)
        (Bigint.to_string (List_set.size_repr tree_elements));
      Format.printf "rotations valid = %b,%b,%b,%b@."
        (valid tree) (valid right_rotation)
      (valid left_right_rotation) (valid right_left_rotation)

end

open Core

module Validity_proofs : sig
      val add_valid_height :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          valid (add_tree element tree)
          && (height (add_tree element tree) = height tree
              || height (add_tree element tree)
                 = Bigint.add (height tree) 1Z)} @ immutable contended
    end = struct
      let (left_rotation_heights @ total) :
          (left_left : Bigint.t) ->
          (left_right : Bigint.t) ->
          (right : Bigint.t) ->
          (left : Bigint.t) ->
          {u : unit |
            if left = Bigint.add 1Z (maximum left_left left_right)
               && left = Bigint.add right 2Z
               && Bigint.compare left_left (Bigint.add left_right 1Z) <= 0
               && Bigint.compare left_right (Bigint.add left_left 1Z) <= 0
               && Bigint.compare left_left left_right >= 0
            then
              let new_right = Bigint.add 1Z (maximum left_right right) in
              Bigint.compare left_right (Bigint.add right 1Z) <= 0
              && Bigint.compare right (Bigint.add left_right 1Z) <= 0
              && Bigint.compare left_left (Bigint.add new_right 1Z) <= 0
              && Bigint.compare new_right (Bigint.add left_left 1Z) <= 0
              && (Bigint.add 1Z (maximum left_left new_right) = left
                  || Bigint.add 1Z (maximum left_left new_right)
                     = Bigint.add left 1Z)
            else true} @ immutable contended =
        fun left_left left_right right left ->
        let new_right = Bigint.add 1Z (maximum left_right right) in
        let u = () in
        if left = Bigint.add 1Z (maximum left_left left_right)
           && left = Bigint.add right 2Z
           && Bigint.compare left_left (Bigint.add left_right 1Z) <= 0
           && Bigint.compare left_right (Bigint.add left_left 1Z) <= 0
           && Bigint.compare left_left left_right >= 0
        then
          let refine_ left_maximum = maximum_def left_left left_right in
          let refine_ right_maximum = maximum_def left_right right in
          let refine_ result_maximum = maximum_def left_left new_right in
          refine_ u
        else refine_ u

      let (right_rotation_heights @ total) :
          (right_left : Bigint.t) ->
          (right_right : Bigint.t) ->
          (left : Bigint.t) ->
          (right : Bigint.t) ->
          {u : unit |
            if right = Bigint.add 1Z (maximum right_left right_right)
               && right = Bigint.add left 2Z
               && Bigint.compare right_left (Bigint.add right_right 1Z) <= 0
               && Bigint.compare right_right (Bigint.add right_left 1Z) <= 0
               && Bigint.compare right_right right_left >= 0
            then
              let new_left = Bigint.add 1Z (maximum left right_left) in
              Bigint.compare left (Bigint.add right_left 1Z) <= 0
              && Bigint.compare right_left (Bigint.add left 1Z) <= 0
              && Bigint.compare new_left (Bigint.add right_right 1Z) <= 0
              && Bigint.compare right_right (Bigint.add new_left 1Z) <= 0
              && (Bigint.add 1Z (maximum new_left right_right) = right
                  || Bigint.add 1Z (maximum new_left right_right)
                     = Bigint.add right 1Z)
            else true} @ immutable contended =
        fun right_left right_right left right ->
        let new_left = Bigint.add 1Z (maximum left right_left) in
        let u = () in
        if right = Bigint.add 1Z (maximum right_left right_right)
           && right = Bigint.add left 2Z
           && Bigint.compare right_left (Bigint.add right_right 1Z) <= 0
           && Bigint.compare right_right (Bigint.add right_left 1Z) <= 0
           && Bigint.compare right_right right_left >= 0
        then
          let refine_ right_maximum = maximum_def right_left right_right in
          let refine_ left_maximum = maximum_def left right_left in
          let refine_ result_maximum = maximum_def new_left right_right in
          refine_ u
        else refine_ u

      let (left_right_heights @ total) :
          (left_left : Bigint.t) ->
          (middle_left : Bigint.t) ->
          (middle_right : Bigint.t) ->
          (right : Bigint.t) ->
          (middle : Bigint.t) ->
          (left : Bigint.t) ->
          {u : unit |
            if left = Bigint.add 1Z (maximum left_left middle)
               && middle
                  = Bigint.add 1Z (maximum middle_left middle_right)
               && left = Bigint.add right 2Z
               && Bigint.compare left_left (Bigint.add middle 1Z) <= 0
               && Bigint.compare middle (Bigint.add left_left 1Z) <= 0
               && Bigint.compare left_left middle < 0
               && Bigint.compare middle_left
                    (Bigint.add middle_right 1Z)
                  <= 0
               && Bigint.compare middle_right
                    (Bigint.add middle_left 1Z)
                  <= 0
            then
              let new_left = Bigint.add 1Z (maximum left_left middle_left) in
              let new_right = Bigint.add 1Z (maximum middle_right right) in
              Bigint.compare left_left (Bigint.add middle_left 1Z) <= 0
              && Bigint.compare middle_left (Bigint.add left_left 1Z) <= 0
              && Bigint.compare middle_right (Bigint.add right 1Z) <= 0
              && Bigint.compare right (Bigint.add middle_right 1Z) <= 0
              && Bigint.compare new_left (Bigint.add new_right 1Z) <= 0
              && Bigint.compare new_right (Bigint.add new_left 1Z) <= 0
              && (Bigint.add 1Z (maximum new_left new_right) = left
                  || Bigint.add 1Z (maximum new_left new_right)
                     = Bigint.add left 1Z)
            else true} @ immutable contended =
        fun left_left middle_left middle_right right middle left ->
        let new_left = Bigint.add 1Z (maximum left_left middle_left) in
        let new_right = Bigint.add 1Z (maximum middle_right right) in
        let u = () in
        if left = Bigint.add 1Z (maximum left_left middle)
           && middle = Bigint.add 1Z (maximum middle_left middle_right)
           && left = Bigint.add right 2Z
           && Bigint.compare left_left (Bigint.add middle 1Z) <= 0
           && Bigint.compare middle (Bigint.add left_left 1Z) <= 0
           && Bigint.compare left_left middle < 0
           && Bigint.compare middle_left (Bigint.add middle_right 1Z) <= 0
           && Bigint.compare middle_right (Bigint.add middle_left 1Z) <= 0
        then
          let refine_ left_maximum = maximum_def left_left middle in
          let refine_ middle_maximum =
            maximum_def middle_left middle_right
          in
          let refine_ new_left_maximum = maximum_def left_left middle_left in
          let refine_ new_right_maximum = maximum_def middle_right right in
          let refine_ result_maximum = maximum_def new_left new_right in
          refine_ u
        else refine_ u

      let (right_left_heights @ total) :
          (left : Bigint.t) ->
          (middle_left : Bigint.t) ->
          (middle_right : Bigint.t) ->
          (right_right : Bigint.t) ->
          (middle : Bigint.t) ->
          (right : Bigint.t) ->
          {u : unit |
            if right = Bigint.add 1Z (maximum middle right_right)
               && middle
                  = Bigint.add 1Z (maximum middle_left middle_right)
               && right = Bigint.add left 2Z
               && Bigint.compare middle (Bigint.add right_right 1Z) <= 0
               && Bigint.compare right_right (Bigint.add middle 1Z) <= 0
               && Bigint.compare right_right middle < 0
               && Bigint.compare middle_left
                    (Bigint.add middle_right 1Z)
                  <= 0
               && Bigint.compare middle_right
                    (Bigint.add middle_left 1Z)
                  <= 0
            then
              let new_left = Bigint.add 1Z (maximum left middle_left) in
              let new_right = Bigint.add 1Z (maximum middle_right right_right) in
              Bigint.compare left (Bigint.add middle_left 1Z) <= 0
              && Bigint.compare middle_left (Bigint.add left 1Z) <= 0
              && Bigint.compare middle_right
                   (Bigint.add right_right 1Z)
                 <= 0
              && Bigint.compare right_right
                   (Bigint.add middle_right 1Z)
                 <= 0
              && Bigint.compare new_left (Bigint.add new_right 1Z) <= 0
              && Bigint.compare new_right (Bigint.add new_left 1Z) <= 0
              && (Bigint.add 1Z (maximum new_left new_right) = right
                  || Bigint.add 1Z (maximum new_left new_right)
                     = Bigint.add right 1Z)
            else true} @ immutable contended =
        fun left middle_left middle_right right_right middle right ->
        let new_left = Bigint.add 1Z (maximum left middle_left) in
        let new_right = Bigint.add 1Z (maximum middle_right right_right) in
        let u = () in
        if right = Bigint.add 1Z (maximum middle right_right)
           && middle = Bigint.add 1Z (maximum middle_left middle_right)
           && right = Bigint.add left 2Z
           && Bigint.compare middle (Bigint.add right_right 1Z) <= 0
           && Bigint.compare right_right (Bigint.add middle 1Z) <= 0
           && Bigint.compare right_right middle < 0
           && Bigint.compare middle_left (Bigint.add middle_right 1Z) <= 0
           && Bigint.compare middle_right (Bigint.add middle_left 1Z) <= 0
        then
          let refine_ right_maximum = maximum_def middle right_right in
          let refine_ middle_maximum =
            maximum_def middle_left middle_right
          in
          let refine_ new_left_maximum = maximum_def left middle_left in
          let refine_ new_right_maximum =
            maximum_def middle_right right_right
          in
          let refine_ result_maximum = maximum_def new_left new_right in
          refine_ u
        else refine_ u

      let rec (all_less_weaken @ total) :
          (lower : int) ->
          (upper : int) ->
          (tree : tree) ->
          {u : unit |
            if lower < upper && all_less lower tree
            then all_less upper tree
            else true} @ immutable contended =
        fun lower upper tree ->
        let u = () in
        if lower < upper && all_less lower tree then
          let refine_ lower_bound = all_less_def lower tree in
          let refine_ upper_bound = all_less_def upper tree in
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            let refine_ left_induction = all_less_weaken lower upper left in
            let refine_ right_induction = all_less_weaken lower upper right in
            refine_ u
        else refine_ u

      let rec (all_greater_weaken @ total) :
          (lower : int) ->
          (upper : int) ->
          (tree : tree) ->
          {u : unit |
            if lower < upper && all_greater upper tree
            then all_greater lower tree
            else true} @ immutable contended =
        fun lower upper tree ->
        let u = () in
        if lower < upper && all_greater upper tree then
          let refine_ upper_bound = all_greater_def upper tree in
          let refine_ lower_bound = all_greater_def lower tree in
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            let refine_ left_induction = all_greater_weaken lower upper left in
            let refine_ right_induction = all_greater_weaken lower upper right in
            refine_ u
        else refine_ u

      let (make_node_all_less @ total) :
          (upper : int) ->
          (left : tree) ->
          (value : int) ->
          (right : tree) ->
          {u : unit |
            if value < upper
               && all_less upper left
               && all_less upper right
            then all_less upper (make_node left value right)
            else true} @ immutable contended =
        fun upper left value right ->
        let result = make_node left value right in
        let u = () in
        if value < upper && all_less upper left && all_less upper right then
          let refine_ construction = make_node_def left value right in
          let refine_ result_bound = all_less_def upper result in
          refine_ u
        else refine_ u

      let (make_node_all_greater @ total) :
          (lower : int) ->
          (left : tree) ->
          (value : int) ->
          (right : tree) ->
          {u : unit |
            if lower < value
               && all_greater lower left
               && all_greater lower right
            then all_greater lower (make_node left value right)
            else true} @ immutable contended =
        fun lower left value right ->
        let result = make_node left value right in
        let u = () in
        if lower < value
           && all_greater lower left
           && all_greater lower right
        then
          let refine_ construction = make_node_def left value right in
          let refine_ result_bound = all_greater_def lower result in
          refine_ u
        else refine_ u

      let (make_node_height @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          height (make_node left value right)
          = Bigint.add 1Z (maximum (height left) (height right))}
          @ immutable contended =
      fun left value right ->
      let result = make_node left value right in
      let refine_ construction = make_node_def left value right in
      let refine_ result_height = height_def result in
      let u = () in
      refine_ u

      let rec (height_nonnegative @ total) :
          (tree : tree) ->
          {u : unit |
            if valid tree
            then Bigint.compare 0Z (height tree) <= 0
            else true} @ immutable contended =
        fun tree ->
        let u = () in
        if valid tree then
          let refine_ invariant = valid_def tree in
          let refine_ tree_height = height_def tree in
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            let left_height = height left in
            let right_height = height right in
            let refine_ left_induction = height_nonnegative left in
            let refine_ right_induction = height_nonnegative right in
            let refine_ height_maximum = maximum_def left_height right_height in
            refine_ u
        else refine_ u

    let (make_node_valid @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if all_less value left
             && all_greater value right
             && Bigint.compare (height left)
                  (Bigint.add (height right) 1Z)
                <= 0
             && Bigint.compare (height right)
                  (Bigint.add (height left) 1Z)
                <= 0
             && valid left
             && valid right
          then valid (make_node left value right)
          else true} @ immutable contended =
      fun left value right ->
      let result = make_node left value right in
      let u = () in
      if all_less value left
         && all_greater value right
         && Bigint.compare (height left) (Bigint.add (height right) 1Z) <= 0
         && Bigint.compare (height right) (Bigint.add (height left) 1Z) <= 0
         && valid left
         && valid right
      then
        let refine_ construction = make_node_def left value right in
        let refine_ result_height = make_node_height left value right in
        let refine_ invariant = valid_def result in
        refine_ u
      else refine_ u

      let (make_left_child @ total) :
          (upper : int) ->
          (left : tree) ->
          (value : int) ->
          (right : tree) ->
          {u : unit |
            if value < upper
               && all_less upper left
               && all_less upper right
               && all_less value left
               && all_greater value right
               && valid left
               && valid right
               && Bigint.compare (height left)
                    (Bigint.add (height right) 1Z)
                  <= 0
               && Bigint.compare (height right)
                    (Bigint.add (height left) 1Z)
                  <= 0
            then
              valid (make_node left value right)
              && all_less upper (make_node left value right)
              && height (make_node left value right)
                 = Bigint.add 1Z (maximum (height left) (height right))
            else true} @ immutable contended =
        fun upper left value right ->
        let u = () in
        if value < upper
           && all_less upper left
           && all_less upper right
           && all_less value left
           && all_greater value right
           && valid left
           && valid right
           && Bigint.compare (height left) (Bigint.add (height right) 1Z) <= 0
           && Bigint.compare (height right) (Bigint.add (height left) 1Z) <= 0
        then
          let refine_ result_bound = make_node_all_less upper left value right in
          let refine_ result_invariant = make_node_valid left value right in
          let refine_ result_height = make_node_height left value right in
          refine_ u
        else refine_ u

    let (make_right_child @ total) :
        (lower : int) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if lower < value
             && all_greater lower left
             && all_less value left
             && all_greater value right
             && valid left
             && valid right
             && Bigint.compare (height left)
                  (Bigint.add (height right) 1Z)
                <= 0
             && Bigint.compare (height right)
                  (Bigint.add (height left) 1Z)
                <= 0
          then
            valid (make_node left value right)
            && all_greater lower (make_node left value right)
            && height (make_node left value right)
               = Bigint.add 1Z (maximum (height left) (height right))
          else true} @ immutable contended =
      fun lower left value right ->
      let u = () in
      if lower < value
         && all_greater lower left
         && all_less value left
         && all_greater value right
         && valid left
         && valid right
         && Bigint.compare (height left) (Bigint.add (height right) 1Z) <= 0
         && Bigint.compare (height right) (Bigint.add (height left) 1Z) <= 0
      then
        let refine_ weakened_right = all_greater_weaken lower value right in
        let refine_ result_bound =
          make_node_all_greater lower left value right
        in
        let refine_ result_invariant = make_node_valid left value right in
        let refine_ result_height = make_node_height left value right in
        refine_ u
      else refine_ u

    let (rotate_right_valid @ total) :
        (left_left : tree) ->
        (left_value : int) ->
        (left_right : tree) ->
        (left_cached_height : Bigint.t) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          let left = Node
            (left_left, left_value, left_right, left_cached_height)
          in
          if valid left
             && valid right
             && all_less value left
             && all_greater value right
             && height left = Bigint.add (height right) 2Z
             && Bigint.compare (height left_left) (height left_right) >= 0
          then
            valid
              (make_node left_left left_value
                 (make_node left_right value right))
          else true} @ immutable contended =
      fun left_left left_value left_right left_cached_height value right ->
      let left = Node
        (left_left, left_value, left_right, left_cached_height)
      in
      let new_right = make_node left_right value right in
      let left_left_height = height left_left in
      let left_right_height = height left_right in
      let right_height = height right in
      let left_height = height left in
      let u = () in
      if valid left
         && valid right
         && all_less value left
         && all_greater value right
         && height left = Bigint.add (height right) 2Z
         && Bigint.compare (height left_left) (height left_right) >= 0
      then
        let refine_ left_invariant = valid_def left in
        let refine_ left_bound = all_less_def value left in
        let refine_ left_height_equation = height_def left in
        let refine_ height_facts =
          left_rotation_heights left_left_height left_right_height
            right_height left_height
        in
        let refine_ new_right_properties =
          make_right_child left_value left_right value right
        in
        let refine_ result_invariant =
          make_node_valid left_left left_value new_right
        in
        refine_ u
      else refine_ u

    let (rotate_left_valid @ total) :
        (left : tree) ->
        (value : int) ->
        (right_left : tree) ->
        (right_value : int) ->
        (right_right : tree) ->
        (right_cached_height : Bigint.t) ->
        {u : unit |
          let right = Node
            (right_left, right_value, right_right, right_cached_height)
          in
          if valid left
             && valid right
             && all_less value left
             && all_greater value right
             && height right = Bigint.add (height left) 2Z
             && Bigint.compare (height right_right) (height right_left) >= 0
          then
            valid
              (make_node (make_node left value right_left)
                 right_value right_right)
          else true} @ immutable contended =
      fun left value right_left right_value right_right right_cached_height ->
      let right = Node
        (right_left, right_value, right_right, right_cached_height)
      in
      let new_left = make_node left value right_left in
      let left_height = height left in
      let right_left_height = height right_left in
      let right_right_height = height right_right in
      let right_height = height right in
      let u = () in
      if valid left
         && valid right
         && all_less value left
         && all_greater value right
         && height right = Bigint.add (height left) 2Z
         && Bigint.compare (height right_right) (height right_left) >= 0
      then
        let refine_ right_invariant = valid_def right in
        let refine_ right_bound = all_greater_def value right in
        let refine_ right_height_equation = height_def right in
        let refine_ height_facts =
          right_rotation_heights right_left_height right_right_height
            left_height right_height
        in
        let refine_ weakened_left =
          all_less_weaken value right_value left
        in
        let refine_ new_left_properties =
          make_left_child right_value left value right_left
        in
        let refine_ result_invariant =
          make_node_valid new_left right_value right_right
        in
        refine_ u
      else refine_ u

    let (rotate_left_right_valid @ total) :
        (left_left : tree) ->
        (left_value : int) ->
        (middle_left : tree) ->
        (middle_value : int) ->
        (middle_right : tree) ->
        (middle_cached_height : Bigint.t) ->
        (left_cached_height : Bigint.t) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          let middle = Node
            (middle_left, middle_value, middle_right, middle_cached_height)
          in
          let left = Node
            (left_left, left_value, middle, left_cached_height)
          in
          if valid left
             && valid right
             && all_less value left
             && all_greater value right
             && height left = Bigint.add (height right) 2Z
             && Bigint.compare (height left_left) (height middle) < 0
          then
            valid
              (make_node (make_node left_left left_value middle_left)
                 middle_value (make_node middle_right value right))
          else true} @ immutable contended =
      fun left_left left_value middle_left middle_value middle_right
          middle_cached_height left_cached_height value right ->
      let middle = Node
        (middle_left, middle_value, middle_right, middle_cached_height)
      in
      let left = Node
        (left_left, left_value, middle, left_cached_height)
      in
      let new_left = make_node left_left left_value middle_left in
      let new_right = make_node middle_right value right in
      let left_left_height = height left_left in
      let middle_left_height = height middle_left in
      let middle_right_height = height middle_right in
      let right_height = height right in
      let middle_height = height middle in
      let left_height = height left in
      let u = () in
      if valid left
         && valid right
         && all_less value left
         && all_greater value right
         && height left = Bigint.add (height right) 2Z
         && Bigint.compare (height left_left) (height middle) < 0
      then
        let refine_ left_invariant = valid_def left in
        let refine_ middle_invariant = valid_def middle in
        let refine_ left_bound = all_less_def value left in
        let refine_ middle_bound = all_less_def value middle in
        let refine_ middle_lower_bound = all_greater_def left_value middle in
        let refine_ left_height_equation = height_def left in
        let refine_ middle_height_equation = height_def middle in
        let refine_ left_left_nonnegative = height_nonnegative left_left in
        let refine_ middle_left_nonnegative = height_nonnegative middle_left in
        let refine_ middle_right_nonnegative = height_nonnegative middle_right in
        let refine_ right_nonnegative = height_nonnegative right in
        let refine_ height_facts =
          left_right_heights left_left_height middle_left_height
            middle_right_height right_height middle_height left_height
        in
        let refine_ weakened_left =
          all_less_weaken left_value middle_value left_left
        in
        let refine_ new_left_properties =
          make_left_child middle_value left_left left_value middle_left
        in
        let refine_ new_right_properties =
          make_right_child middle_value middle_right value right
        in
        let refine_ result_invariant =
          make_node_valid new_left middle_value new_right
        in
        refine_ u
      else refine_ u

    let (rotate_right_left_valid @ total) :
        (left : tree) ->
        (value : int) ->
        (middle_left : tree) ->
        (middle_value : int) ->
        (middle_right : tree) ->
        (middle_cached_height : Bigint.t) ->
        (right_value : int) ->
        (right_right : tree) ->
        (right_cached_height : Bigint.t) ->
        {u : unit |
          let middle = Node
            (middle_left, middle_value, middle_right, middle_cached_height)
          in
          let right = Node
            (middle, right_value, right_right, right_cached_height)
          in
          if valid left
             && valid right
             && all_less value left
             && all_greater value right
             && height right = Bigint.add (height left) 2Z
             && Bigint.compare (height right_right) (height middle) < 0
          then
            valid
              (make_node (make_node left value middle_left)
                 middle_value
                 (make_node middle_right right_value right_right))
          else true} @ immutable contended =
      fun left value middle_left middle_value middle_right
          middle_cached_height right_value right_right right_cached_height ->
      let middle = Node
        (middle_left, middle_value, middle_right, middle_cached_height)
      in
      let right = Node
        (middle, right_value, right_right, right_cached_height)
      in
      let new_left = make_node left value middle_left in
      let new_right = make_node middle_right right_value right_right in
      let left_height = height left in
      let middle_left_height = height middle_left in
      let middle_right_height = height middle_right in
      let right_right_height = height right_right in
      let middle_height = height middle in
      let right_height = height right in
      let u = () in
      if valid left
         && valid right
         && all_less value left
         && all_greater value right
         && height right = Bigint.add (height left) 2Z
         && Bigint.compare (height right_right) (height middle) < 0
      then
        let refine_ right_invariant = valid_def right in
        let refine_ middle_invariant = valid_def middle in
        let refine_ right_bound = all_greater_def value right in
        let refine_ middle_bound = all_greater_def value middle in
        let refine_ middle_upper_bound = all_less_def right_value middle in
        let refine_ right_height_equation = height_def right in
        let refine_ middle_height_equation = height_def middle in
        let refine_ left_nonnegative = height_nonnegative left in
        let refine_ middle_left_nonnegative = height_nonnegative middle_left in
        let refine_ middle_right_nonnegative = height_nonnegative middle_right in
        let refine_ right_right_nonnegative = height_nonnegative right_right in
        let refine_ height_facts =
          right_left_heights left_height middle_left_height
            middle_right_height right_right_height middle_height right_height
        in
        let refine_ weakened_left =
          all_less_weaken value middle_value left
        in
        let refine_ weakened_right =
          all_greater_weaken middle_value right_value right_right
        in
        let refine_ new_left_properties =
          make_left_child middle_value left value middle_left
        in
        let refine_ new_right_properties =
          make_right_child middle_value middle_right right_value right_right
        in
        let refine_ result_invariant =
          make_node_valid new_left middle_value new_right
        in
        refine_ u
      else refine_ u

    let (balance_valid @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if valid left
             && valid right
             && all_less value left
             && all_greater value right
             && Bigint.compare (height left)
                  (Bigint.add (height right) 2Z)
                <= 0
             && Bigint.compare (height right)
                  (Bigint.add (height left) 2Z)
                <= 0
          then valid (balance left value right)
          else true} @ immutable contended =
      fun left value right ->
      let left_height = height left in
      let right_height = height right in
      let u = () in
      if valid left
         && valid right
         && all_less value left
         && all_greater value right
         && Bigint.compare left_height (Bigint.add right_height 2Z) <= 0
         && Bigint.compare right_height (Bigint.add left_height 2Z) <= 0
      then
        let refine_ balance_equation = balance_def left value right in
        let refine_ left_nonnegative = height_nonnegative left in
        let refine_ right_nonnegative = height_nonnegative right in
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          match left with
          | Empty ->
            let refine_ empty_height = height_def left in
            refine_ u
          | Node (left_left, left_value, left_right, cached_height) ->
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              let refine_ rotation =
                rotate_right_valid left_left left_value left_right
                  cached_height value right
              in
              refine_ u
            else
              match left_right with
              | Empty ->
                let refine_ left_invariant = valid_def left in
                let refine_ left_left_nonnegative =
                  height_nonnegative left_left
                in
                let refine_ empty_height = height_def left_right in
                refine_ u
              | Node
                  ( middle_left,
                    middle_value,
                    middle_right,
                    middle_cached_height ) ->
                let refine_ rotation =
                  rotate_left_right_valid left_left left_value middle_left
                    middle_value middle_right middle_cached_height
                    cached_height value right
                in
                refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then
          match right with
          | Empty ->
            let refine_ empty_height = height_def right in
            refine_ u
          | Node (right_left, right_value, right_right, cached_height) ->
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              let refine_ rotation =
                rotate_left_valid left value right_left right_value right_right
                  cached_height
              in
              refine_ u
            else
              match right_left with
              | Empty ->
                let refine_ right_invariant = valid_def right in
                let refine_ right_right_nonnegative =
                  height_nonnegative right_right
                in
                let refine_ empty_height = height_def right_left in
                refine_ u
              | Node
                  ( middle_left,
                    middle_value,
                    middle_right,
                    middle_cached_height ) ->
                let refine_ rotation =
                  rotate_right_left_valid left value middle_left middle_value
                    middle_right middle_cached_height right_value right_right
                    cached_height
                in
                refine_ u
        else
          let refine_ construction = make_node_valid left value right in
          refine_ u
      else refine_ u

    let (balance_left_height @ total) :
        (old_left_height : Bigint.t) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if valid left
             && valid right
             && (height left = old_left_height
                 || height left = Bigint.add old_left_height 1Z)
             && Bigint.compare old_left_height
                  (Bigint.add (height right) 1Z)
                <= 0
             && Bigint.compare (height right)
                  (Bigint.add old_left_height 1Z)
                <= 0
          then
            let old_height =
              Bigint.add 1Z (maximum old_left_height (height right))
            in
            height (balance left value right) = old_height
            || height (balance left value right)
               = Bigint.add old_height 1Z
          else true} @ immutable contended =
      fun old_left_height left value right ->
      let left_height = height left in
      let right_height = height right in
      let u = () in
      if valid left
         && valid right
         && (left_height = old_left_height
             || left_height = Bigint.add old_left_height 1Z)
         && Bigint.compare old_left_height (Bigint.add right_height 1Z) <= 0
         && Bigint.compare right_height (Bigint.add old_left_height 1Z) <= 0
      then
        let refine_ balance_equation = balance_def left value right in
        let refine_ old_maximum = maximum_def old_left_height right_height in
        let refine_ left_nonnegative = height_nonnegative left in
        let refine_ right_nonnegative = height_nonnegative right in
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          match left with
          | Empty ->
            let refine_ empty_height = height_def left in
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            let left_left_height = height left_left in
            let left_right_height = height left_right in
            let refine_ left_invariant = valid_def left in
            let refine_ left_height_equation = height_def left in
            if Bigint.compare left_left_height left_right_height >= 0 then
              let new_right = make_node left_right value right in
              let refine_ height_facts =
                left_rotation_heights left_left_height left_right_height
                  right_height left_height
              in
              let refine_ new_right_height =
                make_node_height left_right value right
              in
              let refine_ result_height =
                make_node_height left_left left_value new_right
              in
              refine_ u
            else
              match left_right with
              | Empty ->
                let refine_ empty_height = height_def left_right in
                let refine_ left_left_nonnegative =
                  height_nonnegative left_left
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let middle_height = height left_right in
                let middle_left_height = height middle_left in
                let middle_right_height = height middle_right in
                let refine_ middle_invariant = valid_def left_right in
                let refine_ middle_height_equation = height_def left_right in
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                let refine_ height_facts =
                  left_right_heights left_left_height middle_left_height
                    middle_right_height right_height middle_height left_height
                in
                let refine_ new_left_height =
                  make_node_height left_left left_value middle_left
                in
                let refine_ new_right_height =
                  make_node_height middle_right value right
                in
                let refine_ result_height =
                  make_node_height new_left middle_value new_right
                in
                refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then refine_ u
        else
          let refine_ result_height = make_node_height left value right in
          let refine_ new_maximum = maximum_def left_height right_height in
          refine_ u
      else refine_ u

    let (balance_right_height @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        (old_right_height : Bigint.t) ->
        {u : unit |
          if valid left
             && valid right
             && (height right = old_right_height
                 || height right = Bigint.add old_right_height 1Z)
             && Bigint.compare (height left)
                  (Bigint.add old_right_height 1Z)
                <= 0
             && Bigint.compare old_right_height
                  (Bigint.add (height left) 1Z)
                <= 0
          then
            let old_height =
              Bigint.add 1Z (maximum (height left) old_right_height)
            in
            height (balance left value right) = old_height
            || height (balance left value right)
               = Bigint.add old_height 1Z
          else true} @ immutable contended =
      fun left value right old_right_height ->
      let left_height = height left in
      let right_height = height right in
      let u = () in
      if valid left
         && valid right
         && (right_height = old_right_height
             || right_height = Bigint.add old_right_height 1Z)
         && Bigint.compare left_height (Bigint.add old_right_height 1Z) <= 0
         && Bigint.compare old_right_height (Bigint.add left_height 1Z) <= 0
      then
        let refine_ balance_equation = balance_def left value right in
        let refine_ old_maximum = maximum_def left_height old_right_height in
        let refine_ left_nonnegative = height_nonnegative left in
        let refine_ right_nonnegative = height_nonnegative right in
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then
          match right with
          | Empty ->
            let refine_ empty_height = height_def right in
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            let right_left_height = height right_left in
            let right_right_height = height right_right in
            let refine_ right_invariant = valid_def right in
            let refine_ right_height_equation = height_def right in
            if Bigint.compare right_right_height right_left_height >= 0 then
              let new_left = make_node left value right_left in
              let refine_ height_facts =
                right_rotation_heights right_left_height right_right_height
                  left_height right_height
              in
              let refine_ new_left_height =
                make_node_height left value right_left
              in
              let refine_ result_height =
                make_node_height new_left right_value right_right
              in
              refine_ u
            else
              match right_left with
              | Empty ->
                let refine_ empty_height = height_def right_left in
                let refine_ right_right_nonnegative =
                  height_nonnegative right_right
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let middle_height = height right_left in
                let middle_left_height = height middle_left in
                let middle_right_height = height middle_right in
                let refine_ middle_invariant = valid_def right_left in
                let refine_ middle_height_equation = height_def right_left in
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                let refine_ height_facts =
                  right_left_heights left_height middle_left_height
                    middle_right_height right_right_height middle_height
                    right_height
                in
                let refine_ new_left_height =
                  make_node_height left value middle_left
                in
                let refine_ new_right_height =
                  make_node_height middle_right right_value right_right
                in
                let refine_ result_height =
                  make_node_height new_left middle_value new_right
                in
                refine_ u
        else
          let refine_ result_height = make_node_height left value right in
          let refine_ new_maximum = maximum_def left_height right_height in
          refine_ u
      else refine_ u

    let (balance_all_less @ total) :
        (upper : int) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if value < upper
             && all_less upper left
             && all_less upper right
          then all_less upper (balance left value right)
          else true} @ immutable contended =
      fun upper left value right ->
      let u = () in
      if value < upper && all_less upper left && all_less upper right then
        let refine_ balance_equation = balance_def left value right in
        if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0
        then
          match left with
          | Empty ->
            let refine_ result_bound =
              make_node_all_less upper left value right
            in
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            let refine_ left_bound = all_less_def upper left in
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              let new_right = make_node left_right value right in
              let refine_ new_right_bound =
                make_node_all_less upper left_right value right
              in
              let refine_ result_bound =
                make_node_all_less upper left_left left_value new_right
              in
              refine_ u
            else
              match left_right with
              | Empty ->
                let refine_ result_bound =
                  make_node_all_less upper left value right
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let refine_ middle_bound = all_less_def upper left_right in
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                let refine_ new_left_bound =
                  make_node_all_less upper left_left left_value middle_left
                in
                let refine_ new_right_bound =
                  make_node_all_less upper middle_right value right
                in
                let refine_ result_bound =
                  make_node_all_less upper new_left middle_value new_right
                in
                refine_ u
        else if
          Bigint.compare (height right) (Bigint.add (height left) 1Z) > 0
        then
          match right with
          | Empty ->
            let refine_ result_bound =
              make_node_all_less upper left value right
            in
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            let refine_ right_bound = all_less_def upper right in
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              let new_left = make_node left value right_left in
              let refine_ new_left_bound =
                make_node_all_less upper left value right_left
              in
              let refine_ result_bound =
                make_node_all_less upper new_left right_value right_right
              in
              refine_ u
            else
              match right_left with
              | Empty ->
                let refine_ result_bound =
                  make_node_all_less upper left value right
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let refine_ middle_bound = all_less_def upper right_left in
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                let refine_ new_left_bound =
                  make_node_all_less upper left value middle_left
                in
                let refine_ new_right_bound =
                  make_node_all_less upper middle_right right_value right_right
                in
                let refine_ result_bound =
                  make_node_all_less upper new_left middle_value new_right
                in
                refine_ u
        else
          let refine_ result_bound =
            make_node_all_less upper left value right
          in
          refine_ u
      else refine_ u

    let (balance_all_greater @ total) :
        (lower : int) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          if lower < value
             && all_greater lower left
             && all_greater lower right
          then all_greater lower (balance left value right)
          else true} @ immutable contended =
      fun lower left value right ->
      let u = () in
      if lower < value && all_greater lower left && all_greater lower right then
        let refine_ balance_equation = balance_def left value right in
        if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0
        then
          match left with
          | Empty ->
            let refine_ result_bound =
              make_node_all_greater lower left value right
            in
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            let refine_ left_bound = all_greater_def lower left in
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              let new_right = make_node left_right value right in
              let refine_ new_right_bound =
                make_node_all_greater lower left_right value right
              in
              let refine_ result_bound =
                make_node_all_greater lower left_left left_value new_right
              in
              refine_ u
            else
              match left_right with
              | Empty ->
                let refine_ result_bound =
                  make_node_all_greater lower left value right
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let refine_ middle_bound = all_greater_def lower left_right in
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                let refine_ new_left_bound =
                  make_node_all_greater lower left_left left_value middle_left
                in
                let refine_ new_right_bound =
                  make_node_all_greater lower middle_right value right
                in
                let refine_ result_bound =
                  make_node_all_greater lower new_left middle_value new_right
                in
                refine_ u
        else if
          Bigint.compare (height right) (Bigint.add (height left) 1Z) > 0
        then
          match right with
          | Empty ->
            let refine_ result_bound =
              make_node_all_greater lower left value right
            in
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            let refine_ right_bound = all_greater_def lower right in
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              let new_left = make_node left value right_left in
              let refine_ new_left_bound =
                make_node_all_greater lower left value right_left
              in
              let refine_ result_bound =
                make_node_all_greater lower new_left right_value right_right
              in
              refine_ u
            else
              match right_left with
              | Empty ->
                let refine_ result_bound =
                  make_node_all_greater lower left value right
                in
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let refine_ middle_bound = all_greater_def lower right_left in
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                let refine_ new_left_bound =
                  make_node_all_greater lower left value middle_left
                in
                let refine_ new_right_bound =
                  make_node_all_greater lower middle_right right_value right_right
                in
                let refine_ result_bound =
                  make_node_all_greater lower new_left middle_value new_right
                in
                refine_ u
        else
          let refine_ result_bound =
            make_node_all_greater lower left value right
          in
          refine_ u
      else refine_ u

    let rec (add_all_less @ total) :
        (element : int) ->
        (upper : int) ->
        (tree : tree) ->
        {u : unit |
          if element < upper && all_less upper tree
          then all_less upper (add_tree element tree)
          else true} @ immutable contended =
      fun element upper tree ->
      let result = add_tree element tree in
      let u = () in
      if element < upper && all_less upper tree then
        let refine_ insertion = add_tree_def element tree in
        let refine_ tree_bound = all_less_def upper tree in
        match tree with
        | Empty ->
          let refine_ result_bound = all_less_def upper result in
          refine_ u
        | Node (left, value, right, _) ->
          if element = value then refine_ u
          else if element < value then
            let new_left = add_tree element left in
            let refine_ induction = add_all_less element upper left in
            let refine_ result_bound =
              balance_all_less upper new_left value right
            in
            refine_ u
          else
            let new_right = add_tree element right in
            let refine_ induction = add_all_less element upper right in
            let refine_ result_bound =
              balance_all_less upper left value new_right
            in
            refine_ u
      else refine_ u

    let rec (add_all_greater @ total) :
        (element : int) ->
        (lower : int) ->
        (tree : tree) ->
        {u : unit |
          if lower < element && all_greater lower tree
          then all_greater lower (add_tree element tree)
          else true} @ immutable contended =
      fun element lower tree ->
      let result = add_tree element tree in
      let u = () in
      if lower < element && all_greater lower tree then
        let refine_ insertion = add_tree_def element tree in
        let refine_ tree_bound = all_greater_def lower tree in
        match tree with
        | Empty ->
          let refine_ result_bound = all_greater_def lower result in
          refine_ u
        | Node (left, value, right, _) ->
          if element = value then refine_ u
          else if element < value then
            let new_left = add_tree element left in
            let refine_ induction = add_all_greater element lower left in
            let refine_ result_bound =
              balance_all_greater lower new_left value right
            in
            refine_ u
          else
            let new_right = add_tree element right in
            let refine_ induction = add_all_greater element lower right in
            let refine_ result_bound =
              balance_all_greater lower left value new_right
            in
            refine_ u
      else refine_ u

    let rec (add_valid_height @ total) :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          valid (add_tree element tree)
          && (height (add_tree element tree) = height tree
              || height (add_tree element tree)
                 = Bigint.add (height tree) 1Z)} @ immutable contended =
      fun element tree validity ->
      let refine_ valid_unit = validity in
      let result = add_tree element tree in
      let u = () in
      let refine_ invariant = valid_def tree in
      let refine_ insertion_equation = add_tree_def element tree in
      match tree with
      | Empty ->
        let zero = 0Z in
        let refine_ empty_height = height_def tree in
        let refine_ result_height = height_def result in
        let refine_ result_invariant = valid_def result in
        let refine_ left_bound = all_less_def element tree in
        let refine_ right_bound = all_greater_def element tree in
        let refine_ height_maximum = maximum_def zero zero in
        refine_ u
      | Node (left, value, right, _) ->
        let left_height = height left in
        let right_height = height right in
        let refine_ tree_height_equation = height_def tree in
        if element = value then refine_ u
        else if element < value then
          let proof = () in
          let left_validity : {u : unit | valid left} = refine_ proof in
          let new_left = add_tree element left in
          let refine_ induction =
            add_valid_height element left left_validity
          in
          let refine_ new_left_bound = add_all_less element value left in
          let refine_ result_invariant =
            balance_valid new_left value right
          in
          let refine_ result_height =
            balance_left_height left_height new_left value right
          in
          refine_ u
        else
          let proof = () in
          let right_validity : {u : unit | valid right} = refine_ proof in
          let new_right = add_tree element right in
          let refine_ induction =
            add_valid_height element right right_validity
          in
          let refine_ new_right_bound = add_all_greater element value right in
          let refine_ result_invariant =
            balance_valid left value new_right
          in
          let refine_ result_height =
            balance_right_height left value new_right right_height
          in
          refine_ u
    end

    module Model_proofs : sig end = struct
    module Element_proofs : sig
      val elements_all_less :
        (upper : int) ->
        (tree : tree) ->
        {u : unit |
          if all_less upper tree
          then List_set.all_less upper (elements tree)
          else true} @ immutable contended
    end = struct
    let rec (elements_all_less @ total) :
        (upper : int) ->
        (tree : tree) ->
        {u : unit |
          if all_less upper tree
          then List_set.all_less upper (elements tree)
          else true} @ immutable contended =
      fun upper tree ->
      let u = () in
      if all_less upper tree then
        let tree_elements = elements tree in
        let refine_ tree_bound = all_less_def upper tree in
        let refine_ tree_elements_equation = elements_def tree in
        match tree with
        | Empty ->
          let refine_ list_bound = List_set.all_less_def upper tree_elements in
          refine_ u
        | Node (left, value, right, _) ->
          let left_elements = elements left in
          let right_elements = elements right in
          let suffix = List_set.Cons (value, right_elements) in
          let refine_ left_induction = elements_all_less upper left in
          let refine_ right_induction = elements_all_less upper right in
          let refine_ suffix_bound = List_set.all_less_def upper suffix in
          let refine_ append_bound =
            List_proofs.all_less_append upper left_elements suffix
          in
          refine_ u
      else refine_ u

    module Lookup_proofs : sig
      val lookup_tree_elements :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          lookup_tree element tree
          === List_set.lookup_repr element (elements tree)}
          @ immutable contended
    end = struct
      let rec (elements_all_greater @ total) :
          (lower : int) ->
          (tree : tree) ->
          {u : unit |
            if all_greater lower tree
            then List_set.all_greater lower (elements tree)
            else true} @ immutable contended =
        fun lower tree ->
        let u = () in
        if all_greater lower tree then
          let tree_elements = elements tree in
          let refine_ tree_bound = all_greater_def lower tree in
          let refine_ tree_elements_equation = elements_def tree in
          match tree with
          | Empty ->
            let refine_ list_bound =
              List_set.all_greater_def lower tree_elements
            in
            refine_ u
          | Node (left, value, right, _) ->
            let left_elements = elements left in
            let right_elements = elements right in
            let suffix = List_set.Cons (value, right_elements) in
            let refine_ left_induction = elements_all_greater lower left in
            let refine_ right_induction = elements_all_greater lower right in
            let refine_ suffix_bound =
              List_set.all_greater_def lower suffix
            in
            let refine_ append_bound =
              List_proofs.all_greater_append lower left_elements suffix
            in
            refine_ u
        else refine_ u

      let rec (lookup_tree_elements @ total) :
          (element : int) ->
          (tree : tree) ->
          (validity : {u : unit | valid tree}) ->
          {u : unit |
            lookup_tree element tree
            === List_set.lookup_repr element (elements tree)}
            @ immutable contended =
        fun element tree validity ->
        let refine_ valid_unit = validity in
        let u = () in
        let refine_ invariant = valid_def tree in
        let refine_ tree_lookup = lookup_tree_def element tree in
        let refine_ tree_elements = elements_def tree in
        match tree with
        | Empty ->
          let model = elements tree in
          let refine_ model_lookup = List_set.lookup_repr_def element model in
          refine_ u
        | Node (left, value, right, _) ->
          let left_elements = elements left in
          let right_elements = elements right in
          let suffix = List_set.Cons (value, right_elements) in
          let refine_ append_lookup =
            List_proofs.lookup_append element left_elements suffix
          in
          let refine_ suffix_lookup =
            List_set.lookup_repr_def element suffix
          in
          if element = value then refine_ u
          else if element < value then
            let proof = () in
            let left_valid : {u : unit | valid left} = refine_ proof in
            let refine_ induction =
              lookup_tree_elements element left left_valid
            in
            let refine_ right_bound = elements_all_greater value right in
            let refine_ right_absent =
              List_proofs.lookup_below element value right_elements
            in
            refine_ u
          else
            let proof = () in
            let right_valid : {u : unit | valid right} = refine_ proof in
            let refine_ induction =
              lookup_tree_elements element right right_valid
            in
            let refine_ left_bound = elements_all_less value left in
            let refine_ left_absent =
              List_proofs.lookup_above element value left_elements
            in
            refine_ u

    end

    let (lookup @ total) :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {found : bool |
          found === List_set.lookup_repr element (elements tree)}
          @ immutable contended =
      fun element tree validity ->
      let found = lookup_tree element tree in
      let refine_ correctness =
        Lookup_proofs.lookup_tree_elements element tree validity
      in
      refine_ found

    end

    module Insertion_model_proofs : sig
      val add_tree_elements :
        (element : int) ->
        (tree : tree) ->
        {u : unit |
          if valid tree
          then
            elements (add_tree element tree)
            === List_set.add_repr element (elements tree)
          else true} @ immutable contended
    end = struct
    let (make_node_elements @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          elements (make_node left value right)
          === List_set.append (elements left)
                (List_set.Cons (value, elements right))}
          @ immutable contended =
      fun left value right ->
      let result = make_node left value right in
      let refine_ construction = make_node_def left value right in
      let refine_ result_elements = elements_def result in
      let u = () in
      refine_ u

    let (cached_height_irrelevant @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        (cached_height : Bigint.t) ->
        {u : unit |
          elements (Node (left, value, right, cached_height))
          === elements (make_node left value right)} @ immutable contended =
      fun left value right cached_height ->
      let tree = Node (left, value, right, cached_height) in
      let refine_ tree_elements = elements_def tree in
      let refine_ rebuilt_elements = make_node_elements left value right in
      let u = () in
      refine_ u

    let (rotate_right_elements @ total) :
        (left_left : tree) ->
        (left_value : int) ->
        (left_right : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          elements
            (make_node left_left left_value
               (make_node left_right value right))
          === elements
                (make_node (make_node left_left left_value left_right)
                   value right)} @ immutable contended =
      fun left_left left_value left_right value right ->
      let old_left = make_node left_left left_value left_right in
      let new_right = make_node left_right value right in
      let left_elements = elements left_left in
      let left_tail = List_set.Cons (left_value, elements left_right) in
      let right_tail = List_set.Cons (value, elements right) in
      let refine_ old_left_elements =
        make_node_elements left_left left_value left_right
      in
      let refine_ new_right_elements =
        make_node_elements left_right value right
      in
      let refine_ result_elements =
        make_node_elements left_left left_value new_right
      in
      let refine_ original_elements = make_node_elements old_left value right in
      let refine_ association =
        List_proofs.append_associative left_elements left_tail right_tail
      in
      let refine_ left_tail_equation = List_set.append_def left_tail right_tail in
      let u = () in
      refine_ u

    let (rotate_left_elements @ total) :
        (left : tree) ->
        (value : int) ->
        (right_left : tree) ->
        (right_value : int) ->
        (right_right : tree) ->
        {u : unit |
          elements
            (make_node (make_node left value right_left)
               right_value right_right)
          === elements
                (make_node left value
                   (make_node right_left right_value right_right))}
          @ immutable contended =
      fun left value right_left right_value right_right ->
      let old_right = make_node right_left right_value right_right in
      let new_left = make_node left value right_left in
      let left_elements = elements left in
      let left_tail = List_set.Cons (value, elements right_left) in
      let right_tail = List_set.Cons (right_value, elements right_right) in
      let refine_ old_right_elements =
        make_node_elements right_left right_value right_right
      in
      let refine_ new_left_elements = make_node_elements left value right_left in
      let refine_ result_elements =
        make_node_elements new_left right_value right_right
      in
      let refine_ original_elements = make_node_elements left value old_right in
      let refine_ association =
        List_proofs.append_associative left_elements left_tail right_tail
      in
      let refine_ left_tail_equation = List_set.append_def left_tail right_tail in
      let u = () in
      refine_ u

    let (rotate_left_right_elements @ total) :
        (left_left : tree) ->
        (left_value : int) ->
        (middle_left : tree) ->
        (middle_value : int) ->
        (middle_right : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          elements
            (make_node (make_node left_left left_value middle_left)
               middle_value (make_node middle_right value right))
          === elements
                (make_node
                   (make_node left_left left_value
                      (make_node middle_left middle_value middle_right))
                   value right)} @ immutable contended =
      fun left_left left_value middle_left middle_value middle_right value
          right ->
      let original_middle =
        make_node middle_left middle_value middle_right
      in
      let original_left = make_node left_left left_value original_middle in
      let new_left = make_node left_left left_value middle_left in
      let rotated_left = make_node new_left middle_value middle_right in
      let refine_ inner_rotation =
        rotate_left_elements left_left left_value middle_left middle_value
          middle_right
      in
      let refine_ outer_rotation =
        rotate_right_elements new_left middle_value middle_right value right
      in
      let refine_ rotated_outer = make_node_elements rotated_left value right in
      let refine_ original_outer = make_node_elements original_left value right in
      let u = () in
      refine_ u

    let (rotate_right_left_elements @ total) :
        (left : tree) ->
        (value : int) ->
        (middle_left : tree) ->
        (middle_value : int) ->
        (middle_right : tree) ->
        (right_value : int) ->
        (right_right : tree) ->
        {u : unit |
          elements
            (make_node (make_node left value middle_left)
               middle_value
               (make_node middle_right right_value right_right))
          === elements
                (make_node left value
                   (make_node
                      (make_node middle_left middle_value middle_right)
                      right_value right_right))} @ immutable contended =
      fun left value middle_left middle_value middle_right right_value
          right_right ->
      let original_middle =
        make_node middle_left middle_value middle_right
      in
      let original_right = make_node original_middle right_value right_right in
      let new_right = make_node middle_right right_value right_right in
      let rotated_right = make_node middle_left middle_value new_right in
      let refine_ inner_rotation =
        rotate_right_elements middle_left middle_value middle_right right_value
          right_right
      in
      let refine_ outer_rotation =
        rotate_left_elements left value middle_left middle_value new_right
      in
      let refine_ rotated_outer = make_node_elements left value rotated_right in
      let refine_ original_outer = make_node_elements left value original_right in
      let u = () in
      refine_ u

    let (balance_elements @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        {u : unit |
          elements (balance left value right)
          === elements (make_node left value right)} @ immutable contended =
      fun left value right ->
      let refine_ balance_equation = balance_def left value right in
      if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0 then
        match left with
        | Empty ->
          let u = () in
          refine_ u
        | Node (left_left, left_value, left_right, cached_height) ->
          let rebuilt_left = make_node left_left left_value left_right in
          let refine_ left_elements =
            cached_height_irrelevant left_left left_value left_right
              cached_height
          in
          let refine_ original_elements = make_node_elements left value right in
          let refine_ rebuilt_elements =
            make_node_elements rebuilt_left value right
          in
          if Bigint.compare (height left_left) (height left_right) >= 0 then
            let refine_ rotation =
              rotate_right_elements left_left left_value left_right value right
            in
            let u = () in
            refine_ u
          else
            match left_right with
            | Empty ->
              let u = () in
              refine_ u
            | Node (middle_left, middle_value, middle_right, middle_height) ->
              let rebuilt_middle =
                make_node middle_left middle_value middle_right
              in
              let canonical_left =
                make_node left_left left_value rebuilt_middle
              in
              let refine_ middle_elements =
                cached_height_irrelevant middle_left middle_value middle_right
                  middle_height
              in
              let refine_ rebuilt_left_elements =
                make_node_elements left_left left_value left_right
              in
              let refine_ canonical_left_elements =
                make_node_elements left_left left_value rebuilt_middle
              in
              let refine_ canonical_outer_elements =
                make_node_elements canonical_left value right
              in
              let refine_ rotation =
                rotate_left_right_elements left_left left_value middle_left
                  middle_value middle_right value right
              in
              let u = () in
              refine_ u
      else if Bigint.compare (height right) (Bigint.add (height left) 1Z) > 0
      then
        match right with
        | Empty ->
          let u = () in
          refine_ u
        | Node (right_left, right_value, right_right, cached_height) ->
          let rebuilt_right = make_node right_left right_value right_right in
          let refine_ right_elements =
            cached_height_irrelevant right_left right_value right_right
              cached_height
          in
          let refine_ original_elements = make_node_elements left value right in
          let refine_ rebuilt_elements =
            make_node_elements left value rebuilt_right
          in
          if Bigint.compare (height right_right) (height right_left) >= 0 then
            let refine_ rotation =
              rotate_left_elements left value right_left right_value right_right
            in
            let u = () in
            refine_ u
          else
            match right_left with
            | Empty ->
              let u = () in
              refine_ u
            | Node (middle_left, middle_value, middle_right, middle_height) ->
              let rebuilt_middle =
                make_node middle_left middle_value middle_right
              in
              let canonical_right =
                make_node rebuilt_middle right_value right_right
              in
              let refine_ middle_elements =
                cached_height_irrelevant middle_left middle_value middle_right
                  middle_height
              in
              let refine_ rebuilt_right_elements =
                make_node_elements right_left right_value right_right
              in
              let refine_ canonical_right_elements =
                make_node_elements rebuilt_middle right_value right_right
              in
              let refine_ canonical_outer_elements =
                make_node_elements left value canonical_right
              in
              let refine_ rotation =
                rotate_right_left_elements left value middle_left middle_value
                  middle_right right_value right_right
              in
              let u = () in
              refine_ u
      else
        let u = () in
        refine_ u

    let (leaf_elements @ total) element :
        {u : unit |
          elements (Node (Empty, element, Empty, 1Z))
          === List_set.Cons (element, List_set.Nil)} =
      let empty_left = Empty in
      let empty_right = Empty in
      let tree = Node (empty_left, element, empty_right, 1Z) in
      let nil = List_set.Nil in
      let suffix = List_set.Cons (element, nil) in
      let refine_ left_elements = elements_def empty_left in
      let refine_ right_elements = elements_def empty_right in
      let refine_ tree_elements = elements_def tree in
      let refine_ append_equation = List_set.append_def nil suffix in
      let u = () in
      refine_ u

    let (add_leaf_step @ total) element :
        {u : unit |
          elements (Node (Empty, element, Empty, 1Z))
          === List_set.add_repr element (elements Empty)} =
      let empty = Empty in
      let empty_elements = elements empty in
      let refine_ leaf_model = leaf_elements element in
      let refine_ empty_model = elements_def empty in
      let refine_ add_model = List_set.add_repr_def element empty_elements in
      let u = () in
      refine_ u

    let (add_equal_step @ total) :
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        (cached_height : Bigint.t) ->
        {u : unit |
          if valid (Node (left, value, right, cached_height))
          then
            elements (Node (left, value, right, cached_height))
            === List_set.add_repr value
                  (elements (Node (left, value, right, cached_height)))
          else true} @ immutable contended =
      fun left value right cached_height ->
      let tree = Node (left, value, right, cached_height) in
      let left_elements = elements left in
      let right_elements = elements right in
      let u = () in
      if valid tree then
        let refine_ invariant = valid_def tree in
        let refine_ tree_model = elements_def tree in
          let refine_ left_bound = Element_proofs.elements_all_less value left in
        let refine_ model_add =
          List_proofs.add_at_pivot left_elements value right_elements
        in
        refine_ u
      else refine_ u

    let (add_left_step @ total) :
        (element : int) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        (cached_height : Bigint.t) ->
        (correctness :
          {u : unit |
            if valid left
            then
              elements (add_tree element left)
              === List_set.add_repr element (elements left)
            else true}) ->
        {u : unit |
          if element < value && valid left
          then
            elements (balance (add_tree element left) value right)
            === List_set.add_repr element
                  (elements (Node (left, value, right, cached_height)))
          else true} @ immutable contended =
      fun element left value right cached_height correctness ->
      let new_left = add_tree element left in
      let refine_ new_left_elements = correctness in
      let tree = Node (left, value, right, cached_height) in
      let left_elements = elements left in
      let right_elements = elements right in
      let u = () in
      if element < value && valid left then
        let refine_ tree_model = elements_def tree in
        let refine_ balance_model = balance_elements new_left value right in
        let refine_ node_model = make_node_elements new_left value right in
        let refine_ model_add =
          List_proofs.add_left element left_elements value right_elements
        in
        refine_ u
      else refine_ u

    let (add_right_step @ total) :
        (element : int) ->
        (left : tree) ->
        (value : int) ->
        (right : tree) ->
        (cached_height : Bigint.t) ->
        (correctness :
          {u : unit |
            if valid right
            then
              elements (add_tree element right)
              === List_set.add_repr element (elements right)
            else true}) ->
        {u : unit |
          if value < element && all_less value left && valid right
          then
            elements (balance left value (add_tree element right))
            === List_set.add_repr element
                  (elements (Node (left, value, right, cached_height)))
          else true} @ immutable contended =
      fun element left value right cached_height correctness ->
      let new_right = add_tree element right in
      let refine_ new_right_elements = correctness in
      let tree = Node (left, value, right, cached_height) in
      let left_elements = elements left in
      let right_elements = elements right in
      let u = () in
      if value < element && all_less value left && valid right then
        let refine_ left_bound = Element_proofs.elements_all_less value left in
        let refine_ tree_model = elements_def tree in
        let refine_ balance_model = balance_elements left value new_right in
        let refine_ node_model = make_node_elements left value new_right in
        let refine_ model_add =
          List_proofs.add_right element left_elements value right_elements
        in
        refine_ u
      else refine_ u

    let rec (add_tree_elements @ total) :
        (element : int) ->
        (tree : tree) ->
        {u : unit |
          if valid tree
          then
            elements (add_tree element tree)
            === List_set.add_repr element (elements tree)
          else true} @ immutable contended =
      fun element tree ->
      let u = () in
      if valid tree then
        let refine_ invariant = valid_def tree in
        let refine_ insertion_equation = add_tree_def element tree in
        match tree with
        | Empty ->
          let refine_ step = add_leaf_step element in
          refine_ u
        | Node (left, value, right, cached_height) ->
          if element = value then
            let refine_ step =
              add_equal_step left value right cached_height
            in
            refine_ u
          else if element < value then
            let induction = add_tree_elements element left in
            let refine_ step =
              add_left_step element left value right cached_height
                induction
            in
            refine_ u
          else
            let refine_ left_bound = Element_proofs.elements_all_less value left in
            let induction = add_tree_elements element right in
            let refine_ step =
              add_right_step element left value right cached_height
                induction
            in
            refine_ u
      else refine_ u

    end

    module Operations : sig end = struct
    let (add @ total) :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {result : tree |
          valid result
          && elements result
             === List_set.add_repr element (elements tree)}
          @ immutable contended =
      fun element tree validity ->
      let result = add_tree element tree in
      let refine_ valid_unit = validity in
      let refine_ result_validity =
        Validity_proofs.add_valid_height element tree validity
      in
      let refine_ result_elements =
        Insertion_model_proofs.add_tree_elements element tree
      in
      refine_ result

    let[@def] rec (add_elements @ total) xs tree =
      match xs with
      | List_set.Nil -> tree
      | List_set.Cons (head, tail) ->
        add_elements tail (add_tree head tree)

    let rec (add_elements_spec @ total) :
        (xs : List_set.repr) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          valid (add_elements xs tree)
          && elements (add_elements xs tree)
             === List_set.union_repr xs (elements tree)}
          @ immutable contended =
      fun xs tree validity ->
      let refine_ valid_unit = validity in
      let u = () in
      let refine_ result_equation = add_elements_def xs tree in
      let tree_elements = elements tree in
      let refine_ model_equation = List_set.union_repr_def xs tree_elements in
      match xs with
      | List_set.Nil -> refine_ u
      | List_set.Cons (head, tail) ->
        let added = add_tree head tree in
        let refine_ added_validity =
          Validity_proofs.add_valid_height head tree validity
        in
        let refine_ added_elements =
          Insertion_model_proofs.add_tree_elements head tree
        in
        let proof = () in
        let added_validity : {u : unit | valid added} = refine_ proof in
        let refine_ induction =
          add_elements_spec tail added added_validity
        in
        refine_ u

    let (union @ total) :
        (left : tree) ->
        (right : tree) ->
        (right_validity : {u : unit | valid right}) ->
        {result : tree |
          valid result
          && elements result
             === List_set.union_repr (elements left) (elements right)}
          @ immutable contended =
      fun left right right_validity ->
      let left_elements = elements left in
      let result = add_elements left_elements right in
      let refine_ result_spec =
        add_elements_spec left_elements right right_validity
      in
      refine_ result

    let (size @ total) tree =
      let tree_elements = elements tree in
      List_set.size_repr tree_elements

    end

    end
