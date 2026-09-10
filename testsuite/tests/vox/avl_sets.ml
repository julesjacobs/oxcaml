(* TEST
 flags = "-extension refinement_types";
 has-z3;
 timeout = "120";
 all_modules = "int_set_intf.mli avl_sets.mli avl_sets.ml avl_set_client.ml avl_stdlib_set.ml";
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

    let[@def] rec (valid @ total) xs =
      match xs with
      | Nil -> true
      | Cons (head, tail) -> all_greater head tail && valid tail

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
      append_def left middle;
      append_def left_middle right;
      append_def left middle_right;
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        append_associative tail middle right;
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
      append_def left right;
      all_less_def upper result;
      all_less_def upper left;
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        all_less_append upper tail right;
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
      append_def left right;
      all_greater_def lower result;
      all_greater_def lower left;
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        all_greater_append lower tail right;
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
      append_def left right;
      lookup_repr_def element appended;
      lookup_repr_def element left;
      match left with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        lookup_append element tail right;
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
        (all_less_def upper xs;
        lookup_repr_def element xs;
        match xs with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          lookup_above element upper tail;
          refine_ u)
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
        (all_greater_def lower xs;
        lookup_repr_def element xs;
        match xs with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          lookup_below element lower tail;
          refine_ u)
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
        append_def left suffix;
        add_repr_def element whole;
        add_repr_def element left;
        append_def added_left suffix;
        match left with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          add_left element tail pivot right;
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
        all_less_def pivot left;
        append_def left suffix;
        add_repr_def element whole;
        append_def left result_suffix;
        match left with
        | Nil ->
          add_repr_def element right;
          refine_ u
        | Cons (_, tail) ->
          add_right element tail pivot right;
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
        all_less_def pivot left;
        append_def left suffix;
        add_repr_def pivot whole;
        match left with
        | Nil -> refine_ u
        | Cons (_, tail) ->
          add_at_pivot tail pivot right;
          refine_ u
      else refine_ u

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

    let rec (valid_append_pivot @ total) :
        (left : repr) ->
        (pivot : int) ->
        (right : repr) ->
        {u : unit |
          if valid left && valid right
             && all_less pivot left && all_greater pivot right
          then valid (append left (Cons (pivot, right)))
          else true} @ immutable contended =
      fun left pivot right ->
      let u = () in
      if valid left && valid right
         && all_less pivot left && all_greater pivot right
      then
        let suffix = Cons (pivot, right) in
        let result = append left suffix in
        append_def left suffix;
        match left with
        | Nil ->
          valid_def result;
          valid_def suffix;
          refine_ u
        | Cons (head, tail) ->
          valid_def left;
          all_less_def pivot left;
          valid_append_pivot tail pivot right;
          valid_def result;
          all_greater_append head tail suffix;
          all_greater_transitive head pivot right;
          all_greater_def head suffix;
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
          (size_repr xs === 0Z) === same_repr xs Nil}
          @ immutable contended =
      fun xs ->
      let nil = Nil in
      size_repr_def xs;
      same_repr_def xs nil;
      match xs with
      | Nil ->
        let u = () in
        refine_ u
      | Cons (_, tail) ->
        size_nonnegative tail;
        let u = () in
        refine_ u

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
              (lookup_below left_head right_head right_tail;
              refine_ u)
            else if right_head < left_head then
              (lookup_below right_head left_head left_tail;
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
                  (lookup_below element left_head left_tail;
                  lookup_below element right_head right_tail;
                  refine_ u)
                else refine_ u
              in
              extensional_repr left_tail right_tail tail_premise;
              refine_ u)
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

    let[@def] rec (elements @ total) tree =
      match tree with
      | Empty -> List_set.Nil
      | Node (left, value, right, _) ->
        List_set.append (elements left)
          (List_set.Cons (value, elements right))

    let (empty @ total) : tree = Empty

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
          (maximum_def left_left left_right;
          maximum_def left_right right;
          maximum_def left_left new_right;
          refine_ u)
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
          (maximum_def right_left right_right;
          maximum_def left right_left;
          maximum_def new_left right_right;
          refine_ u)
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
          (maximum_def left_left middle;
          maximum_def middle_left middle_right;
          maximum_def left_left middle_left;
          maximum_def middle_right right;
          maximum_def new_left new_right;
          refine_ u)
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
          (maximum_def middle right_right;
          maximum_def middle_left middle_right;
          maximum_def left middle_left;
          maximum_def middle_right right_right;
          maximum_def new_left new_right;
          refine_ u)
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
          (all_less_def lower tree;
          all_less_def upper tree;
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            all_less_weaken lower upper left;
            all_less_weaken lower upper right;
            refine_ u)
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
          (all_greater_def upper tree;
          all_greater_def lower tree;
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            all_greater_weaken lower upper left;
            all_greater_weaken lower upper right;
            refine_ u)
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
          (make_node_def left value right;
          all_less_def upper result;
          refine_ u)
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
          (make_node_def left value right;
          all_greater_def lower result;
          refine_ u)
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
      make_node_def left value right;
      height_def result;
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
          (valid_def tree;
          height_def tree;
          match tree with
          | Empty -> refine_ u
          | Node (left, _, right, _) ->
            let left_height = height left in
            let right_height = height right in
            height_nonnegative left;
            height_nonnegative right;
            maximum_def left_height right_height;
            refine_ u)
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
        (make_node_def left value right;
        make_node_height left value right;
        valid_def result;
        refine_ u)
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
          (make_node_all_less upper left value right;
          make_node_valid left value right;
          make_node_height left value right;
          refine_ u)
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
        (all_greater_weaken lower value right;
        make_node_all_greater lower left value right;
        make_node_valid left value right;
        make_node_height left value right;
        refine_ u)
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
        (valid_def left;
        all_less_def value left;
        height_def left;
        left_rotation_heights left_left_height left_right_height
            right_height left_height;
        make_right_child left_value left_right value right;
        make_node_valid left_left left_value new_right;
        refine_ u)
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
        (valid_def right;
        all_greater_def value right;
        height_def right;
        right_rotation_heights right_left_height right_right_height
            left_height right_height;
        all_less_weaken value right_value left;
        make_left_child right_value left value right_left;
        make_node_valid new_left right_value right_right;
        refine_ u)
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
        (valid_def left;
        valid_def middle;
        all_less_def value left;
        all_less_def value middle;
        all_greater_def left_value middle;
        height_def left;
        height_def middle;
        height_nonnegative left_left;
        height_nonnegative middle_left;
        height_nonnegative middle_right;
        height_nonnegative right;
        left_right_heights left_left_height middle_left_height
            middle_right_height right_height middle_height left_height;
        all_less_weaken left_value middle_value left_left;
        make_left_child middle_value left_left left_value middle_left;
        make_right_child middle_value middle_right value right;
        make_node_valid new_left middle_value new_right;
        refine_ u)
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
        (valid_def right;
        valid_def middle;
        all_greater_def value right;
        all_greater_def value middle;
        all_less_def right_value middle;
        height_def right;
        height_def middle;
        height_nonnegative left;
        height_nonnegative middle_left;
        height_nonnegative middle_right;
        height_nonnegative right_right;
        right_left_heights left_height middle_left_height
            middle_right_height right_right_height middle_height right_height;
        all_less_weaken value middle_value left;
        all_greater_weaken middle_value right_value right_right;
        make_left_child middle_value left value middle_left;
        make_right_child middle_value middle_right right_value right_right;
        make_node_valid new_left middle_value new_right;
        refine_ u)
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
        (balance_def left value right;
        height_nonnegative left;
        height_nonnegative right;
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          match left with
          | Empty ->
            height_def left;
            refine_ u
          | Node (left_left, left_value, left_right, cached_height) ->
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              (rotate_right_valid left_left left_value left_right
                  cached_height value right;
              refine_ u)
            else
              match left_right with
              | Empty ->
                valid_def left;
                height_nonnegative left_left;
                height_def left_right;
                refine_ u
              | Node
                  ( middle_left,
                    middle_value,
                    middle_right,
                    middle_cached_height ) ->
                rotate_left_right_valid left_left left_value middle_left
                    middle_value middle_right middle_cached_height
                    cached_height value right;
                refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then
          match right with
          | Empty ->
            height_def right;
            refine_ u
          | Node (right_left, right_value, right_right, cached_height) ->
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              (rotate_left_valid left value right_left right_value right_right
                  cached_height;
              refine_ u)
            else
              match right_left with
              | Empty ->
                valid_def right;
                height_nonnegative right_right;
                height_def right_left;
                refine_ u
              | Node
                  ( middle_left,
                    middle_value,
                    middle_right,
                    middle_cached_height ) ->
                rotate_right_left_valid left value middle_left middle_value
                    middle_right middle_cached_height right_value right_right
                    cached_height;
                refine_ u
        else
          (make_node_valid left value right;
          refine_ u))
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
        (balance_def left value right;
        maximum_def old_left_height right_height;
        height_nonnegative left;
        height_nonnegative right;
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          match left with
          | Empty ->
            height_def left;
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            let left_left_height = height left_left in
            let left_right_height = height left_right in
            valid_def left;
            height_def left;
            if Bigint.compare left_left_height left_right_height >= 0 then
              let new_right = make_node left_right value right in
              left_rotation_heights left_left_height left_right_height
                  right_height left_height;
              make_node_height left_right value right;
              make_node_height left_left left_value new_right;
              refine_ u
            else
              match left_right with
              | Empty ->
                height_def left_right;
                height_nonnegative left_left;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let middle_height = height left_right in
                let middle_left_height = height middle_left in
                let middle_right_height = height middle_right in
                valid_def left_right;
                height_def left_right;
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                left_right_heights left_left_height middle_left_height
                    middle_right_height right_height middle_height left_height;
                make_node_height left_left left_value middle_left;
                make_node_height middle_right value right;
                make_node_height new_left middle_value new_right;
                refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then refine_ u
        else
          (make_node_height left value right;
          maximum_def left_height right_height;
          refine_ u))
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
        (balance_def left value right;
        maximum_def left_height old_right_height;
        height_nonnegative left;
        height_nonnegative right;
        if Bigint.compare left_height (Bigint.add right_height 1Z) > 0 then
          refine_ u
        else if Bigint.compare right_height (Bigint.add left_height 1Z) > 0
        then
          match right with
          | Empty ->
            height_def right;
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            let right_left_height = height right_left in
            let right_right_height = height right_right in
            valid_def right;
            height_def right;
            if Bigint.compare right_right_height right_left_height >= 0 then
              let new_left = make_node left value right_left in
              right_rotation_heights right_left_height right_right_height
                  left_height right_height;
              make_node_height left value right_left;
              make_node_height new_left right_value right_right;
              refine_ u
            else
              match right_left with
              | Empty ->
                height_def right_left;
                height_nonnegative right_right;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                let middle_height = height right_left in
                let middle_left_height = height middle_left in
                let middle_right_height = height middle_right in
                valid_def right_left;
                height_def right_left;
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                right_left_heights left_height middle_left_height
                    middle_right_height right_right_height middle_height
                    right_height;
                make_node_height left value middle_left;
                make_node_height middle_right right_value right_right;
                make_node_height new_left middle_value new_right;
                refine_ u
        else
          (make_node_height left value right;
          maximum_def left_height right_height;
          refine_ u))
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
        (balance_def left value right;
        if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0
        then
          match left with
          | Empty ->
            make_node_all_less upper left value right;
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            all_less_def upper left;
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              let new_right = make_node left_right value right in
              make_node_all_less upper left_right value right;
              make_node_all_less upper left_left left_value new_right;
              refine_ u
            else
              match left_right with
              | Empty ->
                make_node_all_less upper left value right;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                all_less_def upper left_right;
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                make_node_all_less upper left_left left_value middle_left;
                make_node_all_less upper middle_right value right;
                make_node_all_less upper new_left middle_value new_right;
                refine_ u
        else if
          Bigint.compare (height right) (Bigint.add (height left) 1Z) > 0
        then
          match right with
          | Empty ->
            make_node_all_less upper left value right;
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            all_less_def upper right;
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              let new_left = make_node left value right_left in
              make_node_all_less upper left value right_left;
              make_node_all_less upper new_left right_value right_right;
              refine_ u
            else
              match right_left with
              | Empty ->
                make_node_all_less upper left value right;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                all_less_def upper right_left;
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                make_node_all_less upper left value middle_left;
                make_node_all_less upper middle_right right_value right_right;
                make_node_all_less upper new_left middle_value new_right;
                refine_ u
        else
          (make_node_all_less upper left value right;
          refine_ u))
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
        (balance_def left value right;
        if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0
        then
          match left with
          | Empty ->
            make_node_all_greater lower left value right;
            refine_ u
          | Node (left_left, left_value, left_right, _) ->
            all_greater_def lower left;
            if Bigint.compare (height left_left) (height left_right) >= 0
            then
              let new_right = make_node left_right value right in
              make_node_all_greater lower left_right value right;
              make_node_all_greater lower left_left left_value new_right;
              refine_ u
            else
              match left_right with
              | Empty ->
                make_node_all_greater lower left value right;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                all_greater_def lower left_right;
                let new_left = make_node left_left left_value middle_left in
                let new_right = make_node middle_right value right in
                make_node_all_greater lower left_left left_value middle_left;
                make_node_all_greater lower middle_right value right;
                make_node_all_greater lower new_left middle_value new_right;
                refine_ u
        else if
          Bigint.compare (height right) (Bigint.add (height left) 1Z) > 0
        then
          match right with
          | Empty ->
            make_node_all_greater lower left value right;
            refine_ u
          | Node (right_left, right_value, right_right, _) ->
            all_greater_def lower right;
            if Bigint.compare (height right_right) (height right_left) >= 0
            then
              let new_left = make_node left value right_left in
              make_node_all_greater lower left value right_left;
              make_node_all_greater lower new_left right_value right_right;
              refine_ u
            else
              match right_left with
              | Empty ->
                make_node_all_greater lower left value right;
                refine_ u
              | Node (middle_left, middle_value, middle_right, _) ->
                all_greater_def lower right_left;
                let new_left = make_node left value middle_left in
                let new_right = make_node middle_right right_value right_right in
                make_node_all_greater lower left value middle_left;
                make_node_all_greater lower middle_right right_value right_right;
                make_node_all_greater lower new_left middle_value new_right;
                refine_ u
        else
          (make_node_all_greater lower left value right;
          refine_ u))
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
        (add_tree_def element tree;
        all_less_def upper tree;
        match tree with
        | Empty ->
          all_less_def upper result;
          refine_ u
        | Node (left, value, right, _) ->
          if element = value then refine_ u
          else if element < value then
            let new_left = add_tree element left in
            add_all_less element upper left;
            balance_all_less upper new_left value right;
            refine_ u
          else
            let new_right = add_tree element right in
            add_all_less element upper right;
            balance_all_less upper left value new_right;
            refine_ u)
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
        (add_tree_def element tree;
        all_greater_def lower tree;
        match tree with
        | Empty ->
          all_greater_def lower result;
          refine_ u
        | Node (left, value, right, _) ->
          if element = value then refine_ u
          else if element < value then
            let new_left = add_tree element left in
            add_all_greater element lower left;
            balance_all_greater lower new_left value right;
            refine_ u
          else
            let new_right = add_tree element right in
            add_all_greater element lower right;
            balance_all_greater lower left value new_right;
            refine_ u)
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
      validity;
      let result = add_tree element tree in
      let u = () in
      valid_def tree;
      add_tree_def element tree;
      match tree with
      | Empty ->
        let zero = 0Z in
        height_def tree;
        height_def result;
        valid_def result;
        all_less_def element tree;
        all_greater_def element tree;
        maximum_def zero zero;
        refine_ u
      | Node (left, value, right, _) ->
        let left_height = height left in
        let right_height = height right in
        height_def tree;
        if element = value then refine_ u
        else if element < value then
          let proof = () in
          let left_validity : {u : unit | valid left} = refine_ proof in
          let new_left = add_tree element left in
          add_valid_height element left left_validity;
          add_all_less element value left;
          balance_valid new_left value right;
          balance_left_height left_height new_left value right;
          refine_ u
        else
          let proof = () in
          let right_validity : {u : unit | valid right} = refine_ proof in
          let new_right = add_tree element right in
          add_valid_height element right right_validity;
          add_all_greater element value right;
          balance_valid left value new_right;
          balance_right_height left value new_right right_height;
          refine_ u
    end

    module Model_proofs : sig
      module Set : Int_set_intf.Extensional
    end = struct
    module Element_proofs : sig
      val elements_all_less :
        (upper : int) ->
        (tree : tree) ->
        {u : unit |
          if all_less upper tree
          then List_set.all_less upper (elements tree)
          else true} @ immutable contended

      val elements_valid :
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit | List_set.valid (elements tree)} @ immutable contended

      val lookup :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {found : bool |
          found === List_set.lookup_repr element (elements tree)}
          @ immutable contended

      val lookup_tree_elements :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          lookup_tree element tree
          === List_set.lookup_repr element (elements tree)}
          @ immutable contended
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
        all_less_def upper tree;
        elements_def tree;
        match tree with
        | Empty ->
          List_set.all_less_def upper tree_elements;
          refine_ u
        | Node (left, value, right, _) ->
          let left_elements = elements left in
          let right_elements = elements right in
          let suffix = List_set.Cons (value, right_elements) in
          elements_all_less upper left;
          elements_all_less upper right;
          List_set.all_less_def upper suffix;
          List_proofs.all_less_append upper left_elements suffix;
          refine_ u
      else refine_ u

    module Lookup_proofs : sig
      val elements_all_greater :
        (lower : int) ->
        (tree : tree) ->
        {u : unit |
          if all_greater lower tree
          then List_set.all_greater lower (elements tree)
          else true} @ immutable contended

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
          all_greater_def lower tree;
          elements_def tree;
          match tree with
          | Empty ->
            List_set.all_greater_def lower tree_elements;
            refine_ u
          | Node (left, value, right, _) ->
            let left_elements = elements left in
            let right_elements = elements right in
            let suffix = List_set.Cons (value, right_elements) in
            elements_all_greater lower left;
            elements_all_greater lower right;
            List_set.all_greater_def lower suffix;
            List_proofs.all_greater_append lower left_elements suffix;
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
        validity;
        let u = () in
        valid_def tree;
        lookup_tree_def element tree;
        elements_def tree;
        match tree with
        | Empty ->
          let model = elements tree in
          List_set.lookup_repr_def element model;
          refine_ u
        | Node (left, value, right, _) ->
          let left_elements = elements left in
          let right_elements = elements right in
          let suffix = List_set.Cons (value, right_elements) in
          List_proofs.lookup_append element left_elements suffix;
          List_set.lookup_repr_def element suffix;
          if element = value then refine_ u
          else if element < value then
            let proof = () in
            let left_valid : {u : unit | valid left} = refine_ proof in
            lookup_tree_elements element left left_valid;
            elements_all_greater value right;
            List_proofs.lookup_below element value right_elements;
            refine_ u
          else
            let proof = () in
            let right_valid : {u : unit | valid right} = refine_ proof in
            lookup_tree_elements element right right_valid;
            elements_all_less value left;
            List_proofs.lookup_above element value left_elements;
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
      Lookup_proofs.lookup_tree_elements element tree validity;
      refine_ found

    let (lookup_tree_elements @ total) :
        (element : int) ->
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit |
          lookup_tree element tree
          === List_set.lookup_repr element (elements tree)}
          @ immutable contended =
      fun element tree validity ->
      Lookup_proofs.lookup_tree_elements element tree validity

    let rec (elements_valid @ total) :
        (tree : tree) ->
        (validity : {u : unit | valid tree}) ->
        {u : unit | List_set.valid (elements tree)} @ immutable contended =
      fun tree validity ->
      validity;
      let tree_elements = elements tree in
      elements_def tree;
      match tree with
      | Empty ->
        List_set.valid_def tree_elements;
        let u = () in
        refine_ u
      | Node (left, value, right, _) ->
        valid_def tree;
        let left_elements = elements left in
        let right_elements = elements right in
        let proof = () in
        let left_validity : {u : unit | valid left} = refine_ proof in
        let right_validity : {u : unit | valid right} = refine_ proof in
        elements_valid left left_validity;
        elements_valid right right_validity;
        elements_all_less value left;
        Lookup_proofs.elements_all_greater value right;
        List_proofs.valid_append_pivot left_elements value right_elements;
        refine_ proof

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
      make_node_def left value right;
      elements_def result;
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
      elements_def tree;
      make_node_elements left value right;
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
      make_node_elements left_left left_value left_right;
      make_node_elements left_right value right;
      make_node_elements left_left left_value new_right;
      make_node_elements old_left value right;
      List_proofs.append_associative left_elements left_tail right_tail;
      List_set.append_def left_tail right_tail;
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
      make_node_elements right_left right_value right_right;
      make_node_elements left value right_left;
      make_node_elements new_left right_value right_right;
      make_node_elements left value old_right;
      List_proofs.append_associative left_elements left_tail right_tail;
      List_set.append_def left_tail right_tail;
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
      rotate_left_elements left_left left_value middle_left middle_value
          middle_right;
      rotate_right_elements new_left middle_value middle_right value right;
      make_node_elements rotated_left value right;
      make_node_elements original_left value right;
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
      rotate_right_elements middle_left middle_value middle_right right_value
          right_right;
      rotate_left_elements left value middle_left middle_value new_right;
      make_node_elements left value rotated_right;
      make_node_elements left value original_right;
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
      balance_def left value right;
      if Bigint.compare (height left) (Bigint.add (height right) 1Z) > 0 then
        match left with
        | Empty ->
          let u = () in
          refine_ u
        | Node (left_left, left_value, left_right, cached_height) ->
          let rebuilt_left = make_node left_left left_value left_right in
          cached_height_irrelevant left_left left_value left_right
              cached_height;
          make_node_elements left value right;
          make_node_elements rebuilt_left value right;
          if Bigint.compare (height left_left) (height left_right) >= 0 then
            (rotate_right_elements left_left left_value left_right value right;
            let u = () in
            refine_ u)
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
              cached_height_irrelevant middle_left middle_value middle_right
                  middle_height;
              make_node_elements left_left left_value left_right;
              make_node_elements left_left left_value rebuilt_middle;
              make_node_elements canonical_left value right;
              rotate_left_right_elements left_left left_value middle_left
                  middle_value middle_right value right;
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
          cached_height_irrelevant right_left right_value right_right
              cached_height;
          make_node_elements left value right;
          make_node_elements left value rebuilt_right;
          if Bigint.compare (height right_right) (height right_left) >= 0 then
            (rotate_left_elements left value right_left right_value right_right;
            let u = () in
            refine_ u)
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
              cached_height_irrelevant middle_left middle_value middle_right
                  middle_height;
              make_node_elements right_left right_value right_right;
              make_node_elements rebuilt_middle right_value right_right;
              make_node_elements left value canonical_right;
              rotate_right_left_elements left value middle_left middle_value
                  middle_right right_value right_right;
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
      elements_def empty_left;
      elements_def empty_right;
      elements_def tree;
      List_set.append_def nil suffix;
      let u = () in
      refine_ u

    let (add_leaf_step @ total) element :
        {u : unit |
          elements (Node (Empty, element, Empty, 1Z))
          === List_set.add_repr element (elements Empty)} =
      let empty = Empty in
      let empty_elements = elements empty in
      leaf_elements element;
      elements_def empty;
      List_set.add_repr_def element empty_elements;
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
        (valid_def tree;
        elements_def tree;
          Element_proofs.elements_all_less value left;
        List_proofs.add_at_pivot left_elements value right_elements;
        refine_ u)
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
      correctness;
      let tree = Node (left, value, right, cached_height) in
      let left_elements = elements left in
      let right_elements = elements right in
      let u = () in
      if element < value && valid left then
        (elements_def tree;
        balance_elements new_left value right;
        make_node_elements new_left value right;
        List_proofs.add_left element left_elements value right_elements;
        refine_ u)
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
      correctness;
      let tree = Node (left, value, right, cached_height) in
      let left_elements = elements left in
      let right_elements = elements right in
      let u = () in
      if value < element && all_less value left && valid right then
        (Element_proofs.elements_all_less value left;
        elements_def tree;
        balance_elements left value new_right;
        make_node_elements left value new_right;
        List_proofs.add_right element left_elements value right_elements;
        refine_ u)
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
        (valid_def tree;
        add_tree_def element tree;
        match tree with
        | Empty ->
          add_leaf_step element;
          refine_ u
        | Node (left, value, right, cached_height) ->
          if element = value then
            (add_equal_step left value right cached_height;
            refine_ u)
          else if element < value then
            let induction = add_tree_elements element left in
            add_left_step element left value right cached_height
                induction;
            refine_ u
          else
            (Element_proofs.elements_all_less value left;
            let induction = add_tree_elements element right in
            add_right_step element left value right cached_height
                induction;
            refine_ u))
      else refine_ u

    end

    module Operations = struct
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
      validity;
      Validity_proofs.add_valid_height element tree validity;
      Insertion_model_proofs.add_tree_elements element tree;
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
      validity;
      let u = () in
      add_elements_def xs tree;
      let tree_elements = elements tree in
      List_set.union_repr_def xs tree_elements;
      match xs with
      | List_set.Nil -> refine_ u
      | List_set.Cons (head, tail) ->
        let added = add_tree head tree in
        let refine_ added_validity =
          Validity_proofs.add_valid_height head tree validity
        in
        Insertion_model_proofs.add_tree_elements head tree;
        let proof = () in
        let added_validity : {u : unit | valid added} = refine_ proof in
        add_elements_spec tail added added_validity;
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
      add_elements_spec left_elements right right_validity;
      refine_ result

    let (size @ total) tree =
      let tree_elements = elements tree in
      List_set.size_repr tree_elements

    end

    module Set = struct
      type t = {tree : tree | valid tree}

      let (empty @ total) : t =
        let tree = Empty in
        valid_def tree;
        refine_ tree

      let[@def] (lookup @ total) element (set : t) =
        let refine_ tree = set in
        lookup_tree element tree

      let[@def] (add @ total) :
          int -> t -> t @ immutable contended =
        fun element set ->
        let refine_ tree = set in
        let proof = () in
        let validity : {u : unit | valid tree} = refine_ proof in
        let refine_ result_tree = Operations.add element tree validity in
        refine_ result_tree

      let[@def] (union @ total) :
          t -> t -> t @ immutable contended =
        fun left right ->
        let refine_ left_tree = left in
        let refine_ right_tree = right in
        let proof = () in
        let right_validity : {u : unit | valid right_tree} = refine_ proof in
        let refine_ result_tree =
          Operations.union left_tree right_tree right_validity
        in
        refine_ result_tree

      let[@def] (size @ total) (set : t) =
        let refine_ tree = set in
        List_set.size_repr (elements tree)

      let[@def] (equal @ total) (left : t) (right : t) =
        let refine_ left_tree = left in
        let refine_ right_tree = right in
        List_set.same_repr (elements left_tree) (elements right_tree)

      let (lookup_empty @ total) element :
          {u : unit | lookup element empty === false} =
        let empty_set = empty in
        let refine_ tree = empty_set in
        lookup_def element empty_set;
        lookup_tree_def element tree;
        let u = () in
        refine_ u

      let (lookup_add @ total) :
          (element : int) ->
          (added_element : int) ->
          (set : t) ->
          {u : unit |
            lookup element (add added_element set)
            === (element = added_element || lookup element set)}
            @ immutable contended =
        fun element added_element set ->
        let refine_ tree = set in
        let proof = () in
        let validity : {u : unit | valid tree} = refine_ proof in
        let refine_ model_result_tree =
          Operations.add added_element tree validity
        in
        let result = add added_element set in
        add_def added_element set;
        let refine_ result_tree = result in
        let expected = List_set.add_repr added_element (elements tree) in
        let result_elements = elements result_tree in
        List_proofs.same_repr_equal result_elements expected;
        let result_validity : {u : unit | valid result_tree} = refine_ proof in
        Element_proofs.lookup_tree_elements element tree validity;
        Element_proofs.lookup_tree_elements element result_tree
            result_validity;
        lookup_def element set;
        lookup_def element result;
        let tree_elements = elements tree in
        List_proofs.lookup_add_repr element added_element tree_elements;
        refine_ proof

      let (lookup_union @ total) :
          (element : int) ->
          (left : t) ->
          (right : t) ->
          {u : unit |
            lookup element (union left right)
            === (lookup element left || lookup element right)}
            @ immutable contended =
        fun element left right ->
        let refine_ left_tree = left in
        let refine_ right_tree = right in
        let proof = () in
        let right_validity : {u : unit | valid right_tree} = refine_ proof in
        let refine_ model_result_tree =
          Operations.union left_tree right_tree right_validity
        in
        let result = union left right in
        union_def left right;
        let refine_ result_tree = result in
        let left_elements = elements left_tree in
        let right_elements = elements right_tree in
        let expected = List_set.union_repr left_elements right_elements in
        let result_elements = elements result_tree in
        List_proofs.same_repr_equal result_elements expected;
        let left_validity : {u : unit | valid left_tree} = refine_ proof in
        let result_validity : {u : unit | valid result_tree} = refine_ proof in
        Element_proofs.lookup_tree_elements element left_tree left_validity;
        Element_proofs.lookup_tree_elements element right_tree right_validity;
        Element_proofs.lookup_tree_elements element result_tree
            result_validity;
        lookup_def element left;
        lookup_def element right;
        lookup_def element result;
        List_proofs.lookup_union_repr element left_elements right_elements;
        refine_ proof

      let (equal_lookup @ total) :
          (left : t) ->
          (right : t) ->
          (element : int) ->
          {u : unit |
            if equal left right
            then lookup element left === lookup element right
            else true} @ immutable contended =
        fun left right element ->
        let proof = () in
        if equal left right then
          let refine_ left_tree = left in
          let refine_ right_tree = right in
          let left_elements = elements left_tree in
          let right_elements = elements right_tree in
          equal_def left right;
          List_proofs.same_repr_equal left_elements right_elements;
          let left_validity : {u : unit | valid left_tree} = refine_ proof in
          let right_validity : {u : unit | valid right_tree} = refine_ proof in
          Element_proofs.lookup_tree_elements element left_tree left_validity;
          Element_proofs.lookup_tree_elements element right_tree
              right_validity;
          lookup_def element left;
          lookup_def element right;
          refine_ proof
        else refine_ proof

      let (extensional @ total) :
          (left : t) ->
          (right : t) ->
          ((element : int) ->
            {u : unit | lookup element left === lookup element right})
            @ total ->
          {u : unit | equal left right === true} @ immutable contended =
        fun left right premise ->
        let refine_ left_tree = left in
        let refine_ right_tree = right in
        let left_elements = elements left_tree in
        let right_elements = elements right_tree in
        let proof = () in
        let left_validity : {u : unit | valid left_tree} = refine_ proof in
        let right_validity : {u : unit | valid right_tree} = refine_ proof in
        Element_proofs.elements_valid left_tree left_validity;
        Element_proofs.elements_valid right_tree right_validity;
        let (model_premise @ total) :
            (element : int) ->
            {u : unit |
              List_set.lookup_repr element left_elements
              === List_set.lookup_repr element right_elements} =
          fun element ->
          let refine_ same_lookup = premise element in
          Element_proofs.lookup_tree_elements element left_tree left_validity;
          Element_proofs.lookup_tree_elements element right_tree
              right_validity;
          lookup_def element left;
          lookup_def element right;
          refine_ proof
        in
        List_proofs.extensional_repr left_elements right_elements
            model_premise;
        List_proofs.same_repr_reflexive left_elements;
        equal_def left right;
        refine_ proof

      let (size_zero @ total) (set : t) :
          {u : unit | (size set === 0Z) === equal set empty} =
        let empty_set = empty in
        let refine_ tree = set in
        let refine_ empty_tree = empty_set in
        let tree_elements = elements tree in
        size_def set;
        equal_def set empty_set;
        elements_def empty_tree;
        List_proofs.size_zero_repr tree_elements;
        let u = () in
        refine_ u
    end

    end

include Model_proofs.Set
