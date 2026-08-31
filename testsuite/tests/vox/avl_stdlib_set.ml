module Int_order = struct
  type t = int
  external compare : int -> int -> int @@ total = "%compare"
end

module Model = Set.MakeTotal (Int_order)

type paired =
  { implementation : Avl_sets.t;
    model : Model.t
  }

let (empty_refines @ total) query :
    {result : paired |
      Avl_sets.lookup query result.implementation
      === Model.mem query result.model} =
  let implementation = Avl_sets.empty in
  let model = Model.empty in
  let refine_ implementation_law = Avl_sets.lookup_empty query in
  let result =
    { implementation;
      model
    }
  in
  refine_ result

let (add_refines_at @ total) :
    (query : int) ->
    (added : int) ->
    {input : paired |
      Avl_sets.lookup query input.implementation
      === Model.mem query input.model} ->
    {u : unit |
      (query = added)
      === Model.mem query (Model.Refined.singleton added)} ->
    {result : paired |
      Avl_sets.lookup query result.implementation
      === Model.mem query result.model} =
  fun query added input class_compatibility ->
  let refine_ input = input in
  let refine_ class_compatibility = class_compatibility in
  let implementation = input.implementation in
  let model = input.model in
  let refine_ implementation_law =
    Avl_sets.lookup_add query added implementation
  in
  let result =
    { implementation = Avl_sets.add added implementation;
      model = Model.Refined.add added model
    }
  in
  refine_ result

let (union_refines_at @ total) :
    (query : int) ->
    {left : paired |
      Avl_sets.lookup query left.implementation
      === Model.mem query left.model} ->
    {right : paired |
      Avl_sets.lookup query right.implementation
      === Model.mem query right.model} ->
    {result : paired |
      Avl_sets.lookup query result.implementation
      === Model.mem query result.model} =
  fun query left right ->
  let refine_ left = left in
  let refine_ right = right in
  let left_implementation = left.implementation in
  let right_implementation = right.implementation in
  let left_model = left.model in
  let right_model = right.model in
  let refine_ implementation_law =
    Avl_sets.lookup_union query left_implementation right_implementation
  in
  let result =
    { implementation =
        Avl_sets.union left_implementation right_implementation;
      model = Model.Refined.union left_model right_model
    }
  in
  refine_ result

let (inserted_key_refines @ total) added input :
    {result : paired |
      Avl_sets.lookup added result.implementation
      === Model.mem added result.model} =
  let implementation = input.implementation in
  let model = input.model in
  let refine_ implementation_law =
    Avl_sets.lookup_add added added implementation
  in
  let result =
    { implementation = Avl_sets.add added implementation;
      model = Model.Refined.add added model
    }
  in
  refine_ result

let () =
  let one_value = 1 in
  let two_value = 2 in
  let three_value = 3 in
  let empty =
    { implementation = Avl_sets.empty;
      model = Model.empty
    }
  in
  let refine_ one = inserted_key_refines one_value empty in
  let refine_ two = inserted_key_refines two_value one in
  let refine_ three = inserted_key_refines three_value two in
  Format.printf "AVL/model members = %b,%b,%b@."
    (Avl_sets.lookup 1 three.implementation && Model.mem 1 three.model)
    (Avl_sets.lookup 2 three.implementation && Model.mem 2 three.model)
    (Avl_sets.lookup 3 three.implementation && Model.mem 3 three.model)
