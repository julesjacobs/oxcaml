  module MakeTotal : functor (Ord : TotalOrderedType) -> TotalS
    with type elt = Ord.t
     and type t = Set.MakeTotal(Ord).t
