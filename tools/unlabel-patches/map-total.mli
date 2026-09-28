  module MakeTotal : functor (Ord : TotalOrderedType) -> TotalS
    with type key = Ord.t
     and type 'a t = 'a Map.MakeTotal(Ord).t
