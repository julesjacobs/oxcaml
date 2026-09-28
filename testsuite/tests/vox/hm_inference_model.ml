let[@def] rec (substitute @ total)
    (delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (ty : Copy_spec.ty @ immutable) =
  match ty with
  | Copy_spec.Variable p -> delta p
  | Copy_spec.Boolean -> Copy_spec.Boolean
  | Copy_spec.Word64 -> Copy_spec.Word64
  | Copy_spec.List_type a -> Copy_spec.List_type (substitute delta a)
  | Copy_spec.Function (a, b) -> Copy_spec.Function (substitute delta a, substitute delta b)
