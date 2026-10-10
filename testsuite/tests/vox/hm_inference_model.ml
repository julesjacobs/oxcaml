type variable = Hm_declarative.variable

type ty = Copy_spec.ty =
  | Variable of variable
  | Boolean
  | Word64
  | List_type of ty
  | Function of ty * ty
[@@inductive]

let[@def] rec (substitute @ total)
    (delta : (variable @ immutable total -> ty @ immutable total) @ total)
    (ty : ty @ immutable) =
  match ty with
  | Variable p -> delta p
  | Boolean -> Boolean
  | Word64 -> Word64
  | List_type a -> List_type (substitute delta a)
  | Function (a, b) -> Function (substitute delta a, substitute delta b)
