type t =
  [ `Add
  | `And
  | `Bigint_abs
  | `Bigint_add
  | `Bigint_compare
  | `Bigint_ge
  | `Bigint_gt
  | `Bigint_is_zero
  | `Bigint_le
  | `Bigint_lt
  | `Bigint_mul
  | `Bigint_neg
  | `Bigint_of_int
  | `Bigint_one
  | `Bigint_sub
  | `Bigint_zero
  | `Equal
  | `Greater
  | `Greater_equal
  | `Less
  | `Less_equal
  | `Multiply
  | `Not
  | `Not_equal
  | `Or
  | `Subtract ]

let of_primitive = function
  | "%equal" -> Some `Equal
  | "%notequal" -> Some `Not_equal
  | "%lessthan" -> Some `Less
  | "%lessequal" -> Some `Less_equal
  | "%greaterthan" -> Some `Greater
  | "%greaterequal" -> Some `Greater_equal
  | "%addint" -> Some `Add
  | "%subint" -> Some `Subtract
  | "%mulint" -> Some `Multiply
  | "%sequand" -> Some `And
  | "%sequor" -> Some `Or
  | "%boolnot" -> Some `Not
  | _ -> None
;;

let persistent name id =
  Ident.same id (Ident.create_persistent name)
;;

let bigint_root = function
  | Path.Pdot (Path.Pident root, "Bigint") when persistent "Stdlib" root ->
    true
  | Path.Pident root when persistent "Stdlib__Bigint" root -> true
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;

let of_path = function
  | Path.Pdot (root, member) when bigint_root root ->
    (match member with
     | "zero" -> Some `Bigint_zero
     | "one" -> Some `Bigint_one
     | "of_int" -> Some `Bigint_of_int
     | "is_zero" -> Some `Bigint_is_zero
     | "compare" -> Some `Bigint_compare
     | "equal" -> Some `Equal
     | "lt" -> Some `Bigint_lt
     | "le" -> Some `Bigint_le
     | "gt" -> Some `Bigint_gt
     | "ge" -> Some `Bigint_ge
     | "neg" -> Some `Bigint_neg
     | "abs" -> Some `Bigint_abs
     | "add" -> Some `Bigint_add
     | "sub" -> Some `Bigint_sub
     | "mul" -> Some `Bigint_mul
     | _ -> None)
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> None
;;

let is_bigint_type = function
  | Path.Pdot (root, "t") -> bigint_root root
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;
