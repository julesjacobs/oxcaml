type t =
  [ `Add
  | `And
  | `Bit_and
  | `Bit_or
  | `Bit_xor
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
  | `Identity
  | `Int_max
  | `Int_min
  | `Less
  | `Less_equal
  | `Multiply
  | `Negate
  | `Not
  | `Not_equal
  | `Or
  | `Pred
  | `Shift_left
  | `Shift_right_arithmetic
  | `Shift_right_logical
  | `Subtract
  | `Succ ]

let persistent name id =
  Ident.same id (Ident.create_persistent name)
;;

let stdlib_member expected = function
  | Path.Pdot (Path.Pident root, member) ->
    persistent "Stdlib" root && String.equal member expected
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;

let int_member expected = function
  | Path.Pdot
      (Path.Pdot (Path.Pident root, "Int"), member) ->
    persistent "Stdlib" root && String.equal member expected
  | Path.Pdot (Path.Pident root, member) ->
    persistent "Stdlib__Int" root && String.equal member expected
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;

(* Some runtime primitives are shared by operations at several source types.
   Recognize those only at the canonical Stdlib paths whose logical semantics
   we model.  In particular, [%identity] also implements [Fun.id], character
   conversions, and unsafe casts, while the bitwise primitives also implement
   [Bool.logand], [Bool.logor], and [Bool.logxor]. *)
let of_primitive ~path = function
  | "%identity" when stdlib_member "~+" path -> Some `Identity
  | "%equal" -> Some `Equal
  | "%notequal" -> Some `Not_equal
  | "%lessthan" -> Some `Less
  | "%lessequal" -> Some `Less_equal
  | "%greaterthan" -> Some `Greater
  | "%greaterequal" -> Some `Greater_equal
  | "%addint" -> Some `Add
  | "%subint" -> Some `Subtract
  | "%mulint" -> Some `Multiply
  | "%negint" -> Some `Negate
  | "%succint" -> Some `Succ
  | "%predint" -> Some `Pred
  | "%andint"
    when stdlib_member "land" path || int_member "logand" path ->
    Some `Bit_and
  | "%orint" when stdlib_member "lor" path || int_member "logor" path ->
    Some `Bit_or
  | "%xorint"
    when stdlib_member "lxor" path || int_member "logxor" path ->
    Some `Bit_xor
  | "%lslint" -> Some `Shift_left
  | "%lsrint" -> Some `Shift_right_logical
  | "%asrint" -> Some `Shift_right_arithmetic
  | "%sequand" -> Some `And
  | "%sequor" -> Some `Or
  | "%boolnot" -> Some `Not
  | _ -> None
;;

let bigint_root = function
  | Path.Pdot (Path.Pident root, "Bigint") when persistent "Stdlib" root ->
    true
  | Path.Pident root when persistent "Stdlib__Bigint" root -> true
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;

let int_root = function
  | Path.Pdot (Path.Pident root, "Int") when persistent "Stdlib" root -> true
  | Path.Pident root when persistent "Stdlib__Int" root -> true
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
;;

let of_path = function
  | Path.Pdot (Path.Pident root, "max_int") when persistent "Stdlib" root ->
    Some `Int_max
  | Path.Pdot (Path.Pident root, "min_int") when persistent "Stdlib" root ->
    Some `Int_min
  | Path.Pdot (root, "max_int") when int_root root -> Some `Int_max
  | Path.Pdot (root, "min_int") when int_root root -> Some `Int_min
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

type carrier =
  | Int
  | Bool

(* Which operations agree with their model when executed, and why the rest do
   not.  The shifts are undefined in OCaml once the distance leaves [0, 62]
   while the model gives every distance a value.  The comparisons agree at
   [int]: polymorphic equality at [float] is not the backends' reflexive
   equality, and at an abstract carrier it compares a representation the
   model does not describe.  The [Bigint] operations do compute the
   mathematical integers they are modelled as, but their constants carry a
   [logical] modality and so have no run-time value at all; admitting that
   family is separate work. *)
let runtime_result builtin ~operands =
  match builtin, operands with
  | ( (`Add | `Subtract | `Multiply | `Bit_and | `Bit_or | `Bit_xor),
      [ Int; Int ] ) -> Some Int
  | (`Negate | `Succ | `Pred | `Identity), [ Int ] -> Some Int
  | ( ( `Equal | `Not_equal | `Less | `Less_equal | `Greater
      | `Greater_equal ),
      [ Int; Int ] ) -> Some Bool
  | (`And | `Or), [ Bool; Bool ] -> Some Bool
  | `Not, [ Bool ] -> Some Bool
  | ( ( `Add | `And | `Bigint_abs | `Bigint_add | `Bigint_compare
      | `Bigint_ge | `Bigint_gt | `Bigint_is_zero | `Bigint_le | `Bigint_lt
      | `Bigint_mul | `Bigint_neg | `Bigint_of_int | `Bigint_one
      | `Bigint_sub | `Bigint_zero | `Bit_and | `Bit_or | `Bit_xor | `Equal
      | `Greater | `Greater_equal | `Identity | `Int_max | `Int_min | `Less
      | `Less_equal | `Multiply | `Negate | `Not | `Not_equal | `Or | `Pred
      | `Shift_left | `Shift_right_arithmetic | `Shift_right_logical
      | `Subtract | `Succ ),
      _ ) -> None
;;

(* A match arm that did not fire contributes the fact that its scrutinee is
   not that arm's constructor.  The fact is carried as an application of a
   function with this prefix so that each backend can give it meaning: Lean
   case-splits on the subject, and the SMT backends emit the datatype tester.
   It lives here because both backends need to recognise it and neither
   depends on the other. *)
let constructor_mismatch_prefix = "*vox-match-constructor-mismatch*:"

let constructor_mismatch_name constructor =
  constructor_mismatch_prefix ^ constructor

let constructor_mismatch name =
  let length = String.length constructor_mismatch_prefix in
  if String.length name >= length
     && String.equal (String.sub name 0 length) constructor_mismatch_prefix
  then Some (String.sub name length (String.length name - length))
  else None
