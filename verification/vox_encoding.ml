open Types
open Vox_smt

type opaque_key =
  | Variable of int
  | Constructor of Path.t * opaque_key list
  | Tuple of (string option * opaque_key) list
  | Box of opaque_key

module Opaque_keys = Hashtbl.Make (struct
  type t = opaque_key

  let equal = ( = )

  let hash = Hashtbl.hash
end)

let opaque_ids = Opaque_keys.create 32

let next_opaque_id = ref 0

let opaque_id key =
  match Opaque_keys.find_opt opaque_ids key with
  | Some id -> id
  | None ->
    let id = !next_opaque_id in
    incr next_opaque_id;
    Opaque_keys.add opaque_ids key id;
    id

let opaque_key env ty =
  let rec loop visited ty =
    let ty = Ctype.expand_head env ty in
    let id = get_id ty in
    if Numbers.Int.Set.mem id visited
    then None
    else
      let visited = Numbers.Int.Set.add id visited in
      match get_desc ty with
      | Tvar _ | Tunivar _ -> Some (Variable id)
      | Tconstr (path, arguments, _) ->
        Option.map
          (fun arguments ->
            Constructor (Env.normalize_type_path None env path, arguments))
          (Misc.Stdlib.List.map_option (loop visited) arguments)
      | Ttuple components ->
        Option.map
          (fun components -> Tuple components)
          (Misc.Stdlib.List.map_option
             (fun (label, ty) ->
               Option.map (fun key -> label, key) (loop visited ty))
             components)
      | Tmod (ty, _) | Tpoly (ty, []) | Trefine { ref_payload = ty; _ } ->
        loop visited ty
      | Tbox ty -> Option.map (fun key -> Box key) (loop visited ty)
      | Tarrow _ | Tunboxed_tuple _ | Tobject _ | Tfield _ | Tquote _
      | Tsplice _ | Tquote_eval _ | Tnil | Tlink _ | Tsubst _ | Tvariant _
      | Tpoly _ | Trepr _ | Tpackage _ | Tof_kind _ ->
        None
  in
  loop Numbers.Int.Set.empty ty

let sort env ty =
  match Vox_type.classify_payload env ty with
  | Some Vox_type.Int -> Some Int63
  | Some Vox_type.Bool -> Some Bool
  | Some Vox_type.Bigint -> Some Int
  | None -> Option.map (fun key -> Opaque (opaque_id key)) (opaque_key env ty)

let primitive env path =
  match (Env.find_value path env).val_kind with
  | Val_prim p -> Some (p.Primitive.prim_name, p.prim_arity)
  | _ -> None
  | exception Not_found -> None

let value_constant env ty path =
  if sort env ty <> Some Int
  then None
  else
    let path = Env.normalize_value_path None env path in
    if Path.same path (Vox_type.bigint_path "zero")
    then Some (Big_integer "0")
    else if Path.same path (Vox_type.bigint_path "one")
    then Some (Big_integer "1")
    else None

let operation env ~function_type ~result_type name args =
  let unary sort op =
    match args with
    | [Some x] when term_sort x = sort -> Some (App (op, [x]))
    | _ -> None
  in
  let binary sort op =
    match args with
    | [Some x; Some y] when term_sort x = sort && term_sort y = sort ->
      Some (App (op, [x; y]))
    | _ -> None
  in
  let equality op =
    match args with
    | [Some x; Some y] when term_sort x = term_sort y -> Some (App (op, [x; y]))
    | _ -> None
  in
  let argument_type =
    match get_desc (Ctype.expand_head env function_type) with
    | Tarrow (_, arg, _, _) -> Vox_type.classify_payload env arg
    | _ -> None
  in
  let physical_equality op =
    match argument_type with
    | Some (Vox_type.Int | Vox_type.Bool) -> equality op
    | Some Vox_type.Bigint | None -> None
  in
  let structural_equality op =
    match argument_type with
    | Some (Vox_type.Int | Vox_type.Bool | Vox_type.Bigint) -> equality op
    | None -> None
  in
  let comparison int_op bigint_op =
    match argument_type with
    | Some Vox_type.Int -> binary Int63 int_op
    | Some Vox_type.Bigint -> binary Int bigint_op
    | Some Vox_type.Bool | None -> None
  in
  let bigint_division op =
    match binary Int op, args with
    | Some result, [Some x; Some y] ->
      Some
        (App
           ( Ite,
             [ App (Eq, [y; Big_integer "0"]);
               (if op = Int_div then Big_integer "0" else x);
               result ] ))
    | _ -> None
  in
  let result =
    match name with
    | "%addint" -> binary Int63 Add
    | "%subint" -> binary Int63 Sub
    | "%mulint" -> binary Int63 Mul
    | "%divint" | "%modint" ->
      begin match args with
      | [_; Some (Integer divisor)] when divisor <> 0L ->
        binary Int63 (if name = "%divint" then Div else Rem)
      | _ -> None
      end
    | "%negint" -> unary Int63 Neg
    | "%equal" -> structural_equality Eq
    | "%notequal" -> structural_equality Ne
    | "%eq" -> physical_equality Eq
    | "%noteq" -> physical_equality Ne
    | "%lessthan" -> comparison Lt Int_lt
    | "%lessequal" -> comparison Le Int_le
    | "%greaterthan" -> comparison Gt Int_gt
    | "%greaterequal" -> comparison Ge Int_ge
    | "%ltint" -> binary Int63 Lt
    | "%leint" -> binary Int63 Le
    | "%gtint" -> binary Int63 Gt
    | "%geint" -> binary Int63 Ge
    | "%compare" when argument_type = Some Vox_type.Bigint ->
      begin match binary Int Int_lt, equality Eq with
      | Some lt, Some eq ->
        Some
          (App
             (Ite, [lt; Integer (-1L); App (Ite, [eq; Integer 0L; Integer 1L])]))
      | _ -> None
      end
    | "caml_bigint_of_int" ->
      begin match args with
      | [Some (Integer n)] -> Some (Big_integer (Int64.to_string n))
      | _ -> unary Int63 Int_of_int63
      end
    | "caml_bigint_neg" -> unary Int Int_neg
    | "caml_bigint_add" -> binary Int Int_add
    | "caml_bigint_sub" -> binary Int Int_sub
    | "caml_bigint_mul" -> binary Int Int_mul
    | "caml_bigint_div" -> bigint_division Int_div
    | "caml_bigint_modulo" -> bigint_division Int_mod
    | "%boolnot" -> unary Bool Not
    | "%sequand" -> binary Bool And
    | "%sequor" -> binary Bool Or
    | "%identity" -> ( match args with [v] -> v | _ -> None)
    | _ -> None
  in
  match result with
  | Some value when Some (term_sort value) = sort env result_type -> result
  | _ -> None

let rec signature env ty arity =
  if arity = 0
  then Option.map (fun result -> [], result) (sort env ty)
  else
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow ((Nolabel, _, _, _), arg, ret, _) ->
      begin match sort env arg, signature env ret (arity - 1) with
      | Some arg, Some (args, result) -> Some (arg :: args, result)
      | _ -> None
      end
    | _ -> None

let constant = function
  | Typedtree.Const_int n -> Some (Integer (Int64.of_int n))
  | _ -> None

let rconstant c =
  match c.Parsetree.pconst_desc with
  | Parsetree.Pconst_integer (n, None) -> Some (Integer (Int64.of_string n))
  | _ -> None

let constructor env ty name =
  match sort env ty, name with
  | Some Bool, "true" -> Some (Boolean true)
  | Some Bool, "false" -> Some (Boolean false)
  | _ -> None

let rconstructor env ty = function
  | Path.Pextra_ty (_, Path.Pcstr_ty name) -> constructor env ty name
  | _ -> None
