open Types
open Vox_smt

type type_key =
  | Variable of int
  | Constructor of Path.t * type_key list
  | Tuple of (string option * type_key) list
  | Box of type_key

module Type_keys = Hashtbl.Make (struct
  type t = type_key

  let equal = ( = )

  let hash = Hashtbl.hash
end)

type context =
  { opaque_ids : int Type_keys.t;
    mutable iarray_sort : sort option;
    set_sorts : (sort, unit) Hashtbl.t;
    unsupported_logical_equality : (sort, unit) Hashtbl.t;
    unsupported : unit Type_keys.t;
    mutable next_opaque_id : int;
    data_by_key : data Type_keys.t;
    data_by_datatype : (datatype, data) Hashtbl.t;
    mutable building_transaction : (type_key * datatype) list ref option
  }

and data_kind =
  | Tuple_data of Constructor.t
  | Record_data of Constructor.t
  | Variant_data of (string * Constructor.t) list

and data =
  { declaration : datatype_declaration;
    kind : data_kind
  }

let create_context () =
  { opaque_ids = Type_keys.create 32;
    iarray_sort = None;
    set_sorts = Hashtbl.create 8;
    unsupported_logical_equality = Hashtbl.create 8;
    unsupported = Type_keys.create 32;
    next_opaque_id = 0;
    data_by_key = Type_keys.create 32;
    data_by_datatype = Hashtbl.create 32;
    building_transaction = None
  }

let opaque_id ctx key =
  match Type_keys.find_opt ctx.opaque_ids key with
  | Some id -> id
  | None ->
    let id = ctx.next_opaque_id in
    ctx.next_opaque_id <- id + 1;
    Type_keys.add ctx.opaque_ids key id;
    id

let fresh_opaque_sort ctx =
  let id = ctx.next_opaque_id in
  ctx.next_opaque_id <- id + 1;
  Opaque id

let iarray_sort ctx =
  match ctx.iarray_sort with
  | Some sort -> sort
  | None ->
    let sort = fresh_opaque_sort ctx in
    ctx.iarray_sort <- Some sort;
    Hashtbl.replace ctx.unsupported_logical_equality sort ();
    sort

let iarray_element env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, [element], _)
    when Path.same (Env.normalize_type_path None env path) Predef.path_iarray ->
    Some element
  | _ -> None

let standard_make_total =
  Path.Pdot (Path.Pident (Ident.create_persistent "Stdlib__Set"), "MakeTotal")

module Shape_reducer = Shape_reduce.Make (struct
  let fuel () = Misc.Maybe_bounded.of_int 100

  let fuel_for_compilation_units () = Misc.Maybe_bounded.of_int 20

  let max_shape_reduce_steps_per_variable () = Misc.Maybe_bounded.of_int 100

  let max_compilation_unit_depth () = Misc.Maybe_bounded.of_int 10

  let projection_rules_for_merlin_enabled = false

  let cache = Misc.Stdlib.String.Tbl.create 16

  let read_unit_shape ~diagnostics ~unit_name =
    match Misc.Stdlib.String.Tbl.find_opt cache unit_name with
    | Some shape -> shape
    | None ->
      let standard =
        String.equal unit_name "Stdlib"
        || String.equal unit_name "Stdlib__Set"
        || String.equal unit_name "Stdlib__MoreLabels"
      in
      let filename = String.uncapitalize_ascii unit_name in
      let find_artifact extension =
        if not standard then raise Not_found;
        let path =
          Filename.concat Config.standard_library (filename ^ extension)
        in
        if Sys.file_exists path then path else raise Not_found
      in
      let read_cms () =
        match find_artifact ".cms" with
        | path -> Some (Cms_format.read path).cms_impl_shape
        | exception (Not_found | Cms_format.Error _) -> None
      in
      let read_cmt () =
        match find_artifact ".cmt" with
        | path ->
          begin match Cmt_format.read path with
          | _, Some info -> Some info.cmt_impl_shape
          | _, None -> Some None
          | exception Cmt_format.Error _ -> None
          end
        | exception Not_found -> None
      in
      let shape =
        match read_cms () with
        | Some shape -> shape
        | None ->
          begin match read_cmt () with
          | Some shape -> shape
          | None ->
            Shape_reduce.Diagnostics.add_cms_file_missing diagnostics
              (filename ^ ".cms");
            None
          end
      in
      Misc.Stdlib.String.Tbl.add cache unit_name shape;
      shape
end)

let rec resolved_uid = function
  | Shape_reduce.Resolved uid -> Some uid
  | Shape_reduce.Resolved_alias (_, result) -> resolved_uid result
  | Shape_reduce.Unresolved _ | Shape_reduce.Approximated _
  | Shape_reduce.Internal_error_missing_uid ->
    None

let shape_uid env namespace path =
  Option.bind (Env.shape_of_path_opt ~namespace env path) (fun shape ->
      resolved_uid (Shape_reducer.reduce_for_uid env shape))

let make_total_result_uid env modules name namespace =
  Option.bind
    (Env.shape_of_path_opt ~namespace:Shape.Sig_component_kind.Module env
       standard_make_total) (fun shape ->
      let shape = Shape.app shape ~arg:Shape.dummy_mod in
      let shape =
        List.fold_left
          (fun shape name ->
            Shape.proj shape
              (Shape.Item.make name Shape.Sig_component_kind.Module))
          shape modules
      in
      let shape = Shape.proj shape (Shape.Item.make name namespace) in
      resolved_uid (Shape_reducer.reduce_for_uid env shape))

let set_operations =
  [ [], "empty", ("%set_empty", 0);
    [], "mem", ("%set_mem", 2);
    [], "find", ("%set_find", 2);
    ["Refined"], "singleton", ("%set_singleton", 1);
    ["Refined"], "add", ("%set_add", 2);
    ["Refined"], "remove", ("%set_remove", 2);
    ["Refined"], "union", ("%set_union", 2);
    ["Refined"], "inter", ("%set_inter", 2);
    ["Refined"], "diff", ("%set_diff", 2);
    ["Refined"], "find", ("%set_refined_find", 2) ]

let set_value env path =
  let actual = shape_uid env Shape.Sig_component_kind.Value path in
  List.find_map
    (fun (modules, name, operation) ->
      match
        ( actual,
          make_total_result_uid env modules name Shape.Sig_component_kind.Value
        )
      with
      | Some actual, Some standard when Shape.Uid.equal actual standard ->
        Some operation
      | _ -> None)
    set_operations

let is_set_type env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, [], _) ->
    begin match
      ( shape_uid env Shape.Sig_component_kind.Type path,
        make_total_result_uid env [] "t" Shape.Sig_component_kind.Type )
    with
    | Some actual, Some standard -> Shape.Uid.equal actual standard
    | _ -> false
    end
  | _ -> false

let type_key env ty =
  let rec loop visited ty =
    let ty = Ctype.expand_head env ty in
    let id = get_id ty in
    if Numbers.Int.Set.mem id visited
    then None
    else
      let visited = Numbers.Int.Set.add id visited in
      match get_desc ty with
      | (Tvar _ | Tunivar _) when get_level ty = Btype.generic_level ->
        Some (Variable id)
      | Tvar _ | Tunivar _ -> None
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

let record_data ctx key data =
  Type_keys.add ctx.data_by_key key data;
  Hashtbl.add ctx.data_by_datatype data.declaration.datatype data;
  Option.iter
    (fun created -> created := (key, data.declaration.datatype) :: !created)
    ctx.building_transaction

let rollback ctx created =
  List.iter
    (fun (key, datatype) ->
      Type_keys.remove ctx.data_by_key key;
      Hashtbl.remove ctx.data_by_datatype datatype)
    created

let source_type env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, arguments, _) ->
    let path = Env.normalize_type_path None env path in
    begin match Env.find_type path env with
    | declaration -> Some (arguments, declaration)
    | exception Not_found -> None
    end
  | _ -> None

let applied env declaration arguments ty =
  try Some (Ctype.apply env declaration.type_params ty arguments)
  with Ctype.Cannot_apply -> None

let nonregular_recursive_instance key stack =
  match key with
  | Constructor (path, _) when Path.same path Predef.path_list -> false
  | Constructor (path, _) ->
    List.exists
      (fun (stack_key, _) ->
        match stack_key with
        | Constructor (stack_path, _) -> Path.same path stack_path
        | Variable _ | Tuple _ | Box _ -> false)
      stack
  | Variable _ | Tuple _ | Box _ -> false

exception Unsupported_recursive_datatype

let reject_recursive ctx key =
  Type_keys.replace ctx.unsupported key ();
  raise Unsupported_recursive_datatype

let recursive_backedge key stack =
  let rec loop crossed_nominal = function
    | [] -> None
    | (stack_key, (datatype, allowed)) :: stack ->
      if stack_key = key
      then
        Some
          (if crossed_nominal
           then `Mutual
           else if allowed
           then `Allowed datatype
           else `Unsupported)
      else
        let crossed_nominal =
          crossed_nominal
          ||
          match stack_key with
          | Constructor _ -> true
          | Variable _ | Tuple _ | Box _ -> false
        in
        loop crossed_nominal stack
  in
  loop false stack

let rec logical_payload env ty =
  let ty = Ctype.expand_head env ty in
  match get_desc ty with
  | Tmod (ty, _) | Tpoly (ty, []) | Trefine { ref_payload = ty; _ } ->
    logical_payload env ty
  | _ -> ty

let rec classify ctx env stack ty =
  let ty = logical_payload env ty in
  let result =
    match Vox_type.classify_payload env ty with
    | Some Vox_type.Int -> Some Int63
    | Some Vox_type.Bool -> Some Bool
    | Some Vox_type.Bigint -> Some Int
    | None when Option.is_some (iarray_element env ty) -> Some (iarray_sort ctx)
    | None ->
      begin match type_key env ty with
      | None -> None
      | Some key ->
        begin match Type_keys.find_opt ctx.data_by_key key with
        | Some data -> Some (Datatype data.declaration.datatype)
        | None when Type_keys.mem ctx.unsupported key ->
          raise Unsupported_recursive_datatype
        | None ->
          begin match Type_keys.find_opt ctx.opaque_ids key with
          | Some id -> Some (Opaque id)
          | None ->
            begin match recursive_backedge key stack with
            | Some (`Allowed datatype) -> Some (Datatype datatype)
            | Some (`Unsupported | `Mutual) -> reject_recursive ctx key
            | None ->
              if nonregular_recursive_instance key stack
              then reject_recursive ctx key
              else
                begin match build_data ctx env stack key ty with
                | Some data -> Some (Datatype data.declaration.datatype)
                | None -> Some (Opaque (opaque_id ctx key))
                end
            end
          end
        end
      end
  in
  if is_set_type env ty
  then
    Option.iter
      (fun sort ->
        Hashtbl.replace ctx.set_sorts sort ();
        Hashtbl.replace ctx.unsupported_logical_equality sort ())
      result;
  result

and build_data ctx env stack key ty =
  let label =
    match key with
    | Constructor (path, _) -> Path.name path
    | Tuple _ -> "tuple"
    | Variable _ | Box _ -> "datatype"
  in
  let datatype = Datatype.create ~label in
  let finish kind constructors =
    let data = { declaration = { datatype; constructors }; kind } in
    record_data ctx key data;
    Some data
  in
  let field_sort stack ty = classify ctx env stack ty in
  match get_desc (Ctype.expand_head env ty) with
  | Ttuple components ->
    let stack = (key, (datatype, false)) :: stack in
    begin match
      Misc.Stdlib.List.map_option
        (fun (label, ty) ->
          Option.map
            (fun sort -> Option.value ~default:"field" label, sort)
            (field_sort stack ty))
        components
    with
    | None -> None
    | Some fields ->
      let constructor = Constructor.create ~datatype ~label:"tuple" fields in
      finish (Tuple_data constructor) [constructor]
    end
  | Tconstr _ ->
    begin match source_type env ty with
    | None -> None
    | Some (arguments, declaration) ->
      begin match declaration.type_kind with
      | Type_record (labels, Record_boxed, _)
        when List.for_all (fun label -> label.ld_mutable = Immutable) labels ->
        let stack = (key, (datatype, false)) :: stack in
        begin match
          Misc.Stdlib.List.map_option
            (fun label ->
              Option.bind (applied env declaration arguments label.ld_type)
                (fun ty ->
                  Option.map
                    (fun sort -> Ident.name label.ld_id, sort)
                    (field_sort stack ty)))
            labels
        with
        | None -> None
        | Some fields ->
          let constructor =
            Constructor.create ~datatype ~label:"record" fields
          in
          finish (Record_data constructor) [constructor]
        end
      | Type_variant (constructors, Variant_boxed _, _)
        when constructors <> []
             && List.for_all
                  (fun constructor ->
                    constructor.cd_res = None
                    &&
                    match constructor.cd_args with
                    | Cstr_tuple _ -> true
                    | Cstr_record _ -> false)
                  constructors ->
        let stack = (key, (datatype, declaration.type_inductive)) :: stack in
        let constructor constructor =
          match constructor.cd_args with
          | Cstr_record _ -> None
          | Cstr_tuple arguments' ->
            Option.map
              (fun fields ->
                let name = Ident.name constructor.cd_id in
                name, Constructor.create ~datatype ~label:name fields)
              (Misc.Stdlib.List.map_option
                 (fun argument ->
                   Option.bind
                     (applied env declaration arguments argument.ca_type)
                     (fun ty ->
                       Option.map
                         (fun sort -> "field", sort)
                         (field_sort stack ty)))
                 arguments')
        in
        begin match Misc.Stdlib.List.map_option constructor constructors with
        | None -> None
        | Some constructors ->
          finish (Variant_data constructors) (List.map snd constructors)
        end
      | Type_abstract _ | Type_record _ | Type_record_unboxed_product _
      | Type_variant _ | Type_open ->
        None
      end
    end
  | Tvar _ | Tunivar _ | Tarrow _ | Tobject _ | Tfield _ | Tnil | Tlink _
  | Tsubst _ | Tvariant _ | Tpoly _ | Tpackage _ | Tmod _ | Tunboxed_tuple _
  | Trepr _ | Tof_kind _ | Tbox _ | Tquote _ | Tsplice _ | Tquote_eval _
  | Trefine _ ->
    None

let declarations ctx data =
  let seen = Hashtbl.create 16 in
  let rec visit acc declaration =
    let datatype = declaration.datatype in
    if Hashtbl.mem seen datatype
    then acc
    else begin
      Hashtbl.add seen datatype ();
      let acc =
        List.fold_left
          (fun acc constructor ->
            List.fold_left
              (fun acc (_, sort) ->
                match sort with
                | Datatype datatype ->
                  begin match
                    Hashtbl.find_opt ctx.data_by_datatype datatype
                  with
                  | Some data -> visit acc data.declaration
                  | None -> acc
                  end
                | Bool | Int63 | Int | Opaque _ -> acc)
              acc
              (Constructor.fields constructor))
          acc declaration.constructors
      in
      declaration :: acc
    end
  in
  List.rev (visit [] data.declaration)

let classify_top ctx env ty =
  match ctx.building_transaction with
  | Some _ -> classify ctx env [] ty
  | None -> (
    let created = ref [] in
    ctx.building_transaction <- Some created;
    match classify ctx env [] ty with
    | Some (Datatype datatype) as result ->
      ctx.building_transaction <- None;
      begin match Hashtbl.find_opt ctx.data_by_datatype datatype with
      | Some data when datatypes_well_founded (declarations ctx data) -> result
      | Some _ | None ->
        rollback ctx !created;
        Option.map
          (fun key ->
            Type_keys.replace ctx.unsupported key ();
            Opaque (opaque_id ctx key))
          (type_key env ty)
      end
    | result ->
      ctx.building_transaction <- None;
      rollback ctx !created;
      result
    | exception exn ->
      ctx.building_transaction <- None;
      rollback ctx !created;
      begin match exn, type_key env ty with
      | Unsupported_recursive_datatype, Some key ->
        Type_keys.replace ctx.unsupported key ();
        Some (Opaque (opaque_id ctx key))
      | _ -> raise exn
      end)

let data ctx env ty =
  match classify_top ctx env ty, type_key env ty with
  | Some (Datatype _), Some key -> Type_keys.find_opt ctx.data_by_key key
  | _ -> None

let same_nominal_data_type env left right =
  match type_key env left, type_key env right with
  | Some (Constructor (source, _)), Some (Constructor (expected, _)) ->
    Path.same source expected
  | _ -> false

let declarations_of_sort ctx = function
  | Datatype datatype ->
    begin match Hashtbl.find_opt ctx.data_by_datatype datatype with
    | Some data -> declarations ctx data
    | None -> []
    end
  | Bool | Int63 | Int | Opaque _ -> []

let sort_has_unsupported_logical_equality ctx sort =
  let seen = Hashtbl.create 16 in
  let rec loop = function
    | Opaque _ as sort -> Hashtbl.mem ctx.unsupported_logical_equality sort
    | Datatype datatype ->
      if Hashtbl.mem seen datatype
      then false
      else begin
        Hashtbl.add seen datatype ();
        match Hashtbl.find_opt ctx.data_by_datatype datatype with
        | None -> false
        | Some data ->
          List.exists
            (fun constructor ->
              List.exists
                (fun (_, sort) -> loop sort)
                (Constructor.fields constructor))
            data.declaration.constructors
      end
    | Bool | Int63 | Int -> false
  in
  loop sort

let is_set_sort ctx sort = Hashtbl.mem ctx.set_sorts sort

let is_iarray_sort ctx sort = Some sort = ctx.iarray_sort

let iarray ctx env ty =
  match iarray_element env ty with
  | None -> None
  | Some element -> Some (iarray_sort ctx, classify_top ctx env element)

let sort ctx env ty = classify_top ctx env ty

let iarray_value_path fields =
  List.fold_left
    (fun path field -> Path.Pdot (path, field))
    (Path.Pident (Ident.create_persistent "Stdlib__Iarray"))
    fields

let same_value env description path =
  let standard = Subst.Lazy.force_value_description (Env.find_value path env) in
  Uid.equal description.val_uid standard.val_uid

let primitive env path =
  try
    match set_value env path with
    | Some _ as operation -> operation
    | None -> (
      let path = Env.normalize_value_path None env path in
      let description =
        Subst.Lazy.force_value_description (Env.find_value path env)
      in
      match description.val_kind with
      | Val_prim p -> Some (p.Primitive.prim_name, p.prim_arity)
      | _ ->
        if same_value env description (iarray_value_path ["length"])
        then Some ("%array_length", 1)
        else if same_value env description (iarray_value_path ["get"])
        then Some ("%array_safe_get", 2)
        else if
          same_value env description (iarray_value_path ["Refined"; "get"])
        then Some ("%array_safe_get", 2)
        else None)
  with Not_found -> None

let is_set_empty env path = set_value env path = Some ("%set_empty", 0)

let value_constant ctx env ty path =
  if sort ctx env ty <> Some Int
  then None
  else
    let path = Env.normalize_value_path None env path in
    if Path.same path (Vox_type.bigint_path "zero")
    then Some (Big_integer "0")
    else if Path.same path (Vox_type.bigint_path "one")
    then Some (Big_integer "1")
    else None

let operation ctx env ~function_type ~result_type name args =
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
  | Some value when Some (term_sort value) = sort ctx env result_type -> result
  | _ -> None

let rec signature ctx env ty arity =
  if arity = 0
  then Option.map (fun result -> [], result) (sort ctx env ty)
  else
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow ((Nolabel, _, _, _), arg, ret, _) ->
      begin match sort ctx env arg, signature ctx env ret (arity - 1) with
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

let constructor ctx env ty name =
  match sort ctx env ty, name with
  | Some Bool, "true" -> Some (Boolean true)
  | Some Bool, "false" -> Some (Boolean false)
  | _ -> None

let rconstructor ctx env ty = function
  | Path.Pextra_ty (_, Path.Pcstr_ty name) -> constructor ctx env ty name
  | _ -> None
