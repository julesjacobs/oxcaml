open Types

type emission_error =
  { location : Location.t;
    message : string;
  }

exception Emission_error of emission_error

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Solver_error

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
    (* Display-only: fact indices the discharged proof did not reference (from
       Lean's [unusedVariables] linter).  Empty on any non-proved verdict, and
       whenever the linter is silent -- so a fact defaults to "used". *)
    unused_facts : int list;
  }

let string_of_verdict = function
  | Proved -> "proved"
  | Not_proved -> "not-proved"
  | Disproved -> "disproved"
  | Solver_error -> "solver-error"

let error location format =
  Format.kasprintf
    (fun message -> raise (Emission_error { location; message }))
    format

let sanitize text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun character ->
      match character with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        Buffer.add_char buffer character
      | _ -> Buffer.add_char buffer '_')
    text;
  let result = Buffer.contents buffer in
  if String.equal result "" then "anonymous"
  else
    match result.[0] with
    | '0' .. '9' -> "n_" ^ result
    | _ -> result

let lean_constructor_name = function
  | "[]" -> "nil"
  | "::" -> "cons"
  | name -> sanitize name

let digest text = Digest.to_hex (Digest.string text)

type sort =
  | Sint
  | Sbigint
  | Sbool
  | Stuple of sort list
  | Sarrow of sort * sort
  | Sdata of string

type constructor =
  { constructor_name : string;
    constructor_fields : sort list;
  }

type data_definition =
  | Abstract
  | Variant of constructor list
  | Record of (string * sort) list

type data_instance =
  { data_key : string;
    data_name : string;
    data_path : Path.t;
    data_type_arguments : type_expr list;
    data_arguments : sort list;
    mutable data_definition : data_definition option;
  }

type reference =
  { reference_head : refinement_reference;
    reference_name : string;
    reference_sort : sort;
  }

type context =
  { env : Env.t;
    mutable data : data_instance list;
    mutable references : reference list;
  }

let rec sort_key = function
  | Sint -> "int"
  | Sbigint -> "bigint"
  | Sbool -> "bool"
  | Stuple sorts ->
    "tuple(" ^ String.concat "," (List.map sort_key sorts) ^ ")"
  | Sarrow (argument, result) ->
    "arrow(" ^ sort_key argument ^ "," ^ sort_key result ^ ")"
  | Sdata key -> key

let find_data context key =
  List.find_opt
    (fun data -> String.equal data.data_key key)
    context.data

let data_for_key context location key =
  match find_data context key with
  | Some data -> data
  | None -> error location "internal error: missing Lean datatype %s" key

let sort_contains context ~needle sort =
  let rec loop seen sort =
    if String.equal (sort_key needle) (sort_key sort)
    then true
    else
      match sort with
      | Sint | Sbigint | Sbool -> false
      | Stuple sorts -> List.exists (loop seen) sorts
      | Sarrow (argument, result) -> loop seen argument || loop seen result
      | Sdata key ->
        if List.mem key seen
        then false
        else
          let data = data_for_key context Location.none key in
          List.exists (loop (key :: seen)) data.data_arguments
  in
  loop [] sort

let growing_instantiation context previous current =
  let contains previous =
    List.exists (sort_contains context ~needle:previous) current
  in
  let strictly_contains previous current =
    not (String.equal (sort_key previous) (sort_key current))
    && sort_contains context ~needle:previous current
  in
  List.for_all contains previous
  && List.exists
       (fun previous -> List.exists (strictly_contains previous) current)
       previous

let rec lean_sort context location = function
  | Sint -> "BitVec 63"
  | Sbigint -> "Int"
  | Sbool -> "Bool"
  | Stuple [] -> error location "empty tuple type"
  | Stuple [_] -> error location "one-element tuple type"
  | Stuple sorts ->
    "(" ^ String.concat " × " (List.map (lean_sort context location) sorts)
    ^ ")"
  | Sarrow (argument, result) ->
    "(" ^ lean_sort context location argument ^ " → "
    ^ lean_sort context location result ^ ")"
  | Sdata key -> (data_for_key context location key).data_name

let inhabited_data_keys context fixed_abstract_variables =
  let rec sort_is_inhabited inhabited = function
    | Sint | Sbigint | Sbool -> true
    | Stuple sorts -> List.for_all (sort_is_inhabited inhabited) sorts
    | Sarrow (_, result) -> sort_is_inhabited inhabited result
    | Sdata key -> List.mem key inhabited
  in
  let data_is_inhabited inhabited data =
    match data.data_definition with
    | Some Abstract -> false
    | Some (Variant constructors) ->
      List.exists
        (fun constructor ->
          List.for_all
            (sort_is_inhabited inhabited)
            constructor.constructor_fields)
        constructors
    | Some (Record fields) ->
      List.for_all
        (fun (_, sort) -> sort_is_inhabited inhabited sort)
        fields
    | None -> false
  in
  let rec close inhabited =
    let next =
      List.fold_left
        (fun inhabited data ->
          if
            List.mem data.data_key inhabited
            || not (data_is_inhabited inhabited data)
          then inhabited
          else data.data_key :: inhabited)
        inhabited context.data
    in
    if List.length next = List.length inhabited then inhabited else close next
  in
  let trusted_abstract_constants =
    List.filter_map
      (fun reference ->
        match reference.reference_head, reference.reference_sort with
        | (Rglobal path | Rapp path), Sdata key ->
          begin match
            Subst.Lazy.force_value_description
              (Env.find_value path context.env)
          with
          | { val_kind = Val_reg _; _ } -> Some key
          | { val_kind = Val_prim _ | Val_mut _ | Val_ivar _ | Val_self _
                           | Val_anc _; _ } -> None
          | exception Not_found -> None
          end
        | (Rfun _ | Rsibling _ | Rglobal _ | Rapp _), _ ->
          None)
      context.references
  in
  let trusted_environment_constants =
    Env.fold_values
      (fun _name _path lazy_description _mode inhabited ->
        match Subst.Lazy.force_value_description lazy_description with
        | { val_kind = Val_reg _; val_type; _ } ->
          let rec data_instance type_ =
            let type_ = Ctype.expand_head_opt context.env type_ in
            match get_desc type_ with
            | Trefine refinement -> data_instance refinement.ref_skeleton
            | Tpoly (body, []) -> data_instance body
            | Tconstr (path, arguments, _) -> Some (path, arguments)
            | _ -> None
          in
          begin match data_instance val_type with
          | None -> inhabited
          | Some (path, arguments) ->
            List.fold_left
              (fun inhabited data ->
                if Path.same path data.data_path
                   && Ctype.is_equal context.env false arguments
                        data.data_type_arguments
                   && not (List.mem data.data_key inhabited)
                then data.data_key :: inhabited
                else inhabited)
              inhabited context.data
          end
        | { val_kind =
              (Val_prim _ | Val_mut _ | Val_ivar _ | Val_self _ | Val_anc _);
            _ } ->
          inhabited)
      None context.env []
  in
  (* Every iarray element type has the empty immutable array as an inhabitant.
     This supplies only inhabitance; it does not expose constructors, indexing,
     or equations for the abstract solver datatype. *)
  let inhabited_builtins =
    List.filter_map
      (fun data ->
        if Path.same data.data_path Predef.path_iarray
        then Some data.data_key
        else None)
      context.data
  in
  close
    (inhabited_builtins
     @ fixed_abstract_variables
     @ trusted_abstract_constants
     @ trusted_environment_constants)

let sort_is_inhabited inhabited_data =
  let rec loop = function
    | Sint | Sbigint | Sbool -> true
    | Stuple sorts -> List.for_all loop sorts
    (* A constant function inhabits an arrow whenever its result is inhabited.
       We intentionally do not assume classical choice for an empty result. *)
    | Sarrow (_, result) -> loop result
    | Sdata key -> List.mem key inhabited_data
  in
  loop

let instantiate context location declaration arguments type_ =
  try Ctype.apply context.env declaration.type_params type_ arguments with
  | Ctype.Cannot_apply ->
    error location "cannot instantiate datatype field type"

let same_type_arguments left right =
  List.length left = List.length right
  && List.for_all2
       (fun left right -> get_id left = get_id right)
       left right

let ensure_no_function_field location sort =
  let rec loop = function
    | Sint | Sbigint | Sbool | Sdata _ -> ()
    | Stuple sorts -> List.iter loop sorts
    | Sarrow _ ->
      error location "function-valued datatype fields are not supported"
  in
  loop sort

let rec sort_of_type context location type_ =
  match get_desc type_ with
  | Trefine refinement ->
    sort_of_type context location refinement.ref_skeleton
  | Tpoly (type_, []) -> sort_of_type context location type_
  | Tconstr (path, arguments, _) when Path.same path Predef.path_int ->
    if arguments = [] then Sint
    else error location "int type applied to arguments"
  | Tconstr (path, arguments, _) when Path.same path Predef.path_bool ->
    if arguments = [] then Sbool
    else error location "bool type applied to arguments"
  | Tconstr (path, arguments, _) when Vox_builtin.is_bigint_type path ->
    if arguments = [] then Sbigint
    else error location "Bigint.t applied to arguments"
  | Ttuple fields ->
    Stuple
      (List.map
         (fun (_, field) -> sort_of_type context location field)
         fields)
  | Tarrow (_, argument, result, _) ->
    Sarrow
      ( sort_of_type context location argument,
        sort_of_type context location result )
  | Tconstr (path, arguments, _) ->
    let declaration =
      match Env.find_type path context.env with
      | declaration -> declaration
      | exception Not_found ->
        error location "type %s is not available in the emission environment"
          (Path.name path)
    in
    begin
      match declaration.type_kind, declaration.type_manifest with
      | (Type_variant _ | Type_record _), _ ->
        register_data context location path arguments declaration
      | Type_abstract _, Some manifest ->
        let expanded =
          instantiate context location declaration arguments manifest
        in
        sort_of_type context location expanded
      | Type_abstract _, None ->
        register_abstract context location path arguments declaration
      | _ ->
        error location "type %s is not a supported Lean datatype"
          (Path.name path)
    end
  | _ ->
    error location "unsupported refinement-expression type"

and register_abstract context location path arguments declaration =
  let argument_sorts = List.map (sort_of_type context location) arguments in
  let key =
    "abstract:" ^ Path.name path ^ "<"
    ^ String.concat "," (List.map sort_key argument_sorts)
    ^ ">"
  in
  match find_data context key with
  | Some _ -> Sdata key
  | None ->
    if List.length arguments <> declaration.type_arity then
      error location "abstract datatype %s has the wrong number of arguments"
        (Path.name path);
    context.data <-
      { data_key = key;
        data_name = "VoxData_" ^ digest key;
        data_path = path;
        data_type_arguments = arguments;
        data_arguments = argument_sorts;
        data_definition = Some Abstract;
      }
      :: context.data;
    Sdata key

and register_data context location path arguments declaration =
  begin match
    List.find_opt
      (fun data ->
        Path.same data.data_path path
        && Option.is_none data.data_definition)
      context.data
  with
  | Some data
    when not (same_type_arguments data.data_type_arguments arguments) ->
    error location
      "non-regular recursive datatype %s is not supported"
      (Path.name path)
  | Some _ | None -> ()
  end;
  let argument_sorts =
    List.map (sort_of_type context location) arguments
  in
  let key =
    "data:" ^ Path.name path ^ "<"
    ^ String.concat "," (List.map sort_key argument_sorts)
    ^ ">"
  in
  match find_data context key with
  | Some _ -> Sdata key
  | None ->
    if
      List.exists
        (fun data ->
          Path.same data.data_path path
          && Option.is_none data.data_definition
          && growing_instantiation context data.data_arguments argument_sorts)
        context.data
    then
      error location
        "non-regular recursive datatype %s is not supported"
        (Path.name path);
    if List.length arguments <> declaration.type_arity then
      error location "datatype %s has the wrong number of arguments"
        (Path.name path);
    let data =
      { data_key = key;
        data_name = "VoxData_" ^ digest key;
        data_path = path;
        data_type_arguments = arguments;
        data_arguments = argument_sorts;
        data_definition = None;
      }
    in
    context.data <- data :: context.data;
    let field_sort type_ =
      let type_ =
        instantiate context location declaration arguments type_
      in
      let sort = sort_of_type context location type_ in
      ensure_no_function_field location sort;
      sort
    in
    let definition =
      match declaration.type_kind with
      | Type_variant (constructors, _, _) ->
        let constructors =
          List.map
            (fun constructor ->
              if Option.is_some constructor.cd_res then
                error location "GADT constructor %s is not supported"
                  (Ident.name constructor.cd_id);
              let fields =
                match constructor.cd_args with
                | Cstr_tuple arguments ->
                  List.map (fun argument -> field_sort argument.ca_type)
                    arguments
                | Cstr_record _ ->
                  error location
                    "inline-record constructor %s is not supported"
                    (Ident.name constructor.cd_id)
              in
              { constructor_name = Ident.name constructor.cd_id;
                constructor_fields = fields;
              })
            constructors
        in
        Variant constructors
      | Type_record (fields, _, _) ->
        let fields =
          List.map
            (fun field ->
              begin
                match field.ld_mutable with
                | Immutable -> ()
                | Mutable _ ->
                  error location "mutable record field %s is not supported"
                    (Ident.name field.ld_id)
              end;
              Ident.name field.ld_id, field_sort field.ld_type)
            fields
        in
        Record fields
      | _ -> error location "internal error while registering datatype"
    in
    data.data_definition <- Some definition;
    Sdata key

let supports_match_facts ~env type_ =
  let context = { env; data = []; references = [] } in
  match sort_of_type context Location.none type_ with
  | Sint | Sbigint | Sbool | Stuple _ -> true
  | Sdata key ->
    begin match (data_for_key context Location.none key).data_definition with
    | Some (Variant _ | Record _) -> true
    | Some Abstract | None -> false
    end
  | Sarrow _ -> false
  | exception Emission_error _ -> false

let supports_equality ~env type_ =
  let context = { env; data = []; references = [] } in
  let rec has_function_head type_ =
    let type_ = Ctype.expand_head_opt env type_ in
    match get_desc type_ with
    | Trefine refinement -> has_function_head refinement.ref_skeleton
    | Tpoly (type_, []) -> has_function_head type_
    | Tarrow _ -> true
    | _ -> false
  in
  let rec supported = function
    | Sint | Sbigint | Sbool | Sdata _ -> true
    | Stuple fields -> List.for_all supported fields
    | Sarrow _ -> false
  in
  if has_function_head type_ then false
  else
    match supported (sort_of_type context Location.none type_) with
    | supported -> supported
    | exception Emission_error _ -> false

let reference_basename = function
  | Rfun name | Rsibling name -> name
  | Rapp path | Rglobal path -> Path.last path


let constructor_mismatch_name = Vox_builtin.constructor_mismatch_name
let constructor_mismatch = Vox_builtin.constructor_mismatch

(* Source-like rendering of a refinement predicate, for user-facing display:
   the [int{ ... }] predicate shown in signatures ([-i]), type-at-cursor and
   error messages, and the [display] field of the VC dump.  The raw
   [Types.Refinement.print] form ([app[Stdlib!.>] _ 0], [constructor[...]],
   ...) is retained only for the debug [text] field.  Operator recognition
   uses [env] to resolve which primitive a reference denotes, exactly as the
   Lean backend does; if a reference cannot be resolved (e.g. [env] lacks it),
   the fallback is ordinary prefix application, never the raw AST syntax. *)

type display_associativity =
  | Left
  | Right

type display_operator =
  { op_text : string;
    op_precedence : int;
    op_associativity : display_associativity;
  }

type displayed =
  { text : string;
    precedence : int;
  }

let display_operator = function
  | `Or -> { op_text = "||"; op_precedence = 10; op_associativity = Right }
  | `And -> { op_text = "&&"; op_precedence = 20; op_associativity = Right }
  | `Equal -> { op_text = "="; op_precedence = 30; op_associativity = Left }
  | `Not_equal ->
    { op_text = "<>"; op_precedence = 30; op_associativity = Left }
  | `Less -> { op_text = "<"; op_precedence = 30; op_associativity = Left }
  | `Less_equal ->
    { op_text = "<="; op_precedence = 30; op_associativity = Left }
  | `Greater -> { op_text = ">"; op_precedence = 30; op_associativity = Left }
  | `Greater_equal ->
    { op_text = ">="; op_precedence = 30; op_associativity = Left }
  | `Add -> { op_text = "+"; op_precedence = 40; op_associativity = Left }
  | `Subtract -> { op_text = "-"; op_precedence = 40; op_associativity = Left }
  | `Multiply -> { op_text = "*"; op_precedence = 50; op_associativity = Left }
  | `Divide -> { op_text = "/"; op_precedence = 50; op_associativity = Left }
  | `Remainder ->
    { op_text = "mod"; op_precedence = 50; op_associativity = Left }
  | `Bit_and ->
    { op_text = "land"; op_precedence = 50; op_associativity = Left }
  | `Bit_or ->
    { op_text = "lor"; op_precedence = 50; op_associativity = Left }
  | `Bit_xor ->
    { op_text = "lxor"; op_precedence = 50; op_associativity = Left }
  | `Shift_left ->
    { op_text = "lsl"; op_precedence = 60; op_associativity = Right }
  | `Shift_right_logical ->
    { op_text = "lsr"; op_precedence = 60; op_associativity = Right }
  | `Shift_right_arithmetic ->
    { op_text = "asr"; op_precedence = 60; op_associativity = Right }

(* [Path.name] keeps the module qualifier ([Lib.pos]) and drops stamps; a
   cross-module reference must not collapse to its bare last component.  The
   [Stdlib] prefix is dropped because it is always in scope, so a stdlib
   value prints as written in source ([fst], not [Stdlib.fst]). *)
let display_path path =
  let name = Path.name path in
  match String.index_opt name '.' with
  | Some dot when String.sub name 0 dot = "Stdlib" ->
    String.sub name (dot + 1) (String.length name - dot - 1)
  | _ -> name

let display_reference_name = function
  | Rfun name | Rsibling name -> name
  | Rapp path | Rglobal path -> display_path path

(* Source-spelled infix operators used as a display fallback after semantic
   builtin resolution.  This includes partial integer operations and
   noncanonical definitions that must be rendered but not interpreted as
   Stdlib integer builtins.  Precedence and associativity follow OCaml's
   rules for the operator. *)
let display_infix_operator = function
  | "mod" | "/" | "land" | "lor" | "lxor" ->
    Some { op_text = ""; op_precedence = 50; op_associativity = Left }
  | "lsl" | "lsr" | "asr" ->
    Some { op_text = ""; op_precedence = 60; op_associativity = Right }
  | _ -> None

let display_infix_operator name =
  match display_infix_operator name with
  | Some operator -> Some { operator with op_text = name }
  | None -> None

let display_builtin ~env = function
  | Rfun _ | Rsibling _ -> None
  | Rapp path | Rglobal path ->
    begin
      match Subst.Lazy.force_value_description (Env.find_value path env) with
      | { val_kind = Val_prim primitive; _ } ->
        let path = Env.normalize_value_path None env path in
        Vox_builtin.of_primitive ~path primitive.prim_name
      | _ -> None
      | exception Not_found -> None
    end

(* Resolves a two-argument application head to an infix operator for display:
   first the Lean-interpreted primitives (via [env]), then the display-only
   infix operators recognized by source name.  [None] means render as an
   ordinary prefix application. *)
let binary_operator ~env reference =
  match display_builtin ~env reference with
  | Some
      ((`Add | `And | `Equal | `Greater | `Greater_equal | `Less | `Less_equal
       | `Multiply | `Not_equal | `Or | `Subtract | `Bit_and | `Bit_or
       | `Bit_xor | `Shift_left | `Shift_right_logical
       | `Shift_right_arithmetic | `Divide | `Remainder) as builtin) ->
    Some (display_operator builtin)
  | Some (`Bigint_abs | `Bigint_add | `Bigint_compare | `Bigint_ge
         | `Bigint_gt | `Bigint_is_zero | `Bigint_le | `Bigint_lt
         | `Bigint_mul | `Bigint_neg | `Bigint_of_int | `Bigint_one
         | `Bigint_sub | `Bigint_zero | `Not | `Identity | `Int_max
         | `Int_min | `Negate | `Pred | `Succ)
  | None ->
    (match reference with
     | Rfun _ | Rsibling _ -> None
     | Rapp path | Rglobal path -> display_infix_operator (Path.last path))

let display_constant constant =
  constant
  |> Untypeast.constant
  |> Ast_helper.Exp.constant
  |> Pprintast.string_of_expression

let display_function_name name =
  if String.length name = 0 then name
  else
    match name.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> name
    | _ -> "(" ^ name ^ ")"

let display_label = function
  | Nolabel -> ""
  | Labelled label -> "~" ^ label ^ ":"
  | Optional label -> "?" ^ label ^ ":"
  | Position label -> "@" ^ label ^ ":"

let render_predicate ?(names = Out_type.Refinement_names.empty) ~env
    expression =
  let parenthesize displayed = "(" ^ displayed.text ^ ")" in
  let paren_if displayed threshold =
    if displayed.precedence < threshold then parenthesize displayed
    else displayed.text
  in
  let bound_name names id =
    Option.value
      (Out_type.Refinement_names.find_opt id names)
      ~default:(Ident.name id)
  in
  let rec render names expression =
    match expression.rexp_desc with
    | Rexp_ident (Rbound id) ->
      { text = display_function_name (bound_name names id); precedence = 100 }
    | Rexp_ident (Rfree reference) ->
      { text = display_function_name (display_reference_name reference);
        precedence = 100;
      }
    | Rexp_constant constant ->
      { text = display_constant constant; precedence = 100 }
    | Rexp_construct (constructor, arguments) ->
      render_construct names constructor arguments
    | Rexp_field (record, field) ->
      { text = paren_if (render names record) 90 ^ "." ^ field.rfield_name;
        precedence = 90;
      }
    | Rexp_tuple fields ->
      let field (label, field) =
        match label with
        | None -> (render names field).text
        | Some label -> "~" ^ label ^ ":" ^ (render names field).text
      in
      { text = "(" ^ String.concat ", " (List.map field fields) ^ ")";
        precedence = 100;
      }
    | Rexp_ifthenelse (condition, ifso, ifnot) ->
      let condition = (render names condition).text in
      let ifso = paren_if (render names ifso) 6 in
      let text =
        match ifnot with
        | None -> Printf.sprintf "if %s then %s" condition ifso
        | Some ifnot ->
          Printf.sprintf "if %s then %s else %s" condition ifso
            (paren_if (render names ifnot) 6)
      in
      { text; precedence = 5 }
    | Rexp_match (scrutinee, cases) ->
      let case case =
        let arguments, case_names =
          List.fold_left
            (fun (arguments, names) -> function
              | None -> "_" :: arguments, names
              | Some binder ->
                let name, names =
                  Out_type.Refinement_names.bind binder.rb_id names
                in
                name :: arguments, names)
            ([], names) case.rcase_arguments
        in
        let arguments = List.rev arguments in
        Printf.sprintf "| %s%s -> %s"
          case.rcase_constructor.rconstr_name
          (if arguments = [] then "" else " " ^ String.concat " " arguments)
          (render case_names case.rcase_body).text
      in
      { text =
          Printf.sprintf "match %s with %s"
            (render names scrutinee).text
            (String.concat " " (List.map case cases));
        precedence = 5;
      }
    | Rexp_let (bindings, body) ->
      let rendered_bindings =
        List.map (fun binding -> render names binding.rbind_expr) bindings
      in
      let binding_names, body_names =
        List.fold_left
          (fun (binding_names, names) binding ->
            let name, names =
              Out_type.Refinement_names.bind
                binding.rbind_binder.rb_id names
            in
            name :: binding_names, names)
          ([], names) bindings
      in
      let binding_names = List.rev binding_names in
      let rendered_bindings =
        List.map2
          (fun name expression -> Printf.sprintf "%s = %s" name expression.text)
          binding_names rendered_bindings
      in
      { text =
          Printf.sprintf "let %s in %s"
            (String.concat " and " rendered_bindings)
            (render body_names body).text;
        precedence = 5;
      }
    | Rexp_function { arg_label; param; body } ->
      let name, body_names =
        Out_type.Refinement_names.bind param.rb_id names
      in
      { text =
          Printf.sprintf "fun %s%s -> %s" (display_label arg_label)
            name (render body_names body).text;
        precedence = 5;
      }
    | Rexp_apply (function_, arguments) ->
      render_apply names function_ arguments
  and render_apply names function_ arguments =
    match function_.rexp_desc, arguments with
    | Rexp_ident (Rfree (Rfun name)), [Nolabel, argument]
      when Option.is_some (constructor_mismatch name) ->
      { text =
          "is not " ^ Option.get (constructor_mismatch name) ^ " "
          ^ paren_if (render names argument) 71;
        precedence = 70;
      }
    | Rexp_ident (Rfree reference), [Nolabel, argument] ->
      begin
        match display_builtin ~env reference with
        | Some `Not ->
          { text = "not " ^ paren_if (render names argument) 71;
            precedence = 70 }
        | Some
            (`Add | `And | `Equal | `Greater | `Greater_equal | `Less
            | `Less_equal | `Multiply | `Not_equal | `Or | `Subtract
            | `Bit_and | `Bit_or | `Bit_xor | `Shift_left
            | `Shift_right_logical | `Shift_right_arithmetic | `Divide
            | `Remainder)
        | Some (`Bigint_abs | `Bigint_add | `Bigint_compare | `Bigint_ge
               | `Bigint_gt | `Bigint_is_zero | `Bigint_le | `Bigint_lt
               | `Bigint_mul | `Bigint_neg | `Bigint_of_int | `Bigint_one
               | `Bigint_sub | `Bigint_zero | `Identity | `Int_max
               | `Int_min | `Negate | `Pred | `Succ)
        | None ->
          render_prefix names (head_of_reference reference)
            [Nolabel, argument]
      end
    | Rexp_ident (Rfree reference), [Nolabel, left; Nolabel, right] ->
      begin
        match binary_operator ~env reference with
        | Some operator -> render_binary names operator left right
        | None ->
          render_prefix names (head_of_reference reference)
            [Nolabel, left; Nolabel, right]
      end
    | Rexp_ident (Rfree reference), arguments ->
      render_prefix names (head_of_reference reference) arguments
    | Rexp_ident (Rbound id), arguments ->
      render_prefix names (display_function_name (bound_name names id))
        arguments
    | _, arguments ->
      render_prefix names (paren_if (render names function_) 71) arguments
  and head_of_reference reference =
    match display_builtin ~env reference with
    | Some `Not -> "not"
    | Some
        ((`Add | `And | `Equal | `Greater | `Greater_equal | `Less
         | `Less_equal | `Multiply | `Not_equal | `Or | `Subtract
         | `Bit_and | `Bit_or | `Bit_xor | `Shift_left
         | `Shift_right_logical | `Shift_right_arithmetic | `Divide
         | `Remainder)
         as builtin) ->
      display_function_name (display_operator builtin).op_text
    | Some (`Bigint_abs | `Bigint_add | `Bigint_compare | `Bigint_ge
           | `Bigint_gt | `Bigint_is_zero | `Bigint_le | `Bigint_lt
           | `Bigint_mul | `Bigint_neg | `Bigint_of_int | `Bigint_one
           | `Bigint_sub | `Bigint_zero | `Identity | `Int_max | `Int_min
           | `Negate | `Pred | `Succ)
    | None -> display_function_name (display_reference_name reference)
  and render_binary names operator left right =
    let operand side expression =
      let displayed = render names expression in
      let needs_parentheses =
        displayed.precedence < operator.op_precedence
        || (displayed.precedence = operator.op_precedence
            && match operator.op_associativity, side with
               | Left, `Right | Right, `Left -> true
               | Left, `Left | Right, `Right -> false)
      in
      if needs_parentheses then parenthesize displayed else displayed.text
    in
    { text =
        operand `Left left ^ " " ^ operator.op_text ^ " "
        ^ operand `Right right;
      precedence = operator.op_precedence;
    }
  and render_prefix names head arguments =
    let argument (label, expression) =
      display_label label ^ paren_if (render names expression) 71
    in
    { text = String.concat " " (head :: List.map argument arguments);
      precedence = 70;
    }
  and render_construct names constructor arguments =
    let name = constructor.rconstr_name in
    match arguments with
    | [] -> { text = name; precedence = 100 }
    | [left; right] when String.equal name "::" ->
      { text =
          paren_if (render names left) 36 ^ " :: "
          ^ paren_if (render names right) 35;
        precedence = 35;
      }
    | [argument] ->
      { text =
          display_function_name name ^ " "
          ^ paren_if (render names argument) 71;
        precedence = 70;
      }
    | arguments ->
      let tuple =
        "("
        ^ String.concat ", "
            (List.map (fun argument -> (render names argument).text) arguments)
        ^ ")"
      in
      { text = display_function_name name ^ " " ^ tuple; precedence = 70 }
  in
  (render names expression).text


type builtin = [ Vox_builtin.t | `Constructor_mismatch of string ]

let builtin_name context = function
  | Rfun name -> Option.map (fun name -> `Constructor_mismatch name)
      (constructor_mismatch name)
  | Rsibling _ -> None
  | (Rapp path | Rglobal path) ->
    begin match Vox_builtin.of_path path with
    | Some builtin -> Some (builtin :> builtin)
    | None ->
      match
        Subst.Lazy.force_value_description (Env.find_value path context.env)
      with
      | { val_kind = Val_prim primitive; _ } ->
        let path = Env.normalize_value_path None context.env path in
        Option.map
          (fun builtin -> (builtin :> builtin))
          (Vox_builtin.of_primitive ~path primitive.prim_name)
      | _ -> None
      | exception Not_found -> None
    end

let same_reference left right =
  match left, right with
  (* [Rsibling]/[Rfun] compare by source name.  This is sound only because a VC
     is scoped to a single signature context, within which value names are
     unique; two distinct signatures each with a value named [base] would both
     lower to [Rsibling "base"] and be conflated if a VC ever spanned both. *)
  | Rfun left, Rfun right | Rsibling left, Rsibling right ->
    String.equal left right
  | (Rapp left | Rglobal left), (Rapp right | Rglobal right) ->
    Path.same left right
  | (Rfun _ | Rsibling _ | Rapp _ | Rglobal _), _ -> false

let quantifier_name = function
  | Rfun name | Rsibling name ->
    String.equal name "forall_" || String.equal name "exists_"
  | Rapp path | Rglobal path ->
    let name = Path.last path in
    String.equal name "forall_" || String.equal name "exists_"

let reference_description = function
  | Rfun name -> "function " ^ name
  | Rsibling name -> "sibling " ^ name
  | Rapp path -> "application " ^ Path.name path
  | Rglobal path -> "value " ^ Path.name path

let note_reference context expression reference =
  if quantifier_name reference then
    error expression.rexp_loc
      "quantifier combinator %s is not supported in refinements"
      (reference_basename reference);
  match builtin_name context reference with
  | Some _ -> ()
  | None ->
    let sort = sort_of_type context expression.rexp_loc expression.rexp_type in
    begin
      match
        List.find_opt
          (fun existing -> same_reference existing.reference_head reference)
          context.references
      with
      | None ->
        let index = List.length context.references in
        context.references <-
          { reference_head = reference;
            reference_name = "VoxRef_" ^ string_of_int index;
            reference_sort = sort;
          }
          :: context.references
      | Some existing ->
        if not (String.equal (sort_key existing.reference_sort) (sort_key sort))
        then
          error expression.rexp_loc
            "reference %s is used at inconsistent types"
            (reference_description reference)
    end

let rec iter_expression function_ expression =
  function_ expression;
  match expression.rexp_desc with
  | Rexp_ident _ | Rexp_constant _ -> ()
  | Rexp_let (bindings, body) ->
    List.iter
      (fun binding -> iter_expression function_ binding.rbind_expr)
      bindings;
    iter_expression function_ body
  | Rexp_function { body; _ } -> iter_expression function_ body
  | Rexp_apply (function_expression, arguments) ->
    iter_expression function_ function_expression;
    List.iter
      (fun (_, argument) -> iter_expression function_ argument)
      arguments
  | Rexp_tuple fields ->
    List.iter (fun (_, field) -> iter_expression function_ field) fields
  | Rexp_construct (_, arguments) ->
    List.iter (iter_expression function_) arguments
  | Rexp_field (record, _) -> iter_expression function_ record
  | Rexp_ifthenelse (condition, ifso, ifnot) ->
    iter_expression function_ condition;
    iter_expression function_ ifso;
    Option.iter (iter_expression function_) ifnot
  | Rexp_match (scrutinee, cases) ->
    iter_expression function_ scrutinee;
    List.iter
      (fun case -> iter_expression function_ case.rcase_body)
      cases

type variable =
  { variable_id : Ident.t;
    variable_name : string;
    variable_sort : sort;
  }

let find_ident id associations =
  List.find_opt (fun (other, _) -> Ident.same id other) associations

let find_variable id variables =
  List.find_opt (fun variable -> Ident.same id variable.variable_id) variables

let collect context vc =
  let expressions =
    List.map (fun fact -> fact.Vox_vc.expression) vc.Vox_vc.facts
    @ [vc.Vox_vc.goal]
  in
  List.iter
    (fun expression ->
      Types.Refinement.iter_types
        (fun type_ -> ignore (sort_of_type context expression.rexp_loc type_))
        expression;
      iter_expression
        (fun node ->
          match node.rexp_desc with
          | Rexp_ident (Rfree reference) ->
            note_reference context node reference
          | _ -> ())
        expression)
    expressions;
  let variables = ref [] in
  List.iter
    (fun expression ->
      let free = Types.Refinement.free_bound_identifiers expression in
      iter_expression
        (fun node ->
          match node.rexp_desc with
          | Rexp_ident (Rbound id) when Ident.Set.mem id free ->
            let sort = sort_of_type context node.rexp_loc node.rexp_type in
            begin
              match find_variable id !variables with
              | None ->
                let index = List.length !variables in
                variables :=
                  !variables
                  @ [{ variable_id = id;
                       variable_name = "v_" ^ string_of_int index;
                       variable_sort = sort;
                     }]
              | Some existing ->
                if
                  not
                    (String.equal
                       (sort_key existing.variable_sort)
                       (sort_key sort))
                then
                  error node.rexp_loc
                    "bound identifier %s is used at inconsistent types"
                    (Ident.name id)
            end
          | _ -> ())
        expression)
    expressions;
  !variables

type witness_variable =
  { source_name : string;
    model_name : string;
  }

let witness_variables ~env (vc : Vox_vc.t) =
  try
    let context = { env; data = []; references = [] } in
    let variables = collect context vc in
    let free = Types.Refinement.free_bound_identifiers vc.goal in
    let result = ref [] in
    let add source_name model_name =
      if
        not
          (List.exists
             (fun variable -> String.equal variable.model_name model_name)
             !result)
      then result := !result @ [{ source_name; model_name }]
    in
    iter_expression
      (fun node ->
        match node.rexp_desc with
        | Rexp_ident (Rbound id) when Ident.Set.mem id free ->
          begin match find_variable id variables with
          | Some variable -> add (Ident.name id) variable.variable_name
          | None ->
            error node.rexp_loc
              "identifier %s is neither locally bound nor in VC scope"
              (Ident.name id)
          end
        | Rexp_ident (Rfree reference_identifier) ->
          begin match builtin_name context reference_identifier with
          | Some _ -> ()
          | None ->
            let reference =
              match
                List.find_opt
                  (fun existing ->
                    same_reference existing.reference_head
                      reference_identifier)
                  context.references
              with
              | Some reference -> reference
              | None ->
                error node.rexp_loc
                  "internal error: missing Lean reference %s"
                  (reference_description reference_identifier)
            in
            add (reference_basename reference_identifier)
              reference.reference_name
          end
        | Rexp_ident (Rbound _) | Rexp_constant _ | Rexp_let _
        | Rexp_function _ | Rexp_apply _ | Rexp_tuple _ | Rexp_construct _
        | Rexp_field _ | Rexp_ifthenelse _ | Rexp_match _ -> ())
      vc.goal;
    Ok !result
  with
  | Emission_error error -> Error error
  | exception_ ->
    Error
      { location = vc.Vox_vc.location;
        message = Printexc.to_string exception_;
      }

let expect_sort location expected actual =
  if not (String.equal (sort_key expected) (sort_key actual)) then
    error location "refinement expression has an inconsistent type"

let expect_int location = function
  | Sint -> ()
  | _ -> error location "integer operation used at a non-int type"

let expect_bigint location = function
  | Sbigint -> ()
  | _ -> error location "Bigint operation used at an inconsistent type"

let expect_bool location = function
  | Sbool -> ()
  | _ -> error location "boolean operation used at a non-bool type"

let lean_int_constant integer =
  if integer < 0 then
    "(BitVec.ofInt 63 (" ^ string_of_int integer ^ "))"
  else "(BitVec.ofInt 63 " ^ string_of_int integer ^ ")"

let lean_shift_fallback = function
  | `Shift_left -> "VoxInt_shift_left_unspecified"
  | `Shift_right_logical -> "VoxInt_shift_right_logical_unspecified"
  | `Shift_right_arithmetic ->
    "VoxInt_shift_right_arithmetic_unspecified"
  | _ -> invalid_arg "lean_shift_fallback"

(* Division by zero raises rather than producing a value, so the model must
   not hand out one. *)
let lean_division_fallback = function
  | `Divide -> "VoxInt_divide_by_zero"
  | `Remainder -> "VoxInt_remainder_by_zero"
  | _ -> invalid_arg "lean_division_fallback"

let emit_builtin context location builtin arguments =
  let terms = List.map fst arguments in
  let sorts = List.map snd arguments in
  let decide proposition = "(decide " ^ proposition ^ ")" in
  let binary operation check =
    match terms, sorts with
    | [left; right], [left_sort; right_sort] ->
      check left_sort;
      check right_sort;
      "(" ^ left ^ " " ^ operation ^ " " ^ right ^ ")"
    | _ -> error location "binary builtin used with the wrong arity"
  in
  let binary_int_compare operation =
    match terms, sorts with
    | [left; right], [Sint; Sint] ->
      "(" ^ left ^ ".toInt " ^ operation ^ " " ^ right ^ ".toInt)"
    | _ -> error location "integer comparison used with the wrong arity"
  in
  match builtin with
  | `Constructor_mismatch constructor_name ->
    begin
      match terms, sorts with
      | [subject], [Sdata key] ->
        let data = data_for_key context location key in
        let constructor =
          match data.data_definition with
          | Some (Variant constructors) ->
            begin match
              List.find_opt
                (fun constructor ->
                  String.equal constructor.constructor_name constructor_name)
                constructors
            with
            | Some constructor -> constructor
            | None ->
              error location "constructor %s does not belong to type %s"
                constructor_name (Path.name data.data_path)
            end
          | Some (Record _) ->
            error location "%s is a record type" (Path.name data.data_path)
          | Some Abstract ->
            error location "%s is an abstract type" (Path.name data.data_path)
          | None ->
            error location "recursive datatype registration did not finish"
        in
        let fields =
          List.map (fun _ -> "_") constructor.constructor_fields
        in
        let pattern =
          String.concat " "
            ((data.data_name ^ "." ^ sanitize constructor_name) :: fields)
        in
        "(match " ^ subject ^ " with | " ^ pattern
        ^ " => false | _ => true)"
      | [_], [_] ->
        error location "constructor test used at a non-datatype type"
      | _ ->
        error location "constructor test used with the wrong arity"
    end
  | `Equal | `Not_equal ->
    begin
      match terms, sorts with
      | [left; right], [left_sort; right_sort] ->
        expect_sort location left_sort right_sort;
        begin
          match left_sort with
          | Sarrow _ ->
            error location "function equality is not supported"
          | _ -> ()
        end;
        let equality = decide ("(" ^ left ^ " = " ^ right ^ ")") in
        if builtin = `Equal then equality else "(!" ^ equality ^ ")"
      | _ -> error location "equality builtin used with the wrong arity"
    end
  | `Less ->
    (* Comparison is on the modelled machine integer's value, so the
       bitvector operands are read as integers first.  Both the comparison and
       the [decide] wrapper stay atomic, so a nested use cannot capture
       neighbouring arguments. *)
    decide (binary_int_compare "<")
  | `Less_equal ->
    decide (binary_int_compare "≤")
  | `Greater ->
    decide (binary_int_compare ">")
  | `Greater_equal ->
    decide (binary_int_compare "≥")
  | `Add -> binary "+" (expect_int location)
  | `Subtract -> binary "-" (expect_int location)
  | `Multiply -> binary "*" (expect_int location)
  | `Identity ->
    begin match terms, sorts with
    | [argument], [Sint] -> argument
    | _ -> error location "integer identity used with an inconsistent type"
    end
  | (`Negate | `Succ | `Pred) as operation ->
    begin match terms, sorts with
    | [argument], [Sint] ->
      begin match operation with
      | `Negate -> "(-" ^ argument ^ ")"
      | `Succ -> "(" ^ argument ^ " + " ^ lean_int_constant 1 ^ ")"
      | `Pred -> "(" ^ argument ^ " - " ^ lean_int_constant 1 ^ ")"
      end
    | _ -> error location "unary integer builtin used with the wrong arity"
    end
  | (`Bit_and | `Bit_or | `Bit_xor) as operation ->
    let operator =
      match operation with
      | `Bit_and -> "BitVec.and"
      | `Bit_or -> "BitVec.or"
      | `Bit_xor -> "BitVec.xor"
    in
    begin match terms, sorts with
    | [left; right], [Sint; Sint] ->
      "(" ^ operator ^ " " ^ left ^ " " ^ right ^ ")"
    | _ -> error location "bitwise builtin used with the wrong arity"
    end
  | (`Divide | `Remainder) as operation ->
    (* [BitVec.sdiv] truncates towards zero and [BitVec.srem] takes the sign
       of the dividend, which is what OCaml does, including the wrap at
       [min_int / (-1)].  Only the zero divisor parts company, and that
       branch is uninterpreted. *)
    begin match terms, sorts with
    | [left; right], [Sint; Sint] ->
      let operator =
        match operation with
        | `Divide -> "BitVec.sdiv"
        | `Remainder -> "BitVec.srem"
      in
      "(if decide (" ^ right ^ " ≠ " ^ lean_int_constant 0 ^ ") then "
      ^ operator ^ " " ^ left ^ " " ^ right ^ " else "
      ^ lean_division_fallback operation ^ " " ^ left ^ " " ^ right ^ ")"
    | _ -> error location "integer division used with the wrong arity"
    end
  | (`Shift_left | `Shift_right_logical
    | `Shift_right_arithmetic) as operation ->
    begin match terms, sorts with
    | [left; right], [Sint; Sint] ->
      let operator =
        match operation with
        | `Shift_left -> "BitVec.shiftLeft"
        | `Shift_right_logical -> "BitVec.ushiftRight"
        | `Shift_right_arithmetic -> "BitVec.sshiftRight"
      in
      "(if decide (" ^ right ^ ".toNat ≤ 63) then " ^ operator ^ " "
      ^ left ^ " " ^ right ^ ".toNat else " ^ lean_shift_fallback operation
      ^ " " ^ left ^ " " ^ right ^ ")"
    | _ -> error location "integer shift used with the wrong arity"
    end
  | `Int_max -> error location "max_int used as a function"
  | `Int_min -> error location "min_int used as a function"
  | `Bigint_add -> binary "+" (expect_bigint location)
  | `Bigint_sub -> binary "-" (expect_bigint location)
  | `Bigint_mul -> binary "*" (expect_bigint location)
  | `Bigint_neg ->
    begin match terms, sorts with
    | [argument], [Sbigint] -> "(-" ^ argument ^ ")"
    | _ -> error location "Bigint.neg used with an inconsistent type"
    end
  | `Bigint_abs ->
    begin match terms, sorts with
    | [argument], [Sbigint] ->
      "(if " ^ argument ^ " < 0 then -" ^ argument
      ^ " else " ^ argument ^ ")"
    | _ -> error location "Bigint.abs used with an inconsistent type"
    end
  | `Bigint_compare ->
    begin match terms, sorts with
    | [left; right], [Sbigint; Sbigint] ->
      "(if " ^ left ^ " < " ^ right ^ " then " ^ lean_int_constant (-1)
      ^ " else if " ^ left ^ " > " ^ right ^ " then "
      ^ lean_int_constant 1 ^ " else " ^ lean_int_constant 0 ^ ")"
    | _ -> error location "Bigint.compare used with an inconsistent type"
    end
  | `Bigint_lt ->
    decide (binary "<" (expect_bigint location))
  | `Bigint_le ->
    decide (binary "≤" (expect_bigint location))
  | `Bigint_gt ->
    decide (binary ">" (expect_bigint location))
  | `Bigint_ge ->
    decide (binary "≥" (expect_bigint location))
  | `Bigint_of_int ->
    begin match terms, sorts with
    | [argument], [Sint] -> argument ^ ".toInt"
    | _ -> error location "Bigint.of_int used with an inconsistent type"
    end
  | `Bigint_is_zero ->
    begin match terms, sorts with
    | [argument], [Sbigint] -> decide ("(" ^ argument ^ " = 0)")
    | _ -> error location "Bigint.is_zero used with an inconsistent type"
    end
  | `Bigint_zero | `Bigint_one ->
    error location "Bigint constant used as a function"
  | `And -> binary "&&" (expect_bool location)
  | `Or -> binary "||" (expect_bool location)
  | `Implies ->
    begin
      match terms, sorts with
      | [premise; conclusion], [premise_sort; conclusion_sort] ->
        expect_bool location premise_sort;
        expect_bool location conclusion_sort;
        "((!" ^ premise ^ ") || " ^ conclusion ^ ")"
      | _ -> error location "implies builtin used with the wrong arity"
    end
  | `Not ->
    begin
      match terms, sorts with
      | [argument], [sort] ->
        expect_bool location sort;
        "(!" ^ argument ^ ")"
      | _ -> error location "not builtin used with the wrong arity"
    end

let definition location data =
  match data.data_definition with
  | Some definition -> definition
  | None -> error location "recursive datatype registration did not finish"

let constructor location data name =
  match definition location data with
  | Abstract ->
    error location "%s is an abstract type" (Path.name data.data_path)
  | Record _ -> error location "%s is a record type" (Path.name data.data_path)
  | Variant constructors ->
    begin
      match
        List.find_opt
          (fun constructor -> String.equal constructor.constructor_name name)
          constructors
      with
      | Some constructor -> constructor
      | None ->
        error location "constructor %s does not belong to type %s" name
          (Path.name data.data_path)
    end

let record_field location data name =
  match definition location data with
  | Abstract ->
    error location "%s is an abstract type" (Path.name data.data_path)
  | Variant _ ->
    error location "%s is a variant type" (Path.name data.data_path)
  | Record fields ->
    begin
      match List.assoc_opt name fields with
      | Some sort -> sort
      | None ->
        error location "field %s does not belong to type %s" name
          (Path.name data.data_path)
    end

let reference context location reference =
  match
    List.find_opt
      (fun existing -> same_reference existing.reference_head reference)
      context.references
  with
  | Some reference -> reference
  | None ->
    error location "internal error: missing Lean reference %s"
      (reference_description reference)

let emit_expression context variables expression =
  let local_counter = ref 0 in
  let fresh_local () =
    let name = "l_" ^ string_of_int !local_counter in
    incr local_counter;
    name
  in
  let rec emit locals expression =
    let result_sort =
      sort_of_type context expression.rexp_loc expression.rexp_type
    in
    let term =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) ->
        begin
          match find_ident id locals with
          | Some (_, name) -> name
          | None ->
            begin
              match find_variable id variables with
              | Some variable -> variable.variable_name
              | None ->
                error expression.rexp_loc
                  "identifier %s is neither locally bound nor in VC scope"
                  (Ident.name id)
            end
        end
      | Rexp_ident (Rfree reference_identifier) ->
        begin
          match builtin_name context reference_identifier with
          | Some `Bigint_zero -> "0"
          | Some `Bigint_one -> "1"
          | Some `Int_max -> lean_int_constant max_int
          | Some `Int_min -> lean_int_constant min_int
          | Some _ ->
            error expression.rexp_loc
              "builtin %s must be fully applied"
              (reference_basename reference_identifier)
          | None ->
            (reference context expression.rexp_loc reference_identifier)
              .reference_name
        end
      | Rexp_constant (Const_int integer) ->
        lean_int_constant integer
      | Rexp_constant _ ->
        error expression.rexp_loc "only int constants are supported"
      | Rexp_let (bindings, body) ->
        if bindings = [] then
          error expression.rexp_loc "empty refinement let";
        let names = List.map (fun _ -> fresh_local ()) bindings in
        let rendered_bindings =
          List.map2
            (fun binding name ->
              let value, value_sort = emit locals binding.rbind_expr in
              let binder_sort =
                sort_of_type context expression.rexp_loc
                  binding.rbind_binder.rb_type
              in
              expect_sort binding.rbind_expr.rexp_loc binder_sort value_sort;
              name, binder_sort, value)
            bindings names
        in
        let body_locals =
          List.fold_left2
            (fun locals binding name ->
              (binding.rbind_binder.rb_id, name) :: locals)
            locals bindings names
        in
        let body, body_sort = emit body_locals body in
        expect_sort expression.rexp_loc result_sort body_sort;
        let bindings =
          List.map
            (fun (name, sort, value) ->
              "let " ^ name ^ " : "
              ^ lean_sort context expression.rexp_loc sort
              ^ " := " ^ value ^ "; ")
            rendered_bindings
          |> String.concat ""
        in
        "(" ^ bindings ^ body ^ ")"
      | Rexp_function { param; body; _ } ->
        begin
          match result_sort with
          | Sarrow (argument_sort, function_result_sort) ->
            let parameter_sort =
              sort_of_type context expression.rexp_loc param.rb_type
            in
            expect_sort expression.rexp_loc argument_sort parameter_sort;
            let parameter_name = fresh_local () in
            let body_term, body_sort =
              emit ((param.rb_id, parameter_name) :: locals) body
            in
            expect_sort body.rexp_loc function_result_sort body_sort;
            "(fun (" ^ parameter_name ^ " : "
            ^ lean_sort context expression.rexp_loc parameter_sort
            ^ ") => " ^ body_term ^ ")"
          | _ -> error expression.rexp_loc "lambda has a non-function type"
        end
      | Rexp_apply
          ( { rexp_desc = Rexp_ident (Rfree reference_identifier); _ },
            arguments )
        when Option.is_some (builtin_name context reference_identifier) ->
        let arguments =
          List.map (fun (_, argument) -> emit locals argument) arguments
        in
        emit_builtin context expression.rexp_loc
          (Option.get (builtin_name context reference_identifier)) arguments
      | Rexp_apply (function_expression, arguments) ->
        let function_term, function_sort = emit locals function_expression in
        let arguments =
          List.map (fun (_, argument) -> emit locals argument) arguments
        in
        let final_sort =
          List.fold_left
            (fun function_sort (_, argument_sort) ->
              match function_sort with
              | Sarrow (parameter_sort, result_sort) ->
                expect_sort expression.rexp_loc parameter_sort argument_sort;
                result_sort
              | _ ->
                error expression.rexp_loc
                  "application has too many arguments")
            function_sort arguments
        in
        expect_sort expression.rexp_loc result_sort final_sort;
        "(" ^ String.concat " " (function_term :: List.map fst arguments)
        ^ ")"
      | Rexp_tuple fields ->
        let fields = List.map (fun (_, field) -> emit locals field) fields in
        begin
          match result_sort with
          | Stuple expected ->
            if List.length expected <> List.length fields then
              error expression.rexp_loc "tuple arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected fields
          | _ -> error expression.rexp_loc "tuple has a non-tuple type"
        end;
        "(" ^ String.concat ", " (List.map fst fields) ^ ")"
      | Rexp_construct (constructor_description, arguments) ->
        begin
          match result_sort with
          | Sbool ->
            begin
              match constructor_description.rconstr_name, arguments with
              | "true", [] -> "true"
              | "false", [] -> "false"
              | _ ->
                error expression.rexp_loc "invalid bool constructor"
            end
          | Sdata key ->
            let data = data_for_key context expression.rexp_loc key in
            if
              not
                (Path.same data.data_path
                   constructor_description.rconstr_type_path)
            then
              error expression.rexp_loc
                "constructor path does not match its result type";
            let arguments = List.map (emit locals) arguments in
            let name, fields =
              match definition expression.rexp_loc data with
              | Abstract ->
                error expression.rexp_loc
                  "constructor used at an abstract datatype"
              | Variant _ ->
                let constructor =
                  constructor expression.rexp_loc data
                    constructor_description.rconstr_name
                in
                constructor.constructor_name,
                constructor.constructor_fields
              | Record fields ->
                if
                  not
                    (String.equal constructor_description.rconstr_name "mk")
                then
                  error expression.rexp_loc
                    "record construction must use the structure constructor";
                "mk", List.map snd fields
            in
            if List.length fields <> List.length arguments then
              error expression.rexp_loc "constructor arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              fields arguments;
            let head = data.data_name ^ "." ^ lean_constructor_name name in
            "(" ^ String.concat " " (head :: List.map fst arguments) ^ ")"
          | _ ->
            error expression.rexp_loc
              "constructor has a non-datatype result type"
        end
      | Rexp_field (record_expression, field) ->
        let record_term, record_sort = emit locals record_expression in
        begin
          match record_sort with
          | Sdata key ->
            let data = data_for_key context expression.rexp_loc key in
            if not (Path.same data.data_path field.rfield_type_path) then
              error expression.rexp_loc
                "field path does not match its record type";
            let field_sort =
              record_field expression.rexp_loc data field.rfield_name
            in
            expect_sort expression.rexp_loc result_sort field_sort;
            "(" ^ data.data_name ^ "." ^ sanitize field.rfield_name ^ " "
            ^ record_term ^ ")"
          | _ -> error expression.rexp_loc "field applied to a non-record type"
        end
      | Rexp_match (scrutinee, cases) ->
        let scrutinee, scrutinee_sort = emit locals scrutinee in
        begin match scrutinee_sort with
        | Sdata key ->
          let data = data_for_key context expression.rexp_loc key in
          let render_case case =
            if
              not
                (Path.same data.data_path
                   case.rcase_constructor.rconstr_type_path)
            then
              error expression.rexp_loc
                "match constructor path does not match its scrutinee type";
            let constructor =
              constructor expression.rexp_loc data
                case.rcase_constructor.rconstr_name
            in
            if
              List.length constructor.constructor_fields
              <> List.length case.rcase_arguments
            then error expression.rexp_loc "match constructor arity mismatch";
            let arguments, case_locals =
              List.fold_left2
                (fun (arguments, locals) field -> function
                  | None -> "_" :: arguments, locals
                  | Some binder ->
                    let name = fresh_local () in
                    let binder_sort =
                      sort_of_type context expression.rexp_loc binder.rb_type
                    in
                    expect_sort expression.rexp_loc field binder_sort;
                    name :: arguments, (binder.rb_id, name) :: locals)
                ([], locals) constructor.constructor_fields
                case.rcase_arguments
            in
            let body, body_sort = emit case_locals case.rcase_body in
            expect_sort expression.rexp_loc result_sort body_sort;
            let head =
              data.data_name ^ "."
              ^ lean_constructor_name constructor.constructor_name
            in
            "| "
            ^ String.concat " " (head :: List.rev arguments)
            ^ " => " ^ body
          in
          if cases = [] then error expression.rexp_loc "empty match";
          "(match " ^ scrutinee ^ " with "
          ^ String.concat " " (List.map render_case cases)
          ^ ")"
        | _ -> error expression.rexp_loc "match scrutinee is not a datatype"
        end
      | Rexp_ifthenelse (condition, ifso, Some ifnot) ->
        let condition, condition_sort = emit locals condition in
        expect_bool expression.rexp_loc condition_sort;
        let ifso, ifso_sort = emit locals ifso in
        let ifnot, ifnot_sort = emit locals ifnot in
        expect_sort expression.rexp_loc result_sort ifso_sort;
        expect_sort expression.rexp_loc result_sort ifnot_sort;
        "(if " ^ condition ^ " then " ^ ifso ^ " else " ^ ifnot ^ ")"
      | Rexp_ifthenelse (_, _, None) ->
        error expression.rexp_loc "else-less if is not supported"
    in
    term, result_sort
  in
  emit [] expression

let equality_function_name data = "vox_decEq_" ^ data.data_name

(* Lean's stock mutual [DecidableEq] derivation cannot follow a recursive
   occurrence through a product.  Compare tuple leaves directly and make the
   datatype comparisons mutually recursive instead; [simp_all] only proves
   that these executable comparisons agree with constructor equality. *)
let equality_decision context location sort left right =
  match sort with
  | Sint | Sbigint | Sbool -> "decEq " ^ left ^ " " ^ right
  | Sdata key ->
    let data = data_for_key context location key in
    begin match definition location data with
    | Abstract -> "decEq " ^ left ^ " " ^ right
    | Variant _ | Record _ ->
      equality_function_name data ^ " " ^ left ^ " " ^ right
    end
  | Stuple _ -> error location "internal error: unexpanded equality tuple"
  | Sarrow _ -> error location "internal error: equality on function field"

let equality_patterns context location next sort =
  let rec loop sort =
    match sort with
    | Stuple sorts ->
      let fields = List.map loop sorts in
      ( "(" ^ String.concat ", " (List.map (fun (left, _, _) -> left) fields)
        ^ ")",
        "("
        ^ String.concat ", " (List.map (fun (_, right, _) -> right) fields)
        ^ ")",
        List.concat_map (fun (_, _, comparisons) -> comparisons) fields )
    | sort ->
      let index = next () in
      let left = "left_" ^ string_of_int index in
      let right = "right_" ^ string_of_int index in
      left, right, [equality_decision context location sort left right]
  in
  loop sort

let emit_equality_result buffer comparisons =
  match comparisons with
  | [] -> Buffer.add_string buffer "    isTrue rfl\n"
  | _ ->
    Buffer.add_string buffer
      ("    match " ^ String.concat ", " comparisons ^ " with\n");
    Buffer.add_string buffer
      ("    | "
      ^ String.concat ", "
          (List.mapi
             (fun index _ -> "isTrue equal_" ^ string_of_int index)
             comparisons)
      ^ " => isTrue (by simp_all)\n");
    List.iteri
      (fun false_index _ ->
        Buffer.add_string buffer "    | ";
        List.iteri
          (fun index _ ->
            if index > 0 then Buffer.add_string buffer ", ";
            if index = false_index then
              Buffer.add_string buffer
                ("isFalse not_equal_" ^ string_of_int index)
            else Buffer.add_char buffer '_')
          comparisons;
        Buffer.add_string buffer " => isFalse (by simp_all)\n")
      comparisons

let constructor_pattern name fields =
  "." ^ lean_constructor_name name
  ^
  (match fields with
  | [] -> ""
  | _ -> " " ^ String.concat " " fields)

let emit_variant_equality context buffer constructors =
  match constructors with
  | [] -> Buffer.add_string buffer "  | left, _ => nomatch left\n"
  | constructors ->
    List.iter
      (fun left_constructor ->
        List.iter
          (fun right_constructor ->
            let next =
              let index = ref 0 in
              fun () ->
                let result = !index in
                incr index;
                result
            in
            let same_constructor =
              String.equal left_constructor.constructor_name
                right_constructor.constructor_name
            in
            let patterns =
              if same_constructor
              then
                List.map
                  (equality_patterns context Location.none next)
                  left_constructor.constructor_fields
              else []
            in
            let left_fields, right_fields, comparisons =
              if not same_constructor then
                ( List.map (fun _ -> "_") left_constructor.constructor_fields,
                  List.map (fun _ -> "_")
                    right_constructor.constructor_fields,
                  [] )
              else
                ( List.map (fun (left, _, _) -> left) patterns,
                  List.map (fun (_, right, _) -> right) patterns,
                  List.concat_map
                    (fun (_, _, comparisons) -> comparisons)
                    patterns )
            in
            Buffer.add_string buffer
              ("  | "
              ^ constructor_pattern left_constructor.constructor_name
                  left_fields
              ^ ", "
              ^ constructor_pattern right_constructor.constructor_name
                  right_fields
              ^ " =>\n");
            if same_constructor then emit_equality_result buffer comparisons
            else Buffer.add_string buffer "    isFalse (by simp)\n")
          constructors)
      constructors

let emit_record_equality context buffer fields =
  let next =
    let index = ref 0 in
    fun () ->
      let result = !index in
      incr index;
      result
  in
  let patterns =
    List.map
      (fun (_, sort) -> equality_patterns context Location.none next sort)
      fields
  in
  Buffer.add_string buffer
    ("  | .mk "
    ^ String.concat " " (List.map (fun (left, _, _) -> left) patterns)
    ^ ", .mk "
    ^ String.concat " " (List.map (fun (_, right, _) -> right) patterns)
    ^ " =>\n");
  emit_equality_result buffer
    (List.concat_map (fun (_, _, comparisons) -> comparisons) patterns)

let emit_decidable_equality context buffer data =
  match definition Location.none data with
  | Abstract -> ()
  | (Variant _ | Record _) as definition ->
    Buffer.add_string buffer
      ("noncomputable def " ^ equality_function_name data
      ^ " : (left right : "
      ^ data.data_name ^ ") -> Decidable (left = right)\n");
    begin match definition with
    | Variant constructors ->
      emit_variant_equality context buffer constructors
    | Record fields -> emit_record_equality context buffer fields
    | Abstract -> assert false
    end;
    Buffer.add_char buffer '\n'

let emit_equality_instance buffer data =
  match definition Location.none data with
  | Abstract ->
    Buffer.add_string buffer
      ("axiom " ^ equality_function_name data ^ " : DecidableEq "
      ^ data.data_name ^ "\nattribute [instance] "
      ^ equality_function_name data ^ "\n")
  | Variant _ | Record _ ->
    Buffer.add_string buffer
      ("noncomputable instance : DecidableEq " ^ data.data_name ^ " := "
      ^ equality_function_name data ^ "\n")

let emit_data context buffer data =
  match definition Location.none data with
  | Abstract ->
    Buffer.add_string buffer ("axiom " ^ data.data_name ^ " : Type\n\n")
  | Variant constructors ->
    Buffer.add_string buffer ("inductive " ^ data.data_name ^ " where\n");
    List.iter
      (fun constructor ->
        Buffer.add_string buffer
          ("  | " ^ lean_constructor_name constructor.constructor_name);
        List.iteri
          (fun index sort ->
            Buffer.add_string buffer
              (" (field_" ^ string_of_int index ^ " : "
              ^ lean_sort context Location.none sort ^ ")"))
          constructor.constructor_fields;
        Buffer.add_char buffer '\n')
      constructors;
    Buffer.add_char buffer '\n'
  | Record fields ->
    Buffer.add_string buffer ("structure " ^ data.data_name ^ " where\n");
    List.iter
      (fun (name, sort) ->
        Buffer.add_string buffer
          ("  " ^ sanitize name ^ " : "
          ^ lean_sort context Location.none sort ^ "\n"))
      fields;
    Buffer.add_char buffer '\n'

let constructor_mismatch_subject expression =
  match expression.rexp_desc with
  | Rexp_apply
      ( { rexp_desc = Rexp_ident (Rfree (Rfun name)); _ },
        [Nolabel, subject] )
    when Option.is_some (constructor_mismatch name) -> Some subject
  | _ -> None

let emit_internal ~negated ?(linter = false) ~env (vc : Vox_vc.t) =
  let context = { env; data = []; references = [] } in
  let variables = collect context vc in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "set_option autoImplicit false\n";
  (* Enabling the linter only affects diagnostics, never the exit status, so
     the verdict is unchanged; the captured warnings feed the display-only
     unused-fact fade.  The option replaces the blank separator line (rather
     than adding a line) so the theorem keeps its line number: any Lean line
     numbers embedded in a failure detail are unperturbed, and the [linter =
     false] emission stays byte-identical to before. *)
  if linter then
    Buffer.add_string buffer "set_option linter.unusedVariables true\n"
  else Buffer.add_char buffer '\n';
  Buffer.add_string buffer
    ("opaque VoxInt_shift_left_unspecified : "
     ^ "BitVec 63 → BitVec 63 → BitVec 63\n");
  Buffer.add_string buffer
    ("opaque VoxInt_shift_right_logical_unspecified : "
     ^ "BitVec 63 → BitVec 63 → BitVec 63\n");
  Buffer.add_string buffer
    ("opaque VoxInt_shift_right_arithmetic_unspecified : "
     ^ "BitVec 63 → BitVec 63 → BitVec 63\n");
  Buffer.add_string buffer
    ("opaque VoxInt_divide_by_zero : "
     ^ "BitVec 63 → BitVec 63 → BitVec 63\n");
  Buffer.add_string buffer
    ("opaque VoxInt_remainder_by_zero : "
     ^ "BitVec 63 → BitVec 63 → BitVec 63\n\n");
  let data =
    List.sort
      (fun left right -> String.compare left.data_key right.data_key)
      context.data
  in
  let abstract_data, concrete_data =
    List.partition
      (fun data ->
        match definition Location.none data with
        | Abstract -> true
        | Variant _ | Record _ -> false)
      data
  in
  (* Monomorphizing a nested recursive field can turn it into mutual
     recursion between the owner and the instantiated field datatype. *)
  List.iter (emit_data context buffer) abstract_data;
  if List.length concrete_data > 1 then Buffer.add_string buffer "mutual\n";
  List.iter (emit_data context buffer) concrete_data;
  if List.length concrete_data > 1 then Buffer.add_string buffer "end\n\n";
  List.iter (emit_decidable_equality context buffer) abstract_data;
  List.iter (emit_equality_instance buffer) abstract_data;
  if List.length concrete_data > 1 then Buffer.add_string buffer "mutual\n";
  List.iter (emit_decidable_equality context buffer) concrete_data;
  if List.length concrete_data > 1 then Buffer.add_string buffer "end\n\n";
  List.iter (emit_equality_instance buffer) concrete_data;
  if data <> [] then Buffer.add_char buffer '\n';
  let fixed_abstract_variables =
    List.filter_map
      (fun variable ->
        match variable.variable_sort with
        | Sdata key -> Some key
        | Sint | Sbigint | Sbool | Stuple _ | Sarrow _ -> None)
      variables
  in
  let inhabited_data =
    inhabited_data_keys context fixed_abstract_variables
  in
  List.sort
    (fun left right -> String.compare left.reference_name right.reference_name)
    context.references
  |> List.iter (fun reference ->
    if not (sort_is_inhabited inhabited_data reference.reference_sort)
    then
      error vc.location
        "free reference %s has a sort that is not known to be inhabited"
        (reference_description reference.reference_head);
    Buffer.add_string buffer
      ("axiom " ^ reference.reference_name ^ " : "
      ^ lean_sort context vc.location reference.reference_sort ^ "\n"));
  if context.references <> [] then Buffer.add_char buffer '\n';
  Buffer.add_string buffer
    (if negated then "theorem vc_0_disproved " else "theorem vc_0 ");
  List.iter
    (fun variable ->
      Buffer.add_string buffer
        ("(" ^ variable.variable_name ^ " : "
        ^ lean_sort context vc.location variable.variable_sort ^ ") "))
    variables;
  List.iteri
    (fun index (fact : Vox_vc.fact) ->
      let term, sort =
        emit_expression context variables fact.Vox_vc.expression
      in
      expect_bool fact.expression.rexp_loc sort;
      Buffer.add_string buffer
        ("(h_" ^ string_of_int index ^ " : (" ^ term ^ " = true)) "))
    vc.facts;
  let goal, goal_sort = emit_expression context variables vc.goal in
  expect_bool vc.goal.rexp_loc goal_sort;
  Buffer.add_string buffer ": ";
  if negated then Buffer.add_string buffer "¬ ";
  Buffer.add_string buffer ("(" ^ goal ^ " = true) := by\n");
  let mismatch_subjects =
    List.filter_map
      (fun (fact : Vox_vc.fact) ->
        Option.map
          (fun subject -> fst (emit_expression context variables subject))
          (constructor_mismatch_subject fact.expression))
      vc.facts
    |> List.sort_uniq String.compare
  in
  begin match mismatch_subjects with
  | [] -> Buffer.add_string buffer "  grind\n"
  | subjects ->
    Buffer.add_string buffer "  first | grind | (";
    List.iter
      (fun subject -> Buffer.add_string buffer ("cases " ^ subject ^ " <;> "))
      subjects;
    Buffer.add_string buffer "grind)\n"
  end;
  Buffer.contents buffer

let emit ~env (vc : Vox_vc.t) =
  try Ok (emit_internal ~negated:false ~env vc) with
  | Emission_error error -> Error error
  | exception_ ->
    Error
      { location = vc.Vox_vc.location;
        message = Printexc.to_string exception_;
      }

let command_exists command =
  let test =
    if String.contains command '/' then
      "test -x " ^ Filename.quote command
    else "command -v " ^ Filename.quote command ^ " >/dev/null 2>&1"
  in
  Sys.command test = 0

let pinned_lean =
  "/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean"

let resolve_lean ?lean () =
  let candidates =
    match lean with
    | Some lean -> [lean]
    | None ->
      let from_environment =
        match Sys.getenv_opt "VOX_LEAN" with
        | Some lean -> [lean]
        | None -> []
      in
      (* The pinned compiler is part of the toolchain.  Prefer it to an
         unrelated PATH wrapper (which may itself require network access). *)
      from_environment @ [pinned_lean; "lean"]
  in
  List.find_opt command_exists candidates

let lean_available ?lean () = Option.is_some (resolve_lean ?lean ())

let read_output filename =
  let channel = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

type process_result =
  | Process_succeeded of string
  | Process_failed of string
  | Process_timed_out of string

let run_lean ~lean ~timeout_seconds contents =
  let input = Filename.temp_file "vox2-vc" ".lean" in
  let output = Filename.temp_file "vox2-vc" ".out" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file input;
      Misc.remove_file output)
    (fun () ->
      let channel = open_out_bin input in
      output_string channel contents;
      close_out channel;
      let command =
        Printf.sprintf "timeout %ds %s --error=hasSorry %s > %s 2>&1"
          timeout_seconds
          (Filename.quote lean)
          (Filename.quote input)
          (Filename.quote output)
      in
      let status = Sys.command command in
      let detail = read_output output in
      match status with
      | 0 -> Process_succeeded detail
      | 124 | 137 -> Process_timed_out detail
      | _ -> Process_failed detail)

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let automation_failed detail =
  contains detail "`grind` failed"
  || contains detail "tactic 'grind' failed"

(* Scan Lean's [unusedVariables] linter warnings for fact binders the proof
   did not reference.  Each fact is emitted as [(h_N : ...)] and the linter
   prints [Variable name `h_N` is not explicitly referenced.] when grind closed
   the goal without it.  We match [h_<digits>] strictly (scope variables and
   other binders are never faded) and require the exact suffix so an unrelated
   warning can never fade a used fact -- when in doubt the fact stays used. *)
let parse_unused_facts output =
  let marker = "Variable name `h_" in
  let marker_length = String.length marker in
  let suffix = "` is not explicitly referenced" in
  let suffix_length = String.length suffix in
  let length = String.length output in
  let rec loop index acc =
    if index + marker_length > length then List.rev acc
    else if String.sub output index marker_length = marker then begin
      let digits_start = index + marker_length in
      let cursor = ref digits_start in
      while
        !cursor < length && output.[!cursor] >= '0' && output.[!cursor] <= '9'
      do
        incr cursor
      done;
      if
        !cursor > digits_start
        && !cursor + suffix_length <= length
        && String.sub output !cursor suffix_length = suffix
      then
        let n = int_of_string (String.sub output digits_start (!cursor - digits_start)) in
        loop (!cursor + suffix_length) (n :: acc)
      else loop (index + 1) acc
    end
    else loop (index + 1) acc
  in
  loop 0 []

let discharge ?lean ?(timeout_seconds = 30) ~env (vc : Vox_vc.t) =
  let result verdict ?detail ?(unused_facts = []) () =
    { verdict; location = vc.Vox_vc.location; detail; unused_facts }
  in
  if timeout_seconds <= 0 then
    result Solver_error ~detail:"timeout must be positive" ()
  else
    match resolve_lean ?lean () with
    | None -> result Solver_error ~detail:"Lean executable not found" ()
    | Some _lean when not (command_exists "timeout") ->
      result Solver_error ~detail:"timeout executable not found" ()
    | Some lean ->
      begin
        try
          match emit_internal ~negated:false ~linter:true ~env vc with
          | contents ->
            begin
              match run_lean ~lean ~timeout_seconds contents with
              | Process_succeeded detail ->
                result Proved ~unused_facts:(parse_unused_facts detail) ()
              | Process_timed_out detail ->
                result Solver_error ~detail ()
              | Process_failed detail when automation_failed detail ->
                let negated = emit_internal ~negated:true ~env vc in
                begin
                  match run_lean ~lean ~timeout_seconds negated with
                  | Process_succeeded _ -> result Disproved ~detail ()
                  | Process_failed negated_detail
                    when automation_failed negated_detail ->
                    result Not_proved ~detail ()
                  | Process_failed negated_detail
                  | Process_timed_out negated_detail ->
                    result Solver_error ~detail:negated_detail ()
                end
              | Process_failed detail ->
                result Solver_error ~detail ()
            end
        with
        | Emission_error error ->
          { verdict = Solver_error;
            location = error.location;
            detail = Some error.message;
            unused_facts = [];
          }
        | exception_ ->
          result Solver_error ~detail:(Printexc.to_string exception_) ()
      end

(* Install the source-like refinement predicate renderer into the type printer.
   [Out_type] is also linked into the [dynlink] library, which cannot depend on
   this module, so it defaults to the raw AST syntax and the full compiler
   overrides it here at startup. *)
let () =
  Out_type.refinement_predicate_printer :=
    (fun ~env ~names expression -> render_predicate ~env ~names expression)
