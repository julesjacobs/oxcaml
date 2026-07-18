open Types

type emission_error =
  { location : Location.t;
    message : string;
  }

exception Emission_error of emission_error

type backend =
  [ `Z3
  | `Oxsmt
  ]

type query =
  | Prove
  | Disprove

type input_mode =
  | Stdin
  | File_argument

type solver_status =
  | Sat
  | Unsat
  | Unknown

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Solver_error
  | Unavailable

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
  }

let string_of_verdict = function
  | Proved -> "proved"
  | Not_proved -> "not-proved"
  | Disproved -> "disproved"
  | Solver_error -> "solver-error"
  | Unavailable -> "unavailable"

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

let digest text = Digest.to_hex (Digest.string text)

type sort =
  | Sint
  | Sbool
  | Stuple of sort list
  | Sarrow of sort * sort
  | Sdata of string

type constructor =
  { constructor_name : string;
    constructor_fields : sort list;
  }

type data_definition =
  | Variant of constructor list
  | Record of (string * sort) list

type data_instance =
  { data_key : string;
    data_name : string;
    data_path : Path.t;
    mutable data_definition : data_definition option;
  }

type tuple_instance =
  { tuple_key : string;
    tuple_name : string;
    tuple_fields : sort list;
  }

type reference =
  { reference_head : refinement_reference;
    reference_name : string;
    reference_sort : sort;
  }

type context =
  { env : Env.t;
    mutable data : data_instance list;
    mutable tuples : tuple_instance list;
    mutable references : reference list;
  }

let rec sort_key = function
  | Sint -> "int"
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
  | None -> error location "internal error: missing SMT datatype %s" key

let find_tuple context key =
  List.find_opt
    (fun tuple -> String.equal tuple.tuple_key key)
    context.tuples

let tuple_for_sorts context location sorts =
  let key = sort_key (Stuple sorts) in
  match find_tuple context key with
  | Some tuple -> tuple
  | None -> error location "internal error: missing SMT tuple %s" key

let rec ensure_first_order location = function
  | Sint | Sbool | Sdata _ -> ()
  | Stuple sorts -> List.iter (ensure_first_order location) sorts
  | Sarrow _ ->
    error location "higher-order values are not supported by the SMT backend"

let rec ensure_first_order_symbol location = function
  | Sarrow (argument, result) ->
    ensure_first_order location argument;
    ensure_first_order_symbol location result
  | result -> ensure_first_order location result

let register_tuple context location sorts =
  begin
    match sorts with
    | [] -> error location "empty tuple type"
    | [_] -> error location "one-element tuple type"
    | _ -> ()
  end;
  List.iter (ensure_first_order location) sorts;
  let key = sort_key (Stuple sorts) in
  if Option.is_none (find_tuple context key) then
    context.tuples <-
      { tuple_key = key;
        tuple_name = "VoxTuple_" ^ digest key;
        tuple_fields = sorts;
      }
      :: context.tuples;
  Stuple sorts

let instantiate context location declaration arguments type_ =
  try Ctype.apply context.env declaration.type_params type_ arguments with
  | Ctype.Cannot_apply ->
    error location "cannot instantiate datatype field type"

let ensure_no_nested_data location owner sort =
  let rec loop = function
    | Sint | Sbool -> ()
    | Stuple sorts -> List.iter loop sorts
    | Sarrow _ ->
      error location "function-valued datatype fields are not supported"
    | Sdata nested ->
      error location
        "recursive or mutually nested datatype %s in %s is not supported"
        nested owner
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
  | Ttuple fields ->
    let sorts =
      List.map
        (fun (_, field) -> sort_of_type context location field)
        fields
    in
    register_tuple context location sorts
  | Tarrow (_, argument, result, _) ->
    Sarrow
      ( sort_of_type context location argument,
        sort_of_type context location result )
  | Tconstr (path, arguments, _) ->
    let declaration =
      match Env.find_type path context.env with
      | declaration -> declaration
      | exception Not_found ->
        error location
          "type %s is not available in the emission environment"
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
      | _ ->
        error location "type %s is not a supported SMT datatype"
          (Path.name path)
    end
  | _ -> error location "unsupported refinement-expression type"

and register_data context location path arguments declaration =
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
    if List.length arguments <> declaration.type_arity then
      error location "datatype %s has the wrong number of arguments"
        (Path.name path);
    let data =
      { data_key = key;
        data_name = "VoxData_" ^ digest key;
        data_path = path;
        data_definition = None;
      }
    in
    context.data <- data :: context.data;
    let field_sort type_ =
      let type_ =
        instantiate context location declaration arguments type_
      in
      let sort = sort_of_type context location type_ in
      ensure_no_nested_data location key sort;
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
                  List.map
                    (fun argument -> field_sort argument.ca_type)
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

let same_arg_label left right =
  match left, right with
  | Nolabel, Nolabel -> true
  | Labelled left, Labelled right
  | Optional left, Optional right
  | Position left, Position right ->
    String.equal left right
  | (Nolabel | Labelled _ | Optional _ | Position _), _ -> false

let rec normalize expression =
  let with_desc rexp_desc = { expression with rexp_desc } in
  match expression.rexp_desc with
  | Rexp_ident _ | Rexp_constant _ -> expression
  | Rexp_let (bindings, body) ->
    let bindings =
      List.map
        (fun binding ->
          { binding with rbind_expr = normalize binding.rbind_expr })
        bindings
    in
    with_desc (Rexp_let (bindings, normalize body))
  | Rexp_function ({ body; _ } as function_) ->
    with_desc
      (Rexp_function { function_ with body = normalize body })
  | Rexp_apply (function_, arguments) ->
    let function_ = normalize function_ in
    let arguments =
      List.map
        (fun (label, argument) -> label, normalize argument)
        arguments
    in
    reduce_application expression function_ arguments
  | Rexp_tuple fields ->
    with_desc
      (Rexp_tuple
         (List.map
            (fun (label, field) -> label, normalize field)
            fields))
  | Rexp_construct (constructor, arguments) ->
    with_desc
      (Rexp_construct (constructor, List.map normalize arguments))
  | Rexp_field (record, field) ->
    with_desc (Rexp_field (normalize record, field))
  | Rexp_ifthenelse (condition, ifso, ifnot) ->
    with_desc
      (Rexp_ifthenelse
         ( normalize condition,
           normalize ifso,
           Option.map normalize ifnot ))

and reduce_application outer function_ arguments =
  match function_.rexp_desc, arguments with
  | Rexp_function { arg_label; param; body },
    (argument_label, argument) :: rest ->
    if not (same_arg_label arg_label argument_label) then
      error outer.rexp_loc "lambda application label mismatch";
    let reduced =
      Types.Refinement.subst ~id:param.rb_id ~by:argument body
      |> normalize
    in
    begin
      match rest with
      | [] ->
        { reduced with
          rexp_type = outer.rexp_type;
          rexp_loc = outer.rexp_loc;
        }
      | _ -> reduce_application outer reduced rest
    end
  | _ ->
    { outer with rexp_desc = Rexp_apply (function_, arguments) }

let reference_basename = function
  | Rfun name | Rsibling name -> name
  | Rapp path | Rglobal path -> Path.last path

let builtin_name context = function
  | Rfun _ | Rsibling _ -> None
  | Rapp path | Rglobal path ->
    begin
      match
        Subst.Lazy.force_value_description (Env.find_value path context.env)
      with
      | { val_kind = Val_prim primitive; _ } ->
        Vox_lean.primitive_builtin primitive.prim_name
      | _ -> None
      | exception Not_found -> None
    end

let same_reference left right =
  match left, right with
  | Rfun left, Rfun right | Rsibling left, Rsibling right ->
    String.equal left right
  | Rapp left, Rapp right | Rglobal left, Rglobal right ->
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
          (fun existing ->
            same_reference existing.reference_head reference)
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
        if
          not
            (String.equal
               (sort_key existing.reference_sort)
               (sort_key sort))
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

let reject_quantifiers expressions =
  List.iter
    (iter_expression (fun expression ->
       match expression.rexp_desc with
       | Rexp_ident (Rfree reference) when quantifier_name reference ->
         error expression.rexp_loc
           "quantifier combinator %s is not supported in refinements"
           (reference_basename reference)
       | _ -> ()))
    expressions

type variable =
  { variable_id : Ident.t;
    variable_name : string;
    variable_sort : sort;
  }

let find_ident id associations =
  List.find_opt (fun (other, _) -> Ident.same id other) associations

let find_variable id variables =
  List.find_opt
    (fun variable -> Ident.same id variable.variable_id)
    variables

let collect context expressions =
  List.iter
    (fun expression ->
      Types.Refinement.iter_types
        (fun type_ ->
          ignore (sort_of_type context expression.rexp_loc type_))
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
            ensure_first_order_symbol node.rexp_loc sort;
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

let expect_sort location expected actual =
  if not (String.equal (sort_key expected) (sort_key actual)) then
    error location "refinement expression has an inconsistent type"

let expect_int location = function
  | Sint -> ()
  | _ -> error location "integer operation used at a non-int type"

let expect_bool location = function
  | Sbool -> ()
  | _ -> error location "boolean operation used at a non-bool type"

let emit_builtin location builtin arguments =
  let terms = List.map fst arguments in
  let sorts = List.map snd arguments in
  let binary operation check result_sort =
    match terms, sorts with
    | [left; right], [left_sort; right_sort] ->
      check left_sort;
      check right_sort;
      "(" ^ operation ^ " " ^ left ^ " " ^ right ^ ")", result_sort
    | _ -> error location "binary builtin used with the wrong arity"
  in
  match builtin with
  | `Equal | `Not_equal ->
    begin
      match terms, sorts with
      | [left; right], [left_sort; right_sort] ->
        expect_sort location left_sort right_sort;
        ensure_first_order location left_sort;
        let equality = "(= " ^ left ^ " " ^ right ^ ")" in
        let term =
          if builtin = `Equal then equality
          else "(not " ^ equality ^ ")"
        in
        term, Sbool
      | _ -> error location "equality builtin used with the wrong arity"
    end
  | `Less -> binary "<" (expect_int location) Sbool
  | `Less_equal -> binary "<=" (expect_int location) Sbool
  | `Greater -> binary ">" (expect_int location) Sbool
  | `Greater_equal -> binary ">=" (expect_int location) Sbool
  | `Add -> binary "+" (expect_int location) Sint
  | `Subtract -> binary "-" (expect_int location) Sint
  | `Multiply -> binary "*" (expect_int location) Sint
  | `And -> binary "and" (expect_bool location) Sbool
  | `Or -> binary "or" (expect_bool location) Sbool
  | `Not ->
    begin
      match terms, sorts with
      | [argument], [sort] ->
        expect_bool location sort;
        "(not " ^ argument ^ ")", Sbool
      | _ -> error location "not builtin used with the wrong arity"
    end

let definition location data =
  match data.data_definition with
  | Some definition -> definition
  | None -> error location "recursive datatype registration did not finish"

let constructor location data name =
  match definition location data with
  | Record _ -> error location "%s is a record type" (Path.name data.data_path)
  | Variant constructors ->
    let rec loop index = function
      | [] ->
        error location "constructor %s does not belong to type %s" name
          (Path.name data.data_path)
      | constructor :: rest ->
        if String.equal constructor.constructor_name name then
          index, constructor
        else loop (index + 1) rest
    in
    loop 0 constructors

let record_field location data name =
  match definition location data with
  | Variant _ ->
    error location "%s is a variant type" (Path.name data.data_path)
  | Record fields ->
    let rec loop index = function
      | [] ->
        error location "field %s does not belong to type %s" name
          (Path.name data.data_path)
      | (field_name, sort) :: rest ->
        if String.equal field_name name then index, sort
        else loop (index + 1) rest
    in
    loop 0 fields

let reference context location reference_head =
  match
    List.find_opt
      (fun existing ->
        same_reference existing.reference_head reference_head)
      context.references
  with
  | Some reference -> reference
  | None ->
    error location "internal error: missing SMT reference %s"
      (reference_description reference_head)

let rec arrow_signature location arguments = function
  | Sarrow (argument, result) ->
    ensure_first_order location argument;
    arrow_signature location (argument :: arguments) result
  | result ->
    ensure_first_order location result;
    List.rev arguments, result

let negative_integer integer =
  let rendered = string_of_int integer in
  let magnitude = String.sub rendered 1 (String.length rendered - 1) in
  "(- " ^ magnitude ^ ")"

let variant_constructor_name data index constructor =
  data.data_name ^ "_c_" ^ string_of_int index ^ "_"
  ^ sanitize constructor.constructor_name

let variant_selector_name data constructor_index field_index =
  data.data_name ^ "_s_" ^ string_of_int constructor_index ^ "_"
  ^ string_of_int field_index

let record_constructor_name data = data.data_name ^ "_record"

let record_selector_name data index name =
  data.data_name ^ "_f_" ^ string_of_int index ^ "_" ^ sanitize name

let tuple_constructor_name tuple = tuple.tuple_name ^ "_tuple"

let tuple_selector_name tuple index =
  tuple.tuple_name ^ "_p_" ^ string_of_int index

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
          | Some (_, (name, sort)) ->
            expect_sort expression.rexp_loc result_sort sort;
            let arguments, _ =
              arrow_signature expression.rexp_loc [] sort
            in
            if arguments <> [] then
              error expression.rexp_loc
                "local function %s must be fully applied"
                (Ident.name id);
            name
          | None ->
            begin
              match find_variable id variables with
              | Some variable ->
                expect_sort expression.rexp_loc result_sort
                  variable.variable_sort;
                let arguments, _ =
                  arrow_signature expression.rexp_loc []
                    variable.variable_sort
                in
                if arguments <> [] then
                  error expression.rexp_loc
                    "VC function %s must be fully applied"
                    (Ident.name id);
                variable.variable_name
              | None ->
                error expression.rexp_loc
                  "identifier %s is neither locally bound nor in VC scope"
                  (Ident.name id)
            end
        end
      | Rexp_ident (Rfree reference_identifier) ->
        begin
          match builtin_name context reference_identifier with
          | Some _ ->
            error expression.rexp_loc "builtin %s must be fully applied"
              (reference_basename reference_identifier)
          | None ->
            let reference =
              reference context expression.rexp_loc reference_identifier
            in
            let arguments, reference_result =
              arrow_signature expression.rexp_loc [] reference.reference_sort
            in
            if arguments <> [] then
              error expression.rexp_loc
                "opaque function %s must be fully applied"
                (reference_basename reference_identifier);
            expect_sort expression.rexp_loc result_sort reference_result;
            reference.reference_name
        end
      | Rexp_constant (Const_int integer) ->
        if integer < 0 then negative_integer integer
        else string_of_int integer
      | Rexp_constant _ ->
        error expression.rexp_loc "only int constants are supported"
      | Rexp_let (bindings, body) ->
        if bindings = [] then error expression.rexp_loc "empty refinement let";
        let names = List.map (fun _ -> fresh_local ()) bindings in
        let rendered_bindings =
          List.map2
            (fun binding name ->
              let value, value_sort = emit locals binding.rbind_expr in
              let binder_sort =
                sort_of_type context binding.rbind_expr.rexp_loc
                  binding.rbind_binder.rb_type
              in
              ensure_first_order binding.rbind_expr.rexp_loc binder_sort;
              expect_sort binding.rbind_expr.rexp_loc binder_sort value_sort;
              name, binder_sort, value)
            bindings names
        in
        let body_locals =
          List.fold_left2
            (fun locals binding (name, sort, _) ->
              (binding.rbind_binder.rb_id, (name, sort)) :: locals)
            locals bindings rendered_bindings
        in
        let body, body_sort = emit body_locals body in
        expect_sort expression.rexp_loc result_sort body_sort;
        let bindings =
          List.map
            (fun (name, _, value) -> "(" ^ name ^ " " ^ value ^ ")")
            rendered_bindings
          |> String.concat " "
        in
        "(let (" ^ bindings ^ ") " ^ body ^ ")"
      | Rexp_function _ ->
        error expression.rexp_loc
          "lambda remains after beta reduction; partial or higher-order "
          "application is not supported"
      | Rexp_apply
          ( { rexp_desc = Rexp_ident (Rfree reference_identifier); _ },
            arguments ) ->
        let rendered_arguments =
          List.map (fun (_, argument) -> emit locals argument) arguments
        in
        begin
          match builtin_name context reference_identifier with
          | Some builtin ->
            let term, actual_sort =
              emit_builtin expression.rexp_loc builtin rendered_arguments
            in
            expect_sort expression.rexp_loc result_sort actual_sort;
            term
          | None ->
            let reference =
              reference context expression.rexp_loc reference_identifier
            in
            let expected_arguments, reference_result =
              arrow_signature expression.rexp_loc [] reference.reference_sort
            in
            if
              List.length expected_arguments
              <> List.length rendered_arguments
            then
              error expression.rexp_loc
                "opaque function %s is partially or over-applied"
                (reference_basename reference_identifier);
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected_arguments rendered_arguments;
            expect_sort expression.rexp_loc result_sort reference_result;
            begin
              match rendered_arguments with
              | [] -> reference.reference_name
              | _ ->
                "("
                ^ String.concat " "
                    (reference.reference_name
                     :: List.map fst rendered_arguments)
                ^ ")"
            end
        end
      | Rexp_apply
          ({ rexp_desc = Rexp_ident (Rbound id); _ }, arguments) ->
        let rendered_arguments =
          List.map (fun (_, argument) -> emit locals argument) arguments
        in
        begin
          match find_ident id locals with
          | Some _ ->
            error expression.rexp_loc
              "application of a local function is not supported"
          | None ->
            begin
              match find_variable id variables with
              | None ->
                error expression.rexp_loc
                  "function %s is not in VC scope" (Ident.name id)
              | Some variable ->
                let expected_arguments, function_result =
                  arrow_signature expression.rexp_loc []
                    variable.variable_sort
                in
                if
                  List.length expected_arguments
                  <> List.length rendered_arguments
                then
                  error expression.rexp_loc
                    "VC function %s is partially or over-applied"
                    (Ident.name id);
                List.iter2
                  (fun expected (_, actual) ->
                    expect_sort expression.rexp_loc expected actual)
                  expected_arguments rendered_arguments;
                expect_sort expression.rexp_loc result_sort function_result;
                begin
                  match rendered_arguments with
                  | [] -> variable.variable_name
                  | _ ->
                    "("
                    ^ String.concat " "
                        (variable.variable_name
                         :: List.map fst rendered_arguments)
                    ^ ")"
                end
            end
        end
      | Rexp_apply _ ->
        error expression.rexp_loc
          "higher-order application is not supported by the SMT backend"
      | Rexp_tuple fields ->
        let fields =
          List.map (fun (_, field) -> emit locals field) fields
        in
        begin
          match result_sort with
          | Stuple expected ->
            if List.length expected <> List.length fields then
              error expression.rexp_loc "tuple arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected fields;
            let tuple =
              tuple_for_sorts context expression.rexp_loc expected
            in
            "("
            ^ String.concat " "
                (tuple_constructor_name tuple :: List.map fst fields)
            ^ ")"
          | _ -> error expression.rexp_loc "tuple has a non-tuple type"
        end
      | Rexp_construct (constructor_description, arguments) ->
        begin
          match result_sort with
          | Sbool ->
            begin
              match constructor_description.rconstr_name, arguments with
              | "true", [] -> "true"
              | "false", [] -> "false"
              | _ -> error expression.rexp_loc "invalid bool constructor"
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
            let index, constructor =
              constructor expression.rexp_loc data
                constructor_description.rconstr_name
            in
            let arguments = List.map (emit locals) arguments in
            if
              List.length constructor.constructor_fields
              <> List.length arguments
            then error expression.rexp_loc "constructor arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              constructor.constructor_fields arguments;
            let head = variant_constructor_name data index constructor in
            begin
              match arguments with
              | [] -> head
              | _ ->
                "(" ^ String.concat " " (head :: List.map fst arguments)
                ^ ")"
            end
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
            let index, field_sort =
              record_field expression.rexp_loc data field.rfield_name
            in
            expect_sort expression.rexp_loc result_sort field_sort;
            let selector =
              record_selector_name data index field.rfield_name
            in
            "(" ^ selector ^ " " ^ record_term ^ ")"
          | _ ->
            error expression.rexp_loc
              "field applied to a non-record type"
        end
      | Rexp_ifthenelse (condition, ifso, Some ifnot) ->
        let condition, condition_sort = emit locals condition in
        expect_bool expression.rexp_loc condition_sort;
        let ifso, ifso_sort = emit locals ifso in
        let ifnot, ifnot_sort = emit locals ifnot in
        expect_sort expression.rexp_loc result_sort ifso_sort;
        expect_sort expression.rexp_loc result_sort ifnot_sort;
        "(ite " ^ condition ^ " " ^ ifso ^ " " ^ ifnot ^ ")"
      | Rexp_ifthenelse (_, _, None) ->
        error expression.rexp_loc "else-less if is not supported"
    in
    term, result_sort
  in
  emit [] expression

let smt_sort context location sort =
  let render = function
    | Sint -> "Int"
    | Sbool -> "Bool"
    | Stuple sorts ->
      (tuple_for_sorts context location sorts).tuple_name
    | Sdata key -> (data_for_key context location key).data_name
    | Sarrow _ ->
      error location
        "function sort escaped into first-order SMT declaration"
  in
  render sort

type datatype_shape =
  { shape_key : string;
    shape_name : string;
    shape_constructors : (string * (string * sort) list) list;
  }

let tuple_shape tuple =
  { shape_key = tuple.tuple_key;
    shape_name = tuple.tuple_name;
    shape_constructors =
      [ ( tuple_constructor_name tuple,
          List.mapi
            (fun index sort -> tuple_selector_name tuple index, sort)
            tuple.tuple_fields ) ];
  }

let data_shape location data =
  let shape_constructors =
    match definition location data with
    | Variant [] ->
      error location "empty variant datatypes are not supported"
    | Variant constructors ->
      List.mapi
        (fun constructor_index constructor ->
          ( variant_constructor_name data constructor_index constructor,
            List.mapi
              (fun field_index sort ->
                ( variant_selector_name data constructor_index field_index,
                  sort ))
              constructor.constructor_fields ))
        constructors
    | Record fields ->
      [ ( record_constructor_name data,
          List.mapi
            (fun index (name, sort) ->
              record_selector_name data index name, sort)
            fields ) ]
  in
  { shape_key = data.data_key;
    shape_name = data.data_name;
    shape_constructors;
  }

let emit_datatypes context location buffer =
  let shapes =
    List.map tuple_shape context.tuples
    @ List.map (data_shape location) context.data
    |> List.sort (fun left right ->
         String.compare left.shape_key right.shape_key)
  in
  match shapes with
  | [] -> ()
  | _ ->
    let sort_declarations =
      List.map
        (fun shape -> "(" ^ shape.shape_name ^ " 0)")
        shapes
      |> String.concat " "
    in
    let constructor_list shape =
      let constructor (name, fields) =
        let fields =
          List.map
            (fun (selector, sort) ->
              "(" ^ selector ^ " "
              ^ smt_sort context location sort ^ ")")
            fields
          |> String.concat " "
        in
        if String.equal fields "" then "(" ^ name ^ ")"
        else "(" ^ name ^ " " ^ fields ^ ")"
      in
      "(" ^ String.concat " "
        (List.map constructor shape.shape_constructors)
      ^ ")"
    in
    let constructor_lists =
      String.concat " " (List.map constructor_list shapes)
    in
    Buffer.add_string buffer
      ("(declare-datatypes (" ^ sort_declarations ^ ") ("
       ^ constructor_lists ^ "))\n")

let emit_references context location buffer =
  List.sort
    (fun left right ->
      String.compare left.reference_name right.reference_name)
    context.references
  |> List.iter (fun reference ->
    let arguments, result =
      arrow_signature location [] reference.reference_sort
    in
    let arguments =
      String.concat " " (List.map (smt_sort context location) arguments)
    in
    Buffer.add_string buffer
      ("(declare-fun " ^ reference.reference_name ^ " (" ^ arguments
       ^ ") " ^ smt_sort context location result ^ ")\n"))

let emit_variables context location buffer variables =
  List.iter
    (fun variable ->
      let arguments, result =
        arrow_signature location [] variable.variable_sort
      in
      match arguments with
      | [] ->
        Buffer.add_string buffer
          ("(declare-const " ^ variable.variable_name ^ " "
           ^ smt_sort context location result ^ ")\n")
      | _ ->
        let arguments =
          String.concat " "
            (List.map (smt_sort context location) arguments)
        in
        Buffer.add_string buffer
          ("(declare-fun " ^ variable.variable_name ^ " (" ^ arguments
           ^ ") " ^ smt_sort context location result ^ ")\n"))
    variables

let emit_internal ~query ~env (vc : Vox_vc.t) =
  let original_expressions =
    List.map (fun (fact : Vox_vc.fact) -> fact.expression) vc.facts
    @ [vc.goal]
  in
  reject_quantifiers original_expressions;
  let facts =
    List.map
      (fun (fact : Vox_vc.fact) -> normalize fact.expression)
      vc.facts
  in
  let goal = normalize vc.goal in
  let expressions = facts @ [goal] in
  let context = { env; data = []; tuples = []; references = [] } in
  let variables = collect context expressions in
  let fact_terms =
    List.map
      (fun fact ->
        let term, sort = emit_expression context variables fact in
        expect_bool fact.rexp_loc sort;
        term)
      facts
  in
  let goal_term, goal_sort = emit_expression context variables goal in
  expect_bool goal.rexp_loc goal_sort;
  let buffer = Buffer.create 1024 in
  emit_datatypes context vc.location buffer;
  emit_references context vc.location buffer;
  emit_variables context vc.location buffer variables;
  List.iter
    (fun fact ->
      Buffer.add_string buffer ("(assert (= " ^ fact ^ " true))\n"))
    fact_terms;
  let query_term =
    match query with
    | Prove -> "(not (= " ^ goal_term ^ " true))"
    | Disprove -> "(= " ^ goal_term ^ " true)"
  in
  Buffer.add_string buffer ("(assert " ^ query_term ^ ")\n");
  Buffer.add_string buffer "(check-sat)\n";
  Buffer.contents buffer

let emit ~query ~env (vc : Vox_vc.t) =
  try Ok (emit_internal ~query ~env vc) with
  | Emission_error error -> Error error
  | exception_ ->
    Error
      { location = vc.location;
        message = Printexc.to_string exception_;
      }

let parse_status output =
  let statuses =
    String.split_on_char '\n' output
    |> List.filter_map (fun line ->
         let line = String.trim line in
         match line with
         | "sat" -> Some Sat
         | "unsat" -> Some Unsat
         | "unknown" -> Some Unknown
         | _ -> None)
  in
  match statuses with
  | [status] -> Some status
  | [] | _ :: _ :: _ -> None

let read_output filename =
  let channel = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

type process_result =
  { status : int;
    output : string;
  }

let oxsmt_unsupported_input_exit_code = 3

let run_solver ~command ~input_mode ~timeout_seconds contents =
  let input = Filename.temp_file "vox2-vc" ".smt2" in
  let output = Filename.temp_file "vox2-vc" ".out" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file input;
      Misc.remove_file output)
    (fun () ->
      let channel = open_out_bin input in
      output_string channel contents;
      close_out channel;
      let input_argument =
        match input_mode with
        | Stdin -> " < " ^ Filename.quote input
        | File_argument -> " " ^ Filename.quote input
      in
      let shell_command =
        Printf.sprintf "timeout --kill-after=2s %ds %s%s > %s 2>&1"
          timeout_seconds command input_argument (Filename.quote output)
      in
      let status = Sys.command shell_command in
      { status; output = read_output output })

let line_starts_with prefix line =
  let prefix_length = String.length prefix in
  String.length line >= prefix_length
  && String.equal (String.sub line 0 prefix_length) prefix

let output_has_error output =
  String.split_on_char '\n' output
  |> List.exists (fun line ->
       let line = String.trim line in
       line_starts_with "(error" line || line_starts_with "error" line)

let detail_or fallback output =
  if String.equal (String.trim output) "" then fallback else output

let solver_result ~backend ~query process =
  if process.status = 127 then
    `Final
      ( Unavailable,
        Some (detail_or "solver command unavailable (exit 127)" process.output)
      )
  else if process.status = 124 || process.status = 137 then
    `Final
      ( Solver_error,
        Some (detail_or "solver timed out" process.output) )
  else if
    backend = `Oxsmt && process.status = oxsmt_unsupported_input_exit_code
  then `Open "unknown"
  else if process.status <> 0 then
    `Final
      ( Solver_error,
        Some
          (detail_or
             ("solver exited " ^ string_of_int process.status)
             process.output) )
  else if output_has_error process.output then
    `Final (Solver_error, Some process.output)
  else
    match parse_status process.output with
    | None -> `Final (Solver_error, Some process.output)
    | Some Unsat ->
      begin
        match query with
        | Prove -> `Final (Proved, None)
        | Disprove -> `Final (Disproved, None)
      end
    | Some Sat -> `Open "sat"
    | Some Unknown -> `Open "unknown"

let backend_name = function
  | `Z3 -> "z3"
  | `Oxsmt -> "oxsmt"

let default_input_mode = function
  | `Z3 | `Oxsmt -> Stdin

let discharge ~backend ~command ?input_mode ?(timeout_seconds = 30) ~env
    (vc : Vox_vc.t) =
  let result verdict ?detail () =
    { verdict; location = vc.location; detail }
  in
  if timeout_seconds <= 0 then
    result Solver_error ~detail:"timeout must be positive" ()
  else
    match command with
    | None ->
      result Unavailable
        ~detail:(backend_name backend ^ " solver command is not configured")
        ()
    | Some command when String.equal (String.trim command) "" ->
      result Unavailable
        ~detail:(backend_name backend ^ " solver command is empty")
        ()
    | Some command ->
      let input_mode =
        Option.value input_mode ~default:(default_input_mode backend)
      in
      let run query =
        match emit ~query ~env vc with
        | Error emission_error -> raise (Emission_error emission_error)
        | Ok contents ->
          let process =
            run_solver ~command ~input_mode ~timeout_seconds contents
          in
          solver_result ~backend ~query process
      in
      begin
        try
          match run Prove with
          | `Final (verdict, detail) -> result verdict ?detail ()
          | `Open positive_status ->
            begin
              match run Disprove with
              | `Final (verdict, detail) -> result verdict ?detail ()
              | `Open negative_status ->
                result Not_proved
                  ~detail:
                    ("prove query: " ^ positive_status
                     ^ "; disprove query: " ^ negative_status)
                  ()
            end
      with
      | Emission_error emission_error ->
        { verdict = Solver_error;
          location = emission_error.location;
          detail = Some emission_error.message;
        }
      | exception_ ->
        result Solver_error ~detail:(Printexc.to_string exception_) ()
      end
