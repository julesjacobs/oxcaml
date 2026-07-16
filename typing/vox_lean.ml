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

let rec lean_sort context location = function
  | Sint -> "Int"
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
      | _ ->
        error location "type %s is not a supported Lean datatype"
          (Path.name path)
    end
  | _ ->
    error location "unsupported refinement-expression type"

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

let reference_basename = function
  | Rfun name | Rsibling name -> name
  | Rapp path | Rglobal path -> Path.last path

let primitive_builtin = function
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

let builtin_name context = function
  | (Rfun _ | Rsibling _) -> None
  | (Rapp path | Rglobal path) ->
    begin
      match
        Subst.Lazy.force_value_description (Env.find_value path context.env)
      with
      | { val_kind = Val_prim primitive; _ } ->
        primitive_builtin primitive.prim_name
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
  let binary operation check =
    match terms, sorts with
    | [left; right], [left_sort; right_sort] ->
      check left_sort;
      check right_sort;
      "(" ^ left ^ " " ^ operation ^ " " ^ right ^ ")"
    | _ -> error location "binary builtin used with the wrong arity"
  in
  match builtin with
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
        let equality = "decide (" ^ left ^ " = " ^ right ^ ")" in
        if builtin = `Equal then equality else "(!" ^ equality ^ ")"
      | _ -> error location "equality builtin used with the wrong arity"
    end
  | `Less ->
    "decide " ^ binary "<" (expect_int location)
  | `Less_equal ->
    "decide " ^ binary "≤" (expect_int location)
  | `Greater ->
    "decide " ^ binary ">" (expect_int location)
  | `Greater_equal ->
    "decide " ^ binary "≥" (expect_int location)
  | `Add -> binary "+" (expect_int location)
  | `Subtract -> binary "-" (expect_int location)
  | `Multiply -> binary "*" (expect_int location)
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
          | Some _ ->
            error expression.rexp_loc
              "builtin %s must be fully applied"
              (reference_basename reference_identifier)
          | None ->
            (reference context expression.rexp_loc reference_identifier)
              .reference_name
        end
      | Rexp_constant (Const_int integer) ->
        if integer < 0 then "(" ^ string_of_int integer ^ ")"
        else string_of_int integer
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
        emit_builtin expression.rexp_loc
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
            let constructor =
              constructor expression.rexp_loc data
                constructor_description.rconstr_name
            in
            let arguments = List.map (emit locals) arguments in
            if
              List.length constructor.constructor_fields
              <> List.length arguments
            then
              error expression.rexp_loc "constructor arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              constructor.constructor_fields arguments;
            let head =
              data.data_name ^ "."
              ^ sanitize constructor.constructor_name
            in
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

let emit_data context buffer data =
  match definition Location.none data with
  | Variant constructors ->
    Buffer.add_string buffer ("inductive " ^ data.data_name ^ " where\n");
    List.iter
      (fun constructor ->
        Buffer.add_string buffer
          ("  | " ^ sanitize constructor.constructor_name);
        List.iteri
          (fun index sort ->
            Buffer.add_string buffer
              (" (field_" ^ string_of_int index ^ " : "
              ^ lean_sort context Location.none sort ^ ")"))
          constructor.constructor_fields;
        Buffer.add_char buffer '\n')
      constructors;
    Buffer.add_string buffer "deriving DecidableEq\n\n"
  | Record fields ->
    Buffer.add_string buffer ("structure " ^ data.data_name ^ " where\n");
    List.iter
      (fun (name, sort) ->
        Buffer.add_string buffer
          ("  " ^ sanitize name ^ " : "
          ^ lean_sort context Location.none sort ^ "\n"))
      fields;
    Buffer.add_string buffer "deriving DecidableEq\n\n"

let emit_internal ~negated ~env (vc : Vox_vc.t) =
  let context = { env; data = []; references = [] } in
  let variables = collect context vc in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "set_option autoImplicit false\n\n";
  List.sort
    (fun left right -> String.compare left.data_key right.data_key)
    context.data
  |> List.iter (emit_data context buffer);
  List.sort
    (fun left right -> String.compare left.reference_name right.reference_name)
    context.references
  |> List.iter (fun reference ->
    Buffer.add_string buffer
      ("opaque " ^ reference.reference_name ^ " : "
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
  Buffer.add_string buffer ("(" ^ goal ^ " = true) := by\n  grind\n");
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
  | Process_succeeded
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
      | 0 -> Process_succeeded
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

let discharge ?lean ?(timeout_seconds = 30) ~env (vc : Vox_vc.t) =
  let result verdict ?detail () =
    { verdict; location = vc.Vox_vc.location; detail }
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
          match emit_internal ~negated:false ~env vc with
          | contents ->
            begin
              match run_lean ~lean ~timeout_seconds contents with
              | Process_succeeded -> result Proved ()
              | Process_timed_out detail ->
                result Solver_error ~detail ()
              | Process_failed detail when automation_failed detail ->
                let negated = emit_internal ~negated:true ~env vc in
                begin
                  match run_lean ~lean ~timeout_seconds negated with
                  | Process_succeeded -> result Disproved ~detail ()
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
          }
        | exception_ ->
          result Solver_error ~detail:(Printexc.to_string exception_) ()
      end
