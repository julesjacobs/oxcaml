(* This file holds two independent translations, not one translation with two
   solvers.  [emit_expression] and the functions around it print SMT-LIB text
   for an external solver; [oxsmt_expression] and the functions around it
   build oxsmt terms directly and never go near that text.  An operation must
   therefore be given its meaning twice, and a change to one meaning does not
   reach the other.  This is measured rather than assumed: swapping logical
   and arithmetic right shift in the SMT-LIB printer left the oxsmt arm of the
   differential gate reporting no disagreement at all, and only swapping the
   oxsmt operators as well made it fail.  Do not fold the two together on the
   assumption that one feeds the other.

   The vocabularies differ too, which is why the two sides do not look alike:
   the vendored bitvector operators are unsigned only, so signed division is
   spelled [bvsdiv] on one side and built by hand on the other. *)

open Types

module Oxsmt_context = Oxsmt_core.Context
module Oxsmt_bv = Oxsmt_core.Bv
module Oxsmt_datatype_defs = Oxsmt_core.Datatype_defs
module Oxsmt_internal_minter = Oxsmt_core.Internal_minter
module Oxsmt_nia_config = Oxsmt_core.Nia_config
module Oxsmt_nia_lin = Oxsmt_core.Nia_lin
module Oxsmt_rank = Oxsmt_core.Rank
module Oxsmt_session = Oxsmt_interface.Session
module Oxsmt_sort = Oxsmt_core.Sort
module Oxsmt_term = Oxsmt_core.Term

type emission_error =
  { location : Location.t;
    message : string;
  }

exception Emission_error of emission_error
exception Oxsmt_unsupported of string
exception Oxsmt_timeout

external set_alarm : int -> int = "caml_vox_set_alarm"
external with_async_exns : (unit -> 'a) -> 'a = "caml_vox_with_async_exns"
external sigalrm_is_blocked : unit -> int = "caml_vox_sigalrm_is_blocked"
external restore_sigalrm : bool -> bool = "caml_vox_restore_sigalrm"
(* The persistent runner is restricted to compiler-emitted stdin queries. *)
external run_persistent_z3 : string -> int -> string -> int * string
  = "caml_vox_z3_run_persistent"

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
    unused_facts : int list;
  }

type emitted_fact =
  { selector : string;
    term : string;
  }

type emitted_query =
  { contents : string;
    facts : emitted_fact list;
    goal : string;
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
    symbol_namespace : string;
    mutable data : data_instance list;
    mutable tuples : tuple_instance list;
    mutable references : reference list;
    mutable unspecified_shifts : string list;
    mutable unspecified_divisions : string list;
  }

let int_width = 63

let shift_fallback_basename = function
  | `Shift_left -> "VoxInt_shift_left_unspecified"
  | `Shift_right_logical -> "VoxInt_shift_right_logical_unspecified"
  | `Shift_right_arithmetic -> "VoxInt_shift_right_arithmetic_unspecified"
  | _ -> invalid_arg "shift_fallback_basename"

let solver_symbol context basename =
  if String.equal context.symbol_namespace "" then basename
  else basename ^ "_" ^ context.symbol_namespace

let shift_fallback_name context builtin =
  solver_symbol context (shift_fallback_basename builtin)

(* Division by zero raises rather than producing a value, so the model must
   not hand out one.  The quotient and remainder at a zero divisor are an
   uninterpreted function of the operands, exactly as an out-of-range shift
   is: nothing is provable about the result, which is conservative because
   the program does not reach a post-state at all. *)
let division_fallback_basename = function
  | `Divide -> "VoxInt_divide_by_zero"
  | `Remainder -> "VoxInt_remainder_by_zero"
  | _ -> invalid_arg "division_fallback_basename"

let division_fallback_name context builtin =
  solver_symbol context (division_fallback_basename builtin)

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
  | Sint | Sbigint | Sbool | Sdata _ -> ()
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
        tuple_name = solver_symbol context ("VoxTuple_" ^ digest key);
        tuple_fields = sorts;
      }
      :: context.tuples;
  Stuple sorts

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
      | Type_abstract _, None ->
        register_abstract context location path arguments declaration
      | _ ->
        error location "type %s is not a supported SMT datatype"
          (Path.name path)
    end
  | _ -> error location "unsupported refinement-expression type"

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
        data_name = solver_symbol context ("VoxData_" ^ digest key);
        data_path = path;
        data_type_arguments = arguments;
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
    if List.length arguments <> declaration.type_arity then
      error location "datatype %s has the wrong number of arguments"
        (Path.name path);
    let data =
      { data_key = key;
        data_name = solver_symbol context ("VoxData_" ^ digest key);
        data_path = path;
        data_type_arguments = arguments;
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
  | Rexp_match (scrutinee, cases) ->
    with_desc
      (Rexp_match
         ( normalize scrutinee,
           List.map
             (fun case ->
               { case with rcase_body = normalize case.rcase_body })
             cases ))

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
    begin match Vox_builtin.of_path path with
    | Some builtin -> Some builtin
    | None ->
      match
        Subst.Lazy.force_value_description (Env.find_value path context.env)
      with
      | { val_kind = Val_prim primitive; _ } ->
        let path = Env.normalize_value_path None context.env path in
        Vox_builtin.of_primitive ~path primitive.prim_name
      | _ -> None
      | exception Not_found -> None
    end

let same_reference left right =
  match left, right with
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
  match reference with
  (* A match arm's constructor fact is interpreted directly as a datatype
     tester, so its synthetic head is not an opaque symbol.  Registering it as
     one keys it by constructor name alone, and two facts for the same
     polymorphic constructor at different instantiations are then rejected as
     used at inconsistent types. *)
  | Rfun name when Option.is_some (Vox_builtin.constructor_mismatch name) -> ()
  | Rfun _ | Rsibling _ | Rapp _ | Rglobal _ ->
  match builtin_name context reference with
  | Some ((`Shift_left | `Shift_right_logical
          | `Shift_right_arithmetic) as builtin) ->
    let name = shift_fallback_name context builtin in
    if not (List.mem name context.unspecified_shifts) then
      context.unspecified_shifts <- name :: context.unspecified_shifts
  | Some ((`Divide | `Remainder) as builtin) ->
    let name = division_fallback_name context builtin in
    if not (List.mem name context.unspecified_divisions) then
      context.unspecified_divisions <- name :: context.unspecified_divisions
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
            reference_name =
              solver_symbol context ("VoxRef_" ^ string_of_int index);
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
  | Rexp_match (scrutinee, cases) ->
    iter_expression function_ scrutinee;
    List.iter
      (fun case -> iter_expression function_ case.rcase_body)
      cases

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

let expect_bigint location = function
  | Sbigint -> ()
  | _ -> error location "Bigint operation used at an inconsistent type"

let expect_bool location = function
  | Sbool -> ()
  | _ -> error location "boolean operation used at a non-bool type"

let smt_int_constant integer =
  let unsigned = Int64.logand (Int64.of_int integer) Int64.max_int in
  Printf.sprintf "(_ bv%Ld %d)" unsigned int_width

let smt_signed_bv_to_int term =
  let zero = smt_int_constant 0 in
  "(ite (bvslt " ^ term ^ " " ^ zero ^ ") (- (bv2int " ^ term
  ^ ") 9223372036854775808) (bv2int " ^ term ^ "))"

let emit_builtin context location builtin arguments =
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
  | `Less -> binary "bvslt" (expect_int location) Sbool
  | `Less_equal -> binary "bvsle" (expect_int location) Sbool
  | `Greater -> binary "bvsgt" (expect_int location) Sbool
  | `Greater_equal -> binary "bvsge" (expect_int location) Sbool
  | (`Add | `Subtract | `Multiply) as operation ->
    begin match terms, sorts with
    | [left; right], [left_sort; right_sort] ->
      expect_int location left_sort;
      expect_sort location left_sort right_sort;
      let operator =
        match operation with
        | `Add -> "bvadd"
        | `Subtract -> "bvsub"
        | `Multiply -> "bvmul"
      in
      "(" ^ operator ^ " " ^ left ^ " " ^ right ^ ")", left_sort
    | _ -> error location "binary builtin used with the wrong arity"
    end
  | `Identity ->
    begin match terms, sorts with
    | [argument], [Sint] -> argument, Sint
    | _ -> error location "integer identity used with an inconsistent type"
    end
  | (`Negate | `Succ | `Pred) as operation ->
    begin match terms, sorts with
    | [argument], [Sint] ->
      let term =
        match operation with
        | `Negate -> "(bvneg " ^ argument ^ ")"
        | `Succ -> "(bvadd " ^ argument ^ " " ^ smt_int_constant 1 ^ ")"
        | `Pred -> "(bvsub " ^ argument ^ " " ^ smt_int_constant 1 ^ ")"
      in
      term, Sint
    | _ -> error location "unary integer builtin used with the wrong arity"
    end
  | (`Bit_and | `Bit_or | `Bit_xor) as operation ->
    let operator =
      match operation with
      | `Bit_and -> "bvand"
      | `Bit_or -> "bvor"
      | `Bit_xor -> "bvxor"
    in
    binary operator (expect_int location) Sint
  | (`Divide | `Remainder) as operation ->
    (* [bvsdiv] truncates towards zero and [bvsrem] takes the sign of the
       dividend, which is what OCaml does, including the wrap at
       [min_int / (-1)].  Only the zero divisor parts company: SMT-LIB gives
       it a value where the program raises. *)
    begin match terms, sorts with
    | [left; right], [Sint; Sint] ->
      let operator =
        match operation with
        | `Divide -> "bvsdiv"
        | `Remainder -> "bvsrem"
      in
      ( "(ite (distinct " ^ right ^ " " ^ smt_int_constant 0 ^ ") ("
        ^ operator ^ " " ^ left ^ " " ^ right ^ ") ("
        ^ division_fallback_name context operation ^ " " ^ left ^ " "
        ^ right ^ "))",
        Sint )
    | _ -> error location "integer division used with the wrong arity"
    end
  | (`Shift_left | `Shift_right_logical
    | `Shift_right_arithmetic) as operation ->
    begin match terms, sorts with
    | [left; right], [Sint; Sint] ->
      let operator =
        match operation with
        | `Shift_left -> "bvshl"
        | `Shift_right_logical -> "bvlshr"
        | `Shift_right_arithmetic -> "bvashr"
      in
      let valid =
        "(and (bvsle " ^ smt_int_constant 0 ^ " " ^ right
        ^ ") (bvsle " ^ right ^ " " ^ smt_int_constant int_width ^ "))"
      in
      ( "(ite " ^ valid ^ " (" ^ operator ^ " " ^ left ^ " " ^ right
        ^ ") (" ^ shift_fallback_name context operation ^ " " ^ left ^ " "
        ^ right ^ "))",
        Sint )
    | _ -> error location "integer shift used with the wrong arity"
    end
  | `Int_max -> error location "max_int used as a function"
  | `Int_min -> error location "min_int used as a function"
  | (`Bigint_add | `Bigint_sub | `Bigint_mul) as operation ->
    begin match terms, sorts with
    | [left; right], [left_sort; right_sort] ->
      expect_bigint location left_sort;
      expect_sort location left_sort right_sort;
      let operator =
        match operation with
        | `Bigint_add -> "+"
        | `Bigint_sub -> "-"
        | `Bigint_mul -> "*"
      in
      "(" ^ operator ^ " " ^ left ^ " " ^ right ^ ")", Sbigint
    | _ -> error location "Bigint arithmetic used with the wrong arity"
    end
  | `Bigint_neg ->
    begin match terms, sorts with
    | [argument], [Sbigint] -> "(- " ^ argument ^ ")", Sbigint
    | _ -> error location "Bigint.neg used with an inconsistent type"
    end
  | `Bigint_abs ->
    begin match terms, sorts with
    | [argument], [Sbigint] ->
      ( "(ite (< " ^ argument ^ " 0) (- " ^ argument ^ ") "
        ^ argument ^ ")",
        Sbigint )
    | _ -> error location "Bigint.abs used with an inconsistent type"
    end
  | `Bigint_compare ->
    begin match terms, sorts with
    | [left; right], [Sbigint; Sbigint] ->
      ( "(ite (< " ^ left ^ " " ^ right ^ ") " ^ smt_int_constant (-1)
        ^ " (ite (> " ^ left ^ " " ^ right ^ ") " ^ smt_int_constant 1
        ^ " " ^ smt_int_constant 0 ^ "))",
        Sint )
    | _ -> error location "Bigint.compare used with an inconsistent type"
    end
  | `Bigint_lt -> binary "<" (expect_bigint location) Sbool
  | `Bigint_le -> binary "<=" (expect_bigint location) Sbool
  | `Bigint_gt -> binary ">" (expect_bigint location) Sbool
  | `Bigint_ge -> binary ">=" (expect_bigint location) Sbool
  | `Bigint_of_int ->
    begin match terms, sorts with
    | [argument], [Sint] -> smt_signed_bv_to_int argument, Sbigint
    | _ -> error location "Bigint.of_int used with an inconsistent type"
    end
  | `Bigint_is_zero ->
    begin match terms, sorts with
    | [argument], [Sbigint] -> "(= " ^ argument ^ " 0)", Sbool
    | _ -> error location "Bigint.is_zero used with an inconsistent type"
    end
  | `Bigint_zero | `Bigint_one ->
    error location "Bigint constant used as a function"
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
  | Abstract ->
    error location "%s is an abstract type" (Path.name data.data_path)
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

type construction =
  | Variant_construction of int * constructor
  | Record_construction of (string * sort) list

let construction location data name =
  match definition location data with
  | Abstract ->
    error location "%s is an abstract type" (Path.name data.data_path)
  | Variant _ ->
    let index, constructor = constructor location data name in
    Variant_construction (index, constructor)
  | Record fields ->
    if not (String.equal name "mk") then
      error location "record construction must use the structure constructor";
    Record_construction fields

let record_field location data name =
  match definition location data with
  | Abstract ->
    error location "%s is an abstract type" (Path.name data.data_path)
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
          | Some `Bigint_zero -> "0"
          | Some `Bigint_one -> "1"
          | Some `Int_max -> smt_int_constant max_int
          | Some `Int_min -> smt_int_constant min_int
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
        smt_int_constant integer
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
      (* A match arm that did not fire says its scrutinee is not that arm's
         constructor.  Emitting the datatype tester keeps that meaning; left
         as an uninterpreted application it says nothing, and a later arm
         cannot then conclude which constructor remains. *)
      | Rexp_apply
          ( { rexp_desc = Rexp_ident (Rfree (Rfun mismatch_name)); _ },
            [ (_, subject) ] )
        when Option.is_some (Vox_builtin.constructor_mismatch mismatch_name) ->
        let constructor_name =
          Option.get (Vox_builtin.constructor_mismatch mismatch_name)
        in
        let subject_term, subject_sort = emit locals subject in
        begin
          match subject_sort with
          | Sdata key ->
            let data = data_for_key context expression.rexp_loc key in
            begin
              match
                construction expression.rexp_loc data constructor_name
              with
              | Variant_construction (index, constructor) ->
                expect_sort expression.rexp_loc result_sort Sbool;
                "(not ((_ is "
                ^ variant_constructor_name data index constructor
                ^ ") " ^ subject_term ^ "))"
              | Record_construction _ ->
                error expression.rexp_loc
                  "a record type has no constructor to test"
            end
          | Sint | Sbigint | Sbool | Stuple _ | Sarrow _ ->
            error expression.rexp_loc
              "a constructor test needs a datatype subject"
        end
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
              emit_builtin context expression.rexp_loc builtin
                rendered_arguments
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
            let arguments = List.map (emit locals) arguments in
            let head, expected =
              match
                construction expression.rexp_loc data
                  constructor_description.rconstr_name
              with
              | Variant_construction (index, constructor) ->
                ( variant_constructor_name data index constructor,
                  constructor.constructor_fields )
              | Record_construction fields ->
                record_constructor_name data, List.map snd fields
            in
            if List.length expected <> List.length arguments then
              error expression.rexp_loc "constructor arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected arguments;
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
            let index, constructor =
              constructor expression.rexp_loc data
                case.rcase_constructor.rconstr_name
            in
            if
              List.length constructor.constructor_fields
              <> List.length case.rcase_arguments
            then error expression.rexp_loc "match constructor arity mismatch";
            let arguments, case_locals =
              List.fold_left2
                (fun (arguments, locals) field argument ->
                  let name = fresh_local () in
                  let locals =
                    match argument with
                    | None -> locals
                    | Some binder ->
                      let binder_sort =
                        sort_of_type context expression.rexp_loc binder.rb_type
                      in
                      expect_sort expression.rexp_loc field binder_sort;
                      (binder.rb_id, (name, field)) :: locals
                  in
                  name :: arguments, locals)
                ([], locals) constructor.constructor_fields
                case.rcase_arguments
            in
            let body, body_sort = emit case_locals case.rcase_body in
            expect_sort expression.rexp_loc result_sort body_sort;
            let head = variant_constructor_name data index constructor in
            let pattern =
              match List.rev arguments with
              | [] -> head
              | arguments ->
                "(" ^ String.concat " " (head :: arguments) ^ ")"
            in
            "(" ^ pattern ^ " " ^ body ^ ")"
          in
          if cases = [] then error expression.rexp_loc "empty match";
          "(match " ^ scrutinee ^ " ("
          ^ String.concat " " (List.map render_case cases)
          ^ "))"
        | _ -> error expression.rexp_loc "match scrutinee is not a datatype"
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
    | Sint -> "(_ BitVec 63)"
    | Sbigint -> "Int"
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
    | Abstract ->
      error location "internal error: abstract datatype has a concrete shape"
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

let check_abstract_inhabitance (context : context) variables location =
  let fixed_variable key =
    List.exists
      (fun variable ->
        match variable.variable_sort with
        | Sdata variable_key -> String.equal key variable_key
        | Sint | Sbigint | Sbool | Stuple _ | Sarrow _ -> false)
      variables
  in
  let trusted_constant key =
    List.exists
      (fun reference ->
        match reference.reference_head, reference.reference_sort with
        | (Rglobal path | Rapp path), Sdata reference_key
          when String.equal key reference_key ->
          begin match
            Subst.Lazy.force_value_description
              (Env.find_value path context.env)
          with
          | { val_kind = Val_reg _; _ } -> true
          | { val_kind = Val_prim _ | Val_mut _ | Val_ivar _ | Val_self _
                           | Val_anc _; _ } -> false
          | exception Not_found -> false
          end
        | (Rfun _ | Rsibling _ | Rglobal _ | Rapp _), _ ->
          false)
      context.references
  in
  let trusted_environment_constant data =
    Env.fold_values
      (fun _name _path lazy_description _mode inhabited ->
        inhabited
        ||
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
          Option.fold ~none:false
            ~some:(fun (path, arguments) ->
              Path.same path data.data_path
              && Ctype.is_equal context.env false arguments
                   data.data_type_arguments)
            (data_instance val_type)
        | { val_kind =
              (Val_prim _ | Val_mut _ | Val_ivar _ | Val_self _ | Val_anc _);
            _ } ->
          false)
      None context.env false
  in
  let inhabited_builtin data =
    Path.same data.data_path Predef.path_iarray
  in
  List.iter
    (fun data ->
      match definition location data with
      | Abstract
        when not
               (inhabited_builtin data
                || fixed_variable data.data_key
                || trusted_constant data.data_key
                || trusted_environment_constant data) ->
        error location
          "abstract type %s is not known to be inhabited"
          (Path.name data.data_path)
      | Abstract | Variant _ | Record _ -> ())
    context.data

let check_concrete_inhabitance (context : context) location =
  let rec sort_is_inhabited inhabited = function
    | Sint | Sbigint | Sbool -> true
    | Stuple sorts -> List.for_all (sort_is_inhabited inhabited) sorts
    | Sdata key -> List.mem key inhabited
    | Sarrow _ -> false
  in
  let inhabited =
    ref
      (List.filter_map
         (fun data ->
           match definition location data with
           | Abstract -> Some data.data_key
           | Variant _ | Record _ -> None)
         context.data)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    let add key condition =
      if condition && not (List.mem key !inhabited) then begin
        inhabited := key :: !inhabited;
        changed := true
      end
    in
    List.iter
      (fun data ->
        let definition_is_inhabited =
          match definition location data with
          | Abstract -> true
          | Variant constructors ->
            List.exists
              (fun constructor ->
                List.for_all (sort_is_inhabited !inhabited)
                  constructor.constructor_fields)
              constructors
          | Record fields ->
            List.for_all
              (fun (_, sort) -> sort_is_inhabited !inhabited sort)
              fields
        in
        add data.data_key definition_is_inhabited)
      context.data
  done;
  List.iter
    (fun data ->
      if not (List.mem data.data_key !inhabited) then
        error location "datatype %s is not well-founded"
          (Path.name data.data_path))
    context.data

type oxsmt_environment =
  { terms : Oxsmt_context.t;
    bv_minter : Oxsmt_bv.minter;
    datatypes : (string * Oxsmt_datatype_defs.datatype) list;
    references : (reference * Oxsmt_core.Symbol.t) list;
    variables : (variable * Oxsmt_core.Symbol.t) list;
    nia_minter : Oxsmt_internal_minter.t option;
    mutable nia_mul_symbol : Oxsmt_core.Symbol.t option;
    mutable nia_products : Oxsmt_nia_lin.product list;
  }

let oxsmt_shapes context location =
  List.map tuple_shape context.tuples
  @ List.filter_map
      (fun data ->
        match definition location data with
        | Abstract -> None
        | Variant _ | Record _ -> Some (data_shape location data))
      context.data
  |> List.sort (fun left right ->
       String.compare left.shape_key right.shape_key)

let oxsmt_find_named location description name entries =
  match
    List.find_opt
      (fun (entry_name, _) -> String.equal entry_name name)
      entries
  with
  | Some (_, entry) -> entry
  | None ->
    error location "internal error: missing oxsmt %s %s" description name

let oxsmt_sort context sorts location = function
  | Sint -> Oxsmt_sort.bitvec int_width
  | Sbigint -> Oxsmt_sort.int
  | Sbool -> Oxsmt_sort.bool
  | Stuple fields ->
    let tuple = tuple_for_sorts context location fields in
    oxsmt_find_named location "tuple sort" tuple.tuple_key sorts
  | Sdata key -> oxsmt_find_named location "datatype sort" key sorts
  | Sarrow _ ->
    error location
      "function sort escaped into first-order oxsmt declaration"

let oxsmt_declare_datatypes session context location =
  let shapes = oxsmt_shapes context location in
  let sort_declarations =
    List.map
      (fun shape ->
        ( shape.shape_key,
          shape.shape_name,
          fun symbol -> Oxsmt_sort.datatype_ symbol ))
      shapes
    @ List.filter_map
        (fun data ->
          match definition location data with
          | Abstract ->
            Some
              ( data.data_key,
                data.data_name,
                fun symbol -> Oxsmt_sort.uninterpreted symbol )
          | Variant _ | Record _ -> None)
        context.data
    |> List.sort (fun (left, _, _) (right, _, _) ->
         String.compare left right)
  in
  let sorts =
    List.map
      (fun (key, name, sort_of_symbol) ->
        let symbol = Oxsmt_session.declare_sort session name in
        key, sort_of_symbol symbol)
      sort_declarations
  in
  let datatypes =
    List.map
      (fun shape ->
        let datatype_sort =
          oxsmt_find_named location "datatype sort" shape.shape_key sorts
        in
        let constructors =
          List.map
            (fun (constructor_name, fields) ->
              let fields =
                List.map
                  (fun (selector_name, field_sort) ->
                    ( selector_name,
                      oxsmt_sort context sorts location field_sort ))
                  fields
              in
              { Oxsmt_session.ctor_name = constructor_name; fields })
            shape.shape_constructors
        in
        let datatype =
          Oxsmt_session.declare_datatype
            session datatype_sort constructors
        in
        shape.shape_key, datatype)
      shapes
  in
  sorts, datatypes

let oxsmt_declare_references session (context : context) sorts location =
  List.sort
    (fun (left : reference) (right : reference) ->
      String.compare left.reference_name right.reference_name)
    context.references
  |> List.map (fun reference ->
    let arguments, result =
      arrow_signature location [] reference.reference_sort
    in
    let arguments =
      List.map (oxsmt_sort context sorts location) arguments
    in
    let result = oxsmt_sort context sorts location result in
    let symbol =
      Oxsmt_session.declare_fun
        session reference.reference_name
        (Oxsmt_rank.create arguments result)
    in
    reference, symbol)

let oxsmt_declare_variables session context sorts location variables =
  List.map
    (fun variable ->
      let arguments, result =
        arrow_signature location [] variable.variable_sort
      in
      let arguments =
        List.map (oxsmt_sort context sorts location) arguments
      in
      let result = oxsmt_sort context sorts location result in
      let symbol =
        match arguments with
        | [] ->
          Oxsmt_session.declare_const
            session variable.variable_name result
        | _ :: _ ->
          Oxsmt_session.declare_fun
            session variable.variable_name
            (Oxsmt_rank.create arguments result)
      in
      variable, symbol)
    variables

let oxsmt_reference_symbol environment location reference_head =
  match
    List.find_opt
      (fun (reference, _) ->
        same_reference reference.reference_head reference_head)
      environment.references
  with
  | Some (_, symbol) -> symbol
  | None ->
    error location "internal error: missing oxsmt reference %s"
      (reference_description reference_head)

let oxsmt_variable environment id =
  List.find_opt
    (fun (variable, _) -> Ident.same id variable.variable_id)
    environment.variables

let oxsmt_datatype environment location key =
  oxsmt_find_named location "datatype" key environment.datatypes

let oxsmt_constructor environment location key index =
  let datatype = oxsmt_datatype environment location key in
  match
    List.nth_opt
      datatype.Oxsmt_datatype_defs.constructors
      index
  with
  | Some constructor -> constructor
  | None ->
    error location
      "internal error: missing oxsmt constructor %d for %s" index key

let oxsmt_selector environment location key constructor_index field_index =
  let constructor =
    oxsmt_constructor environment location key constructor_index
  in
  match
    List.nth_opt constructor.Oxsmt_datatype_defs.selectors field_index
  with
  | Some selector -> selector.Oxsmt_datatype_defs.sym
  | None ->
    error location "internal error: missing oxsmt selector %d for %s"
      field_index key

let oxsmt_multiply ~allow_nonlinear environment location left right =
  let terms = environment.terms in
  match left.Oxsmt_term.node, right.Oxsmt_term.node with
  | Oxsmt_term.Int_const coefficient, _ ->
    Oxsmt_context.mul_const_big terms coefficient right
  | _, Oxsmt_term.Int_const coefficient ->
    Oxsmt_context.mul_const_big terms coefficient left
  | _ when not allow_nonlinear ->
    raise
      (Oxsmt_unsupported
         (Format.asprintf
            "%a: nonlinear integer multiplication is not supported"
            Location.print_loc location))
  | _ ->
    begin
      match environment.nia_minter with
      | None ->
        raise
          (Oxsmt_unsupported
             (Format.asprintf
                "%a: nonlinear integer multiplication is not supported"
                Location.print_loc location))
      | Some minter ->
        let symbol =
          match environment.nia_mul_symbol with
          | Some symbol -> symbol
          | None ->
            let symbol =
              Oxsmt_internal_minter.mint
                minter Oxsmt_nia_config.mul_name
                (Oxsmt_rank.create [Oxsmt_sort.int; Oxsmt_sort.int]
                   Oxsmt_sort.int)
            in
            environment.nia_mul_symbol <- Some symbol;
            symbol
        in
        let product = Oxsmt_context.app terms symbol [left; right] in
        environment.nia_products <-
          { Oxsmt_nia_lin.p = product; a = left; b = right }
          :: environment.nia_products;
        product
    end

let oxsmt_nia_lemmas environment =
  let seen = Oxsmt_term.Table.create 64 in
  let distinct_products =
    List.filter
      (fun { Oxsmt_nia_lin.p; _ } ->
        match Oxsmt_term.Table.find_opt seen p with
        | Some () -> false
        | None ->
          Oxsmt_term.Table.replace seen p ();
          true)
      environment.nia_products
  in
  Oxsmt_nia_lin.lemmas environment.terms distinct_products

let oxsmt_int_constant environment integer =
  Oxsmt_bv.const environment.terms environment.bv_minter
    ~value:(Oxsmt_core.Bigint.of_int integer) ~width:int_width

let oxsmt_builtin environment location builtin arguments =
  let terms = environment.terms in
  let term_values = List.map fst arguments in
  let term_sorts = List.map snd arguments in
  let binary check result_sort operation =
    match term_values, term_sorts with
    | [left; right], [left_sort; right_sort] ->
      check left_sort;
      check right_sort;
      operation left right, result_sort
    | _ -> error location "binary builtin used with the wrong arity"
  in
  match builtin with
  | `Equal | `Not_equal ->
    begin
      match term_values, term_sorts with
      | [left; right], [left_sort; right_sort] ->
        expect_sort location left_sort right_sort;
        ensure_first_order location left_sort;
        let equality = Oxsmt_context.eq terms left right in
        let term =
          if builtin = `Equal then equality
          else Oxsmt_context.not_ terms equality
        in
        term, Sbool
      | _ -> error location "equality builtin used with the wrong arity"
    end
  | `Less ->
    binary (expect_int location) Sbool
      (Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvslt)
  | `Less_equal ->
    binary (expect_int location) Sbool
      (Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvsle)
  | `Greater ->
    binary (expect_int location) Sbool
      (fun left right ->
        Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvslt
          right left)
  | `Greater_equal ->
    binary (expect_int location) Sbool
      (fun left right ->
        Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvsle
          right left)
  | (`Add | `Subtract | `Multiply) as operation ->
    begin match term_values, term_sorts with
    | [left; right], [left_sort; right_sort] ->
      expect_int location left_sort;
      expect_sort location left_sort right_sort;
      let term =
        match operation with
        | `Add ->
          Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvadd
            left right
        | `Subtract ->
          Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvsub
            left right
        | `Multiply ->
          Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvmul
            left right
      in
      term, left_sort
    | _ -> error location "binary builtin used with the wrong arity"
    end
  | `Identity ->
    begin match term_values, term_sorts with
    | [argument], [Sint] -> argument, Sint
    | _ -> error location "integer identity used with an inconsistent type"
    end
  | (`Negate | `Succ | `Pred) as operation ->
    begin match term_values, term_sorts with
    | [argument], [Sint] ->
      let term =
        match operation with
        | `Negate ->
          Oxsmt_bv.unop terms environment.bv_minter Oxsmt_bv.Bvneg
            argument
        | `Succ ->
          Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvadd
            argument (oxsmt_int_constant environment 1)
        | `Pred ->
          Oxsmt_bv.binop terms environment.bv_minter Oxsmt_bv.Bvsub
            argument (oxsmt_int_constant environment 1)
      in
      term, Sint
    | _ -> error location "unary integer builtin used with the wrong arity"
    end
  | (`Bit_and | `Bit_or | `Bit_xor) as operation ->
    let operator =
      match operation with
      | `Bit_and -> Oxsmt_bv.Bvand
      | `Bit_or -> Oxsmt_bv.Bvor
      | `Bit_xor -> Oxsmt_bv.Bvxor
    in
    binary (expect_int location) Sint
      (Oxsmt_bv.binop terms environment.bv_minter operator)
  | (`Divide | `Remainder) as operation ->
    (* The vendored bitvector operators are unsigned, so the signed quotient
       and remainder are built here the way SMT-LIB defines them: on the
       magnitudes, with the sign put back.  Truncation towards zero and a
       remainder carrying the dividend's sign are what OCaml does, and
       [min_int / (-1)] wraps because negating [min_int] wraps.

       A zero divisor raises rather than producing a value.  The other two
       backends send that case to an uninterpreted symbol, which this one
       cannot do: the blaster is QF_BV and refuses an uninterpreted function
       over bitvectors.  So a divisor that is not a literal known to be
       non-zero is refused outright, exactly as an out-of-range shift count
       is.  That is weaker than the other two backends and fails closed. *)
    begin match term_values, term_sorts with
    | [left; right], [Sint; Sint] ->
      (* The divisor's sign is read off the literal rather than tested in
         the term.  Each branch of the term is a full division circuit, and
         the facts of earlier divisions accumulate, so halving the branches
         halves a cost that grows with the square of the divisions in a
         compilation unit. *)
      let divisor_negative =
        match Oxsmt_bv.view right with
        | Some (Oxsmt_bv.Const { value; width }) when width = int_width ->
          if Oxsmt_core.Bigint.equal value (Oxsmt_core.Bigint.of_int 0) then
            None
          else
            (* The literal is canonical in [0, 2^63), so it stands for a
               negative number exactly when it is at least 2^62, and this
               host's own [max_int] is 2^62 - 1. *)
            Some
              (Oxsmt_core.Bigint.compare value
                 (Oxsmt_core.Bigint.of_int max_int)
               > 0)
        | Some (Oxsmt_bv.Const _ | Oxsmt_bv.Op _) | None -> None
      in
      let divisor_negative =
        match divisor_negative with
        | Some negative -> negative
        | None ->
          raise
            (Oxsmt_unsupported
               "integer division by a divisor not known to be non-zero")
      in
      let apply operator x y =
        Oxsmt_bv.binop terms environment.bv_minter operator x y
      in
      let negate x =
        Oxsmt_bv.unop terms environment.bv_minter Oxsmt_bv.Bvneg x
      in
      let below_zero x =
        apply Oxsmt_bv.Bvslt x (oxsmt_int_constant environment 0)
      in
      let magnitude =
        match operation with
        | `Divide -> Oxsmt_bv.Bvudiv
        | `Remainder -> Oxsmt_bv.Bvurem
      in
      (* The sign of the result follows the dividend for a remainder, and the
         two operands' signs for a quotient. *)
      let signed_by ~dividend_negative ~divisor_negative =
        let value =
          apply magnitude
            (if dividend_negative then negate left else left)
            (if divisor_negative then negate right else right)
        in
        let flip =
          match operation with
          | `Divide -> dividend_negative <> divisor_negative
          | `Remainder -> dividend_negative
        in
        if flip then negate value else value
      in
      ( Oxsmt_context.ite terms (below_zero left)
          (signed_by ~dividend_negative:true ~divisor_negative)
          (signed_by ~dividend_negative:false ~divisor_negative),
        Sint )
    | _ -> error location "integer division used with the wrong arity"
    end
  | (`Shift_left | `Shift_right_logical
    | `Shift_right_arithmetic) as operation ->
    begin match term_values, term_sorts with
    | [left; right], [Sint; Sint] ->
      let valid_constant =
        match Oxsmt_bv.view right with
        | Some (Oxsmt_bv.Const { value; width = 63 }) ->
          begin match Oxsmt_core.Bigint.to_int_opt value with
          | Some count -> count <= int_width
          | None -> false
          end
        | Some (Oxsmt_bv.Const _ | Oxsmt_bv.Op _) | None -> false
      in
      if not valid_constant then
        raise
          (Oxsmt_unsupported
             "symbolic or unspecified-count integer shift is not pure QF_BV");
      let operator =
        match operation with
        | `Shift_left -> Oxsmt_bv.Bvshl
        | `Shift_right_logical -> Oxsmt_bv.Bvlshr
        | `Shift_right_arithmetic -> Oxsmt_bv.Bvashr
      in
      ( Oxsmt_bv.binop terms environment.bv_minter operator left right,
        Sint )
    | _ -> error location "integer shift used with the wrong arity"
    end
  | `Int_max -> error location "max_int used as a function"
  | `Int_min -> error location "min_int used as a function"
  | (`Bigint_add | `Bigint_sub | `Bigint_mul) as operation ->
    begin match term_values, term_sorts with
    | [left; right], [left_sort; right_sort] ->
      expect_bigint location left_sort;
      expect_sort location left_sort right_sort;
      let term =
        match operation with
        | `Bigint_add -> Oxsmt_context.add terms left right
        | `Bigint_sub -> Oxsmt_context.sub terms left right
        | `Bigint_mul ->
          oxsmt_multiply ~allow_nonlinear:true environment location
            left right
      in
      term, Sbigint
    | _ -> error location "Bigint arithmetic used with the wrong arity"
    end
  | `Bigint_neg ->
    begin match term_values, term_sorts with
    | [argument], [Sbigint] ->
      ( Oxsmt_context.sub terms (Oxsmt_context.int_const terms 0) argument,
        Sbigint )
    | _ -> error location "Bigint.neg used with an inconsistent type"
    end
  | `Bigint_abs ->
    begin match term_values, term_sorts with
    | [argument], [Sbigint] ->
      let zero = Oxsmt_context.int_const terms 0 in
      ( Oxsmt_context.ite terms
          (Oxsmt_context.lt terms argument zero)
          (Oxsmt_context.sub terms zero argument)
          argument,
        Sbigint )
    | _ -> error location "Bigint.abs used with an inconsistent type"
    end
  | `Bigint_compare ->
    begin match term_values, term_sorts with
    | [left; right], [Sbigint; Sbigint] ->
      ( Oxsmt_context.ite terms
          (Oxsmt_context.lt terms left right)
          (oxsmt_int_constant environment (-1))
          (Oxsmt_context.ite terms
             (Oxsmt_context.gt terms left right)
             (oxsmt_int_constant environment 1)
             (oxsmt_int_constant environment 0)),
        Sint )
    | _ -> error location "Bigint.compare used with an inconsistent type"
    end
  | `Bigint_lt ->
    binary (expect_bigint location) Sbool (Oxsmt_context.lt terms)
  | `Bigint_le ->
    binary (expect_bigint location) Sbool (Oxsmt_context.le terms)
  | `Bigint_gt ->
    binary (expect_bigint location) Sbool (Oxsmt_context.gt terms)
  | `Bigint_ge ->
    binary (expect_bigint location) Sbool (Oxsmt_context.ge terms)
  | `Bigint_of_int ->
    begin match term_values, term_sorts with
    | [argument], [Sint] ->
      begin match Oxsmt_bv.view argument with
      | Some (Oxsmt_bv.Const { value; width = 63 }) ->
        let sign = Oxsmt_core.Bigint.of_string "4611686018427387904" in
        let modulus = Oxsmt_core.Bigint.of_string "9223372036854775808" in
        let signed =
          if Oxsmt_core.Bigint.compare value sign >= 0 then
            Oxsmt_core.Bigint.sub value modulus
          else value
        in
        Oxsmt_context.int_const_big terms signed, Sbigint
      | Some (Oxsmt_bv.Const _ | Oxsmt_bv.Op _) | None ->
        raise
          (Oxsmt_unsupported
             "symbolic signed BV63-to-Int conversion is unsupported")
      end
    | _ -> error location "Bigint.of_int used with an inconsistent type"
    end
  | `Bigint_is_zero ->
    begin match term_values, term_sorts with
    | [argument], [Sbigint] ->
      Oxsmt_context.eq terms argument (Oxsmt_context.int_const terms 0),
      Sbool
    | _ -> error location "Bigint.is_zero used with an inconsistent type"
    end
  | `Bigint_zero | `Bigint_one ->
    error location "Bigint constant used as a function"
  | `And ->
    binary (expect_bool location) Sbool (fun left right ->
      Oxsmt_context.and_ terms [left; right])
  | `Or ->
    binary (expect_bool location) Sbool (fun left right ->
      Oxsmt_context.or_ terms [left; right])
  | `Not ->
    begin
      match term_values, term_sorts with
      | [argument], [sort] ->
        expect_bool location sort;
        Oxsmt_context.not_ terms argument, Sbool
      | _ -> error location "not builtin used with the wrong arity"
    end

let oxsmt_expression context environment expression =
  let rec build locals expression =
    let result_sort =
      sort_of_type context expression.rexp_loc expression.rexp_type
    in
    let term =
      match expression.rexp_desc with
      | Rexp_ident (Rbound id) ->
        begin
          match find_ident id locals with
          | Some (_, (term, sort)) ->
            expect_sort expression.rexp_loc result_sort sort;
            let arguments, _ =
              arrow_signature expression.rexp_loc [] sort
            in
            if arguments <> [] then
              error expression.rexp_loc
                "local function %s must be fully applied"
                (Ident.name id);
            term
          | None ->
            begin
              match oxsmt_variable environment id with
              | Some (variable, symbol) ->
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
                Oxsmt_context.const environment.terms symbol
              | None ->
                error expression.rexp_loc
                  "identifier %s is neither locally bound nor in VC scope"
                  (Ident.name id)
            end
        end
      | Rexp_ident (Rfree reference_identifier) ->
        begin
          match builtin_name context reference_identifier with
          | Some `Bigint_zero -> Oxsmt_context.int_const environment.terms 0
          | Some `Bigint_one -> Oxsmt_context.int_const environment.terms 1
          | Some `Int_max -> oxsmt_int_constant environment max_int
          | Some `Int_min -> oxsmt_int_constant environment min_int
          | Some _ ->
            error expression.rexp_loc "builtin %s must be fully applied"
              (reference_basename reference_identifier)
          | None ->
            let reference =
              reference context expression.rexp_loc reference_identifier
            in
            let arguments, reference_result =
              arrow_signature expression.rexp_loc []
                reference.reference_sort
            in
            if arguments <> [] then
              error expression.rexp_loc
                "opaque function %s must be fully applied"
                (reference_basename reference_identifier);
            expect_sort expression.rexp_loc result_sort reference_result;
            let symbol =
              oxsmt_reference_symbol
                environment expression.rexp_loc reference_identifier
            in
            Oxsmt_context.const environment.terms symbol
        end
      | Rexp_constant (Const_int integer) ->
        oxsmt_int_constant environment integer
      | Rexp_constant _ ->
        error expression.rexp_loc "only int constants are supported"
      | Rexp_let (bindings, body) ->
        if bindings = [] then error expression.rexp_loc "empty refinement let";
        let built_bindings =
          List.map
            (fun binding ->
              let value, value_sort = build locals binding.rbind_expr in
              let binder_sort =
                sort_of_type context binding.rbind_expr.rexp_loc
                  binding.rbind_binder.rb_type
              in
              ensure_first_order binding.rbind_expr.rexp_loc binder_sort;
              expect_sort binding.rbind_expr.rexp_loc
                binder_sort value_sort;
              binding.rbind_binder.rb_id, (value, binder_sort))
            bindings
        in
        let body_locals =
          List.fold_left
            (fun locals binding -> binding :: locals)
            locals built_bindings
        in
        let body, body_sort = build body_locals body in
        expect_sort expression.rexp_loc result_sort body_sort;
        body
      | Rexp_function _ ->
        error expression.rexp_loc
          "lambda remains after beta reduction; partial or higher-order "
          "application is not supported"
      (* The same interpretation the external translation gives this fact:
         without it the application is an opaque symbol and a later arm cannot
         tell which constructor remains. *)
      | Rexp_apply
          ( { rexp_desc = Rexp_ident (Rfree (Rfun mismatch_name)); _ },
            [ (_, subject) ] )
        when Option.is_some (Vox_builtin.constructor_mismatch mismatch_name) ->
        let constructor_name =
          Option.get (Vox_builtin.constructor_mismatch mismatch_name)
        in
        let subject_term, subject_sort = build locals subject in
        begin
          match subject_sort with
          | Sdata key ->
            let data = data_for_key context expression.rexp_loc key in
            begin
              match
                construction expression.rexp_loc data constructor_name
              with
              | Variant_construction (index, _) ->
                expect_sort expression.rexp_loc result_sort Sbool;
                let tester =
                  (oxsmt_constructor environment expression.rexp_loc key index)
                    .Oxsmt_datatype_defs.tester
                in
                Oxsmt_context.not_ environment.terms
                  (Oxsmt_context.app environment.terms tester [subject_term])
              | Record_construction _ ->
                error expression.rexp_loc
                  "a record type has no constructor to test"
            end
          | Sint | Sbigint | Sbool | Stuple _ | Sarrow _ ->
            error expression.rexp_loc
              "a constructor test needs a datatype subject"
        end
      | Rexp_apply
          ( { rexp_desc = Rexp_ident (Rfree reference_identifier); _ },
            arguments ) ->
        let built_arguments =
          List.map (fun (_, argument) -> build locals argument) arguments
        in
        begin
          match builtin_name context reference_identifier with
          | Some builtin ->
            let term, actual_sort =
              oxsmt_builtin environment expression.rexp_loc
                builtin built_arguments
            in
            expect_sort expression.rexp_loc result_sort actual_sort;
            term
          | None ->
            let reference =
              reference context expression.rexp_loc reference_identifier
            in
            let expected_arguments, reference_result =
              arrow_signature expression.rexp_loc []
                reference.reference_sort
            in
            if
              List.length expected_arguments
              <> List.length built_arguments
            then
              error expression.rexp_loc
                "opaque function %s is partially or over-applied"
                (reference_basename reference_identifier);
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected_arguments built_arguments;
            expect_sort expression.rexp_loc result_sort reference_result;
            let symbol =
              oxsmt_reference_symbol
                environment expression.rexp_loc reference_identifier
            in
            Oxsmt_context.app environment.terms symbol
              (List.map fst built_arguments)
        end
      | Rexp_apply
          ({ rexp_desc = Rexp_ident (Rbound id); _ }, arguments) ->
        let built_arguments =
          List.map (fun (_, argument) -> build locals argument) arguments
        in
        begin
          match find_ident id locals with
          | Some _ ->
            error expression.rexp_loc
              "application of a local function is not supported"
          | None ->
            begin
              match oxsmt_variable environment id with
              | None ->
                error expression.rexp_loc
                  "function %s is not in VC scope" (Ident.name id)
              | Some (variable, symbol) ->
                let expected_arguments, function_result =
                  arrow_signature expression.rexp_loc []
                    variable.variable_sort
                in
                if
                  List.length expected_arguments
                  <> List.length built_arguments
                then
                  error expression.rexp_loc
                    "VC function %s is partially or over-applied"
                    (Ident.name id);
                List.iter2
                  (fun expected (_, actual) ->
                    expect_sort expression.rexp_loc expected actual)
                  expected_arguments built_arguments;
                expect_sort expression.rexp_loc result_sort function_result;
                Oxsmt_context.app environment.terms symbol
                  (List.map fst built_arguments)
            end
        end
      | Rexp_apply _ ->
        error expression.rexp_loc
          "higher-order application is not supported by the SMT backend"
      | Rexp_tuple fields ->
        let fields =
          List.map (fun (_, field) -> build locals field) fields
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
            let constructor =
              oxsmt_constructor
                environment expression.rexp_loc tuple.tuple_key 0
            in
            Oxsmt_context.app environment.terms
              constructor.Oxsmt_datatype_defs.sym
              (List.map fst fields)
          | _ -> error expression.rexp_loc "tuple has a non-tuple type"
        end
      | Rexp_construct (constructor_description, arguments) ->
        begin
          match result_sort with
          | Sbool ->
            begin
              match constructor_description.rconstr_name, arguments with
              | "true", [] ->
                Oxsmt_context.bool_const environment.terms true
              | "false", [] ->
                Oxsmt_context.bool_const environment.terms false
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
            let arguments = List.map (build locals) arguments in
            let index, expected =
              match
                construction expression.rexp_loc data
                  constructor_description.rconstr_name
              with
              | Variant_construction (index, constructor) ->
                index, constructor.constructor_fields
              | Record_construction fields ->
                0, List.map snd fields
            in
            if List.length expected <> List.length arguments then
              error expression.rexp_loc "constructor arity mismatch";
            List.iter2
              (fun expected (_, actual) ->
                expect_sort expression.rexp_loc expected actual)
              expected arguments;
            let constructor =
              oxsmt_constructor environment expression.rexp_loc key index
            in
            Oxsmt_context.app environment.terms
              constructor.Oxsmt_datatype_defs.sym
              (List.map fst arguments)
          | _ ->
            error expression.rexp_loc
              "constructor has a non-datatype result type"
        end
      | Rexp_field (record_expression, field) ->
        let record_term, record_sort = build locals record_expression in
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
              oxsmt_selector environment expression.rexp_loc key 0 index
            in
            Oxsmt_context.app environment.terms selector [record_term]
          | _ ->
            error expression.rexp_loc
              "field applied to a non-record type"
        end
      | Rexp_match (scrutinee, cases) ->
        let scrutinee, scrutinee_sort = build locals scrutinee in
        begin
          match scrutinee_sort with
          | Sdata key ->
            let data = data_for_key context expression.rexp_loc key in
            let build_case case =
              if
                not
                  (Path.same data.data_path
                     case.rcase_constructor.rconstr_type_path)
              then
                error expression.rexp_loc
                  "match constructor path does not match its scrutinee type";
              let constructor_index, constructor =
                constructor expression.rexp_loc data
                  case.rcase_constructor.rconstr_name
              in
              if
                List.length constructor.constructor_fields
                <> List.length case.rcase_arguments
              then error expression.rexp_loc "match constructor arity mismatch";
              let _, case_locals =
                List.fold_left2
                  (fun (field_index, locals) field argument ->
                    let selected =
                      let selector =
                        oxsmt_selector environment expression.rexp_loc key
                          constructor_index field_index
                      in
                      Oxsmt_context.app environment.terms selector
                        [scrutinee]
                    in
                    let locals =
                      match argument with
                      | None -> locals
                      | Some binder ->
                        let binder_sort =
                          sort_of_type context expression.rexp_loc
                            binder.rb_type
                        in
                        expect_sort expression.rexp_loc field binder_sort;
                        (binder.rb_id, (selected, field)) :: locals
                    in
                    field_index + 1, locals)
                  (0, locals) constructor.constructor_fields
                  case.rcase_arguments
              in
              let body, body_sort = build case_locals case.rcase_body in
              expect_sort expression.rexp_loc result_sort body_sort;
              let tester =
                (oxsmt_constructor environment expression.rexp_loc key
                   constructor_index)
                  .Oxsmt_datatype_defs.tester
              in
              ( constructor_index,
                Oxsmt_context.app environment.terms tester [scrutinee],
                body )
            in
            let cases = List.map build_case cases in
            let actual_indices =
              List.map (fun (index, _, _) -> index) cases
              |> List.sort_uniq Int.compare
            in
            let expected_indices =
              match definition expression.rexp_loc data with
              | Variant constructors ->
                List.init (List.length constructors) Fun.id
              | Abstract | Record _ ->
                error expression.rexp_loc "match scrutinee is not a variant"
            in
            if actual_indices <> expected_indices then
              error expression.rexp_loc
                "match cases do not cover each constructor exactly once";
            let rec build_match = function
              | [] -> error expression.rexp_loc "empty match"
              | [(_, _, body)] -> body
              | (_, tester, body) :: rest ->
                Oxsmt_context.ite environment.terms tester body
                  (build_match rest)
            in
            build_match cases
          | _ ->
            error expression.rexp_loc "match scrutinee is not a datatype"
        end
      | Rexp_ifthenelse (condition, ifso, Some ifnot) ->
        let condition, condition_sort = build locals condition in
        expect_bool expression.rexp_loc condition_sort;
        let ifso, ifso_sort = build locals ifso in
        let ifnot, ifnot_sort = build locals ifnot in
        expect_sort expression.rexp_loc result_sort ifso_sort;
        expect_sort expression.rexp_loc result_sort ifnot_sort;
        Oxsmt_context.ite environment.terms condition ifso ifnot
      | Rexp_ifthenelse (_, _, None) ->
        error expression.rexp_loc "else-less if is not supported"
    in
    term, result_sort
  in
  build [] expression

let contains_substring string substring =
  let string_length = String.length string in
  let substring_length = String.length substring in
  let rec loop index =
    index + substring_length <= string_length
    &&
    (String.equal
       (String.sub string index substring_length)
       substring
     || loop (index + 1))
  in
  substring_length = 0 || loop 0

let validate_oxsmt_verdict
    ?(allow_nia_model_rejection = false) session verdict =
  let reason = Oxsmt_session.last_unknown_reason session in
  match verdict with
  | Oxsmt_session.Unknown
    when contains_substring reason "timeout" ->
    raise Oxsmt_timeout
  | Oxsmt_session.Unknown
    when String.starts_with ~prefix:"poison-solve:" reason
         || String.equal reason "clausify-fail"
         || String.equal reason "register-poison"
         || String.equal reason "assumption-register-poison"
         || (String.ends_with ~suffix:"model-check-failed" reason
             && not
                  (allow_nia_model_rejection
                   && String.equal reason "r1-model-check-failed")) ->
    failwith ("oxsmt internal failure: " ^ reason)
  | Oxsmt_session.Sat
  | Oxsmt_session.Unsat
  | Oxsmt_session.Unknown -> verdict

let check_oxsmt_session ?allow_nia_model_rejection session =
  let verdict = Oxsmt_session.check_sat session in
  validate_oxsmt_verdict ?allow_nia_model_rejection session verdict

let check_oxsmt_session_assuming
    ?allow_nia_model_rejection session assumptions =
  let result = Oxsmt_session.check_sat_assuming session assumptions in
  ignore
    (validate_oxsmt_verdict
       ?allow_nia_model_rejection session result.verdict);
  match result.verdict, result.unsat_core with
  | Oxsmt_session.Unsat, _
  | (Oxsmt_session.Sat | Oxsmt_session.Unknown), None -> result
  | (Oxsmt_session.Sat | Oxsmt_session.Unknown), Some _ ->
    failwith "oxsmt returned an assumption core for a non-unsat result"

let inject_oxsmt_core_for_testing fact_assumptions
    (result : Oxsmt_session.assumption_check) =
  match Sys.getenv_opt "VOX_OXSMT_TEST_UNSAT_CORE" with
  | Some "empty" when result.Oxsmt_session.verdict = Oxsmt_session.Unsat ->
    { result with unsat_core = Some [] }
  | Some "non-covering"
    when result.Oxsmt_session.verdict = Oxsmt_session.Unsat ->
    let assumption = snd (List.hd (List.rev fact_assumptions)) in
    { result with unsat_core = Some [assumption] }
  | Some _ | None -> result

let unused_oxsmt_facts session fact_assumptions core =
  let used_facts =
    List.map
      (fun (core_term, core_polarity) ->
        match
          List.find_opt
            (fun (_, (fact_term, fact_polarity)) ->
              Bool.equal core_polarity fact_polarity
              && Oxsmt_term.equal core_term fact_term)
            fact_assumptions
        with
        | Some (index, _) -> index
        | None -> failwith "oxsmt returned an invalid assumption core")
      core
  in
  let unique_used_facts = List.sort_uniq Int.compare used_facts in
  if List.length unique_used_facts <> List.length used_facts then
    failwith "oxsmt returned a duplicate in its assumption core";
  let replay = check_oxsmt_session_assuming session core in
  match replay.verdict with
  | Oxsmt_session.Unsat ->
    List.filter_map
      (fun (index, _) ->
        if List.mem index unique_used_facts then None else Some index)
      fact_assumptions
  | Oxsmt_session.Sat | Oxsmt_session.Unknown ->
    failwith "oxsmt returned an assumption core that does not prove the query"

let oxsmt_timeout_active = Atomic.make false

let with_oxsmt_timeout ~timeout_seconds function_ =
  if not (Atomic.compare_and_set oxsmt_timeout_active false true) then
    failwith "concurrent oxsmt solver interaction";
  Fun.protect
    ~finally:(fun () -> Atomic.set oxsmt_timeout_active false)
    (fun () ->
      let timed_out = ref false in
      let solving = ref false in
      let previous_handler =
        Sys.signal Sys.sigalrm
          (Sys.Signal_handle (fun _ ->
             timed_out := true;
             if !solving then raise Sys.Break))
      in
      let previous_mask = sigalrm_is_blocked () in
      if previous_mask < 0 then begin
        ignore (Sys.signal Sys.sigalrm previous_handler);
        failwith "unable to inspect the SIGALRM mask"
      end;
      let sigalrm_was_blocked = previous_mask = 1 in
      let signal_state_restored = ref false in
      let restore_signal_state () =
        if !signal_state_restored then true
        else begin
          let mask_restored = restore_sigalrm sigalrm_was_blocked in
          ignore (Sys.signal Sys.sigalrm previous_handler);
          signal_state_restored := true;
          mask_restored
        end
      in
      Fun.protect
        ~finally:(fun () -> ignore (restore_signal_state ()))
        (fun () ->
          if not (restore_sigalrm false) then
            failwith "unable to unblock SIGALRM for the oxsmt timeout";
          let previous_alarm = set_alarm timeout_seconds in
          if previous_alarm < 0 then begin
            let signal_state_restored = restore_signal_state () in
            if not signal_state_restored then
              failwith "unable to restore the SIGALRM mask";
            failwith "unable to arm the oxsmt solver timeout"
          end else if previous_alarm > 0 then begin
            let canceled = set_alarm 0 in
            let signal_state_restored = restore_signal_state () in
            let restored = set_alarm previous_alarm in
            if canceled < 0 || not signal_state_restored || restored < 0 then
              failwith "unable to restore a pre-existing process alarm";
            failwith "cannot arm oxsmt timeout while a process alarm is active"
          end;
          let cleaned_up = ref false in
          let cleanup () =
            if not !cleaned_up then begin
              solving := false;
              let canceled = set_alarm 0 in
              let signal_state_restored = restore_signal_state () in
              cleaned_up := true;
              if canceled < 0 || not signal_state_restored then
                failwith "unable to clean up the oxsmt solver timeout"
            end
          in
          Fun.protect
            ~finally:cleanup
            (fun () ->
              try
                with_async_exns (fun () ->
                  solving := true;
                  match function_ () with
                  | result ->
                    cleanup ();
                    if !timed_out then raise Oxsmt_timeout;
                    result
                  | exception exception_ ->
                    let backtrace = Printexc.get_raw_backtrace () in
                    cleanup ();
                    Printexc.raise_with_backtrace exception_ backtrace)
              with
              | Sys.Break when !timed_out -> raise Oxsmt_timeout)))

let solve_oxsmt_query ~query ~env (vc : Vox_vc.t) =
  (match Sys.getenv_opt "VOX_OXSMT_TEST_RAISE" with
   | Some "1" -> failwith "VOX_OXSMT_TEST_RAISE"
   | Some _ | None -> ());
  (match Sys.getenv_opt "VOX_OXSMT_TEST_SPIN" with
   | Some "1" ->
     let rec spin () =
       Gc.minor ();
       spin ()
     in
     spin ()
   | Some _ | None -> ());
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
  let context =
    { env;
      symbol_namespace = "";
      data = [];
      tuples = [];
      references = [];
      unspecified_shifts = [];
      unspecified_divisions = [];
    }
  in
  let variables = collect context expressions in
  check_abstract_inhabitance context variables vc.location;
  check_concrete_inhabitance context vc.location;
  let session = Oxsmt_session.create () in
  let terms = Oxsmt_session.context session in
  let nia_minter =
    if Oxsmt_nia_config.enabled () then
      Some (Oxsmt_session.parse_minter session)
    else None
  in
  let sorts, datatypes =
    oxsmt_declare_datatypes session context vc.location
  in
  let references =
    oxsmt_declare_references session context sorts vc.location
  in
  let variables =
    oxsmt_declare_variables
      session context sorts vc.location variables
  in
  let environment =
    { terms;
      bv_minter =
        Oxsmt_internal_minter.mint (Oxsmt_session.parse_minter session);
      datatypes;
      references;
      variables;
      nia_minter;
      nia_mul_symbol = None;
      nia_products = [];
    }
  in
  let true_term = Oxsmt_context.bool_const terms true in
  let facts =
    List.map
      (fun fact ->
        let term, sort = oxsmt_expression context environment fact in
        expect_bool fact.rexp_loc sort;
        Oxsmt_context.eq terms term true_term)
      facts
  in
  let goal, goal_sort = oxsmt_expression context environment goal in
  expect_bool vc.goal.rexp_loc goal_sort;
  let goal = Oxsmt_context.eq terms goal true_term in
  let nia_lemmas = oxsmt_nia_lemmas environment in
  let allow_nia_model_rejection = environment.nia_products <> [] in
  match query with
  | Prove ->
    let fact_assumptions =
      List.mapi
        (fun index _ ->
          let symbol =
            Oxsmt_session.declare_const session
              ("VoxFact_" ^ string_of_int index)
              Oxsmt_sort.bool
          in
          index, (Oxsmt_context.const terms symbol, true))
        facts
    in
    let guarded_facts =
      List.map2
        (fun fact (_, (selector, _)) ->
          Oxsmt_context.implies terms selector fact)
        facts fact_assumptions
    in
    Oxsmt_session.assert_presolved session
      (guarded_facts @ [Oxsmt_context.not_ terms goal] @ nia_lemmas);
    let result =
      check_oxsmt_session_assuming
        ~allow_nia_model_rejection session
        (List.map snd fact_assumptions)
      |> inject_oxsmt_core_for_testing fact_assumptions
    in
    begin
      match result.verdict, result.unsat_core with
      | Oxsmt_session.Unsat, Some core ->
        result.verdict, unused_oxsmt_facts session fact_assumptions core
      | Oxsmt_session.Unsat, None -> result.verdict, []
      | (Oxsmt_session.Sat | Oxsmt_session.Unknown), None ->
        result.verdict, []
      | (Oxsmt_session.Sat | Oxsmt_session.Unknown), Some _ ->
        failwith "oxsmt returned an assumption core for a non-unsat result"
    end
  | Disprove ->
    Oxsmt_session.assert_presolved session (facts @ [goal] @ nia_lemmas);
    check_oxsmt_session ~allow_nia_model_rejection session, []

let oxsmt_query_result ~query ~timeout_seconds ~env vc =
  try
    let verdict, unused_facts =
      with_oxsmt_timeout ~timeout_seconds (fun () ->
        solve_oxsmt_query ~query ~env vc)
    in
    match verdict with
    | Oxsmt_session.Unsat ->
      begin
        match query with
        | Prove -> `Final (Proved, None, unused_facts)
        | Disprove -> `Final (Disproved, None, [])
      end
    | Oxsmt_session.Sat -> `Open "sat"
    | Oxsmt_session.Unknown -> `Open "unknown"
  with
  | Oxsmt_timeout ->
    `Final (Solver_error, Some "oxsmt solver timed out", [])
  | Oxsmt_unsupported _
  | Oxsmt_term.Overflow
  | Oxsmt_term.Unsupported _ -> `Open "unknown"

let emit_datatypes context location buffer =
  let abstract_data, concrete_data =
    List.partition
      (fun data ->
        match definition location data with
        | Abstract -> true
        | Variant _ | Record _ -> false)
      context.data
  in
  List.sort
    (fun left right -> String.compare left.data_key right.data_key)
    abstract_data
  |> List.iter (fun data ->
       Buffer.add_string buffer
         ("(declare-sort " ^ data.data_name ^ " 0)\n"));
  let shapes =
    List.map tuple_shape context.tuples
    @ List.map (data_shape location) concrete_data
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

let emit_references (context : context) location buffer =
  List.sort_uniq String.compare
    (context.unspecified_shifts @ context.unspecified_divisions)
  |> List.iter (fun name ->
       Buffer.add_string buffer
         ("(declare-fun " ^ name
          ^ " ((_ BitVec 63) (_ BitVec 63)) (_ BitVec 63))\n"));
  List.sort
    (fun (left : reference) (right : reference) ->
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

(* [model] turns the prove query into a request for the assignment that
   makes it satisfiable.  That assignment is the counterexample to the
   obligation, and is only asked for once a verdict is already known, so the
   ordinary query text -- which is what the cache is keyed on -- is
   unchanged. *)
let emit_internal ?(symbol_namespace = "") ?(model = false) ~query ~env
    (vc : Vox_vc.t) =
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
  let context =
    { env;
      symbol_namespace;
      data = [];
      tuples = [];
      references = [];
      unspecified_shifts = [];
      unspecified_divisions = [];
    }
  in
  let variables = collect context expressions in
  check_abstract_inhabitance context variables vc.location;
  check_concrete_inhabitance context vc.location;
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
  begin
    match query with
    | Prove ->
      Buffer.add_string buffer
        (if model
         then "(set-option :produce-models true)\n"
         else "(set-option :produce-unsat-cores true)\n")
    | Disprove -> ()
  end;
  emit_datatypes context vc.location buffer;
  emit_references context vc.location buffer;
  emit_variables context vc.location buffer variables;
  let emitted_facts =
    List.mapi
      (fun index fact ->
        { selector = "h_" ^ string_of_int index;
          term = "(= " ^ fact ^ " true)";
        })
      fact_terms
  in
  List.iter
    (fun fact ->
      Buffer.add_string buffer
        (if model
         then "(assert " ^ fact.term ^ ")\n"
         else
           "(assert (! " ^ fact.term ^ " :named " ^ fact.selector ^ "))\n"))
    emitted_facts;
  let query_term =
    match query with
    | Prove -> "(not (= " ^ goal_term ^ " true))"
    | Disprove -> "(= " ^ goal_term ^ " true)"
  in
  Buffer.add_string buffer ("(assert " ^ query_term ^ ")\n");
  Buffer.add_string buffer "(check-sat)\n";
  begin
    match query with
    | Prove ->
      Buffer.add_string buffer
        (if model then "(get-model)\n" else "(get-unsat-core)\n")
    | Disprove -> ()
  end;
  { contents = Buffer.contents buffer;
    facts = emitted_facts;
    goal = query_term;
  }

let emit_query_with_namespace ?symbol_namespace ?model ~query ~env
    (vc : Vox_vc.t) =
  try Ok (emit_internal ?symbol_namespace ?model ~query ~env vc) with
  | Emission_error error -> Error error
  | exception_ ->
    Error
      { location = vc.location;
        message = Printexc.to_string exception_;
      }

let emit_with_namespace ?symbol_namespace ?model ~query ~env vc =
  Result.map
    (fun query -> query.contents)
    (emit_query_with_namespace ?symbol_namespace ?model ~query ~env vc)

let emit_query ~query ~env vc =
  emit_query_with_namespace ~query ~env vc

let emit ~query ~env vc =
  emit_with_namespace ~query ~env vc

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

let noninteractive_commands = Hashtbl.create 7

let command_uses_shell_helper command =
  let command = String.trim command in
  let starts_with prefix =
    let length = String.length prefix in
    String.length command >= length
    && String.equal (String.sub command 0 length) prefix
  in
  starts_with "/bin/sh " || starts_with "sh -c "

let persistent_unsat_core_option =
  "(set-option :produce-unsat-cores true)\n"

let persistent_query_counter = Atomic.make 0

let fresh_persistent_symbol_namespace () =
  "q" ^ string_of_int (Atomic.fetch_and_add persistent_query_counter 1)

let persistent_z3_contents contents =
  let prefix_length = String.length persistent_unsat_core_option in
  if
    String.length contents >= prefix_length
    && String.equal
         (String.sub contents 0 prefix_length)
         persistent_unsat_core_option
  then
    String.sub contents prefix_length (String.length contents - prefix_length)
  else
    contents

let run_z3_solver ~command ~input_mode ~timeout_seconds contents =
  match input_mode with
  | File_argument ->
    run_solver ~command ~input_mode ~timeout_seconds contents
  | Stdin
    when command_uses_shell_helper command
         || Hashtbl.mem noninteractive_commands command ->
    run_solver ~command ~input_mode ~timeout_seconds contents
  | Stdin ->
    let status, output =
      run_persistent_z3 command timeout_seconds (persistent_z3_contents contents)
    in
    if status = -1 then begin
      Hashtbl.replace noninteractive_commands command ();
      run_solver ~command ~input_mode ~timeout_seconds contents
    end else
      { status; output }

let line_starts_with prefix line =
  let prefix_length = String.length prefix in
  String.length line >= prefix_length
  && String.equal (String.sub line 0 prefix_length) prefix

let contains_substring text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.sub text index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let is_error_line line =
  line_starts_with "(error" line || line_starts_with "error" line

let is_unavailable_core_error ~query ~status line =
  is_error_line line
  &&
  match query, status with
  | Prove, (Sat | Unknown) ->
    contains_substring line "unsat core is not available"
  | (Prove, Unsat) | Disprove, _ -> false

let output_has_unavailable_core_error ~query ~status output =
  String.split_on_char '\n' output
  |> List.exists (fun line ->
       is_unavailable_core_error ~query ~status (String.trim line))

(* The model in a solver reply: the complete parenthesised expression that
   opens it, with the contents of string literals passed over so a bracket
   inside one cannot close it early.  Scanning for the model itself rather
   than for the first bracket anywhere matters, because a banner is exactly
   the sort of thing that carries brackets of its own. *)
let contains_substring text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.equal (String.sub text index needle_length) needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let balanced_expression output =
  let length = String.length output in
  let opening = "(model" in
  let opening_length = String.length opening in
  let rec start index =
    if index + opening_length > length then None
    else if String.equal (String.sub output index opening_length) opening
    then Some index
    else start (index + 1)
  in
  match start 0 with
  | None -> None
  | Some first ->
    let rec walk index depth in_string =
      if index >= length then None
      else
        match output.[index] with
        | '"' -> walk (index + 1) depth (not in_string)
        | _ when in_string -> walk (index + 1) depth in_string
        | '(' -> walk (index + 1) (depth + 1) in_string
        | ')' when depth <= 1 ->
          Some (String.sub output first (index - first + 1))
        | ')' -> walk (index + 1) (depth - 1) in_string
        | _ -> walk (index + 1) depth in_string
    in
    walk first 0 false

let output_has_error ~query ~status output =
  String.split_on_char '\n' output
  |> List.exists (fun line ->
       let line = String.trim line in
       is_error_line line
       && not (is_unavailable_core_error ~query ~status line))

let detail_or fallback output =
  if String.equal (String.trim output) "" then fallback else output

let fact_id name =
  let prefix = "h_" in
  let prefix_length = String.length prefix in
  let length = String.length name in
  if length <= prefix_length || not (line_starts_with prefix name) then None
  else
    let rec all_digits index =
      if index = length then true
      else
        match name.[index] with
        | '0' .. '9' -> all_digits (index + 1)
        | _ -> false
    in
    if all_digits prefix_length then
      int_of_string_opt
        (String.sub name prefix_length (length - prefix_length))
    else None

let parse_unsat_core ~fact_count output =
  let payload =
    String.split_on_char '\n' output
    |> List.filter_map (fun line ->
         let line = String.trim line in
         match line with
         | "" | "sat" | "unsat" | "unknown" -> None
         | _ -> Some line)
    |> String.concat " "
    |> String.trim
  in
  let length = String.length payload in
  if length < 2 || payload.[0] <> '(' || payload.[length - 1] <> ')'
  then None
  else
    let contents = String.sub payload 1 (length - 2) in
    if String.contains contents '(' || String.contains contents ')' then None
    else
      let names =
        String.map
          (function
            | '\t' | '\r' -> ' '
            | character -> character)
          contents
        |> String.split_on_char ' '
        |> List.filter (fun name -> not (String.equal name ""))
      in
      let ids = List.map fact_id names in
      if
        List.exists Option.is_none ids
        || List.exists
             (function
               | Some id -> id < 0 || id >= fact_count
               | None -> false)
             ids
      then None
      else
        let used = List.filter_map (fun id -> id) ids in
        Some
          (List.init fact_count Fun.id
           |> List.filter (fun id -> not (List.mem id used)))

let solver_result ~backend ~query ~fact_count process =
  if process.status = 127 then
    `Final
      ( Unavailable,
        Some (detail_or "solver command unavailable (exit 127)" process.output),
        []
      )
  else if process.status = 124 || process.status = 137 then
    `Final
      ( Solver_error,
        Some (detail_or "solver timed out" process.output),
        [] )
  else if
    backend = `Oxsmt && process.status = oxsmt_unsupported_input_exit_code
  then `Open "unknown"
  else
    let status = parse_status process.output in
    let expected_unavailable_core_exit =
      match status with
      | Some status ->
        process.status = 1
        && output_has_unavailable_core_error ~query ~status process.output
        && not (output_has_error ~query ~status process.output)
      | None -> false
    in
    if process.status <> 0 && not expected_unavailable_core_exit then
      `Final
        ( Solver_error,
          Some
            (detail_or
               ("solver exited " ^ string_of_int process.status)
               process.output),
          [] )
    else
      match status with
      | None -> `Final (Solver_error, Some process.output, [])
      | Some status when output_has_error ~query ~status process.output ->
        `Final (Solver_error, Some process.output, [])
      | Some Unsat ->
        begin
          match query with
          | Prove ->
            begin
              match parse_unsat_core ~fact_count process.output with
              | Some unused_facts ->
                `Final (Proved, None, unused_facts)
              | None ->
                `Final
                  ( Solver_error,
                    Some
                      ("solver returned unsat without a valid unsat core:\n"
                       ^ process.output),
                    [] )
            end
          | Disprove -> `Final (Disproved, None, [])
        end
      | Some Sat -> `Open "sat"
      | Some Unknown -> `Open "unknown"

let backend_name = function
  | `Z3 -> "z3"
  | `Oxsmt -> "oxsmt"

let default_input_mode = function
  | `Z3 | `Oxsmt -> Stdin

let discharge_oxsmt ?(timeout_seconds = 30) ~env (vc : Vox_vc.t) =
  let result verdict ?detail ?(unused_facts = []) () =
    { verdict; location = vc.location; detail; unused_facts }
  in
  if timeout_seconds <= 0 then
    result Solver_error ~detail:"timeout must be positive" ()
  else
    try
      match oxsmt_query_result ~query:Prove ~timeout_seconds ~env vc with
      | `Final (verdict, detail, unused_facts) ->
        result verdict ?detail ~unused_facts ()
      | `Open positive_status ->
        begin
          match
            oxsmt_query_result
              ~query:Disprove ~timeout_seconds ~env vc
          with
          | `Final (verdict, detail, unused_facts) ->
            result verdict ?detail ~unused_facts ()
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
        unused_facts = [];
      }
    | exception_ ->
      result Solver_error ~detail:(Printexc.to_string exception_) ()

let discharge ~backend ~command ?prove_contents ?input_mode
    ?(timeout_seconds = 30) ~env (vc : Vox_vc.t) =
  let result verdict ?detail ?(unused_facts = []) () =
    { verdict; location = vc.location; detail; unused_facts }
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
        let emitted =
          match query, prove_contents with
          | Prove, Some contents -> Ok contents
          | (Prove | Disprove), _ ->
            let symbol_namespace =
              match backend with
              | `Z3 -> Some (fresh_persistent_symbol_namespace ())
              | `Oxsmt -> None
            in
            emit_with_namespace ?symbol_namespace ~query ~env vc
        in
        match emitted with
        | Error emission_error -> raise (Emission_error emission_error)
        | Ok contents ->
          let process =
            match backend, prove_contents with
            | `Z3, None ->
              run_z3_solver ~command ~input_mode ~timeout_seconds contents
            | (`Z3, Some _) | (`Oxsmt, _) ->
              run_solver ~command ~input_mode ~timeout_seconds contents
          in
          solver_result ~backend ~query ~fact_count:(List.length vc.facts)
            process
      in
      (* A disproved obligation is one whose goal no state satisfies, and the
         assignment that refutes it is exactly what the prove query already
         found satisfiable.  That assignment was never asked for, so ask for
         it now, once, and only where there is one to report. *)
      let counterexample () =
        match backend with
        | `Oxsmt -> None
        | `Z3 ->
          (* The verdict is already settled, so nothing this query does may
             change it.  Running a solver writes temporary files, which the
             ordinary z3 path does not, so a read-only temporary directory
             would otherwise turn a refuted obligation into a solver error. *)
          try
          (* No namespace suffix.  It exists to keep symbols apart inside
             the shared persistent session; this query has a process to
             itself, and a suffix would only make the reported names disagree
             with the ones in the dumped query. *)
          let emitted =
            emit_with_namespace ~model:true ~symbol_namespace:"" ~query:Prove
              ~env vc
          in
          begin
            match emitted with
            | Error _ -> None
            | Ok contents ->
              (* A fresh process rather than the persistent one: asking for
                 models is a session-wide option, and this query is rare
                 enough that it should not change how every later obligation
                 is solved. *)
              let process =
                run_solver ~command ~input_mode ~timeout_seconds contents
              in
              (* Judge the reply by the same rules as any other: a clean
                 exit, one unambiguous status, and no error line.  A reply
                 that is any of failed, error-tainted or self-contradictory
                 is not an assignment, and reporting it would present an
                 untrustworthy diagnostic as the values that refute the
                 obligation.  Reading the status rather than the first line
                 also lets a solver print a banner. *)
              let status = parse_status process.output in
              if
                process.status <> 0
                || status <> Some Sat
                || output_has_error ~query:Prove ~status:Sat process.output
              then None
              else
                (* Take one complete parenthesised reply rather than every
                   remaining line, so a trailing status or note cannot be
                   read as part of the assignment. *)
                match balanced_expression process.output with
                | None -> None
                | Some assignment ->
                  (* An assignment that names nothing is not one, and a
                     solver omits whatever it did not need.  Cutting a long
                     one is worse than reporting none: the values come out
                     in reverse order of declaration, so the fragment that
                     survives is the one least likely to mention what the
                     goal is about. *)
                  let limit = 4000 in
                  if
                    not
                      (contains_substring assignment "define-fun"
                       && String.length assignment <= limit)
                  then None
                  else Some ("counterexample:\n" ^ assignment)
          end
          with _ -> None
      in
      begin
        try
          match run Prove with
          | `Final (verdict, detail, unused_facts) ->
            result verdict ?detail ~unused_facts ()
          | `Open positive_status ->
            begin
              match run Disprove with
              | `Final (Disproved, None, unused_facts) ->
                result Disproved ?detail:(counterexample ()) ~unused_facts ()
              | `Final (verdict, detail, unused_facts) ->
                result verdict ?detail ~unused_facts ()
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
          unused_facts = [];
        }
      | exception_ ->
        result Solver_error ~detail:(Printexc.to_string exception_) ()
      end
