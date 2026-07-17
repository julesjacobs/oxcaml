(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Verification-condition generation for refinements         *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Types
open Data_types

module Facts = Vox_vc.Fact_env

type definition =
  { id : Ident.t;
    parameters : Ident.t list;
    type_ : type_expr;
  }

type state =
  { mutable facts : Facts.t;
    mutable definitions : definition list;
  }

exception Unsupported_subject of Location.t * string

let carrier type_ =
  match get_desc type_ with
  | Trefine refinement -> refinement.ref_skeleton
  | _ -> type_

let rec refinement type_ =
  match get_desc type_ with
  | Trefine refinement -> Some refinement
  | Tpoly (type_, _) -> refinement type_
  | _ -> None

let node expression desc =
  Refinement.create ~loc:expression.exp_loc ~type_:(carrier expression.exp_type)
    desc

let bool_node ~loc value =
  Refinement.create ~loc ~type_:Predef.type_bool
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_bool;
           rconstr_name = if value then "true" else "false";
         },
         [] ))

let unsupported expression what =
  raise (Unsupported_subject (expression.exp_loc, what))

let pattern_variable pattern =
  match pattern.pat_desc with
  | Tpat_var { id; _ } -> Some id
  | Tpat_alias { id; _ } -> Some id
  | _ -> None

let parameter_variable parameter =
  match parameter.fp_kind with
  | Tparam_pat pattern
  | Tparam_optional_default (pattern, _, _) -> pattern_variable pattern

let rec definition_parameters expression =
  match expression.exp_desc with
  | Texp_function { params; body; _ } ->
    let here = List.filter_map parameter_variable params in
    let later =
      match body with
      | Tfunction_body body -> definition_parameters body
      | Tfunction_cases _ -> []
    in
    here @ later
  | _ -> []

let contains_refinement type_ =
  with_type_mark (fun mark ->
    let found = ref false in
    let rec visit type_ =
      if not !found && try_mark_node mark type_ then
        match get_desc type_ with
        | Trefine _ -> found := true
        | _ -> Btype.iter_type_expr visit type_
    in
    visit type_;
    !found)

let register_definition state binding =
  match pattern_variable binding.vb_pat with
  | None -> ()
  | Some id ->
    let parameters = definition_parameters binding.vb_expr in
    if parameters <> [] && contains_refinement binding.vb_pat.pat_type then
      state.definitions <-
        { id; parameters; type_ = binding.vb_pat.pat_type }
        :: state.definitions

let find_definition state = function
  | Path.Pident id ->
    List.find_opt
      (fun definition -> Ident.same definition.id id)
      state.definitions
  | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> None

let rec subject state ?(function_head = false) expression =
  let lower = subject state in
  match expression.exp_desc with
  | Texp_ident { path = Pident id; _ } when Facts.in_scope id state.facts ->
    node expression (Rexp_ident (Rbound id))
  | Texp_ident { path; _ } ->
    let head = if function_head then Rapp path else Rglobal path in
    node expression (Rexp_ident (Rfree head))
  | Texp_constant constant -> node expression (Rexp_constant constant)
  | Texp_let (Nonrecursive, bindings, body) ->
    let lowered, bound =
      List.fold_left
        (fun (lowered, bound) binding ->
          match pattern_variable binding.vb_pat with
          | None -> unsupported expression "a non-variable let pattern"
          | Some id ->
            let binder = { rb_id = id; rb_type = binding.vb_pat.pat_type } in
            let binding =
              { rbind_binder = binder;
                rbind_expr =
                  subject { state with facts = bound } binding.vb_expr;
              }
            in
            binding :: lowered, Facts.enter id bound)
        ([], state.facts) bindings
    in
    let body = subject { state with facts = bound } body in
    node expression (Rexp_let (List.rev lowered, body))
  | Texp_let (Recursive, _, _) ->
    unsupported expression "a recursive let expression"
  | Texp_function
      { params = [parameter]; body = Tfunction_body body; _ } ->
    begin match parameter.fp_kind with
    | Tparam_pat pattern ->
      begin match pattern_variable pattern with
      | Some id ->
        let param = { rb_id = id; rb_type = pattern.pat_type } in
        let facts = Facts.enter id state.facts in
        let body = subject { state with facts } body in
        node expression
          (Rexp_function { arg_label = parameter.fp_arg_label; param; body })
      | None -> unsupported expression "a non-variable function parameter"
      end
    | Tparam_optional_default _ ->
      unsupported expression "an optional function parameter"
    end
  | Texp_function _ ->
    unsupported expression "a multi-parameter or case function"
  | Texp_apply (function_, arguments, _, _, _) ->
    let function_ = subject state ~function_head:true function_ in
    let arguments =
      List.map
        (function
          | label, Arg (argument, _) -> label, lower argument
          | _, Omitted _ -> unsupported expression "an omitted argument")
        arguments
    in
    node expression (Rexp_apply (function_, arguments))
  | Texp_tuple (fields, _) ->
    node expression
      (Rexp_tuple (List.map (fun (label, field) -> label, lower field) fields))
  | Texp_construct (_, constructor, _, arguments, _) ->
    let constructor =
      { rconstr_type_path = cstr_res_type_path constructor;
        rconstr_name = constructor.cstr_name;
      }
    in
    node expression
      (Rexp_construct
         ( constructor,
           List.map (fun (_, argument) -> lower argument) arguments ))
  | Texp_field { record; label; _ } when label.lbl_mut = Immutable ->
    let field =
      { rfield_type_path = lbl_res_type_path label;
        rfield_name = label.lbl_name;
      }
    in
    node expression (Rexp_field (lower record, field))
  | Texp_ifthenelse (condition, ifso, ifnot) ->
    node expression
      (Rexp_ifthenelse
         (lower condition, lower ifso, Option.map lower ifnot))
  | Texp_sequence _ -> unsupported expression "a sequence"
  | Texp_mutvar _ -> unsupported expression "a mutable variable"
  | _ -> unsupported expression "this expression form"

let same_parameter_reference id name =
  String.equal (Ident.name id) name

let rec replace_parameters replacements expression =
  let replace = replace_parameters replacements in
  let replace_reference reference =
    List.find_map
      (fun (id, replacement) ->
        match reference with
        | Rglobal (Pident other) | Rapp (Pident other)
          when Ident.same id other -> Some replacement
        | Rsibling name when same_parameter_reference id name ->
          Some replacement
        | Rfun _ | Rsibling _ | Rglobal _ | Rapp _ -> None)
      replacements
  in
  match expression.rexp_desc with
  | Rexp_ident (Rfree reference) ->
    Option.value (replace_reference reference) ~default:expression
  | Rexp_ident _ | Rexp_constant _ -> expression
  | Rexp_let (bindings, body) ->
    { expression with
      rexp_desc =
        Rexp_let
          ( List.map
              (fun binding ->
                { binding with rbind_expr = replace binding.rbind_expr })
              bindings,
            replace body );
    }
  | Rexp_function function_ ->
    { expression with
      rexp_desc =
        Rexp_function { function_ with body = replace function_.body };
    }
  | Rexp_apply (function_, arguments) ->
    { expression with
      rexp_desc =
        Rexp_apply
          (replace function_,
           List.map
             (fun (label, argument) -> label, replace argument)
             arguments);
    }
  | Rexp_tuple fields ->
    { expression with
      rexp_desc =
        Rexp_tuple
          (List.map (fun (label, field) -> label, replace field) fields);
    }
  | Rexp_construct (constructor, arguments) ->
    { expression with
      rexp_desc = Rexp_construct (constructor, List.map replace arguments);
    }
  | Rexp_field (record, field) ->
    { expression with rexp_desc = Rexp_field (replace record, field) }
  | Rexp_ifthenelse (condition, ifso, ifnot) ->
    { expression with
      rexp_desc =
        Rexp_ifthenelse
          (replace condition, replace ifso, Option.map replace ifnot);
    }

let verification_error ~loc verdict =
  Location.raise_errorf ~loc "Refinement verification failed (%s)"
    (Vox_lean.string_of_verdict verdict)

let prove state ~env ~loc goal =
  match Facts.snapshot ~loc ~goal state.facts with
  | Error { escaped; _ } ->
    Location.raise_errorf ~loc
      "Refinement verification failed: goal mentions out-of-scope value%s %s"
      (if List.length escaped = 1 then "" else "s")
      (String.concat ", " (List.map Ident.name escaped))
  | Ok condition ->
    let result = Vox_lean.discharge ~env condition in
    begin match result.verdict with
    | Vox_lean.Proved -> state.facts <- Facts.add ~loc goal state.facts
    | (Not_proved | Disproved | Solver_error) as verdict ->
      verification_error ~loc verdict
    end

let prove_refinement state ~env ~loc ~subject refinement replacements =
  let goal = Vox_vc.instantiate ~refinement ~with_:subject in
  let goal = replace_parameters replacements goal in
  prove state ~env ~loc goal

let verify_seal_obligation ~env ~seal_location
    (obligation : Ctype.refinement_seal_obligation) =
  let subject_id = Ident.create_local "_seal_value" in
  let subject =
    Refinement.create ~loc:seal_location ~type_:obligation.rso_skeleton
      (Rexp_ident (Rbound subject_id))
  in
  let hypothesis =
    Vox_vc.instantiate ~refinement:obligation.rso_hypothesis ~with_:subject
  in
  let goal =
    Vox_vc.instantiate ~refinement:obligation.rso_conclusion ~with_:subject
  in
  let condition =
    Vox_vc.create ~loc:seal_location
      ~facts:
        [{ Vox_vc.expression = hypothesis;
           location = Some obligation.rso_implementation_location;
         }]
      ~goal
  in
  let result = Vox_lean.discharge ~env condition in
  match result.verdict with
  | Vox_lean.Proved -> ()
  | (Not_proved | Disproved | Solver_error) as verdict ->
    let sub =
      [ Location.msg ~loc:obligation.rso_interface_location
          "Interface declaration for value %s"
          obligation.rso_value_name;
        Location.msg ~loc:obligation.rso_implementation_location
          "Implementation declaration for value %s"
          obligation.rso_value_name;
      ]
    in
    Location.raise_errorf ~loc:seal_location ~sub
      "Refinement verification failed at module seal for value %S (%s)"
      obligation.rso_value_name (Vox_lean.string_of_verdict verdict)

let verify_seal_obligations ~env ~seal_location obligations =
  List.iter (verify_seal_obligation ~env ~seal_location) obligations

let marked_refinements expression =
  List.filter_map
    (fun (extra, loc, _) ->
      match extra with
      | Texp_constraint core_type ->
        Option.map (fun refinement -> loc, refinement)
          (refinement core_type.ctyp_type)
      | Texp_coerce _ | Texp_poly _ | Texp_newtype _ | Texp_stack
      | Texp_mode _ | Texp_inspected_type _ | Texp_borrowed
      | Texp_ghost_region
        -> None)
    expression.exp_extra

(* Binder facts are recorded UNCONDITIONALLY (no purity gate), unlike the
   branch-condition facts below.  The asymmetry is sound and deliberate
   (SHOULD-1 ruling): a refined binder's predicate is a PROVEN contract --
   discharged as an obligation at the value's definition -- so the fact is a
   property of the specific value now bound to the identifier, not a claim about
   re-evaluating an expression.  Re-reading that identifier yields the same
   value, so the fact stays valid however impure the surrounding code.  A branch
   condition, by contrast, records a fact about an *expression's* value, which
   only stays valid across occurrences when the expression is pure -- hence the
   [condition_is_total] gate below. *)
let rec enter_pattern
    : type k. state -> fact:bool -> k general_pattern -> unit =
  fun state ~fact pattern ->
  match pattern.pat_desc with
  | Tpat_var { id; _ } ->
    state.facts <- Facts.enter id state.facts;
    if fact then
      Option.iter
        (fun refinement ->
          let with_ =
            Refinement.create ~loc:pattern.pat_loc
              ~type_:(carrier pattern.pat_type) (Rexp_ident (Rbound id))
          in
          let expression = Vox_vc.instantiate ~refinement ~with_ in
          state.facts <- Facts.add ~loc:pattern.pat_loc expression state.facts)
        (refinement pattern.pat_type)
  | Tpat_alias { pattern; id; _ } ->
    enter_pattern state ~fact pattern;
    state.facts <- Facts.enter id state.facts
  | _ -> ()

(* Q-003 purity gate for branch-condition facts.  A condition fact is stable
   across occurrences -- and so sound to identify structurally in the
   verification condition -- only when the condition is a deterministic,
   side-effect-free function of immutable variables: a conservative syntactic
   total form built from constants, identifiers, and applications of the
   total/pure builtins (comparisons, integer arithmetic, boolean connectives)
   recognized by [Vox_lean.primitive_builtin].  An application of any other
   function is an opaque, possibly impure call, which makes the condition
   non-total, so no fact is recorded: otherwise a fact about one evaluation of
   [f ()] could discharge an obligation about a different evaluation of the same
   syntactic call. *)
let rec condition_is_total expression =
  match expression.exp_desc with
  | Texp_constant _ | Texp_ident _ -> true
  | Texp_apply (function_, arguments, _, _, _) ->
    total_builtin_head function_
    && List.for_all
         (function
           | _, Arg (argument, _) -> condition_is_total argument
           | _, Omitted _ -> false)
         arguments
  | _ -> false

and total_builtin_head expression =
  match expression.exp_desc with
  | Texp_ident { desc = { val_kind = Val_prim primitive; _ }; _ } ->
    Option.is_some (Vox_lean.primitive_builtin primitive.prim_name)
  | _ -> false

let rec walk_expression state expression =
  let marks = marked_refinements expression in
  match expression.exp_desc with
  | Texp_let (rec_flag, bindings, body) ->
    let saved_facts = state.facts in
    let saved_definitions = state.definitions in
    if rec_flag = Recursive then begin
      List.iter
        (enter_pattern state ~fact:false)
        (List.map (fun binding -> binding.vb_pat) bindings);
      List.iter (register_definition state) bindings
    end;
    List.iter (fun binding -> walk_expression state binding.vb_expr) bindings;
    if rec_flag = Nonrecursive then
      List.iter (register_definition state) bindings;
    List.iter
      (enter_pattern state ~fact:true)
      (List.map (fun binding -> binding.vb_pat) bindings);
    walk_expression state body;
    state.facts <- saved_facts;
    state.definitions <- saved_definitions;
    check_marks state expression marks
  | Texp_letmutable (binding, body) ->
    walk_expression state binding.vb_expr;
    let saved_facts = state.facts in
    enter_pattern state ~fact:false binding.vb_pat;
    walk_expression state body;
    state.facts <- saved_facts;
    check_marks state expression marks
  | Texp_function { params; body; _ } ->
    let saved_facts = state.facts in
    List.iter
      (fun parameter ->
        match parameter.fp_kind with
        | Tparam_pat pattern -> enter_pattern state ~fact:true pattern
        | Tparam_optional_default (pattern, default, _) ->
          walk_expression state default;
          enter_pattern state ~fact:true pattern)
      params;
    begin match body with
    | Tfunction_body body -> walk_expression state body
    | Tfunction_cases cases ->
      List.iter (walk_case state) cases.fc_cases
    end;
    state.facts <- saved_facts;
    check_marks state expression marks
  | Texp_apply (function_, arguments, _, _, _) ->
    walk_expression state function_;
    List.iter
      (function
        | _, Arg (argument, _) -> walk_expression state argument
        | _, Omitted _ -> ())
      arguments;
    check_application state expression function_ arguments;
    check_marks state expression marks
  | Texp_ifthenelse (condition, ifso, ifnot) ->
    walk_expression state condition;
    (* Record the condition (and, in the else branch, its negation) as a
       branch-local fact around every [if], not only those carrying a
       refinement mark of their own: obligations nested in a guarded branch
       depend on the guard.  A condition that cannot be lowered contributes no
       fact, which only weakens the branch conditions. *)
    let condition_fact =
      if condition_is_total condition then
        match subject state condition with
        | condition_subject -> Some condition_subject
        | exception Unsupported_subject _ -> None
      else None
    in
    let saved_facts = state.facts in
    Option.iter
      (fun condition_subject ->
        state.facts <-
          Facts.add ~loc:condition.exp_loc condition_subject state.facts)
      condition_fact;
    walk_expression state ifso;
    List.iter
      (fun (loc, refinement) ->
        prove_refinement state ~env:expression.exp_env ~loc
          ~subject:(subject state ifso) refinement [])
      marks;
    state.facts <- saved_facts;
    begin match ifnot with
    | None -> ()
    | Some ifnot ->
      Option.iter
        (fun condition_subject ->
          let negated =
            Refinement.create ~loc:condition.exp_loc ~type_:Predef.type_bool
              (Rexp_ifthenelse
                 ( condition_subject,
                   bool_node ~loc:condition.exp_loc false,
                   Some (bool_node ~loc:condition.exp_loc true) ))
          in
          state.facts <- Facts.add ~loc:condition.exp_loc negated state.facts)
        condition_fact;
      walk_expression state ifnot;
      List.iter
        (fun (loc, refinement) ->
          prove_refinement state ~env:expression.exp_env ~loc
            ~subject:(subject state ifnot) refinement [])
        marks;
      state.facts <- saved_facts
    end
  | _ ->
    walk_default_expression state expression;
    check_marks state expression marks

and check_marks state expression marks =
  List.iter
    (fun (loc, refinement) ->
      let subject = subject state expression in
      prove_refinement state ~env:expression.exp_env ~loc ~subject refinement
        [])
    marks

and walk_case : type k. state -> k case -> unit =
  fun state case ->
  let saved_facts = state.facts in
  enter_pattern state ~fact:true case.c_lhs;
  Option.iter (walk_expression state) case.c_guard;
  walk_expression state case.c_rhs;
  state.facts <- saved_facts

and check_application state application function_ arguments =
  let definition =
    match function_.exp_desc with
    | Texp_ident { path; _ } -> find_definition state path
    | _ -> None
  in
  let parameters, function_type =
    match definition with
    | None -> [], function_.exp_type
    | Some definition -> definition.parameters, definition.type_
  in
  let rec loop type_ parameters replacements = function
    | [] -> type_, replacements
    | (_, argument) :: arguments ->
      let type_ =
        match get_desc type_ with
        | Tpoly (type_, _) -> type_
        | _ -> type_
      in
      begin match get_desc type_ with
      | Tarrow (_, domain, result, _) ->
        let parameter, parameters =
          match parameters with
          | [] -> None, []
          | parameter :: parameters -> Some parameter, parameters
        in
        let replacements =
          match argument with
          | Omitted _ -> replacements
          | Arg (argument, _) ->
            begin match refinement domain, parameter with
            | None, None -> replacements
            | domain_refinement, parameter ->
              let argument_subject = subject state argument in
              Option.iter
                (fun refinement ->
                  prove_refinement state ~env:application.exp_env
                    ~loc:argument.exp_loc ~subject:argument_subject refinement
                    replacements)
                domain_refinement;
              begin match parameter with
              | None -> replacements
              | Some parameter ->
                (parameter, argument_subject) :: replacements
              end
            end
        in
        loop result parameters replacements arguments
      | _ -> type_, replacements
      end
  in
  let result_type, replacements =
    loop function_type parameters [] arguments
  in
  Option.iter
    (fun refinement ->
      let result_subject = subject state application in
      let fact = Vox_vc.instantiate ~refinement ~with_:result_subject in
      let fact = replace_parameters replacements fact in
      state.facts <- Facts.add ~loc:application.exp_loc fact state.facts)
    (refinement result_type)

and walk_default_expression state expression =
  let super = Tast_iterator.default_iterator in
  let iterator = iterator state in
  super.expr iterator expression

and iterator state =
  let super = Tast_iterator.default_iterator in
  { super with
    expr = (fun _ expression -> walk_expression state expression);
    structure = (fun _ structure -> walk_structure state structure);
    value_bindings =
      (fun _ (rec_flag, bindings) ->
        walk_value_bindings state ~persist:true rec_flag bindings);
  }

and walk_value_bindings state ~persist rec_flag bindings =
  let saved_facts = state.facts in
  let saved_definitions = state.definitions in
  if rec_flag = Recursive then begin
    List.iter
      (fun binding -> enter_pattern state ~fact:false binding.vb_pat)
      bindings;
    List.iter (register_definition state) bindings
  end;
  List.iter (fun binding -> walk_expression state binding.vb_expr) bindings;
  if rec_flag = Nonrecursive then
    List.iter (register_definition state) bindings;
  List.iter
    (fun binding -> enter_pattern state ~fact:true binding.vb_pat)
    bindings;
  if not persist then begin
    state.facts <- saved_facts;
    state.definitions <- saved_definitions
  end

and walk_structure state structure =
  let saved_facts = state.facts in
  let saved_definitions = state.definitions in
  let iterator = iterator state in
  List.iter
    (Tast_iterator.default_iterator.structure_item iterator)
    structure.str_items;
  state.facts <- saved_facts;
  state.definitions <- saved_definitions

let toplevel_facts = ref Facts.empty
let toplevel_definitions = ref []

let verify_structure ?(toplevel = false) structure =
  let state =
    if toplevel
    then { facts = !toplevel_facts; definitions = !toplevel_definitions }
    else { facts = Facts.empty; definitions = [] }
  in
  let walk_root () =
    let iterator = iterator state in
    List.iter
      (Tast_iterator.default_iterator.structure_item iterator)
      structure.str_items;
    if toplevel then begin
      toplevel_facts := state.facts;
      toplevel_definitions := state.definitions
    end
  in
  try walk_root () with
  | Unsupported_subject (loc, what) ->
    Location.raise_errorf ~loc
      "Refinement verification failed: %s cannot yet be represented in a \
       verification condition"
      what
