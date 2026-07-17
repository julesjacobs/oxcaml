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
module Json = Misc.Json

let json_string string =
  let buffer = Buffer.create (String.length string + 2) in
  let add_byte_escape byte =
    Printf.bprintf buffer "\\u00%02X" byte
  in
  let continuation byte = byte land 0xc0 = 0x80 in
  let valid_utf8_length index =
    let length = String.length string in
    let byte offset = Char.code string.[index + offset] in
    let first = byte 0 in
    if first >= 0xc2 && first <= 0xdf
       && index + 1 < length && continuation (byte 1)
    then 2
    else if index + 2 < length && continuation (byte 2)
            && ((first = 0xe0 && byte 1 >= 0xa0 && byte 1 <= 0xbf)
                || ((first >= 0xe1 && first <= 0xec
                     || first >= 0xee && first <= 0xef)
                    && continuation (byte 1))
                || (first = 0xed && byte 1 >= 0x80 && byte 1 <= 0x9f))
    then 3
    else if index + 3 < length
            && continuation (byte 2) && continuation (byte 3)
            && ((first = 0xf0 && byte 1 >= 0x90 && byte 1 <= 0xbf)
                || (first >= 0xf1 && first <= 0xf3
                    && continuation (byte 1))
                || (first = 0xf4 && byte 1 >= 0x80 && byte 1 <= 0x8f))
    then 4
    else 0
  in
  Buffer.add_char buffer '"';
  let rec loop index =
    if index < String.length string then begin
      let byte = Char.code string.[index] in
      let consumed =
        match string.[index] with
        | '"' -> Buffer.add_string buffer "\\\""; 1
        | '\\' -> Buffer.add_string buffer "\\\\"; 1
        | '\b' -> Buffer.add_string buffer "\\b"; 1
        | '\012' -> Buffer.add_string buffer "\\f"; 1
        | '\n' -> Buffer.add_string buffer "\\n"; 1
        | '\r' -> Buffer.add_string buffer "\\r"; 1
        | '\t' -> Buffer.add_string buffer "\\t"; 1
        | _ when byte < 0x20 -> add_byte_escape byte; 1
        | _ when byte < 0x80 -> Buffer.add_char buffer string.[index]; 1
        | _ ->
          let length = valid_utf8_length index in
          if length = 0 then begin
            add_byte_escape byte;
            1
          end else begin
            Buffer.add_substring buffer string index length;
            length
          end
      in
      loop (index + consumed)
    end
  in
  loop 0;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

type vc_provenance =
  { kind : string;
    name : string option;
    source_span : Location.t option;
    related_spans : (string * Location.t) list;
  }

let dumped_vcs = ref []

let json_position (position : Lexing.position) =
  Json.object_
    [ Json.field "line" (Json.int position.pos_lnum);
      Json.field "column"
        (Json.int (position.pos_cnum - position.pos_bol));
    ]

let json_span (location : Location.t) =
  Json.object_
    [ Json.field "file" (json_string location.loc_start.pos_fname);
      Json.field "start" (json_position location.loc_start);
      Json.field "end" (json_position location.loc_end);
      Json.field "ghost" (string_of_bool location.loc_ghost);
    ]

let json_related_span (role, location) =
  Json.object_
    [ Json.field "role" (json_string role);
      Json.field "span" (json_span location);
    ]

let json_provenance provenance =
  Json.object_
    [ Json.field "kind" (json_string provenance.kind);
      Json.field "name" (Json.option json_string provenance.name);
      Json.field "source_span"
        (Json.option json_span provenance.source_span);
      Json.field "related_spans"
        (Json.array (List.map json_related_span provenance.related_spans));
    ]

let render_expression expression =
  Format.asprintf "%a" Types.Refinement.print expression

type display_associativity =
  | Left
  | Right

type display_operator =
  { text : string;
    precedence : int;
    associativity : display_associativity;
  }

type displayed_expression =
  { text : string;
    precedence : int;
  }

let display_operator = function
  | `Or -> { text = "||"; precedence = 10; associativity = Right }
  | `And -> { text = "&&"; precedence = 20; associativity = Right }
  | `Equal -> { text = "="; precedence = 30; associativity = Left }
  | `Not_equal -> { text = "<>"; precedence = 30; associativity = Left }
  | `Less -> { text = "<"; precedence = 30; associativity = Left }
  | `Less_equal -> { text = "<="; precedence = 30; associativity = Left }
  | `Greater -> { text = ">"; precedence = 30; associativity = Left }
  | `Greater_equal -> { text = ">="; precedence = 30; associativity = Left }
  | `Add -> { text = "+"; precedence = 40; associativity = Left }
  | `Subtract -> { text = "-"; precedence = 40; associativity = Left }
  | `Multiply -> { text = "*"; precedence = 50; associativity = Left }

let display_reference_name = function
  | Rfun name | Rsibling name -> name
  | Rapp path | Rglobal path -> Path.last path

let display_builtin env = function
  | Rfun _ | Rsibling _ -> None
  | Rapp path | Rglobal path ->
    begin
      match
        Subst.Lazy.force_value_description (Env.find_value path env)
      with
      | { val_kind = Val_prim primitive; _ } ->
        Vox_lean.primitive_builtin primitive.prim_name
      | _ -> None
      | exception Not_found -> None
    end

let display_constant constant =
  constant
  |> Untypeast.constant
  |> Ast_helper.Exp.constant
  |> Pprintast.string_of_expression

let display_function_name name =
  if String.length name = 0 then name
  else
    match name.[0] with
    | 'a'..'z' | 'A'..'Z' | '_' -> name
    | _ -> "(" ^ name ^ ")"

let display_raw expression =
  { text = render_expression expression; precedence = 100 }

let render_display ~env expression =
  let parenthesize displayed = "(" ^ displayed.text ^ ")" in
  let rec render expression =
    match expression.rexp_desc with
    | Rexp_ident (Rbound id) ->
      { text = Ident.name id; precedence = 100 }
    | Rexp_ident (Rfree reference) ->
      { text = display_function_name (display_reference_name reference);
        precedence = 100;
      }
    | Rexp_constant constant ->
      { text = display_constant constant; precedence = 100 }
    | Rexp_construct (constructor, [])
      when Path.same constructor.rconstr_type_path Predef.path_bool
           && (String.equal constructor.rconstr_name "true"
               || String.equal constructor.rconstr_name "false") ->
      { text = constructor.rconstr_name; precedence = 100 }
    | Rexp_apply
        ( { rexp_desc = Rexp_ident (Rfree reference); _ },
          [Nolabel, argument] ) ->
      begin match display_builtin env reference with
      | Some `Not ->
        let argument = render argument in
        let argument =
          if argument.precedence <= 70 then parenthesize argument
          else argument.text
        in
        { text = "not " ^ argument; precedence = 70 }
      | Some (`Add | `And | `Equal | `Greater | `Greater_equal | `Less
             | `Less_equal | `Multiply | `Not_equal | `Or | `Subtract)
      | None -> render_application expression reference [Nolabel, argument]
      end
    | Rexp_apply
        ( { rexp_desc = Rexp_ident (Rfree reference); _ },
          [Nolabel, left; Nolabel, right] ) ->
      begin match display_builtin env reference with
      | Some `Not ->
        render_application expression reference
          [Nolabel, left; Nolabel, right]
      | Some ((`Add | `And | `Equal | `Greater | `Greater_equal | `Less
              | `Less_equal | `Multiply | `Not_equal | `Or | `Subtract)
              as builtin) ->
        render_binary builtin left right
      | None ->
        render_application expression reference
          [Nolabel, left; Nolabel, right]
      end
    | Rexp_apply
        ({ rexp_desc = Rexp_ident (Rfree reference); _ }, arguments) ->
      render_application expression reference arguments
    | Rexp_apply
        ({ rexp_desc = Rexp_ident (Rbound id); _ }, arguments) ->
      render_prefix_application expression (Ident.name id) arguments
    | Rexp_let _ | Rexp_function _ | Rexp_apply _ | Rexp_tuple _
    | Rexp_construct _ | Rexp_field _ | Rexp_ifthenelse _ ->
      display_raw expression
  and render_binary builtin left right =
    let operator = display_operator builtin in
    let operand side expression =
      let displayed = render expression in
      let needs_parentheses =
        displayed.precedence < operator.precedence
        || (displayed.precedence = operator.precedence
            && match operator.associativity, side with
               | Left, `Right | Right, `Left -> true
               | Left, `Left | Right, `Right -> false)
      in
      if needs_parentheses then parenthesize displayed else displayed.text
    in
    { text =
        operand `Left left ^ " " ^ operator.text ^ " "
        ^ operand `Right right;
      precedence = operator.precedence;
    }
  and render_application whole reference arguments =
    let head =
      match display_builtin env reference with
      | Some `Not -> "not"
      | Some ((`Add | `And | `Equal | `Greater | `Greater_equal | `Less
              | `Less_equal | `Multiply | `Not_equal | `Or | `Subtract)
              as builtin) ->
        (display_operator builtin).text
      | None -> display_reference_name reference
    in
    render_prefix_application whole head arguments
  and render_prefix_application whole head arguments =
    if arguments = [] then display_raw whole
    else
      let head = display_function_name head in
      let argument (label, expression) =
        let displayed = render expression in
        let text =
          if displayed.precedence <= 70 then parenthesize displayed
          else displayed.text
        in
        match label with
        | Nolabel -> text
        | Labelled label -> "~" ^ label ^ ":" ^ text
        | Optional label -> "?" ^ label ^ ":" ^ text
        | Position label -> "@" ^ label ^ ":" ^ text
      in
      { text =
          String.concat " " (head :: List.map argument arguments);
        precedence = 70;
      }
  in
  (render expression).text

let display_location location =
  let start = location.Location.loc_start in
  let end_ = location.Location.loc_end in
  let file =
    if String.equal start.pos_fname "" then "<unknown>"
    else Filename.basename start.pos_fname
  in
  let start_column = start.pos_cnum - start.pos_bol in
  let end_column = end_.pos_cnum - end_.pos_bol in
  if start.pos_lnum = end_.pos_lnum then
    Printf.sprintf "%s:%d:%d-%d" file start.pos_lnum start_column end_column
  else
    Printf.sprintf "%s:%d:%d-%d:%d" file start.pos_lnum start_column
      end_.pos_lnum end_column

let dump_vc ~kind ~env (condition : Vox_vc.t) =
  Format.eprintf "VC %s at %s@." kind
    (display_location condition.Vox_vc.location);
  List.iter
    (fun (fact : Vox_vc.fact) ->
      Format.eprintf "  %s@." (render_display ~env fact.expression))
    condition.Vox_vc.facts;
  Format.eprintf "|- %s@.@." (render_display ~env condition.Vox_vc.goal)

let not_discharged_result (condition : Vox_vc.t) : Vox_lean.result =
  { verdict = Not_proved;
    location = condition.location;
    detail = Some "not discharged (-vox-dump-vc)";
  }

let json_fact ~env (fact : Vox_vc.fact) =
  let origin = fact.origin in
  Json.object_
    [ Json.field "text" (json_string (render_expression fact.expression));
      Json.field "display"
        (json_string (render_display ~env fact.expression));
      Json.field "source_span" (Json.option json_span fact.location);
      Json.field "origin"
        (Json.object_
           [ Json.field "kind" (json_string origin.kind);
             Json.field "name" (Json.option json_string origin.name);
             Json.field "span" (Json.option json_span origin.span);
           ]);
    ]

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let counterexample (result : Vox_lean.result) =
  match result.verdict, result.detail with
  | Disproved, Some detail ->
    let lower = String.lowercase_ascii detail in
    if contains lower "counterexample" || contains lower "witness"
    then Some detail
    else None
  | (Proved | Not_proved | Solver_error), _ | Disproved, None -> None

let json_emission_error (error : Vox_lean.emission_error) =
  Json.object_
    [ Json.field "message" (json_string error.message);
      Json.field "location" (json_span error.location);
    ]

let record_vc ~kind ~program_point ~provenance ~env
    (condition : Vox_vc.t) (result : Vox_lean.result) =
  let generated_lean, emission_error =
    match Vox_lean.emit ~env condition with
    | Ok source -> Some source, None
    | Error error -> None, Some error
  in
  let goal =
    Json.object_
      [ Json.field "text"
          (json_string (render_expression condition.Vox_vc.goal));
        Json.field "display"
          (json_string (render_display ~env condition.Vox_vc.goal));
        Json.field "source_span"
          (json_span condition.Vox_vc.goal.rexp_loc);
      ]
  in
  let discharge =
    Json.object_
      [ Json.field "status"
          (json_string (Vox_lean.string_of_verdict result.verdict));
        Json.field "detail" (Json.option json_string result.detail);
        Json.field "counterexample"
          (Json.option json_string (counterexample result));
      ]
  in
  let json =
    Json.object_
      [ Json.field "location" (json_span condition.Vox_vc.location);
        Json.field "program_point" (json_span program_point);
        Json.field "kind" (json_string kind);
        Json.field "goal" goal;
        Json.field "facts"
          (Json.array (List.map (json_fact ~env) condition.Vox_vc.facts));
        Json.field "discharge" discharge;
        Json.field "generated_lean"
          (Json.option json_string generated_lean);
        Json.field "emission_error"
          (Json.option json_emission_error emission_error);
        Json.field "provenance" (json_provenance provenance);
      ]
  in
  dumped_vcs := json :: !dumped_vcs

let () =
  at_exit (fun () ->
    match !Clflags.vox_dump_vc_json with
    | None -> ()
    | Some file ->
      begin
        try
          let document =
            Json.object_
              [ Json.field "schema_version" (Json.int 2);
                Json.field "verification_conditions"
                  (Json.array (List.rev !dumped_vcs));
              ]
          in
          let channel = open_out file in
          Misc.try_finally
            ~always:(fun () -> close_out_noerr channel)
            (fun () ->
              output_string channel document;
              output_char channel '\n';
              close_out channel)
        with exception_ ->
          begin
            try
              Format.eprintf "Warning: could not write VC dump to %S: %s@."
                file (Printexc.to_string exception_)
            with _ -> ()
          end
      end)

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

let fact_origin ?name ~kind span : Vox_vc.fact_origin =
  { kind; name; span = Some span }

let fact_origin_of_provenance provenance : Vox_vc.fact_origin =
  { kind = provenance.kind;
    name = provenance.name;
    span = provenance.source_span;
  }

let prove state ~env ~loc ~kind ~program_point ~provenance goal =
  match Facts.snapshot ~loc ~goal state.facts with
  | Error { escaped; _ } ->
    Location.raise_errorf ~loc
      "Refinement verification failed: goal mentions out-of-scope value%s %s"
      (if List.length escaped = 1 then "" else "s")
      (String.concat ", " (List.map Ident.name escaped))
  | Ok condition ->
    let provenance = lazy (provenance ()) in
    if !Clflags.vox_dump_vc then begin
      dump_vc ~kind ~env condition;
      let origin =
        fact_origin_of_provenance (Lazy.force provenance)
      in
      if Option.is_some !Clflags.vox_dump_vc_json then
        record_vc ~kind ~program_point
          ~provenance:(Lazy.force provenance) ~env condition
          (not_discharged_result condition);
      state.facts <- Facts.add ~origin ~loc goal state.facts
    end else begin
      let result = Vox_lean.discharge ~env condition in
      if Option.is_some !Clflags.vox_dump_vc_json then
        record_vc ~kind ~program_point
          ~provenance:(Lazy.force provenance) ~env condition result;
      match result.verdict with
      | Vox_lean.Proved ->
        let origin =
          fact_origin_of_provenance (Lazy.force provenance)
        in
        state.facts <- Facts.add ~origin ~loc goal state.facts
      | (Not_proved | Disproved | Solver_error) as verdict ->
        verification_error ~loc verdict
    end

let prove_refinement state ~env ~loc ~kind ~program_point ~provenance
    ~subject refinement replacements =
  let goal = Vox_vc.instantiate ~refinement ~with_:subject in
  let goal = replace_parameters replacements goal in
  prove state ~env ~loc ~kind ~program_point ~provenance goal

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
           origin =
             fact_origin ~kind:"seal-implication"
               ~name:obligation.rso_value_name
               obligation.rso_implementation_location;
         }]
      ~goal
  in
  let provenance =
    { kind = "seal-implication";
      name = Some obligation.rso_value_name;
      source_span = Some seal_location;
      related_spans =
        [ "interface", obligation.rso_interface_location;
          "implementation", obligation.rso_implementation_location;
        ];
    }
  in
  if !Clflags.vox_dump_vc then begin
    dump_vc ~kind:"seal-implication" ~env condition;
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:seal_location
        ~provenance ~env condition (not_discharged_result condition)
  end else begin
    let result = Vox_lean.discharge ~env condition in
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:seal_location
        ~provenance ~env condition result;
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
  end

let verify_seal_obligations ~env ~seal_location obligations =
  if not !Clflags.vox_type_only then
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
          let origin =
            fact_origin ~kind:"binder" ~name:(Ident.name id)
              pattern.pat_loc
          in
          state.facts <-
            Facts.add ~origin ~loc:pattern.pat_loc expression state.facts)
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

let annotation_provenance ~annotation_location ~subject_location =
  { kind = "annotation";
    name = None;
    source_span = Some annotation_location;
    related_spans = ["subject", subject_location];
  }

let contract_argument_provenance ~application_location ~argument_location
    ~parameter refinement =
  let name =
    match parameter with
    | Some parameter -> Ident.name parameter
    | None -> Ident.name refinement.ref_view.rb_id
  in
  { kind = "contract-argument";
    name = Some name;
    source_span = Some refinement.ref_pred.rexp_loc;
    related_spans =
      [ "argument", argument_location;
        "application", application_location;
      ];
  }

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
        let origin = fact_origin ~kind:"branch" condition.exp_loc in
        state.facts <-
          Facts.add ~origin ~loc:condition.exp_loc condition_subject
            state.facts)
      condition_fact;
    walk_expression state ifso;
    List.iter
      (fun (loc, refinement) ->
        let provenance () =
          annotation_provenance ~annotation_location:loc
            ~subject_location:ifso.exp_loc
        in
        prove_refinement state ~env:expression.exp_env ~loc
          ~kind:"annotation" ~program_point:expression.exp_loc ~provenance
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
          let origin = fact_origin ~kind:"branch" condition.exp_loc in
          state.facts <-
            Facts.add ~origin ~loc:condition.exp_loc negated state.facts)
        condition_fact;
      walk_expression state ifnot;
      List.iter
        (fun (loc, refinement) ->
          let provenance () =
            annotation_provenance ~annotation_location:loc
              ~subject_location:ifnot.exp_loc
          in
          prove_refinement state ~env:expression.exp_env ~loc
            ~kind:"annotation" ~program_point:expression.exp_loc ~provenance
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
      let provenance () =
        annotation_provenance ~annotation_location:loc
          ~subject_location:expression.exp_loc
      in
      prove_refinement state ~env:expression.exp_env ~loc ~subject refinement
        ~kind:"annotation" ~program_point:expression.exp_loc ~provenance [])
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
                  let provenance () =
                    contract_argument_provenance
                      ~application_location:application.exp_loc
                      ~argument_location:argument.exp_loc ~parameter refinement
                  in
                  prove_refinement state ~env:application.exp_env
                    ~loc:argument.exp_loc ~kind:"contract-argument"
                    ~program_point:application.exp_loc ~provenance
                    ~subject:argument_subject refinement replacements)
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
      let name =
        match function_.exp_desc with
        | Texp_ident { path; _ } -> Some (Path.last path)
        | _ -> None
      in
      let origin = fact_origin ?name ~kind:"application" application.exp_loc in
      state.facts <-
        Facts.add ~origin ~loc:application.exp_loc fact state.facts)
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

let finish_dump () =
  if !Clflags.vox_dump_vc then begin
    Format.eprintf "Error: VCs dumped, not discharged.@.";
    raise Location.Already_displayed_error
  end

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
  begin try walk_root () with
  | Unsupported_subject (loc, what) ->
    Location.raise_errorf ~loc
      "Refinement verification failed: %s cannot yet be represented in a \
       verification condition"
      what
  end;
  finish_dump ()
