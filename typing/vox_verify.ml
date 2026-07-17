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

let json_bool value = if value then "true" else "false"

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

(* Types of refinement-predicate sub-expressions, keyed by source span, for
   the IDE's "type at cursor" readout inside a refinement like
   [int{ _ > 0 }].  Gathered per refinement type encountered during
   translation (see [collect_refinement_types]), independent of VC
   generation, so a refined parameter that produces no obligation still
   contributes cursor types.  Populated and emitted only when
   [-vox-dump-vc-json] is set. *)
let refinement_expression_types = ref []

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

(* Source-like predicate rendering lives in [Vox_lean] so the type printer
   ([Out_type]) can share it; see [Vox_lean.render_predicate]. *)
let render_display ~env expression =
  Vox_lean.render_predicate ~env expression

(* Render a sub-expression's type source-like, in [env] for path
   shortening.  Refinement types render as [int{ _ > 0 }] (never the raw
   AST) because [Printtyp]/[Out_type] use the source-like predicate printer
   installed by [Vox_lean]. *)
let render_type ~env type_ =
  Printtyp.wrap_printing_env ~error:true env (fun () ->
    Format.asprintf "%a" Printtyp.type_expr type_)

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
    unused_facts = [];
  }

let json_fact ~env ~unused_facts index (fact : Vox_vc.fact) =
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
      (* Display-only fade signal: false only when the discharged proof did not
         reference this fact (per Lean's linter); defaults to used otherwise. *)
      Json.field "used" (json_bool (not (List.mem index unused_facts)));
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
          (Json.array
             (List.mapi
                (json_fact ~env ~unused_facts:result.unused_facts)
                condition.Vox_vc.facts));
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
          let refinement_expression_types_field =
            match List.rev !refinement_expression_types with
            | [] -> []
            | entries ->
              [ Json.field "refinement_expression_types" (Json.array entries) ]
          in
          let document =
            Json.object_
              ([ Json.field "schema_version" (Json.int 2);
                 Json.field "verification_conditions"
                   (Json.array (List.rev !dumped_vcs));
               ]
               @ refinement_expression_types_field)
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

(* Negation of a boolean [subject], used for the else-branch fact.  When [not]
   resolves to the [%boolnot] primitive (the usual case) we emit a real
   application of it, so the fact renders as [not c] and the Lean backend
   interprets it as boolean negation.  If [not] is shadowed or unresolvable we
   fall back to the semantically identical [if c then false else true], so the
   branch fact is never lost. *)
let negate_condition ~env ~loc condition_subject =
  let fallback () =
    Refinement.create ~loc ~type_:Predef.type_bool
      (Rexp_ifthenelse
         ( condition_subject,
           bool_node ~loc false,
           Some (bool_node ~loc true) ))
  in
  match Env.find_value_by_name (Longident.Lident "not") env with
  | exception Not_found -> fallback ()
  | path, description ->
    match description.val_kind with
    | Val_prim primitive when String.equal primitive.prim_name "%boolnot" ->
      let head =
        Refinement.create ~loc ~type_:description.val_type
          (Rexp_ident (Rfree (Rapp path)))
      in
      Refinement.create ~loc ~type_:Predef.type_bool
        (Rexp_apply (head, [ Nolabel, condition_subject ]))
    | _ -> fallback ()

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

(* Reconcile a predicate's references to in-scope variables with the way the
   [subject] lowering represents them.  A parameter (or other in-scope local)
   mentioned in a predicate is lowered at elaboration as a free reference
   [Rfree (Rglobal/Rapp (Pident id))] (Lean: an opaque [VoxRef_N]), whereas the
   [subject] here lowers the same in-scope [Pident id] as [Rbound id] (Lean: a
   universally-fixed variable [v_N]).  Left unreconciled the two become distinct
   Lean symbols, so a goal like [_ = x] over parameter [x] is unprovable.
   Rewrite every free reference to an in-scope [Pident id] into [Rbound id],
   preserving the node, so identical references collapse to one symbol.  This
   connects identical references only: a subject [x + 1] against predicate [x]
   still yields [v_0 + 1 = v_0], correctly disproved. *)
let rec bind_scope_references scope expression =
  let recur = bind_scope_references scope in
  match expression.rexp_desc with
  | Rexp_ident (Rfree (Rglobal (Pident id) | Rapp (Pident id)))
    when Ident.Set.mem id scope ->
    { expression with rexp_desc = Rexp_ident (Rbound id) }
  | Rexp_ident _ | Rexp_constant _ -> expression
  | Rexp_let (bindings, body) ->
    { expression with
      rexp_desc =
        Rexp_let
          ( List.map
              (fun binding ->
                { binding with rbind_expr = recur binding.rbind_expr })
              bindings,
            recur body );
    }
  | Rexp_function function_ ->
    { expression with
      rexp_desc = Rexp_function { function_ with body = recur function_.body };
    }
  | Rexp_apply (function_, arguments) ->
    { expression with
      rexp_desc =
        Rexp_apply
          (recur function_,
           List.map (fun (label, argument) -> label, recur argument) arguments);
    }
  | Rexp_tuple fields ->
    { expression with
      rexp_desc =
        Rexp_tuple (List.map (fun (label, field) -> label, recur field) fields);
    }
  | Rexp_construct (constructor, arguments) ->
    { expression with
      rexp_desc = Rexp_construct (constructor, List.map recur arguments);
    }
  | Rexp_field (record, field) ->
    { expression with rexp_desc = Rexp_field (recur record, field) }
  | Rexp_ifthenelse (condition, ifso, ifnot) ->
    { expression with
      rexp_desc =
        Rexp_ifthenelse (recur condition, recur ifso, Option.map recur ifnot);
    }

(* The companion lemma generated for a [let[@vox.def] ...] binding carries a
   TRUSTED refinement (the compiler asserts [f p1 ... pn = rhs] from [f]'s own
   checked, total body; the unit body [()] does not prove it).  Its body must
   therefore not be verified -- doing so would emit an unprovable obligation.
   The equation still reaches callers as an ordinary fact: the lemma is
   registered as a dependent definition, so [check_application] deposits the
   instantiated equation at each [f_def a1 ... an] call site.

   Recognition is by expander provenance -- the physical identity of the ghost
   location [Vox_defeq] minted for the lemma -- NOT by any spellable attribute:
   a hand-written binding cannot carry that location object, so it is always
   verified normally (a forged [@@vox.def.axiom] does not skip verification). *)
let is_def_axiom_binding binding =
  Vox_defeq.is_generated_lemma_loc binding.vb_loc

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
  let goal = bind_scope_references (Facts.scope state.facts) goal in
  prove state ~env ~loc ~kind ~program_point ~provenance goal

let verify_seal_obligation ~env ~seal_location
    (obligation : Ctype.refinement_seal_obligation) =
  (* Surface the seal obligation on the implementation's refined-type
     annotation (the [.ml]'s [int{ _ > 0 }]), because that refinement must
     entail the interface's -- mirroring how editors show [.ml]/[.mli]
     conformance in the [.ml].  Fall back to the implementation binding
     location when the annotation predicate has no real span (e.g. an
     inferred refinement).  [seal_location] (the interface site) is retained
     only as the failure-message anchor. *)
  let anchor =
    let predicate_loc = obligation.rso_implementation_predicate_location in
    if predicate_loc.Location.loc_ghost
    then obligation.rso_implementation_location
    else predicate_loc
  in
  let subject_id = Ident.create_local "value" in
  let subject =
    Refinement.create ~loc:anchor ~type_:obligation.rso_skeleton
      (Rexp_ident (Rbound subject_id))
  in
  let hypothesis =
    Vox_vc.instantiate ~refinement:obligation.rso_hypothesis ~with_:subject
  in
  let goal =
    Vox_vc.instantiate ~refinement:obligation.rso_conclusion ~with_:subject
  in
  (* Anchor the goal's own span to the implementation annotation so a click on
     the obligation jumps into the [.ml].  This is display-only: the emitted
     Lean uses positional names ([v_0], [h_0]) and reads no source span, so
     the generated proof obligation stays byte-identical. *)
  let goal = { goal with rexp_loc = anchor } in
  let condition =
    Vox_vc.create ~loc:anchor
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
      source_span = Some anchor;
      related_spans =
        [ "interface", obligation.rso_interface_location;
          "implementation", obligation.rso_implementation_location;
        ];
    }
  in
  if !Clflags.vox_dump_vc then begin
    dump_vc ~kind:"seal-implication" ~env condition;
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:anchor
        ~provenance ~env condition (not_discharged_result condition)
  end else begin
    let result = Vox_lean.discharge ~env condition in
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:anchor
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
          let expression =
            bind_scope_references (Facts.scope state.facts) expression
          in
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
    List.iter
      (fun binding ->
        if not (is_def_axiom_binding binding) then
          walk_expression state binding.vb_expr)
      bindings;
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
        prove_refinement state ~env:expression.exp_env ~loc:ifso.exp_loc
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
            negate_condition ~env:condition.exp_env
              ~loc:condition.exp_loc condition_subject
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
          prove_refinement state ~env:expression.exp_env ~loc:ifnot.exp_loc
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
      let fact = bind_scope_references (Facts.scope state.facts) fact in
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
  List.iter
    (fun binding ->
      if not (is_def_axiom_binding binding) then
        walk_expression state binding.vb_expr)
    bindings;
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

(* Walk a refinement predicate, recording [{location, type}] for every
   sub-expression node, using its stored [rexp_loc]/[rexp_type].  The hole [_]
   appears as an [Rexp_ident] node, so it contributes its own entry. *)
let rec collect_refinement_expression ~env (expression : refinement_expression)
    =
  let json =
    Json.object_
      [ Json.field "location" (json_span expression.rexp_loc);
        Json.field "type" (json_string (render_type ~env expression.rexp_type));
      ]
  in
  refinement_expression_types := json :: !refinement_expression_types;
  match expression.rexp_desc with
  | Rexp_ident _ | Rexp_constant _ -> ()
  | Rexp_let (bindings, body) ->
    List.iter
      (fun binding -> collect_refinement_expression ~env binding.rbind_expr)
      bindings;
    collect_refinement_expression ~env body
  | Rexp_function { body; _ } -> collect_refinement_expression ~env body
  | Rexp_apply (function_, arguments) ->
    collect_refinement_expression ~env function_;
    List.iter
      (fun (_, argument) -> collect_refinement_expression ~env argument)
      arguments
  | Rexp_tuple fields ->
    List.iter
      (fun (_, field) -> collect_refinement_expression ~env field)
      fields
  | Rexp_construct (_, arguments) ->
    List.iter (collect_refinement_expression ~env) arguments
  | Rexp_field (record, _) -> collect_refinement_expression ~env record
  | Rexp_ifthenelse (condition, ifso, ifnot) ->
    collect_refinement_expression ~env condition;
    collect_refinement_expression ~env ifso;
    Option.iter (collect_refinement_expression ~env) ifnot

(* Record the predicate subterm types of every refinement type written in the
   structure.  Refinement annotations survive as core types whose [ctyp_type]
   is a [Trefine] (the predicate expression itself is dropped during lowering);
   we walk all core types with the default iterator so every syntactic
   refinement -- including refined parameters that generate no obligation -- is
   covered.  Only invoked when [-vox-dump-vc-json] is set. *)
let collect_refinement_types structure =
  let super = Tast_iterator.default_iterator in
  let iterator =
    { super with
      typ =
        (fun sub (core_type : core_type) ->
          (match get_desc core_type.ctyp_type with
           | Trefine refinement ->
             collect_refinement_expression ~env:core_type.ctyp_env
               refinement.ref_pred
           | _ -> ());
          super.typ sub core_type);
    }
  in
  List.iter (iterator.structure_item iterator) structure.str_items

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
    if Option.is_some !Clflags.vox_dump_vc_json then
      collect_refinement_types structure;
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
  end
