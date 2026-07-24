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

let expression_type_judgments = ref []

type lexical_binding =
  { id : Ident.t;
    name : string;
    declaration_span : Location.t;
    scope : Location.t;
  }

let lexical_bindings = ref []

(* Per-identifier-occurrence typed classification for the IDE's semantic
   coloring (see [collect_semantic_tokens]).  Populated and emitted only when
   [-vox-dump-vc-json] is set. *)
let semantic_identifier_tokens = ref []

(* Companion [{location, mode}] entries, one per non-ordinary semantic token,
   in the editor's established identifier-mode readout shape. *)
let dumped_identifier_modes = ref []

let valid_local_span (location : Location.t) =
  let start = location.loc_start in
  let end_ = location.loc_end in
  let valid_position (position : Lexing.position) =
    position.Lexing.pos_lnum >= 1
    && position.pos_bol >= 0
    && position.pos_cnum >= position.pos_bol
  in
  let ordered =
    start.pos_lnum < end_.pos_lnum
    || (start.pos_lnum = end_.pos_lnum
        && start.pos_cnum - start.pos_bol
           <= end_.pos_cnum - end_.pos_bol)
  in
  not location.loc_ghost
  && valid_position start
  && valid_position end_
  && ordered
  && String.equal start.pos_fname !Location.input_name
  && String.equal end_.pos_fname !Location.input_name

let same_span (left : Location.t) (right : Location.t) =
  left.loc_ghost = right.loc_ghost
  && left.loc_start = right.loc_start
  && left.loc_end = right.loc_end

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

let json_provenance (provenance : vc_provenance) =
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

let backend_selection () =
  match Vox_backend.selection_of_string !Clflags.vox_backend with
  | Ok selection -> selection
  | Error message -> invalid_arg message

let not_discharged_result (condition : Vox_vc.t) : Vox_backend.result =
  let unused_facts =
    match backend_selection () with
    | Vox_backend.Single Vox_backend.Lean -> Some []
    | Vox_backend.Cross -> None
    | Vox_backend.Single (Vox_backend.Z3 | Vox_backend.Oxsmt) -> None
  in
  { verdict = Not_proved;
    location = condition.location;
    detail = Some "not discharged (-vox-dump-vc)";
    unused_facts;
    backend_results = [];
  }

let json_fact ~env ~unused_facts index (fact : Vox_vc.fact) =
  let origin = fact.origin in
  let fields =
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
  in
  let usage =
    match unused_facts with
    | None -> []
    | Some unused_facts ->
      [ Json.field "used" (json_bool (not (List.mem index unused_facts))) ]
  in
  let scope =
    match fact.scope with
    | Some scope when valid_local_span scope ->
      [Json.field "scope" (json_span scope)]
    | None | Some _ -> []
  in
  let bound_identifiers =
    Types.Refinement.free_bound_identifiers fact.expression
    |> Ident.Set.elements
    |> List.map (fun id ->
      Json.object_
        [ Json.field "name" (json_string (Ident.name id));
          Json.field "id" (json_string (Ident.unique_name id));
        ])
  in
  let bound_identifiers =
    match bound_identifiers with
    | [] -> []
    | identifiers ->
      [Json.field "bound_identifiers" (Json.array identifiers)]
  in
  Json.object_ (fields @ usage @ scope @ bound_identifiers)

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let counterexample (result : Vox_backend.result) =
  match result.verdict, result.detail with
  | Vox_backend.Disproved, Some detail ->
    let lower = String.lowercase_ascii detail in
    if contains lower "counterexample" || contains lower "witness"
    then Some detail
    else None
  | (Proved | Not_proved | Unknown | Solver_error | Unavailable), _
  | Disproved, None -> None

let json_emission_error (error : Vox_lean.emission_error) =
  Json.object_
    [ Json.field "message" (json_string error.message);
      Json.field "location" (json_span error.location);
    ]

let json_smt_emission_error (error : Vox_smt.emission_error) =
  Json.object_
    [ Json.field "message" (json_string error.message);
      Json.field "location" (json_span error.location);
    ]

let emit_generated_smt ~env condition =
  if !Clflags.vox_dump_vc_json_smt then
    Some (Vox_smt.emit_query ~query:Vox_smt.Prove ~env condition)
  else None

let generated_smt_prove_contents ~env condition = function
  | Some (Ok (query : Vox_smt.emitted_query)) ->
    (* Keep solver eligibility behind a separate ordinary emission.  On R5
       the dump and ordinary paths deliberately use the same bounded emitter. *)
    begin
      match Vox_smt.emit ~query:Vox_smt.Prove ~env condition with
      | Ok _ -> Some query.contents
      | Error _ -> None
    end
  | None | Some (Error _) -> None

let normalize_condition_paths ~env (condition : Vox_vc.t) =
  let normalize expression =
    Refinement.map_paths
      ~value_path:(Env.normalize_value_path None env)
      ~type_path:(Env.normalize_type_path None env)
      expression
  in
  { condition with
    facts =
      List.map
        (fun (fact : Vox_vc.fact) ->
          { fact with expression = normalize fact.expression })
        condition.facts;
    goal = normalize condition.goal;
  }

let json_generated_smt = function
  | Ok (query : Vox_smt.emitted_query) ->
    let facts =
      List.mapi
        (fun fact_index (fact : Vox_smt.emitted_fact) ->
          Json.object_
            [ Json.field "fact_index" (Json.int fact_index);
              Json.field "selector" (json_string fact.selector);
              Json.field "term" (json_string fact.term);
            ])
        query.facts
    in
    Json.object_
      [ Json.field "prove" (json_string query.contents);
        Json.field "facts" (Json.array facts);
        Json.field "goal" (json_string query.goal);
        Json.field "emission_error" (Json.option json_smt_emission_error None);
      ]
  | Error error ->
    Json.object_
      [ Json.field "prove" (Json.option json_string None);
        Json.field "facts" (Json.array []);
        Json.field "goal" (Json.option json_string None);
        Json.field "emission_error"
          (Json.option json_smt_emission_error (Some error));
      ]

let json_backend_result (result : Vox_backend.backend_result) =
  let fact_usage =
    match result.capabilities.fact_usage with
    | Vox_backend.Fact_usage -> true
    | Vox_backend.No_fact_usage -> false
  in
  Json.object_
    [ Json.field "backend"
        (json_string (Vox_backend.string_of_backend result.backend));
      Json.field "status"
        (json_string (Vox_backend.string_of_verdict result.verdict));
      Json.field "detail" (Json.option json_string result.detail);
      Json.field "fact_usage" (json_bool fact_usage);
    ]

let witness_relevance_fields ~env condition =
  match Vox_lean.witness_variables ~env condition with
  | Error _ -> []
  | Ok variables ->
    let json_variable (variable : Vox_lean.witness_variable) =
      Json.object_
        [ Json.field "name" (json_string variable.source_name);
          Json.field "model_name" (json_string variable.model_name);
        ]
    in
    [ Json.field "witness_relevance"
        (Json.object_
           [ Json.field "relevant" (json_bool (variables <> []));
             Json.field "goal_variables"
               (Json.array (List.map json_variable variables));
           ]);
    ]

let record_vc ~kind ~program_point ?result_span ~provenance ~env
    ~generated_smt ~(emitted_condition : Vox_vc.t)
    (condition : Vox_vc.t) (result : Vox_backend.result) =
  let generated_lean, emission_error =
    match Vox_lean.emit ~env emitted_condition with
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
  let backend_results =
    match backend_selection () with
    | Vox_backend.Cross ->
      [ Json.field "backends"
          (Json.array (List.map json_backend_result result.backend_results)) ]
    | Vox_backend.Single _ -> []
  in
  let discharge =
    Json.object_
      (
      [ Json.field "status"
          (json_string (Vox_backend.string_of_verdict result.verdict));
        Json.field "detail" (Json.option json_string result.detail);
        Json.field "counterexample"
          (Json.option json_string (counterexample result));
      ]
      @ backend_results)
  in
  let result_span_field =
    match result_span with
    | Some span when valid_local_span span ->
      [Json.field "result_span" (json_span span)]
    | None | Some _ -> []
  in
  let witness_relevance_fields =
    witness_relevance_fields ~env emitted_condition
  in
  let generated_smt_field =
    match generated_smt with
    | None -> []
    | Some generated_smt ->
      [Json.field "generated_smt" (json_generated_smt generated_smt)]
  in
  let json =
    Json.object_
      ([ Json.field "location" (json_span condition.Vox_vc.location);
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
      @ result_span_field
      @ witness_relevance_fields
      @ generated_smt_field)
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
          let expression_type_judgments_field =
            match List.rev !expression_type_judgments with
            | [] -> []
            | entries ->
              [ Json.field "expression_type_judgments"
                  (Json.array entries) ]
          in
          let lexical_bindings_field =
            let entries =
              List.rev !lexical_bindings
              |> List.filter_map (fun binding ->
                if
                  valid_local_span binding.declaration_span
                  && valid_local_span binding.scope
                then
                  Some
                    (Json.object_
                       [ Json.field "name" (json_string binding.name);
                         Json.field "id"
                           (json_string (Ident.unique_name binding.id));
                         Json.field "declaration_span"
                           (json_span binding.declaration_span);
                         Json.field "scope" (json_span binding.scope);
                       ])
                else None)
            in
            match entries with
            | [] -> []
            | entries ->
              [Json.field "lexical_bindings" (Json.array entries)]
          in
          let semantic_tokens_field =
            match List.rev !semantic_identifier_tokens with
            | [] -> []
            | entries -> [Json.field "semantic_tokens" (Json.array entries)]
          in
          let identifier_modes_field =
            match List.rev !dumped_identifier_modes with
            | [] -> []
            | entries -> [Json.field "identifier_modes" (Json.array entries)]
          in
          let document =
            Json.object_
              ([ Json.field "schema_version" (Json.int 2);
                 Json.field "verification_conditions"
                   (Json.array (List.rev !dumped_vcs));
               ]
               @ refinement_expression_types_field
               @ expression_type_judgments_field
               @ lexical_bindings_field
               @ semantic_tokens_field
               @ identifier_modes_field)
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

type state =
  { mutable facts : Facts.t;
    total_functions : unit Types.Uid.Tbl.t;
    call_subjects : (Location.t, (expression * Ident.t) list) Hashtbl.t;
  }

exception Unsupported_subject of Location.t * string

let expand_head_for_refinement ~env type_ =
  let snapshot = Btype.snapshot () in
  match Ctype.expand_head env type_ with
  | expanded ->
    let expanded = Ctype.duplicate_type expanded in
    Btype.backtrack snapshot;
    expanded
  | exception exn ->
    Btype.backtrack snapshot;
    raise exn

let refinement_alias_cache : unit Path.Tbl.t = Path.Tbl.create 31

let with_fresh_refinement_alias_cache f =
  Path.Tbl.clear refinement_alias_cache;
  Fun.protect f ~finally:(fun () -> Path.Tbl.clear refinement_alias_cache)

let type_may_reveal_refinement ~env type_ =
  with_type_mark (fun mark ->
    let seen_paths = ref Path.Set.empty in
    let rec visit type_ =
      if not (try_mark_node mark type_) then false
      else
        match get_desc type_ with
        | Trefine _ -> true
        | Tconstr (path, arguments, _) ->
          List.exists visit arguments || visit_path path
        | _ ->
          let found = ref false in
          Btype.iter_type_expr
            (fun child -> if visit child then found := true)
            type_;
          !found
    and visit_path path =
      let compute () =
        if Path.Set.mem path !seen_paths then false
        else begin
          seen_paths := Path.Set.add path !seen_paths;
          match (Env.find_type path env).type_manifest with
          | Some manifest -> visit manifest
          | None -> false
          | exception Not_found -> false
        end
      in
      if Env.has_local_constraints env then compute ()
      else
        match Path.Tbl.find_opt refinement_alias_cache path with
        | Some () -> true
        | None ->
          let result = compute () in
          (* A negative result can come from the cycle cut or from a lookup in
             an environment that does not yet expose the declaration.  It is
             therefore not stable enough to cache globally.  A positive result
             has found an actual refinement and is definitive. *)
          if result then Path.Tbl.replace refinement_alias_cache path ();
          result
    in
    visit type_)

let rec refinement ~env type_ =
  match get_desc type_ with
  | Trefine refinement -> Some refinement
  | Tpoly (type_, _) -> refinement ~env type_
  | Tconstr _ when type_may_reveal_refinement ~env type_ ->
    begin match get_desc (expand_head_for_refinement ~env type_) with
    | Trefine refinement -> Some refinement
    | Tpoly (type_, _) -> refinement ~env type_
    | _ -> None
    end
  | _ -> None

let carrier ~env type_ =
  match refinement ~env type_ with
  | Some refinement -> refinement.ref_skeleton
  | None -> type_

let node expression desc =
  Refinement.create ~loc:expression.exp_loc
    ~type_:(carrier ~env:expression.exp_env expression.exp_type)
    desc

let bool_node ~loc value =
  Refinement.create ~loc ~type_:Predef.type_bool
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_bool;
           rconstr_name = if value then "true" else "false";
         },
         [] ))

let unit_node ~loc =
  Refinement.create ~loc ~type_:Predef.type_unit
    (Rexp_construct
       ({ rconstr_type_path = Predef.path_unit; rconstr_name = "()" }, []))

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

let find_stdlib_value env name =
  let qualified =
    Longident.Ldot
      ( Location.mknoloc (Longident.Lident "Stdlib"),
        Location.mknoloc name )
  in
  match Env.find_value_by_name qualified env with
  | value -> value
  | exception Not_found ->
    Env.find_value_by_name (Longident.Lident name) env

let primitive_type ~env description primitive =
  let type_, _, _, _ =
    Ctype.instance_prim env primitive description.val_type
  in
  Ctype.instance type_

let equality_arrow_shapes ~env description primitive =
  let type_ = primitive_type ~env description primitive in
  match get_desc type_ with
  | Tarrow (first, _, rest, first_commutable) ->
    begin match get_desc rest with
    | Tarrow (second, _, _, second_commutable) ->
      Some (first, first_commutable, second, second_commutable)
    | _ -> None
    end
  | _ -> None

let equality_types ~env description primitive argument_type =
  Option.map
    (fun (first, first_commutable, second, second_commutable) ->
      let argument_type = Ctype.duplicate_type argument_type in
      let result_type = Predef.type_bool in
      let after_first =
        Btype.newgenty
          (Tarrow
             ( second,
               Ctype.duplicate_type argument_type,
               result_type,
               Btype.copy_commu second_commutable ))
      in
      let head_type =
        Btype.newgenty
          (Tarrow
             ( first,
               argument_type,
               after_first,
               Btype.copy_commu first_commutable ))
      in
      head_type, after_first, result_type)
    (equality_arrow_shapes ~env description primitive)

let equality ~env ~loc left right =
  match find_stdlib_value env "=" with
  | exception Not_found -> None
  | path, description ->
    begin match description.val_kind with
    | Val_prim primitive when String.equal primitive.prim_name "%equal" ->
      begin match
        equality_types ~env description primitive left.rexp_type
      with
      | None -> None
      | Some (head_type, _, result_type) ->
        let head =
          Refinement.create ~loc ~type_:head_type
            (Rexp_ident (Rfree (Rapp path)))
        in
        Some
          (Refinement.create ~loc ~type_:result_type
             (Rexp_apply (head, [Nolabel, left; Nolabel, right])))
      end
    | _ -> None
    end

let constructor_mismatch ~env ~loc ~constructor subject =
  match find_stdlib_value env "=" with
  | exception Not_found -> None
  | _, description ->
    begin match description.val_kind with
    | Val_prim primitive when String.equal primitive.prim_name "%equal" ->
      begin match
        equality_types ~env description primitive subject.rexp_type
      with
      | None -> None
      | Some (_, test_type, result_type) ->
          let name = Vox_lean.constructor_mismatch_name constructor in
          let head =
            Refinement.create ~loc ~type_:test_type
              (Rexp_ident (Rfree (Rfun name)))
          in
          Some
            (Refinement.create ~loc ~type_:result_type
               (Rexp_apply (head, [Nolabel, subject])))
      end
    | _ -> None
    end

let unsupported expression what =
  raise (Unsupported_subject (expression.exp_loc, what))

let pattern_variable pattern =
  match pattern.pat_desc with
  | Tpat_var { id; _ } -> Some id
  | Tpat_alias { id; _ } -> Some id
  | _ -> None

let contains_refinement ~env type_ =
  type_may_reveal_refinement ~env type_

let identifier_contract expression =
  match expression.exp_desc with
  | Texp_ident { desc; _ } ->
    begin match
      desc.val_kind, refinement ~env:expression.exp_env desc.val_type
    with
    | Val_reg _, Some _ -> Some desc
    | (Val_mut _ | Val_prim _ | Val_ivar _ | Val_self _ | Val_anc _), _
    | Val_reg _, None -> None
    end
  | _ -> None

let totality_is_total totality =
  match Mode.Totality.Guts.check_const_conservative totality with
  | Some Mode.Totality.Const.Total -> true
  | Some Mode.Totality.Const.Partial | None -> false

let expression_has_total_mode expression =
  List.exists
    (fun (extra, _, _) ->
      match extra with
      | Texp_mode modes ->
        begin match modes.mode_modes.totality with
        | Some Mode.Totality.Const.Total -> true
        | Some Mode.Totality.Const.Partial | None -> false
        end
      | _ -> false)
    expression.exp_extra

(* A call can be represented structurally only when evaluating its head is
   deterministic and the resulting function is known total.  Identifier
   identity comes from the typechecker's [val_uid], so aliases and qualified
   paths do not depend on their surface syntax.  Non-identifier heads must
   carry a total mode and themselves have a structural, stable evaluation. *)
let rec call_head_is_stable state expression =
  match expression.exp_desc with
  | Texp_ident { desc = { val_kind = Val_prim primitive; _ }; _ }
    when Option.is_some (Vox_builtin.of_primitive primitive.prim_name)
         || String.equal primitive.prim_name "%identity"
         || String.equal primitive.prim_name "%obj_magic" ->
    true
  | Texp_ident { desc; mode; _ } ->
    expression_has_total_mode expression
    || Types.Uid.Tbl.mem state.total_functions desc.val_uid
    || totality_is_total
         (Mode.Value.proj_comonadic Mode.Axis.Totality mode)
  | Texp_function { alloc_mode; _ } ->
    expression_has_total_mode expression
    || totality_is_total
         (Mode.Alloc.proj_comonadic Mode.Axis.Totality alloc_mode)
  | Texp_let (Nonrecursive, bindings, body) ->
    (expression_has_total_mode expression
     && expression_is_stable state expression)
    || (List.for_all
          (fun binding -> expression_is_stable state binding.vb_expr)
          bindings
        && call_head_is_stable state body)
  | Texp_ifthenelse (condition, ifso, Some ifnot) ->
    expression_is_stable state condition
    && (expression_has_total_mode expression
        || (call_head_is_stable state ifso
            && call_head_is_stable state ifnot))
  | _ ->
    expression_has_total_mode expression
    && expression_is_stable state expression

and expression_is_stable state expression =
  let stable = expression_is_stable state in
  match expression.exp_desc with
  | Texp_constant _ | Texp_function _ -> true
  | Texp_ident { desc = { val_kind = Val_mut _; _ }; _ } -> false
  | Texp_ident _ -> true
  | Texp_let (Nonrecursive, bindings, body) ->
    List.for_all (fun binding -> stable binding.vb_expr) bindings
    && stable body
  | Texp_apply (function_, arguments, _, _, _) ->
    call_head_is_stable state function_
    && List.for_all
         (function
           | _, Arg (argument, _) -> stable argument
           | _, Omitted _ -> false)
         arguments
  | Texp_tuple (fields, _) ->
    List.for_all (fun (_, field) -> stable field) fields
  | Texp_construct (_, _, _, arguments, _) ->
    List.for_all (fun (_, argument) -> stable argument) arguments
  | Texp_field { record; label; _ } when label.lbl_mut = Immutable ->
    stable record
  | Texp_ifthenelse (condition, ifso, ifnot) ->
    stable condition && stable ifso && Option.fold ~none:true ~some:stable ifnot
  | _ -> false

(* Avoid manufacturing logical equality types while walking ordinary OCaml
   that cannot produce a refinement VC.  Besides saving work, this keeps the
   verification pass observational: instantiating polymorphic primitives for
   an irrelevant match must not perturb layout variables in the typedtree. *)
let expression_contains_refinement expression =
  let found = ref false in
  let super = Tast_iterator.default_iterator in
  let iterator =
    { super with
      expr =
        (fun iterator expression ->
          if Option.is_some (identifier_contract expression)
             || contains_refinement ~env:expression.exp_env expression.exp_type
          then found := true
          else super.expr iterator expression);
      pat =
        (fun iterator pattern ->
          if contains_refinement ~env:pattern.pat_env pattern.pat_type
          then found := true
          else super.pat iterator pattern);
      typ =
        (fun iterator core_type ->
          if contains_refinement ~env:core_type.ctyp_env core_type.ctyp_type
          then found := true
          else super.typ iterator core_type);
    }
  in
  iterator.expr iterator expression;
  !found

let register_definition state binding =
  match pattern_variable binding.vb_pat with
  | None -> ()
  | Some _ ->
    if call_head_is_stable state binding.vb_expr then begin
      match binding.vb_pat.pat_desc with
      | Tpat_var { uid; _ } | Tpat_alias { uid; _ } ->
        Types.Uid.Tbl.replace state.total_functions uid ()
      | _ -> ()
    end

(* A non-stable call has no structural image: two evaluations of the
   same syntax may return different values.  Give each source occurrence a
   fresh logical name, memoized because the same occurrence is lowered more
   than once while checking dependent arguments and recording result facts. *)
let opaque_call_subject state expression =
  let same_expression (other, _) = other == expression in
  let at_location =
    Option.value ~default:[]
      (Hashtbl.find_opt state.call_subjects expression.exp_loc)
  in
  let id =
    match List.find_opt same_expression at_location with
    | Some (_, id) -> id
    | None ->
      let id = Ident.create_local "call_result" in
      Hashtbl.replace state.call_subjects expression.exp_loc
        ((expression, id) :: at_location);
      id
  in
  node expression (Rexp_ident (Rfree (Rglobal (Pident id))))
(* The syntactic selfification fragment is deliberately narrower than
   [subject].  Applications are handled separately below, only when the call
   is stable and its result has solver-supported equality. *)
let rec stable_expression expression =
  let stable = stable_expression in
  let supports_equality () =
    Vox_lean.supports_equality ~env:expression.exp_env expression.exp_type
  in
  let immutable_labels labels =
    Array.for_all
      (fun (label : label_description) -> label.lbl_mut = Immutable)
      labels
  in
  match expression.exp_desc with
  | Texp_ident _ | Texp_constant (Const_int _) -> supports_equality ()
  | Texp_construct (_, _, _, arguments, _) ->
    supports_equality ()
    && List.for_all (fun (_, argument) -> stable argument) arguments
  | Texp_record { fields; extended_expression; _ } ->
    supports_equality ()
    && immutable_labels
      (Array.map (fun (label, _, _) -> label) fields)
    &&
    (match extended_expression with
     | None -> true
     | Some (record, _, _) -> stable record)
    && Array.for_all
         (fun (_, _, definition) ->
           match definition with
           | Kept _ -> true
           | Overridden (_, field) -> stable field)
         fields
  | Texp_tuple (fields, _) ->
    supports_equality ()
    && List.for_all
      (fun (label, field) -> Option.is_none label && stable field)
      fields
  | Texp_field { record; label; _ } ->
    supports_equality () && immutable_labels label.lbl_all && stable record
  | _ -> false

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
  | Texp_apply (function_, _, _, _, _)
    when not (call_head_is_stable state function_) ->
    opaque_call_subject state expression
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
  | Texp_record { fields; extended_expression; _ }
    (* A record does not require each field expression to be syntactically
       stable.  [lower] gives unstable calls one occurrence-local subject, and
       the record then denotes the value assembled from those exact results.
       The backend gate keeps mutable and otherwise unmodelable records out. *)
    when Vox_lean.supports_equality
           ~env:expression.exp_env expression.exp_type
         && Array.for_all
              (fun (label, _, _) -> label.lbl_mut = Immutable)
              fields ->
    let path =
      let label, _, _ = fields.(0) in
      lbl_res_type_path label
    in
    let base =
      Option.map (fun (record, _, _) -> lower record) extended_expression
    in
    let field (label, _, definition) =
      match definition, base with
      | Overridden (_, expression), _ -> lower expression
      | Kept _, Some record ->
        let field =
          { rfield_type_path = path; rfield_name = label.lbl_name }
        in
        Refinement.create ~loc:expression.exp_loc
          ~type_:(carrier ~env:expression.exp_env label.lbl_arg)
          (Rexp_field (record, field))
      | Kept _, None ->
        unsupported expression "a record with a kept field but no base"
    in
    let constructor =
      { rconstr_type_path = path; rconstr_name = "mk" }
    in
    node expression
      (Rexp_construct (constructor, List.map field (Array.to_list fields)))
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
  | Texp_sequence (_, _, result) ->
    (* A normally completing sequence returns exactly its right operand.
       Evaluation and fact flow are handled by [walk_expression]; lowering
       only identifies the value produced on that normal-return path. *)
    lower result
  | Texp_letmutable (_, result)
  | Texp_open (_, result)
  | Texp_exclave result
  | Texp_letexception (_, result)
  | Texp_letmodule (_, _, _, _, result) ->
    (* These wrappers likewise return their body on their single normal path.
       Their setup is walked separately.  If the body itself depends on an
       unsupported local entity (for example a mutable read), lowering that
       body still fails closed at the existing case. *)
    lower result
  | Texp_antiquotation _ ->
    (* The future value produced by dynamically supplied code is not
       structurally available while checking the generator.  A fresh opaque
       subject still permits stage-local facts to discharge predicates which
       do not constrain that value, while value-dependent claims remain
       fail-closed. *)
    opaque_call_subject state expression
  | Texp_mutvar _ -> unsupported expression "a mutable variable"
  | _ -> unsupported expression "this expression form"

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
  | Rexp_match (scrutinee, cases) ->
    { expression with
      rexp_desc =
        Rexp_match
          ( recur scrutinee,
            List.map
              (fun case ->
                { case with rcase_body = recur case.rcase_body })
              cases );
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

let discharge ~generated_smt ~env condition =
  let prove_contents =
    generated_smt_prove_contents ~env condition generated_smt
  in
  Vox_backend.discharge ~selection:(backend_selection ())
    ~smt_solver:!Clflags.vox_smt_solver
    ~oxsmt_solver:!Clflags.vox_oxsmt_solver ?prove_contents ~env condition

let failure_text (result : Vox_backend.result) =
  match backend_selection (), result.detail with
  | Vox_backend.Cross, Some detail -> detail
  | Vox_backend.Cross, None | Vox_backend.Single _, _ ->
    Vox_backend.string_of_verdict result.verdict

let verification_error ~loc result =
  Location.raise_errorf ~loc "Refinement verification failed (%s)"
    (failure_text result)

let fact_origin ?name ~kind span : Vox_vc.fact_origin =
  { kind; name; span = Some span }

let fact_origin_of_provenance
    (provenance : vc_provenance) : Vox_vc.fact_origin =
  { kind = provenance.kind;
    name = provenance.name;
    span = provenance.source_span;
  }

let prove state ~env ~loc ~kind ~program_point ?result_span ~provenance goal =
  match Facts.snapshot ~loc ~goal state.facts with
  | Error { escaped; _ } ->
    Location.raise_errorf ~loc
      "Refinement verification failed: goal mentions out-of-scope value%s %s"
      (if List.length escaped = 1 then "" else "s")
      (String.concat ", " (List.map Ident.name escaped))
  | Ok condition ->
    let provenance = lazy (provenance ()) in
    let emitted_condition = normalize_condition_paths ~env condition in
    let generated_smt = emit_generated_smt ~env emitted_condition in
    if !Clflags.vox_dump_vc then begin
      dump_vc ~kind ~env condition;
      let origin =
        fact_origin_of_provenance (Lazy.force provenance)
      in
      if Option.is_some !Clflags.vox_dump_vc_json then
        record_vc ~kind ~program_point ?result_span
          ~provenance:(Lazy.force provenance) ~env ~generated_smt
          ~emitted_condition condition
          (not_discharged_result emitted_condition);
      state.facts <- Facts.add ~origin ~loc goal state.facts
    end else begin
      let result = discharge ~generated_smt ~env emitted_condition in
      if Option.is_some !Clflags.vox_dump_vc_json then
        record_vc ~kind ~program_point ?result_span
          ~provenance:(Lazy.force provenance) ~env ~generated_smt
          ~emitted_condition condition result;
      match result.verdict with
      | Vox_backend.Proved ->
        let origin =
          fact_origin_of_provenance (Lazy.force provenance)
        in
        state.facts <- Facts.add ~origin ~loc goal state.facts
      | (Vox_backend.Not_proved | Disproved | Unknown | Solver_error
        | Unavailable) ->
        verification_error ~loc result
    end

let prove_refinement state ~env ~loc ~kind ~program_point ?result_span
    ~provenance ~subject refinement =
  let goal = Vox_vc.instantiate ~refinement ~with_:subject in
  let goal = bind_scope_references (Facts.scope state.facts) goal in
  prove state ~env ~loc ~kind ~program_point ?result_span ~provenance goal

let align_seal_sibling_references ~env interface_predicate
    implementation_predicate =
  let rec normalize_interface expression =
    let normalize = normalize_interface in
    let rexp_desc =
      match expression.rexp_desc with
      | Rexp_ident (Rfree (Rglobal path | Rapp path)) ->
        begin match Env.find_value path env with
        | _ -> expression.rexp_desc
        | exception Not_found ->
          Rexp_ident (Rfree (Rsibling (Path.last path)))
        end
      | Rexp_ident _ | Rexp_constant _ -> expression.rexp_desc
      | Rexp_let (bindings, body) ->
        Rexp_let
          ( List.map
              (fun binding ->
                { binding with rbind_expr = normalize binding.rbind_expr })
              bindings,
            normalize body )
      | Rexp_function function_ ->
        Rexp_function { function_ with body = normalize function_.body }
      | Rexp_apply (function_, arguments) ->
        Rexp_apply
          ( normalize function_,
            List.map
              (fun (label, argument) -> label, normalize argument)
              arguments )
      | Rexp_tuple fields ->
        Rexp_tuple
          (List.map (fun (label, field) -> label, normalize field) fields)
      | Rexp_construct (constructor, arguments) ->
        Rexp_construct (constructor, List.map normalize arguments)
      | Rexp_field (record, field) -> Rexp_field (normalize record, field)
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        Rexp_ifthenelse
          (normalize condition, normalize ifso, Option.map normalize ifnot)
      | Rexp_match (scrutinee, cases) ->
        Rexp_match
          ( normalize scrutinee,
            List.map
              (fun case ->
                { case with rcase_body = normalize case.rcase_body })
              cases )
    in
    { expression with rexp_desc }
  in
  let interface_predicate = normalize_interface interface_predicate in
  let names = ref [] in
  let exact_references = ref [] in
  let rec collect expression =
    begin match expression.rexp_desc with
    | Rexp_ident (Rfree (Rsibling name | Rfun name)) ->
      if not (List.mem name !names) then names := name :: !names
    | Rexp_ident
        (Rfree ((Rglobal path | Rapp path) as reference)) ->
      begin match
        Subst.Lazy.force_value_description (Env.find_value path env)
      with
      | description ->
        exact_references :=
          (reference, path, description.val_uid) :: !exact_references
      | exception Not_found -> ()
      end
    | Rexp_ident _ | Rexp_constant _ -> ()
    | Rexp_let (bindings, body) ->
      List.iter (fun binding -> collect binding.rbind_expr) bindings;
      collect body
    | Rexp_function { body; _ } -> collect body
    | Rexp_apply (function_, arguments) ->
      collect function_;
      List.iter (fun (_, argument) -> collect argument) arguments
    | Rexp_tuple fields -> List.iter (fun (_, field) -> collect field) fields
    | Rexp_construct (_, arguments) -> List.iter collect arguments
    | Rexp_field (record, _) -> collect record
    | Rexp_ifthenelse (condition, ifso, ifnot) ->
      collect condition;
      collect ifso;
      Option.iter collect ifnot
    | Rexp_match (scrutinee, cases) ->
      collect scrutinee;
      List.iter (fun case -> collect case.rcase_body) cases
    end
  in
  collect interface_predicate;
  let paths =
    List.filter_map
      (fun name ->
        match
          Env.lookup_value ~use:false ~loc:Location.none
            (Longident.Lident name) env
        with
        | path, description, _ -> Some (name, path, description.val_uid)
        | exception Env.Error _ -> None)
      !names
  in
  let rec rewrite expression =
    let rexp_desc =
      match expression.rexp_desc with
      | Rexp_ident
          (Rfree ((Rglobal path | Rapp path) as reference)) ->
        let value_uid =
          match
            Subst.Lazy.force_value_description (Env.find_value path env)
          with
          | description -> Some description.val_uid
          | exception Not_found -> None
        in
        let is_unprojected_declaration = function
          | Path.Pident id ->
            not (Ident.is_global_or_predef id)
            && Ident.scope id = Ident.highest_scope
          | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> false
        in
        let same_value candidate_path candidate_uid =
          let same_normalized_path =
            Path.same
              (Env.normalize_value_path None env path)
              (Env.normalize_value_path None env candidate_path)
          in
          same_normalized_path
          ||
          (is_unprojected_declaration path
           <> is_unprojected_declaration candidate_path)
          &&
          match value_uid with
          | Some value_uid -> Uid.equal candidate_uid value_uid
          | None -> false
        in
        begin match
          List.find_opt
            (fun (candidate, candidate_path, candidate_uid) ->
              let same_kind =
                match reference, candidate with
                | Rglobal _, Rglobal _ | Rapp _, Rapp _ -> true
                | _ -> false
              in
              same_kind && same_value candidate_path candidate_uid)
            !exact_references
        with
        | Some (candidate, _, _) -> Rexp_ident (Rfree candidate)
        | None ->
          begin match
            List.find_opt
              (fun (_, sibling_path, sibling_uid) ->
                same_value sibling_path sibling_uid)
              paths
          with
          | Some (name, _, _) -> Rexp_ident (Rfree (Rsibling name))
          | None -> expression.rexp_desc
          end
        end
      | Rexp_ident _ | Rexp_constant _ -> expression.rexp_desc
      | Rexp_let (bindings, body) ->
        Rexp_let
          ( List.map
              (fun binding ->
                { binding with rbind_expr = rewrite binding.rbind_expr })
              bindings,
            rewrite body )
      | Rexp_function function_ ->
        Rexp_function { function_ with body = rewrite function_.body }
      | Rexp_apply (function_, arguments) ->
        Rexp_apply
          ( rewrite function_,
            List.map
              (fun (label, argument) -> label, rewrite argument)
              arguments )
      | Rexp_tuple fields ->
        Rexp_tuple
          (List.map (fun (label, field) -> label, rewrite field) fields)
      | Rexp_construct (constructor, arguments) ->
        Rexp_construct (constructor, List.map rewrite arguments)
      | Rexp_field (record, field) -> Rexp_field (rewrite record, field)
      | Rexp_ifthenelse (condition, ifso, ifnot) ->
        Rexp_ifthenelse
          (rewrite condition, rewrite ifso, Option.map rewrite ifnot)
      | Rexp_match (scrutinee, cases) ->
        Rexp_match
          ( rewrite scrutinee,
            List.map
              (fun case ->
                { case with rcase_body = rewrite case.rcase_body })
              cases )
    in
    { expression with rexp_desc }
  in
  interface_predicate, rewrite implementation_predicate

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
  (* Signature-relative references deliberately survive .cmi persistence by
     name.  Resolved functor-result references may instead retain the exact
     result-signature path while the implementation uses the projected
     instance path.  At an implementation seal, reconcile these only when
     their alias-normalized paths identify the same value.  A functor-result
     signature can also retain one unprojected local declaration path while
     the implementation has the concrete projected path; a matching value UID
     reconciles exactly that local-to-projected transition.  UIDs alone never
     reconcile two projected paths, because distinct applications of one
     functor retain the same declaration UID.  This keeps shadowed locals and
     distinct functor instances separate. *)
  let goal, hypothesis =
    if obligation.rso_is_contravariant
    then
      let hypothesis, goal =
        align_seal_sibling_references ~env hypothesis goal
      in
      goal, hypothesis
    else align_seal_sibling_references ~env goal hypothesis
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
           scope = None;
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
  let emitted_condition = normalize_condition_paths ~env condition in
  let generated_smt = emit_generated_smt ~env emitted_condition in
  if !Clflags.vox_dump_vc then begin
    dump_vc ~kind:"seal-implication" ~env condition;
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:anchor
        ~provenance ~env ~generated_smt ~emitted_condition condition
        (not_discharged_result emitted_condition)
  end else begin
    let result = discharge ~generated_smt ~env emitted_condition in
    if Option.is_some !Clflags.vox_dump_vc_json then
      record_vc ~kind:"seal-implication" ~program_point:anchor
        ~provenance ~env ~generated_smt ~emitted_condition condition result;
    match result.verdict with
    | Vox_backend.Proved -> ()
    | (Vox_backend.Not_proved | Disproved | Unknown | Solver_error
      | Unavailable) ->
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
        obligation.rso_value_name (failure_text result)
  end

let verify_seal_obligations ~env ~seal_location obligations =
  if not !Clflags.vox_type_only then
    with_fresh_refinement_alias_cache (fun () ->
      List.iter (verify_seal_obligation ~env ~seal_location) obligations)

type verification_mark =
  { annotation_location : Location.t;
    result_location : Location.t option;
    result_may_complete : (expression -> bool) option;
    refinement : refinement_desc;
  }

let marked_refinements expression =
  List.filter_map
    (fun (extra, loc, _) ->
      match extra with
      | Texp_constraint core_type ->
        Option.map
          (fun refinement ->
            { annotation_location = loc;
              result_location = None;
              result_may_complete = None;
              refinement;
            })
          (refinement ~env:core_type.ctyp_env core_type.ctyp_type)
      | Texp_refinement_constraint type_ ->
        Option.map
          (fun refinement ->
            { annotation_location = loc;
              result_location = None;
              result_may_complete = None;
              refinement;
            })
          (refinement ~env:expression.exp_env type_)
      | Texp_coerce _ | Texp_poly _ | Texp_newtype _ | Texp_stack
      | Texp_mode _ | Texp_refinement_application _
      | Texp_inspected_type _ | Texp_borrowed
      | Texp_ghost_region
        -> None)
    expression.exp_extra

let result_marks expression marks =
  List.filter_map
    (fun mark ->
      if
        Option.fold ~none:true ~some:(fun may_complete -> may_complete expression)
          mark.result_may_complete
      then Some { mark with result_location = Some expression.exp_loc }
      else None)
    marks

let restrict_result_marks make_may_complete marks =
  List.map
    (fun mark ->
      let outer_may_complete =
        Option.value ~default:(fun _ -> true) mark.result_may_complete
      in
      let result_may_complete = make_may_complete outer_may_complete in
      { mark with result_may_complete = Some result_may_complete })
    marks

(* Binder facts are recorded UNCONDITIONALLY (no purity gate), unlike the
   branch-condition facts below.  The asymmetry is sound and deliberate
   (SHOULD-1 ruling): a refined binder's predicate is a PROVEN contract --
   discharged as an obligation at the value's definition -- so the fact is a
   property of the specific value now bound to the identifier, not a claim about
   re-evaluating an expression.  Re-reading that identifier yields the same
   value, so the fact stays valid however impure the surrounding code.  A branch
   condition, by contrast, records a fact about an *expression's* value, which
   only stays valid across occurrences when the expression has a stable logical
   representation. *)
let add_refinement_fact state ~env ~kind ?name ~loc ?scope ~subject type_ =
  Option.iter
    (fun refinement ->
      let expression = Vox_vc.instantiate ~refinement ~with_:subject in
      let expression =
        bind_scope_references (Facts.scope state.facts) expression
      in
      let origin = fact_origin ~kind ?name loc in
      state.facts <- Facts.add ~origin ~loc ?scope expression state.facts)
    (refinement ~env type_)

let add_established_result_contract state ~kind ?name expression type_ =
  match refinement ~env:expression.exp_env type_ with
  | None -> ()
  | Some _ ->
    add_refinement_fact state ~env:expression.exp_env ~kind ?name
      ~loc:expression.exp_loc
      ~subject:(subject state expression) type_

let add_identifier_contract state expression description =
  match
    description.val_kind,
    refinement ~env:expression.exp_env description.val_type
  with
  | Val_reg _, Some _ ->
    let name =
      match expression.exp_desc with
      | Texp_ident { path; _ } -> Some (Path.last path)
      | _ -> None
    in
    add_established_result_contract state ~kind:"identifier" ?name
      expression description.val_type
  | (Val_mut _ | Val_prim _ | Val_ivar _ | Val_self _ | Val_anc _), _
  | Val_reg _, None -> ()

let pattern_bindings pattern =
  let seen = ref Ident.Set.empty in
  let bindings = ref [] in
  let add id name type_ =
    if not (Ident.Set.mem id !seen) then begin
      seen := Ident.Set.add id !seen;
      bindings := (id, name, type_) :: !bindings
    end
  in
  let collect : type k. k general_pattern -> unit =
    fun pattern ->
    match pattern.pat_desc with
    | Tpat_var { id; name; _ }
    | Tpat_fun_layout { id; name; _ } ->
      add id name pattern.pat_type
    | Tpat_alias { id; name; type_expr; _ } ->
      add id name type_expr
    | _ -> ()
  in
  Typedtree.iter_general_pattern
    { f = collect }
    pattern;
  List.rev !bindings

let enter_pattern
    : type k.
      state -> fact:bool -> ?scope:Location.t -> k general_pattern -> unit =
  fun state ~fact ?scope pattern ->
  let bindings = pattern_bindings pattern in
  Option.iter
    (fun scope ->
      List.iter
        (fun (id, name, _) ->
          if
            not
              (List.exists
                 (fun (binding : lexical_binding) ->
                   Ident.same binding.id id)
                 !lexical_bindings)
          then
            lexical_bindings :=
              { id;
                name = Ident.name id;
                declaration_span = name.loc;
                scope;
              }
              :: !lexical_bindings)
        bindings)
    scope;
  state.facts <-
    Facts.enter_many (List.map (fun (id, _, _) -> id) bindings)
      state.facts;
  if fact then
    List.iter
      (fun (id, name, type_) ->
        let subject =
          Refinement.create ~loc:name.loc
            ~type_:(carrier ~env:pattern.pat_env type_)
            (Rexp_ident (Rbound id))
        in
        add_refinement_fact state ~env:pattern.pat_env ~kind:"binder"
          ~name:(Ident.name id) ~loc:name.loc ?scope ~subject type_)
      bindings

let add_match_fact state ~loc ?scope expression =
  let origin = fact_origin ~kind:"match" loc in
  state.facts <- Facts.add ~origin ~loc ?scope expression state.facts

let fresh_match_subject state ~env ~loc type_ =
  let id = Ident.create_local "*match-component*" in
  state.facts <- Facts.enter id state.facts;
  Refinement.create ~loc ~type_:(carrier ~env type_)
    (Rexp_ident (Rbound id))

let value_pattern_subject state (pattern : value general_pattern) =
  match pattern.pat_desc with
  | Tpat_var { id; _ } | Tpat_alias { id; _ } ->
    Refinement.create ~loc:pattern.pat_loc
      ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
      (Rexp_ident (Rbound id))
  | Tpat_constant (Const_int _ as constant) ->
    Refinement.create ~loc:pattern.pat_loc
      ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
      (Rexp_constant constant)
  | _ ->
    fresh_match_subject state ~env:pattern.pat_env ~loc:pattern.pat_loc
      pattern.pat_type

let rec add_value_pattern_facts state ~subject ?scope
    (pattern : value general_pattern) =
  add_refinement_fact state ~env:pattern.pat_env ~kind:"match"
    ~loc:pattern.pat_loc ?scope ~subject pattern.pat_type;
  let add_equality left right =
    Option.iter (add_match_fact state ~loc:pattern.pat_loc ?scope)
      (equality ~env:pattern.pat_env ~loc:pattern.pat_loc left right)
  in
  match pattern.pat_desc with
  | Tpat_any -> ()
  | Tpat_var { id; _ } ->
    let variable =
      Refinement.create ~loc:pattern.pat_loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_ident (Rbound id))
    in
    add_equality variable subject
  | Tpat_alias { pattern = subpattern; id; _ } ->
    let alias =
      Refinement.create ~loc:pattern.pat_loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_ident (Rbound id))
    in
    add_equality alias subject;
    add_value_pattern_facts state ~subject ?scope subpattern
  | Tpat_constant (Const_int _ as constant) ->
    let constant =
      Refinement.create ~loc:pattern.pat_loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_constant constant)
    in
    add_equality subject constant
  | Tpat_construct (_, constructor, _, arguments, _)
    when Vox_lean.supports_match_facts ~env:pattern.pat_env
           pattern.pat_type ->
    let components =
      List.map
        (fun (_, pattern) -> value_pattern_subject state pattern, pattern)
        arguments
    in
    let constructor =
      { rconstr_type_path = cstr_res_type_path constructor;
        rconstr_name = constructor.cstr_name;
      }
    in
    let constructed =
      Refinement.create ~loc:pattern.pat_loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_construct (constructor, List.map fst components))
    in
    add_equality subject constructed;
    List.iter
      (fun (component, pattern) ->
        add_value_pattern_facts state ~subject:component ?scope pattern)
      components
  | Tpat_tuple fields
    when List.for_all (fun (label, _) -> Option.is_none label) fields
         && Vox_lean.supports_match_facts ~env:pattern.pat_env
              pattern.pat_type ->
    let components =
      List.map
        (fun (label, pattern) ->
          label, value_pattern_subject state pattern, pattern)
        fields
    in
    let tuple =
      Refinement.create ~loc:pattern.pat_loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_tuple
           (List.map (fun (label, component, _) -> label, component)
              components))
    in
    add_equality subject tuple;
    List.iter
      (fun (_, component, pattern) ->
        add_value_pattern_facts state ~subject:component ?scope pattern)
      components
  | Tpat_record (fields, _, _, _)
    when Vox_lean.supports_match_facts ~env:pattern.pat_env
           pattern.pat_type ->
    List.iter
      (fun (_, label, field_pattern) ->
        if label.lbl_mut = Immutable then begin
          let field =
            { rfield_type_path = lbl_res_type_path label;
              rfield_name = label.lbl_name;
            }
          in
          let projection =
            Refinement.create ~loc:field_pattern.pat_loc
              ~type_:(carrier ~env:field_pattern.pat_env field_pattern.pat_type)
              (Rexp_field (subject, field))
          in
          add_value_pattern_facts state ~subject:projection ?scope field_pattern
        end)
      fields
  | Tpat_or _ | Tpat_fun_layout _ | Tpat_unboxed_unit
  | Tpat_unboxed_bool _ | Tpat_unboxed_tuple _ | Tpat_variant _
  | Tpat_record_unboxed_product _ | Tpat_array _ | Tpat_lazy _
  | Tpat_constant _ | Tpat_construct _ | Tpat_tuple _ | Tpat_record _ -> ()

let rec ground_pattern_term (pattern : value general_pattern) =
  let make desc =
    Refinement.create ~loc:pattern.pat_loc
      ~type_:(carrier ~env:pattern.pat_env pattern.pat_type) desc
  in
  match pattern.pat_desc with
  | Tpat_constant (Const_int _ as constant) ->
    Some (make (Rexp_constant constant))
  | Tpat_alias { pattern; _ } -> ground_pattern_term pattern
  | Tpat_construct (_, constructor, _, arguments, _)
    when Vox_lean.supports_match_facts ~env:pattern.pat_env
           pattern.pat_type ->
    let arguments =
      List.map (fun (_, pattern) -> ground_pattern_term pattern) arguments
    in
    if List.for_all Option.is_some arguments then
      let constructor =
        { rconstr_type_path = cstr_res_type_path constructor;
          rconstr_name = constructor.cstr_name;
        }
      in
      Some
        (make
           (Rexp_construct
              (constructor, List.map Option.get arguments)))
    else None
  | Tpat_tuple fields
    when List.for_all (fun (label, _) -> Option.is_none label) fields
         && Vox_lean.supports_match_facts ~env:pattern.pat_env
              pattern.pat_type ->
    let fields =
      List.map
        (fun (label, pattern) ->
          Option.map (fun term -> label, term) (ground_pattern_term pattern))
        fields
    in
    if List.for_all Option.is_some fields then
      Some (make (Rexp_tuple (List.map Option.get fields)))
    else None
  | _ -> None

let rec irrefutable_pattern (pattern : value general_pattern) =
  match pattern.pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | Tpat_alias { pattern; _ } -> irrefutable_pattern pattern
  | Tpat_construct (_, constructor, _, arguments, _)
    when constructor.cstr_consts + constructor.cstr_nonconsts = 1 ->
    List.for_all
      (fun (_, pattern) -> irrefutable_pattern pattern)
      arguments
  | Tpat_tuple fields ->
    List.for_all (fun (_, pattern) -> irrefutable_pattern pattern) fields
  | _ -> false

let rec constructor_head (pattern : value general_pattern) =
  match pattern.pat_desc with
  | Tpat_alias { pattern; _ } -> constructor_head pattern
  | Tpat_construct (_, constructor, _, arguments, _)
    when List.for_all
           (fun (_, pattern) -> irrefutable_pattern pattern)
           arguments
         && Vox_lean.supports_match_facts ~env:pattern.pat_env
              pattern.pat_type ->
    Some constructor.cstr_name
  | _ -> None

let pattern_negation ~subject (pattern : value general_pattern) =
  match ground_pattern_term pattern with
  | Some term ->
    Option.map
      (negate_condition ~env:pattern.pat_env ~loc:pattern.pat_loc)
      (equality ~env:pattern.pat_env ~loc:pattern.pat_loc subject term)
  | None ->
    Option.bind (constructor_head pattern) (fun constructor ->
      constructor_mismatch ~env:pattern.pat_env ~loc:pattern.pat_loc
        ~constructor subject)

let computation_value_pattern pattern =
  match pattern.pat_desc with
  | Tpat_value pattern -> Some (pattern :> value general_pattern)
  | _ -> None

type effect_constructor_match =
  | Definitely_matches
  | Definitely_does_not_match
  | Match_unknown

let rec effect_pattern_matches_constructor pattern constructor =
  match pattern.pat_desc with
  | Tpat_any | Tpat_var _ -> Definitely_matches
  | Tpat_alias { pattern; _ } ->
    effect_pattern_matches_constructor pattern constructor
  | Tpat_or (left, right, _) ->
    begin match
      effect_pattern_matches_constructor left constructor,
      effect_pattern_matches_constructor right constructor
    with
    | Definitely_matches, _ | _, Definitely_matches -> Definitely_matches
    | Definitely_does_not_match, Definitely_does_not_match ->
      Definitely_does_not_match
    | (Match_unknown | Definitely_does_not_match),
      (Match_unknown | Definitely_does_not_match) ->
      Match_unknown
    end
  | Tpat_construct (_, pattern_constructor, _, arguments, _) ->
    if Data_types.equal_constr pattern_constructor constructor
       &&
      List.for_all
        (fun (_, argument) -> irrefutable_pattern argument)
        arguments
    then Definitely_matches
    else if Data_types.may_equal_constr pattern_constructor constructor
    then Match_unknown
    else Definitely_does_not_match
  | Tpat_constant _ | Tpat_tuple _ | Tpat_variant _ | Tpat_record _
  | Tpat_record_unboxed_product _ | Tpat_array _ | Tpat_lazy _
  | Tpat_fun_layout _ | Tpat_unboxed_unit | Tpat_unboxed_bool _
  | Tpat_unboxed_tuple _ ->
    Match_unknown

let performed_effect_constructor expression =
  match expression.exp_desc with
  | Texp_apply
      ( { exp_desc =
            Texp_ident
              { desc = { val_kind = Val_prim primitive; _ };
                _
              };
          _
        },
        arguments,
        _, _, _ )
    when String.equal primitive.prim_name "%perform" ->
    begin match arguments with
    | [argument] ->
      begin match snd argument with
      | Arg (performed, _) ->
        begin match performed.exp_desc with
        | Texp_construct (_, constructor, _, _, _) -> Some constructor
        | _ -> None
        end
      | Omitted _ -> None
      end
    | _ -> None
    end
  | _ -> None

type local_effect_handling =
  | Definitely_nonresuming
  | Definitely_may_resume
  | Possibly_may_resume
  | Not_definitely_handled

let rec final_sequence_result expression =
  match expression.exp_desc with
  | Texp_sequence (_, _, result) -> final_sequence_result result
  | _ -> expression

(* The source site of a single-result expression is narrower than the value
   used by selfification above.  These wrappers each have exactly one normal
   result path; their setup may fail to return, but when it does return the
   wrapper's value is its body.  Branching forms deliberately remain whole. *)
let rec final_result_site expression =
  match expression.exp_desc with
  | Texp_sequence (_, _, result)
  | Texp_let (_, _, result)
  | Texp_letmutable (_, result)
  | Texp_open (_, result)
  | Texp_exclave result
  | Texp_letexception (_, result)
  | Texp_letmodule (_, _, _, _, result) ->
    final_result_site result
  | _ -> expression

let selfification_fact state ?scope binding =
  let result = final_sequence_result binding.vb_expr in
  let selfifiable =
    stable_expression result
    || (expression_is_stable state result
        && Vox_lean.supports_equality ~env:result.exp_env result.exp_type)
  in
  let rec add pattern subject =
    let add_identifier id =
      let variable =
        Refinement.create ~loc:pattern.pat_loc ~type_:subject.rexp_type
          (Rexp_ident (Rbound id))
      in
      Option.iter
        (fun equation ->
          let origin =
            fact_origin ~kind:"selfification" ~name:(Ident.name id)
              pattern.pat_loc
          in
          state.facts <-
            Facts.add ~origin ~loc:pattern.pat_loc ?scope equation state.facts)
        (equality ~env:pattern.pat_env ~loc:pattern.pat_loc variable subject)
    in
    match pattern.pat_desc with
    | Tpat_var { id; _ } -> add_identifier id
    | Tpat_alias { pattern = subpattern; id; _ } ->
      add_identifier id;
      add subpattern subject
    | _ -> ()
  in
  if selfifiable then
    match subject state result with
    | result_subject -> add binding.vb_pat result_subject
    | exception Unsupported_subject _ -> ()

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

let merge_facts left right =
  List.fold_left
    (fun facts (fact : Vox_vc.fact) ->
      Facts.add ~origin:fact.origin ?loc:fact.location ?scope:fact.scope
        fact.expression facts)
    left (Facts.facts right)

let short_circuit_application function_ arguments =
  let kind =
    match function_.exp_desc with
    | Texp_ident { desc = { val_kind = Val_prim primitive; _ }; _ }
      when String.equal primitive.prim_name "%sequand" ->
      Some `And
    | Texp_ident { desc = { val_kind = Val_prim primitive; _ }; _ }
      when String.equal primitive.prim_name "%sequor" ->
      Some `Or
    | _ -> None
  in
  match kind, arguments with
  | Some kind,
    [ _, Arg (left, _);
      _, Arg (right, _)
    ] ->
    Some (kind, left, right)
  | (None | Some _), _ -> None

let expression_needs_boundary_walk expression =
  match expression.exp_desc with
  | Texp_sequence _ | Texp_let _ | Texp_letmutable _ | Texp_function _
  | Texp_apply _ | Texp_tuple _ | Texp_unboxed_tuple _
  | Texp_construct _ | Texp_record _ | Texp_record_unboxed_product _
  | Texp_setfield _ | Texp_array _ | Texp_override _ | Texp_letop _
  | Texp_overwrite _
  | Texp_while _ | Texp_for _
  | Texp_list_comprehension _ | Texp_array_comprehension _
  | Texp_lazy _ | Texp_quotation _ | Texp_antiquotation _
  | Texp_probe _ | Texp_assert _
  | Texp_open _ | Texp_exclave _ | Texp_letmodule _ | Texp_letexception _
  | Texp_ifthenelse _ | Texp_try _ | Texp_match _ -> true
  | _ -> false

let rec walk_expression ?(inherited_marks = []) state expression =
  let marks = inherited_marks @ marked_refinements expression in
  if marks = []
     && Option.is_none (identifier_contract expression)
     && not (expression_needs_boundary_walk expression)
     && not (expression_contains_refinement expression)
  then
    walk_default_expression state expression
  else match expression.exp_desc with
  | Texp_ident { desc; _ } ->
    add_identifier_contract state expression desc;
    check_marks state expression marks
  | Texp_sequence (first, _, second) ->
    let entry_facts = state.facts in
    walk_expression state first;
    if expression_may_complete first then begin
      (* A sequence returns exactly its right-hand value.  Facts established
         by the left side are available only after its normal completion, and
         an enclosing result refinement is therefore checked on the right-hand
         expression rather than on the non-value sequence node. *)
      walk_expression
        ~inherited_marks:(result_marks second marks) state second;
      if not (expression_may_complete second) then
        state.facts <- entry_facts
    end else begin
      (* The right side is unreachable at runtime, but still walk it from the
         original environment to check its own local obligations.  The outer
         result obligation is unreachable, and no facts escape. *)
      state.facts <- entry_facts;
      walk_expression state second;
      state.facts <- entry_facts
    end
  | Texp_let (Nonrecursive, bindings, body) ->
    let saved_facts = state.facts in
    let try_summaries = ref [] in
    let all_rhs_complete = ref true in
    let returning_rhs_facts =
      List.filter_map
      (fun binding ->
        state.facts <- saved_facts;
        if not (is_def_axiom_binding binding) then
          match pattern_variable binding.vb_pat, binding.vb_expr.exp_desc with
          | Some _, Texp_try (tried, cases, effect_cases) ->
            let paths =
              walk_try state binding.vb_expr tried cases effect_cases
                (marked_refinements binding.vb_expr)
            in
            try_summaries := (binding.vb_pat, paths) :: !try_summaries;
            if expression_may_complete binding.vb_expr
            then Some state.facts
            else begin
              all_rhs_complete := false;
              None
            end
          | _, _ ->
            walk_expression state binding.vb_expr;
            if expression_may_complete binding.vb_expr
            then Some state.facts
            else begin
              all_rhs_complete := false;
              None
            end
        else begin
          if not (expression_may_complete binding.vb_expr) then
            all_rhs_complete := false;
          None
        end)
      bindings
    in
    state.facts <-
      List.fold_left merge_facts saved_facts returning_rhs_facts;
    List.iter (register_definition state) bindings;
    List.iter
      (enter_pattern state ~fact:true ~scope:body.exp_loc)
      (List.map (fun binding -> binding.vb_pat) bindings);
    List.iter
      (fun (pattern, paths) -> add_try_result_fact state pattern paths)
      (List.rev !try_summaries);
    List.iter (selfification_fact state ~scope:body.exp_loc) bindings;
    walk_expression
      ~inherited_marks:
        (if !all_rhs_complete then result_marks body marks else [])
      state body;
    state.facts <-
      if !all_rhs_complete
      then Facts.restrict (Facts.scope saved_facts) state.facts
      else saved_facts
  | Texp_let (Recursive, bindings, body) ->
    let saved_facts = state.facts in
    List.iter
      (enter_pattern state ~fact:false)
      (List.map (fun binding -> binding.vb_pat) bindings);
    List.iter (register_definition state) bindings;
    List.iter
      (fun binding ->
        if not (is_def_axiom_binding binding) then
          walk_expression state binding.vb_expr)
      bindings;
    List.iter
      (enter_pattern state ~fact:true ~scope:body.exp_loc)
      (List.map (fun binding -> binding.vb_pat) bindings);
    walk_expression ~inherited_marks:(result_marks body marks) state body;
    state.facts <- Facts.restrict (Facts.scope saved_facts) state.facts
  | Texp_letmutable (binding, body) ->
    walk_expression state binding.vb_expr;
    let saved_facts = state.facts in
    enter_pattern state ~fact:false ~scope:body.exp_loc binding.vb_pat;
    walk_expression
      ~inherited_marks:
        (if expression_may_complete binding.vb_expr
         then result_marks body marks
         else [])
      state body;
    state.facts <- saved_facts
  | Texp_open (open_declaration, body) ->
    let module_expression = open_declaration.open_expr in
    let iterator = iterator state in
    Tast_iterator.default_iterator.module_expr iterator module_expression;
    walk_expression
      ~inherited_marks:
        (if module_expression_may_complete module_expression
         then result_marks body marks
         else [])
      state body
  | Texp_exclave body | Texp_letexception (_, body) ->
    walk_expression ~inherited_marks:(result_marks body marks) state body
  | Texp_letmodule (_, _, _, module_expression, body) ->
    let iterator = iterator state in
    Tast_iterator.default_iterator.module_expr iterator module_expression;
    walk_expression
      ~inherited_marks:
        (if module_expression_may_complete module_expression
         then result_marks body marks
         else [])
      state body
  | Texp_function { params; body; _ } ->
    let saved_facts = state.facts in
    let parameter_scope =
      match body with
      | Tfunction_body body -> Some body.exp_loc
      | Tfunction_cases _ -> None
    in
    let enter_parameter_pattern pattern =
      enter_pattern state ~fact:true ?scope:parameter_scope pattern;
      match pattern.pat_desc with
      | Tpat_var _ -> ()
      | _ ->
        let subject = value_pattern_subject state pattern in
        add_value_pattern_facts state ~subject ?scope:parameter_scope pattern
    in
    List.iter
      (fun parameter ->
        match parameter.fp_kind with
        | Tparam_pat pattern ->
          enter_parameter_pattern pattern
        | Tparam_optional_default (pattern, default, _) ->
          let before_default = state.facts in
          walk_expression state default;
          (* The default is evaluated only on the omitted-argument path.  A
             supplied argument reaches the body without its postconditions. *)
          state.facts <- before_default;
          enter_parameter_pattern pattern)
      params;
    begin match body with
    | Tfunction_body body -> walk_expression state body
    | Tfunction_cases cases ->
      List.iter (walk_value_function_case state) cases.fc_cases
    end;
    state.facts <- saved_facts;
    check_marks state expression marks
  | Texp_apply (function_, arguments, _, _, _) ->
    let entry_facts = state.facts in
    let walk_from facts expression =
      state.facts <- facts;
      walk_expression state expression;
      state.facts
    in
    begin match short_circuit_application function_ arguments with
    | Some (kind, left, right) ->
      let function_facts = walk_from entry_facts function_ in
      let left_facts = walk_from function_facts left in
      let right_entry_facts =
        if expression_may_complete function_
           && expression_may_complete left
           && expression_is_stable state left
        then
          match subject { state with facts = left_facts } left with
          | left_subject ->
            let taken =
              match kind with
              | `And -> left_subject
              | `Or ->
                negate_condition ~env:left.exp_env ~loc:left.exp_loc
                  left_subject
            in
            let origin = fact_origin ~kind:"branch" left.exp_loc in
            Facts.add ~origin ~loc:left.exp_loc ~scope:right.exp_loc
              taken left_facts
          | exception Unsupported_subject _ -> left_facts
        else left_facts
      in
      let right_facts = walk_from right_entry_facts right in
      state.facts <-
        if not
             (expression_may_complete function_
              && expression_may_complete left)
        then entry_facts
        else if expression_may_complete right
        then Facts.intersect left_facts right_facts
        else left_facts;
      if expression_may_complete expression then check_marks state expression marks
    | None ->
      let walk_sibling expression = walk_from entry_facts expression in
      let completed_facts = ref [walk_sibling function_] in
      let argument_facts =
        List.map
        (function
          | _, Arg (argument, _) ->
            let facts = walk_sibling argument in
            completed_facts := facts :: !completed_facts;
            Some facts
          | _, Omitted _ -> None)
          arguments
      in
      if expression_may_complete expression then begin
        state.facts <-
          List.fold_left merge_facts entry_facts (List.rev !completed_facts);
        check_application state expression function_ arguments
          ~entry_facts ~argument_facts;
        (* Keep the whole application as the program point, but designate its
           callee as the compact site which produces the result.  A callback
           argument may contain an arbitrarily large body; using the full call
           as [result_span] would paint that body as one enormous goal. *)
        check_marks ~result_span:function_.exp_loc state expression marks
      end else
        state.facts <- entry_facts
    end
  | Texp_tuple (fields, _) ->
    walk_unordered_siblings state (List.map snd fields);
    check_marks state expression marks
  | Texp_unboxed_tuple fields ->
    walk_unordered_siblings state
      (List.map (fun (_, field, _) -> field) fields);
    check_marks state expression marks
  | Texp_construct (_, _, _, arguments, _) ->
    walk_unordered_siblings state (List.map snd arguments);
    check_marks state expression marks
  | Texp_record { fields; extended_expression; _ } ->
    let children =
      Array.fold_right
        (fun (_, _, definition) children ->
          match definition with
          | Kept _ -> children
          | Overridden (_, field) -> field :: children)
        fields []
    in
    let children =
      match extended_expression with
      | None -> children
      | Some (base, _, _) -> base :: children
    in
    walk_unordered_siblings state children;
    check_marks state expression marks
  | Texp_record_unboxed_product { fields; extended_expression; _ } ->
    let children =
      Array.fold_right
        (fun (_, _, definition) children ->
          match definition with
          | Kept _ -> children
          | Overridden (_, field) -> field :: children)
        fields []
    in
    let children =
      match extended_expression with
      | None -> children
      | Some (base, _) -> base :: children
    in
    walk_unordered_siblings state children;
    check_marks state expression marks
  | Texp_setfield { record; newval; _ } ->
    walk_unordered_siblings state [record; newval];
    check_marks state expression marks
  | Texp_array (_, _, elements, _) ->
    walk_unordered_siblings state elements;
    check_marks state expression marks
  | Texp_override (_, overrides) ->
    walk_unordered_siblings state
      (List.map (fun (_, _, value) -> value) overrides);
    check_marks state expression marks
  | Texp_letop { let_; ands; body; _ } ->
    walk_unordered_siblings state
      (let_.bop_exp :: List.map (fun binding -> binding.bop_exp) ands);
    ignore (walk_case_facts state body : Facts.t);
    check_marks state expression marks
  | Texp_overwrite (destination, source) ->
    walk_unordered_siblings state [destination; source];
    check_marks state expression marks
  | Texp_for { for_id; for_from; for_to; for_body; _ } ->
    walk_unordered_siblings state [for_from; for_to];
    let bounds_facts = state.facts in
    state.facts <- Facts.enter for_id bounds_facts;
    walk_expression state for_body;
    (* A loop may execute zero times, and facts from one iteration cannot be
       assumed at the next backedge.  Only the bounds have completed on every
       normal exit. *)
    state.facts <- bounds_facts;
    check_marks state expression marks
  | Texp_while { wh_cond; wh_body; _ } ->
    let entry_facts = state.facts in
    walk_expression state wh_cond;
    let condition_facts = state.facts in
    state.facts <- condition_facts;
    if expression_is_stable state wh_cond then begin
      match subject state wh_cond with
      | condition_subject ->
        let origin = fact_origin ~kind:"while-condition" wh_cond.exp_loc in
        state.facts <-
          Facts.add ~origin ~loc:wh_cond.exp_loc ~scope:wh_body.exp_loc
            condition_subject state.facts
      | exception Unsupported_subject _ -> ()
    end;
    walk_expression state wh_body;
    (* The body may not run, and its postconditions do not form an invariant. *)
    state.facts <- entry_facts;
    check_marks state expression marks
  | Texp_list_comprehension comprehension
  | Texp_array_comprehension (_, _, comprehension) ->
    walk_comprehension state comprehension;
    check_marks state expression marks
  | Texp_lazy delayed ->
    let captured_facts = state.facts in
    walk_expression state delayed;
    (* Constructing a lazy value does not evaluate its body. *)
    state.facts <- captured_facts;
    check_marks state expression marks
  | Texp_quotation quoted ->
    walk_quotation state quoted;
    check_marks state expression marks
  | Texp_antiquotation _ ->
    (* The containing quotation checks every payload which executes at its
       current stage.  During the isolated future-code walk this node denotes
       the inserted expression, so its payload must not be checked again. *)
    check_marks state expression marks
  | Texp_probe { handler; _ } ->
    let construction_facts = state.facts in
    walk_expression state handler;
    (* A probe handler runs only on executions where the probe is enabled. *)
    state.facts <- construction_facts;
    check_marks state expression marks
  | Texp_assert (condition, _) ->
    let entry_facts = state.facts in
    walk_expression state condition;
    if !Clflags.noassert then
      (* [-noassert] removes evaluation of the condition.  It is still walked
         to check obligations in its source, but cannot establish facts. *)
      state.facts <- entry_facts
    else if expression_is_stable state condition then begin
      match subject state condition with
      | condition_subject ->
        let origin = fact_origin ~kind:"assert" condition.exp_loc in
        state.facts <-
          Facts.add ~origin ~loc:condition.exp_loc condition_subject state.facts
      | exception Unsupported_subject _ -> ()
    end;
    check_marks state expression marks
  | Texp_ifthenelse (condition, ifso, ifnot) ->
    let pre_condition_facts = state.facts in
    walk_expression state condition;
    if not (expression_may_complete condition) then begin
      state.facts <- pre_condition_facts;
      walk_expression state ifso;
      Option.iter
        (fun ifnot ->
          state.facts <- pre_condition_facts;
          walk_expression state ifnot)
        ifnot;
      state.facts <- pre_condition_facts
    end else begin
    (* Only after the condition has completed, record its observation (and, in
       the else branch, its negation).  Stable total calls can therefore combine
       their result contract with the taken-branch observation.  Partial calls,
       mutable reads, and expressions that cannot be lowered contribute no
       observation, which only weakens the branch context. *)
    let condition_fact =
      if expression_is_stable state condition then
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
          Facts.add ~origin ~loc:condition.exp_loc ~scope:ifso.exp_loc
            condition_subject state.facts)
      condition_fact;
    walk_expression ~inherited_marks:(result_marks ifso marks) state ifso;
    let ifso_facts =
      if expression_may_complete ifso then Some state.facts else None
    in
    state.facts <- saved_facts;
    begin match ifnot with
    | None ->
      Option.iter
        (fun condition_subject ->
          let negated =
            negate_condition ~env:condition.exp_env
              ~loc:condition.exp_loc condition_subject
          in
          let origin = fact_origin ~kind:"branch" condition.exp_loc in
          state.facts <-
            Facts.add ~origin ~loc:condition.exp_loc ~scope:expression.exp_loc
              negated state.facts)
        condition_fact;
      check_marks_against state ~env:expression.exp_env
        ~subject_location:expression.exp_loc
        ~subject:(unit_node ~loc:expression.exp_loc)
        (result_marks expression marks);
      let ifnot_facts = state.facts in
      state.facts <-
        begin match ifso_facts with
        | Some ifso_facts -> Facts.intersect ifso_facts ifnot_facts
        | None -> ifnot_facts
        end
    | Some ifnot ->
      Option.iter
        (fun condition_subject ->
          let negated =
            negate_condition ~env:condition.exp_env
              ~loc:condition.exp_loc condition_subject
          in
          let origin = fact_origin ~kind:"branch" condition.exp_loc in
          state.facts <-
            Facts.add ~origin ~loc:condition.exp_loc ~scope:ifnot.exp_loc
              negated state.facts)
        condition_fact;
      walk_expression ~inherited_marks:(result_marks ifnot marks) state ifnot;
      let ifnot_facts =
        if expression_may_complete ifnot then Some state.facts else None
      in
      state.facts <-
        begin match ifso_facts, ifnot_facts with
        | Some ifso_facts, Some ifnot_facts ->
          Facts.intersect ifso_facts ifnot_facts
        | Some facts, None | None, Some facts -> facts
        | None, None -> saved_facts
        end
    end
    end
  | Texp_try (tried, cases, effect_cases) ->
    ignore
      (walk_try state expression tried cases effect_cases marks
        : (Facts.t * expression) list)
  | Texp_match (scrutinee, _, cases, effect_cases, _) ->
    walk_match state scrutinee cases effect_cases marks
  | _ ->
    walk_default_expression state expression;
    check_marks state expression marks

and walk_unordered_siblings state expressions =
  (* OxCaml does not expose a source-level evaluation order for the children of
     these aggregate forms.  Verify each child against the common entry
     environment, then retain facts established by every child that can return
     normally.  On a normal return from the aggregate all such children have
     completed, irrespective of the order selected by later compilation. *)
  let entry_facts = state.facts in
  let completed_facts =
    List.filter_map
      (fun expression ->
        state.facts <- entry_facts;
        walk_expression state expression;
        if expression_may_complete expression then Some state.facts else None)
      expressions
  in
  state.facts <- List.fold_left merge_facts entry_facts completed_facts

and walk_quotation state quoted =
  (* The fold follows nested quote/antiquote depth and returns exactly the
     payloads which execute while this quotation is constructed.  Their
     evaluation order is unspecified, so check each from the same entry facts
     and discard every postcondition. *)
  let entry_facts = state.facts in
  List.iter
    (fun splice ->
      state.facts <- entry_facts;
      walk_expression state splice)
    (current_stage_splices quoted);
  state.facts <- entry_facts;
  (* Generated code is checked without construction-time path facts.  This
     state is discarded, so future facts cannot flow back into construction. *)
  let future_state =
    { facts = Facts.empty;
      total_functions = Types.Uid.Tbl.create 16;
      call_subjects = Hashtbl.create 16;
    }
  in
  walk_expression future_state quoted;
  state.facts <- entry_facts

and current_stage_splices quoted =
  List.rev
    (Typedtree.fold_antiquote_exp
       (fun splices splice -> splice :: splices)
       [] quoted)

and walk_comprehension state { comp_body; comp_clauses } =
  let entry_facts = state.facts in
  let enter_iterator = function
    | { comp_cb_iterator = Texp_comp_range { ident; _ }; _ } ->
      state.facts <- Facts.enter ident state.facts
    | { comp_cb_iterator = Texp_comp_in { pattern; _ }; _ } ->
      enter_pattern state ~fact:true pattern
  in
  List.iter
    (function
      | Texp_comp_for bindings ->
        let sources =
          List.concat_map
            (fun binding ->
              match binding.comp_cb_iterator with
              | Texp_comp_range { start; stop; _ } -> [start; stop]
              | Texp_comp_in { sequence; _ } -> [sequence])
            bindings
        in
        (* Bindings in one [for ... and ...] group, and each range's two
           endpoints, are checked from the same pre-group environment. *)
        walk_unordered_siblings state sources;
        List.iter enter_iterator bindings
      | Texp_comp_when condition ->
        walk_expression state condition;
        if expression_is_stable state condition then
          match subject state condition with
          | condition_subject ->
            let origin =
              fact_origin ~kind:"comprehension-guard" condition.exp_loc
            in
            state.facts <-
              Facts.add ~origin ~loc:condition.exp_loc condition_subject
                state.facts
          | exception Unsupported_subject _ -> ())
    comp_clauses;
  walk_expression state comp_body;
  (* Iterators can be empty, guards can be false, and the body can execute many
     times.  No per-iteration postcondition is unconditional after creation of
     the result container. *)
  state.facts <- entry_facts

and add_guard_observation state facts guard ~taken =
  state.facts <- facts;
  if expression_is_stable state guard then
    match subject state guard with
    | guard_subject ->
      let observation =
        if taken then guard_subject
        else
          negate_condition ~env:guard.exp_env ~loc:guard.exp_loc
            guard_subject
      in
      let origin = fact_origin ~kind:"guard" guard.exp_loc in
      state.facts <-
        Facts.add ~origin ~loc:guard.exp_loc observation state.facts
    | exception Unsupported_subject _ -> ()

and walk_guard_edges state ~entry_scope matched_facts guard =
  state.facts <- matched_facts;
  walk_expression state guard;
  let completed_guard_facts = state.facts in
  if expression_may_complete guard then begin
    add_guard_observation state completed_guard_facts guard ~taken:true;
    let rhs_facts = state.facts in
    add_guard_observation state completed_guard_facts guard ~taken:false;
    let fallthrough_facts = Facts.restrict entry_scope state.facts in
    rhs_facts, Some fallthrough_facts
  end else completed_guard_facts, None

and value_case_fallthrough ~entry_facts ~reachable ~subject ~pattern
    ~guard_fallthrough =
  if not reachable then None
  else
    let mismatch_facts =
      if irrefutable_pattern pattern then None
      else
        let mismatch = ref entry_facts in
        Option.iter
          (fun negation ->
            let origin = fact_origin ~kind:"match" pattern.pat_loc in
            mismatch :=
              Facts.add ~origin ~loc:pattern.pat_loc negation !mismatch)
          (pattern_negation ~subject pattern);
        Some !mismatch
    in
    let fallthrough_paths =
      List.filter_map Fun.id [mismatch_facts; guard_fallthrough]
    in
    match fallthrough_paths with
    | [] -> None
    | first :: rest -> Some (List.fold_left Facts.intersect first rest)

and prepare_case_edges : type k.
    state ->
    entry_facts:Facts.t ->
    reachable:bool ->
    selection_pattern:value general_pattern option ->
    k case ->
    Facts.t * Facts.t option * Facts.t option =
  fun state ~entry_facts ~reachable ~selection_pattern case ->
  let scope = case_scope case in
  let entry_scope = Facts.scope entry_facts in
  state.facts <- entry_facts;
  enter_pattern state ~fact:true ~scope case.c_lhs;
  let matched_facts = state.facts in
  let mismatch_facts =
    if not reachable then None
    else
      match selection_pattern with
      | Some pattern when irrefutable_pattern pattern -> None
      | None | Some _ -> Some entry_facts
  in
  let rhs_facts, guard_fallthrough =
    match case.c_guard with
    | None -> matched_facts, None
    | Some guard ->
      walk_guard_edges state ~entry_scope matched_facts guard
  in
  let fallthrough_paths =
    List.filter_map Fun.id [mismatch_facts; guard_fallthrough]
  in
  let fallthrough =
    if not reachable then None
    else
      match fallthrough_paths with
      | [] -> None
      | first :: rest -> Some (List.fold_left Facts.intersect first rest)
  in
  rhs_facts, guard_fallthrough, fallthrough

and walk_try state _expression tried cases effect_cases marks =
  (* A handler starts before [tried] has completed, so it cannot inherit
     facts from that evaluation.  At the join, however, keep facts common to
     every path that can complete: the normal path, returning handlers, and
     a resumed effect path (which completes the captured computation). *)
  let pre_try_facts = state.facts in
  let make_normal_try_result_may_complete outer_may_complete =
    expression_may_complete_without_handled_effect
      ~outer_may_complete effect_cases
  in
  let normal_try_result_may_complete =
    make_normal_try_result_may_complete (fun _ -> true)
  in
  let try_returns = normal_try_result_may_complete tried in
  let tried_marks =
    restrict_result_marks make_normal_try_result_may_complete marks
  in
  walk_expression
    ~inherited_marks:
      (if try_returns then result_marks tried tried_marks else [])
    state tried;
  let normal_try_facts = state.facts in
  let case_returns case =
    Option.fold ~none:true ~some:expression_may_complete case.c_guard
    && expression_may_complete case.c_rhs
  in
  let walk_handler_cases cases =
    let rec loop fallthrough returning = function
      | [] -> List.rev returning
      | case :: cases ->
        let reachable = Option.is_some fallthrough in
        let entry_facts =
          Option.value ~default:pre_try_facts fallthrough
        in
        let rhs_facts, _, next =
          prepare_case_edges state ~entry_facts ~reachable
            ~selection_pattern:(Some case.c_lhs) case
        in
        state.facts <- rhs_facts;
        if reachable && case_returns case
        then
          walk_expression
            ~inherited_marks:(result_marks case.c_rhs marks)
            state case.c_rhs
        else walk_expression state case.c_rhs;
        let handler_facts = state.facts in
        let returning =
          if reachable && case_returns case then
            let handler_path = handler_facts, case.c_rhs in
            if effect_case_may_resume case
            (* [continue] is a library function, not a compiler primitive, so
               no resolved library name is sufficient evidence that this arm
               must resume.  Keep both possibilities even for a direct call;
               this deliberately trades summary precision for correctness. *)
            then (normal_try_facts, tried) :: handler_path :: returning
            else handler_path :: returning
          else returning
        in
        state.facts <- pre_try_facts;
        loop next returning cases
    in
    loop (Some pre_try_facts) [] cases
  in
  let returning_handlers =
    walk_handler_cases cases @ walk_handler_cases effect_cases
  in
  let returning_paths =
    (if try_returns then [normal_try_facts, tried] else [])
    @ returning_handlers
  in
  state.facts <-
    begin match returning_paths with
    | (first_facts, _) :: rest ->
      List.fold_left
        (fun facts (path_facts, _) -> Facts.intersect facts path_facts)
        first_facts rest
    | [] -> pre_try_facts
    end;
  returning_paths

and add_try_result_fact state pattern paths =
  match pattern_variable pattern with
  | None -> ()
  | Some id ->
    let loc = pattern.pat_loc in
    let result =
      Refinement.create ~loc
        ~type_:(carrier ~env:pattern.pat_env pattern.pat_type)
        (Rexp_ident (Rbound id))
    in
    let outer_scope = Facts.scope state.facts in
    let rec completed_result expression =
      match expression.exp_desc with
      | Texp_sequence (_, _, result) -> completed_result result
      | Texp_let (Nonrecursive, _, _) -> Some expression
      | Texp_let (Recursive, _, _) | Texp_letmutable _ -> None
      | Texp_open (_, body) | Texp_exclave body ->
        completed_result body
      | _ -> Some expression
    in
    let conjoin left right =
      Refinement.create ~loc ~type_:Predef.type_bool
        (Rexp_ifthenelse (left, right, Some (bool_node ~loc false)))
    in
    let disjoin left right =
      Refinement.create ~loc ~type_:Predef.type_bool
        (Rexp_ifthenelse (left, bool_node ~loc true, Some right))
    in
    let path_fact (facts, expression) =
      match completed_result expression with
      | None -> None
      | Some expression ->
        let path_state = { state with facts } in
        match expression_is_stable path_state expression,
              subject path_state expression with
        | false, _ -> None
        | exception Unsupported_subject _ -> None
        | true, path_result ->
          Option.map
            (fun result_equality ->
              let path_facts =
                List.filter
                  (fun (fact : Vox_vc.fact) ->
                    Ident.Set.subset
                      (Refinement.free_bound_identifiers fact.expression)
                      outer_scope)
                  (Facts.facts facts)
              in
              List.fold_right
                (fun (fact : Vox_vc.fact) formula ->
                  conjoin fact.expression formula)
                path_facts result_equality)
            (equality ~env:pattern.pat_env ~loc result path_result)
    in
    let rec all_path_facts paths =
      match paths with
      | [] -> Some []
      | path :: paths ->
        begin match path_fact path, all_path_facts paths with
        | Some fact, Some facts -> Some (fact :: facts)
        | None, _ | _, None -> None
        end
    in
    begin match all_path_facts paths with
    | None | Some [] -> ()
    | Some (first :: rest) ->
      let summary = List.fold_left disjoin first rest in
      let origin = fact_origin ~kind:"try-result" loc in
      state.facts <- Facts.add ~origin ~loc summary state.facts
    end

and check_marks_against state ~env ~subject_location ?result_span ~subject marks =
  let result_span = Option.value ~default:subject_location result_span in
  List.iter
    (fun
      { annotation_location;
        result_location;
        result_may_complete = _;
        refinement;
      } ->
      let loc =
        Option.value ~default:annotation_location result_location
      in
      let provenance () =
        annotation_provenance ~annotation_location
          ~subject_location
      in
      prove_refinement state ~env ~loc ~subject refinement
        ~kind:"annotation" ~program_point:subject_location
        ~result_span ~provenance)
    marks

and check_marks ?result_span state expression marks =
  match marks with
  | [] -> ()
  | _ ->
    check_marks_against state ~env:expression.exp_env
      ~subject_location:expression.exp_loc ?result_span
      ~subject:(subject state expression) marks

and case_scope : type k. k case -> Location.t =
  fun case ->
  match case.c_guard with
  | None -> case.c_rhs.exp_loc
  | Some guard ->
    Location.merge ~ghost:false [guard.exp_loc; case.c_rhs.exp_loc]

and walk_case_facts : type k. state -> k case -> Facts.t =
  fun state case ->
  let saved_facts = state.facts in
  enter_pattern state ~fact:true ~scope:(case_scope case) case.c_lhs;
  Option.iter (walk_expression state) case.c_guard;
  walk_expression state case.c_rhs;
  let case_facts = state.facts in
  state.facts <- saved_facts;
  case_facts

and walk_value_function_case state (case : value case) =
  let saved_facts = state.facts in
  let scope = case_scope case in
  enter_pattern state ~fact:true ~scope case.c_lhs;
  begin match case.c_lhs.pat_desc with
  | Tpat_var _ -> ()
  | _ ->
    let subject = value_pattern_subject state case.c_lhs in
    add_value_pattern_facts state ~subject ~scope case.c_lhs
  end;
  Option.iter (walk_expression state) case.c_guard;
  walk_expression state case.c_rhs;
  state.facts <- saved_facts

and expression_may_complete expression =
  match expression.exp_desc with
  | Texp_apply
      ( { exp_desc =
            Texp_ident
              { desc =
                  { val_kind = Val_prim primitive;
                    _
                  };
                _
              };
          _
        },
        _, _, _, _ )
    when List.mem primitive.prim_name
           ["%raise"; "%reraise"; "%raise_notrace"] ->
    false
  | Texp_apply (function_, arguments, _, _, _)
    when Option.is_some (short_circuit_application function_ arguments) ->
    begin match short_circuit_application function_ arguments with
    | Some (_, left, _) ->
      expression_may_complete function_ && expression_may_complete left
    | None -> assert false
    end
  | Texp_apply (function_, arguments, _, _, _) ->
    expression_may_complete function_
    && List.for_all
         (function
           | _, Arg (argument, _) -> expression_may_complete argument
           | _, Omitted _ -> true)
         arguments
  | Texp_unreachable -> false
  | Texp_sequence (first, _, second) ->
    expression_may_complete first && expression_may_complete second
  | Texp_let (_, bindings, body) ->
    List.for_all
      (fun binding -> expression_may_complete binding.vb_expr)
      bindings
    && expression_may_complete body
  | Texp_letmutable (binding, body) ->
    expression_may_complete binding.vb_expr
    && expression_may_complete body
  | Texp_ifthenelse (condition, ifso, Some ifnot) ->
    expression_may_complete condition
    && (expression_may_complete ifso || expression_may_complete ifnot)
  | Texp_ifthenelse (condition, _, None) ->
    (* A missing [else] has a normally returning unit path, but that path is
       reachable only after the condition itself returns. *)
    expression_may_complete condition
  | Texp_match (scrutinee, _, cases, effect_cases, _) ->
    let case_may_complete case =
      Option.fold ~none:true ~some:expression_may_complete case.c_guard
      && expression_may_complete case.c_rhs
    in
    let value_case_may_complete case =
      Option.is_some (computation_value_pattern case.c_lhs)
      && case_may_complete case
    in
    let interrupted_case_may_complete case =
      Option.is_none (computation_value_pattern case.c_lhs)
      && case_may_complete case
    in
    (expression_may_complete scrutinee
     && List.exists value_case_may_complete cases)
    || List.exists interrupted_case_may_complete cases
    || List.exists case_may_complete effect_cases
  | Texp_try (tried, cases, effect_cases) ->
    let case_may_complete case =
      Option.fold ~none:true ~some:expression_may_complete case.c_guard
      && expression_may_complete case.c_rhs
    in
    expression_may_complete tried
    || List.exists case_may_complete (cases @ effect_cases)
  | Texp_quotation _ -> true
  | Texp_open ({ open_expr = module_expression; _ }, body) ->
    module_expression_may_complete module_expression
    && expression_may_complete body
  | Texp_exclave body ->
    expression_may_complete body
  | _ -> true

and expression_may_complete_without_handled_effect
    ?(outer_may_complete = fun _ -> true) effect_cases expression =
  let may_complete =
    expression_may_complete_without_handled_effect
      ~outer_may_complete effect_cases
  in
  let case_may_complete case =
    Option.fold ~none:true ~some:may_complete case.c_guard
    && may_complete case.c_rhs
  in
  match expression.exp_desc with
  | Texp_apply _ when Option.is_some (performed_effect_constructor expression) ->
    begin match local_effect_handling effect_cases expression with
    | Definitely_nonresuming ->
      (* This exact effect transfers to the first matching local arm, and that
         arm cannot resume this body. *)
      false
    | Definitely_may_resume | Possibly_may_resume ->
      (* The innermost matching arm may resume this body. *)
      true
    | Not_definitely_handled ->
      (* Delegate an unmatched or uncertain effect to the next enclosing
         handler scope. *)
      outer_may_complete expression
    end
  | Texp_apply
      ( { exp_desc =
            Texp_ident
              { desc = { val_kind = Val_prim primitive; _ };
                _
              };
          _
        },
        _, _, _, _ )
    when List.mem primitive.prim_name
           ["%raise"; "%reraise"; "%raise_notrace"] ->
    false
  | Texp_apply (function_, arguments, _, _, _)
    when Option.is_some (short_circuit_application function_ arguments) ->
    begin match short_circuit_application function_ arguments with
    | Some (_, left, _) -> may_complete function_ && may_complete left
    | None -> assert false
    end
  | Texp_apply (function_, arguments, _, _, _) ->
    may_complete function_
    && List.for_all
         (function
           | _, Arg (argument, _) -> may_complete argument
           | _, Omitted _ -> true)
         arguments
  | Texp_unreachable -> false
  | Texp_sequence (first, _, second) ->
    may_complete first && may_complete second
  | Texp_let (_, bindings, body) ->
    List.for_all (fun binding -> may_complete binding.vb_expr) bindings
    && may_complete body
  | Texp_letmutable (binding, body) ->
    may_complete binding.vb_expr && may_complete body
  | Texp_ifthenelse (condition, ifso, Some ifnot) ->
    may_complete condition && (may_complete ifso || may_complete ifnot)
  | Texp_ifthenelse (condition, _, None) -> may_complete condition
  | Texp_match (scrutinee, _, cases, nested_effect_cases, _) ->
    let value_case_may_complete case =
      Option.is_some (computation_value_pattern case.c_lhs)
      && case_may_complete case
    in
    let interrupted_case_may_complete case =
      Option.is_none (computation_value_pattern case.c_lhs)
      && case_may_complete case
    in
    (may_complete scrutinee
     && List.exists value_case_may_complete cases)
    || List.exists interrupted_case_may_complete cases
    || List.exists case_may_complete nested_effect_cases
  | Texp_try (tried, cases, nested_effect_cases) ->
    may_complete tried
    || List.exists case_may_complete (cases @ nested_effect_cases)
  | Texp_open ({ open_expr = module_expression; _ }, body) ->
    module_expression_may_complete module_expression && may_complete body
  | Texp_exclave body -> may_complete body
  | Texp_quotation _ -> true
  | _ -> outer_may_complete expression

and module_expression_may_complete module_expression =
  match module_expression.mod_desc with
  | Tmod_ident _ | Tmod_functor _ | Tmod_structure _
  | Tmod_apply _ | Tmod_apply_unit _ -> true
  | Tmod_constraint (module_expression, _, _, _) ->
    module_expression_may_complete module_expression
  | Tmod_unpack (expression, _) -> expression_may_complete expression

and effect_case_may_resume case =
  match case.c_cont with
  | None -> false
  | Some continuation ->
    (* A continuation can be resumed through an alias or helper that this pass
       cannot inspect.  Any use therefore keeps both the handler-return and
       resumed-computation paths. *)
    let used = ref false in
    let super = Tast_iterator.default_iterator in
    let iterator =
      { super with
        expr =
          (fun iterator expression ->
            begin match expression.exp_desc with
            | Texp_ident { path = Pident id; _ }
              when Ident.same continuation id ->
              used := true
            | _ -> ()
            end;
            if not !used then super.expr iterator expression);
      }
    in
    iterator.expr iterator case.c_rhs;
    !used

and local_effect_handling effect_cases expression =
  match performed_effect_constructor expression with
  | None -> Not_definitely_handled
  | Some constructor ->
    let may_resume = function
      | Definitely_may_resume | Possibly_may_resume -> true
      | Definitely_nonresuming | Not_definitely_handled -> false
    in
    let rec first_matching_case = function
      | [] -> Not_definitely_handled
      | case :: cases ->
        begin match effect_pattern_matches_constructor case.c_lhs constructor with
        | Definitely_does_not_match -> first_matching_case cases
        | Match_unknown ->
          if effect_case_may_resume case
             || may_resume (first_matching_case cases)
          then Possibly_may_resume
          else Not_definitely_handled
        | Definitely_matches ->
          begin match case.c_guard with
          | Some _ ->
            if effect_case_may_resume case
               || may_resume (first_matching_case cases)
            then Possibly_may_resume
            else Not_definitely_handled
          | None ->
            if effect_case_may_resume case
            then Definitely_may_resume
            else Definitely_nonresuming
          end
        end
    in
    first_matching_case effect_cases

and walk_match state scrutinee cases effect_cases marks =
  let pre_scrutinee_facts = state.facts in
  walk_expression state scrutinee;
  let completed_scrutinee_facts = state.facts in
  let scrutinee_returns = expression_may_complete scrutinee in
  let scrutinee_subject, synthetic =
    match scrutinee.exp_desc with
    | Texp_ident _ ->
      begin match subject state scrutinee with
      | subject -> subject, None
      | exception Unsupported_subject _ ->
        let id = Ident.create_local "*match-scrutinee*" in
        ( Refinement.create ~loc:scrutinee.exp_loc
            ~type_:(carrier ~env:scrutinee.exp_env scrutinee.exp_type)
            (Rexp_ident (Rbound id)),
          Some id )
      end
    | _ ->
      let id = Ident.create_local "*match-scrutinee*" in
      ( Refinement.create ~loc:scrutinee.exp_loc
          ~type_:(carrier ~env:scrutinee.exp_env scrutinee.exp_type)
          (Rexp_ident (Rbound id)),
        Some id )
  in
  let exception Unsupported_normal_result in
  let rec normal_results expression =
    match expression.exp_desc with
    | Texp_ifthenelse (condition, ifso, Some ifnot)
      when expression_may_complete condition ->
      normal_results ifso @ normal_results ifnot
    | Texp_sequence (first, _, second)
      when expression_may_complete first ->
      normal_results second
    | Texp_let (Nonrecursive, bindings, _)
      when List.for_all
             (fun binding -> expression_may_complete binding.vb_expr)
             bindings
           && expression_may_complete expression ->
      (* Keep the binder context attached to the result.  [subject] lowers a
         nonrecursive let to a logical let; stripping it here made the body
         refer to identifiers that had already left scope. *)
      [expression]
    | Texp_let (Recursive, _, _)
      when expression_may_complete expression ->
      (* A completing recursive let contributes a real normal result, but
         [subject] cannot lower its recursive binder.  Returning [[]] here
         would silently erase this path from an enclosing [if] summary and
         could make another match arm appear unreachable. *)
      raise Unsupported_normal_result
    | Texp_let (Recursive, _, _) -> []
    | Texp_letmutable (binding, body)
      when expression_may_complete binding.vb_expr ->
      normal_results body
    | Texp_open ({ open_expr = module_expression; _ }, body)
      when module_expression_may_complete module_expression ->
      normal_results body
    | Texp_open _ -> []
    | Texp_exclave body -> normal_results body
    | Texp_apply _
      when Option.is_some (performed_effect_constructor expression) ->
      begin match local_effect_handling effect_cases expression with
      | Definitely_nonresuming -> []
      | Definitely_may_resume | Possibly_may_resume ->
        raise Unsupported_normal_result
      | Not_definitely_handled ->
        if expression_may_complete expression then [expression] else []
      end
    | _ when expression_may_complete expression -> [expression]
    | _ -> []
  in
  let normal_scrutinee_summary =
    match synthetic with
    | None -> None
    | Some _ ->
      let result_equality result =
        match subject { state with facts = completed_scrutinee_facts } result with
        | result_subject ->
          equality ~env:scrutinee.exp_env ~loc:scrutinee.exp_loc
            scrutinee_subject result_subject
        | exception Unsupported_subject _ -> None
      in
      let rec all_equalities results =
        match results with
        | [] -> Some []
        | result :: results ->
          begin match result_equality result, all_equalities results with
          | Some equality, Some equalities -> Some (equality :: equalities)
          | None, _ | _, None -> None
          end
      in
      let equalities =
        match normal_results scrutinee with
        | results -> all_equalities results
        | exception Unsupported_normal_result -> None
      in
      begin match equalities with
      | None | Some [] -> None
      | Some (first :: rest) ->
        let summary =
          List.fold_left
            (fun left right ->
              Refinement.create ~loc:scrutinee.exp_loc
                ~type_:Predef.type_bool
                (Rexp_ifthenelse
                   (left, bool_node ~loc:scrutinee.exp_loc true, Some right)))
            first rest
        in
        Some summary
      end
  in
  let normal_scrutinee_facts = completed_scrutinee_facts in
  let normal_value_entry_facts =
    state.facts <- normal_scrutinee_facts;
    Option.iter
      (fun id -> state.facts <- Facts.enter id state.facts)
      synthetic;
    Option.iter
      (fun summary ->
        let origin = fact_origin ~kind:"match-result" scrutinee.exp_loc in
        state.facts <-
          Facts.add ~origin ~loc:scrutinee.exp_loc summary state.facts)
      normal_scrutinee_summary;
    state.facts
  in
  let returning_facts = ref [] in
  let leaf_marks expression = result_marks expression marks in
  let case_returns case =
    Option.fold ~none:true ~some:expression_may_complete case.c_guard
    && expression_may_complete case.c_rhs
  in
  let record_returning_facts ~reachable entry_facts case =
    if reachable && case_returns case then
      returning_facts :=
        Facts.restrict (Facts.scope entry_facts) state.facts
        :: !returning_facts
  in
  let walk_interrupted_case entry_facts ~fallthrough_reachable ~rhs_reachable
      ~selection_pattern case =
    let rhs_facts, guard_fallthrough, fallthrough =
      prepare_case_edges state ~entry_facts
        ~reachable:fallthrough_reachable ~selection_pattern case
    in
    state.facts <- rhs_facts;
    if rhs_reachable && case_returns case
    then
      walk_expression ~inherited_marks:(leaf_marks case.c_rhs)
        state case.c_rhs
    else walk_expression state case.c_rhs;
    state.facts, guard_fallthrough, fallthrough
  in
  let walk_value_case entry case pattern =
    let scope = case_scope case in
    let reachable = scrutinee_returns && Option.is_some entry in
    let entry_facts =
      Option.value ~default:normal_value_entry_facts entry
    in
    let entry_scope = Facts.scope entry_facts in
    state.facts <- entry_facts;
    enter_pattern state ~fact:true ~scope case.c_lhs;
    add_refinement_fact state ~env:scrutinee.exp_env ~kind:"match"
      ~loc:scrutinee.exp_loc ~scope ~subject:scrutinee_subject
      scrutinee.exp_type;
    add_value_pattern_facts state ~subject:scrutinee_subject ~scope pattern;
    let matched_facts = state.facts in
    let rhs_facts, guard_fallthrough =
      match case.c_guard with
      | None -> matched_facts, None
      | Some guard ->
        walk_guard_edges state ~entry_scope matched_facts guard
    in
    state.facts <- rhs_facts;
    if reachable && case_returns case
    then
      walk_expression ~inherited_marks:(leaf_marks case.c_rhs)
        state case.c_rhs
    else walk_expression state case.c_rhs;
    record_returning_facts ~reachable
      normal_scrutinee_facts case;
    state.facts <- normal_scrutinee_facts;
    value_case_fallthrough ~entry_facts ~reachable
      ~subject:scrutinee_subject ~pattern ~guard_fallthrough
  in
  let has_interrupted_case = ref false in
  let interrupted_fallthrough = ref (Some pre_scrutinee_facts) in
  let fallthrough =
    List.fold_left
      (fun fallthrough case ->
        match computation_value_pattern case.c_lhs with
        | None ->
          has_interrupted_case := true;
          let interrupted_reachable =
            Option.is_some !interrupted_fallthrough
          in
          let interrupted_entry_facts =
            Option.value ~default:pre_scrutinee_facts !interrupted_fallthrough
          in
          let value_entry_facts =
            Option.value ~default:normal_value_entry_facts fallthrough
          in
          let value_pattern, exception_pattern = split_pattern case.c_lhs in
          let value_reachable =
            scrutinee_returns
            && Option.is_some fallthrough
            && Option.is_some value_pattern
          in
          let rhs_reachable = interrupted_reachable || value_reachable in
          let case_facts, guard_fallthrough, next =
            walk_interrupted_case interrupted_entry_facts
              ~fallthrough_reachable:interrupted_reachable ~rhs_reachable
              ~selection_pattern:exception_pattern case
          in
          interrupted_fallthrough := next;
          state.facts <- case_facts;
          record_returning_facts ~reachable:rhs_reachable
            interrupted_entry_facts case;
          state.facts <- pre_scrutinee_facts;
          begin match value_pattern with
          | None -> fallthrough
          | Some pattern ->
            let guard_fallthrough =
              Option.map
                (fun facts -> Facts.intersect facts value_entry_facts)
                guard_fallthrough
            in
            value_case_fallthrough ~entry_facts:value_entry_facts
              ~reachable:value_reachable ~subject:scrutinee_subject ~pattern
              ~guard_fallthrough
          end
        | Some pattern ->
          walk_value_case fallthrough case pattern)
      (if scrutinee_returns then Some normal_value_entry_facts else None)
      cases
  in
  ignore (fallthrough : Facts.t option);
  if effect_cases <> [] then has_interrupted_case := true;
  let effect_fallthrough = ref (Some pre_scrutinee_facts) in
  List.iter
    (fun case ->
      let reachable = Option.is_some !effect_fallthrough in
      let entry_facts =
        Option.value ~default:pre_scrutinee_facts !effect_fallthrough
      in
      let case_facts, _, next =
        walk_interrupted_case entry_facts ~fallthrough_reachable:reachable
          ~rhs_reachable:reachable
          ~selection_pattern:(Some case.c_lhs) case
      in
      effect_fallthrough := next;
      state.facts <- case_facts;
      record_returning_facts ~reachable entry_facts case;
      state.facts <- pre_scrutinee_facts)
    effect_cases;
  (* Facts at the normal join must hold on every arm that can return.  Value
     arms start after normal completion of the scrutinee; exception and effect
     arms start before it.  Pattern-local and synthetic identifiers are closed
     at each arm boundary before the returning environments are intersected. *)
  state.facts <-
    match !returning_facts with
    | first :: rest -> List.fold_left Facts.intersect first rest
    | [] ->
      if !has_interrupted_case
      then pre_scrutinee_facts
      else if scrutinee_returns
      then normal_scrutinee_facts
      else pre_scrutinee_facts

and check_application state application function_ arguments
    ~entry_facts ~argument_facts =
  let boundary_facts = state.facts in
  let relation_facts = ref entry_facts in
  let metadata =
    List.find_map
      (fun (extra, _, _) ->
        match extra with
        | Texp_refinement_application metadata -> Some metadata
        | Texp_constraint _ | Texp_coerce _ | Texp_poly _
        | Texp_newtype _ | Texp_stack | Texp_mode _
        | Texp_refinement_constraint _
        | Texp_inspected_type _ | Texp_borrowed | Texp_ghost_region ->
          None)
      application.exp_extra
  in
  let rec check_arguments arguments argument_facts metadata =
    match arguments, argument_facts, metadata with
    | [], [], [] -> ()
    | (label, argument) :: arguments,
      argument_facts :: remaining_facts,
      contract :: metadata ->
      begin match argument, contract.rap_supplied with
      | Arg (argument, _), true ->
        let result = final_result_site argument in
        let argument_facts = Option.get argument_facts in
        state.facts <- merge_facts argument_facts !relation_facts;
        Option.iter
          (fun stored_subject ->
            let actual_subject = subject state argument in
            if
              Refinement.alpha_equal ~equal_type:(fun _ _ -> true)
                stored_subject actual_subject
            then ()
            else match
              equality ~env:application.exp_env ~loc:argument.exp_loc
                stored_subject actual_subject
            with
            | Some equation ->
              let origin =
                fact_origin ~kind:"argument-value" argument.exp_loc
              in
              state.facts <-
                Facts.add ~origin ~loc:argument.exp_loc equation state.facts;
              relation_facts :=
                Facts.add ~origin ~loc:argument.exp_loc equation
                  !relation_facts
            | None ->
              Location.raise_errorf ~loc:argument.exp_loc
                "dependent argument value cannot be represented in a \
                 refinement equality")
          contract.rap_subject;
        Option.iter
          (fun refinement ->
            let provenance () =
              contract_argument_provenance
                ~application_location:application.exp_loc
                ~argument_location:argument.exp_loc
                ~parameter:contract.rap_binder refinement
            in
            prove_refinement state ~env:application.exp_env
              ~loc:result.exp_loc ~kind:"contract-argument"
              ~program_point:application.exp_loc
              ~result_span:result.exp_loc ~provenance
              ~subject:(subject state argument) refinement)
          (refinement ~env:application.exp_env contract.rap_domain)
      | Omitted _, false -> ()
      | (Arg _ | Omitted _), _ ->
        Location.raise_errorf ~loc:application.exp_loc
          "inconsistent refinement application metadata for argument %s"
          (Printtyp.string_of_label label)
      end;
      check_arguments arguments remaining_facts metadata
    | ([], _, _) | (_, [], _) | (_, _, []) ->
      Location.raise_errorf ~loc:application.exp_loc
        "incomplete refinement application metadata"
  in
  begin match metadata with
  | Some metadata ->
    check_arguments arguments argument_facts metadata.rapp_arguments
  | None
    when contains_refinement ~env:function_.exp_env function_.exp_type ->
    Location.raise_errorf ~loc:application.exp_loc
      "missing refinement application metadata"
  | None -> ()
  end;
  state.facts <- merge_facts boundary_facts !relation_facts;
  Option.iter
    (fun metadata ->
      let name =
        match function_.exp_desc with
        | Texp_ident { path; _ } -> Some (Path.last path)
        | _ -> None
      in
      add_established_result_contract state ~kind:"application" ?name
        application metadata.rapp_result)
    metadata

and walk_default_expression state expression =
  let super = Tast_iterator.default_iterator in
  let iterator = iterator state in
  super.expr iterator expression

and iterator state =
  let super = Tast_iterator.default_iterator in
  { super with
    expr = (fun _ expression -> walk_expression state expression);
    class_declaration =
      (fun iterator declaration ->
        let saved_facts = state.facts in
        super.class_declaration iterator declaration;
        (* A class declaration packages initialization for later instances.
           This does not affect object literals, whose class structures are
           reached directly from [Texp_object] and execute immediately. *)
        state.facts <- saved_facts);
    structure = (fun _ structure -> walk_structure state structure);
    value_bindings =
      (fun _ (rec_flag, bindings) ->
        walk_value_bindings state ~persist:true rec_flag bindings);
  }

and walk_value_bindings state ~persist rec_flag bindings =
  let saved_facts = state.facts in
  if rec_flag = Recursive then begin
    List.iter
      (fun binding -> enter_pattern state ~fact:false binding.vb_pat)
      bindings;
    List.iter (register_definition state) bindings
  end;
  let rhs_entry_facts = state.facts in
  let rhs_facts =
    List.map
      (fun binding ->
        state.facts <- rhs_entry_facts;
        if is_def_axiom_binding binding then Some rhs_entry_facts
        else begin
          walk_expression state binding.vb_expr;
          if expression_may_complete binding.vb_expr
          then Some state.facts
          else None
        end)
      bindings
  in
  state.facts <-
    if List.for_all Option.is_some rhs_facts
    then
      List.fold_left merge_facts rhs_entry_facts
        (List.filter_map Fun.id rhs_facts)
    else rhs_entry_facts;
  if rec_flag = Nonrecursive then
    List.iter (register_definition state) bindings;
  List.iter
    (fun binding -> enter_pattern state ~fact:true binding.vb_pat)
    bindings;
  if rec_flag = Nonrecursive then
    List.iter (selfification_fact state) bindings;
  if not persist then begin
    state.facts <- saved_facts
  end

and walk_structure state structure =
  let saved_facts = state.facts in
  let iterator = iterator state in
  List.iter
    (Tast_iterator.default_iterator.structure_item iterator)
    structure.str_items;
  state.facts <- saved_facts

let toplevel_facts = ref Facts.empty
let toplevel_total_functions = Types.Uid.Tbl.create 16

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
  | Rexp_match (scrutinee, cases) ->
    collect_refinement_expression ~env scrutinee;
    List.iter
      (fun case -> collect_refinement_expression ~env case.rcase_body)
      cases

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

let add_expression_type_judgment ~env ~location ~provenance ?source_span
    type_ =
  if valid_local_span location then
    match source_span with
    | Some source_span when not (valid_local_span source_span) -> ()
    | None | Some _ ->
      let source_span_field =
        match source_span with
        | None -> []
        | Some source_span ->
          [Json.field "source_span" (json_span source_span)]
      in
      expression_type_judgments :=
        Json.object_
          ([ Json.field "location" (json_span location);
             Json.field "type" (json_string (render_type ~env type_));
             Json.field "provenance" (json_string provenance);
           ]
           @ source_span_field)
        :: !expression_type_judgments

let collect_imposition_type_judgments () =
  let judgments = Typecore.take_refinement_imposition_judgments () in
  List.iter
    (fun (judgment : Typecore.refinement_imposition_judgment) ->
      Option.iter
        (add_expression_type_judgment ~env:judgment.env
           ~location:judgment.location ~provenance:"checked")
        judgment.checked_type;
      add_expression_type_judgment ~env:judgment.env
        ~location:judgment.location ~provenance:"imposed"
        ~source_span:judgment.annotation_location judgment.imposed_type)
    judgments;
  List.map
    (fun (judgment : Typecore.refinement_imposition_judgment) ->
      judgment.location)
    judgments

let collect_checked_expression_types ~imposition_locations structure =
  let seen = ref imposition_locations in
  let super = Tast_iterator.default_iterator in
  let iterator =
    { super with
      expr =
        (fun sub expression ->
          if
            valid_local_span expression.exp_loc
            && not (List.exists (same_span expression.exp_loc) !seen)
          then begin
            seen := expression.exp_loc :: !seen;
            add_expression_type_judgment ~env:expression.exp_env
              ~location:expression.exp_loc ~provenance:"checked"
              expression.exp_type
          end;
          super.expr sub expression);
    }
  in
  List.iter (iterator.structure_item iterator) structure.str_items

(* Per-identifier-occurrence semantic classification for editor coloring.
   Every [Texp_ident] occurrence with a real source span yields one token
   recording the occurrence's totality and logicality axes as read off the
   typed occurrence, its syntactic role (application head, statement-position
   mention, or ordinary use), and the derived classification.  The
   classification reuses the totality evidence from [call_head_is_stable] --
   the [total_functions] registry, an explicit [Texp_mode] totality, and the
   conservative totality projection.  Its presentation policy is deliberately
   narrower than general fact admission: only an eventual refined-unit law
   result is a proof artifact.  Partial/effectful calls therefore remain
   ordinary even when their established result contract can contribute a fact.
   Unlike [call_head_is_stable] there is no unconditional primitive escape:
   arithmetic on ints is not a lemma call.  A registered total definition
   supplies totality but does not by itself make a computation a proof
   artifact. *)

type semantic_role =
  | Semantic_head
  | Semantic_statement
  | Semantic_use

let semantic_role_string = function
  | Semantic_head -> "call-head"
  | Semantic_statement -> "statement"
  | Semantic_use -> "use"

let semantic_totality_string expression mode =
  if expression_has_total_mode expression then "total"
  else
    match
      Mode.Totality.Guts.check_const_conservative
        (Mode.Value.proj_comonadic Mode.Axis.Totality mode)
    with
    | Some Mode.Totality.Const.Total -> "total"
    | Some Mode.Totality.Const.Partial -> "partial"
    | None -> "unknown"

let semantic_logicality_string mode =
  match
    Mode.Logicality.Guts.check_const_conservative
      (Mode.Value.proj_monadic Mode.Axis.Logicality mode)
  with
  | Some Mode.Logicality.Const.Logical -> "logical"
  | Some Mode.Logicality.Const.Physical -> "physical"
  | None -> "unknown"

let semantic_arrow_type ~env type_ =
  match get_desc (Ctype.expand_head env type_) with
  | Tarrow _ -> true
  | _ -> false
  | exception _ -> false

let semantic_proof_contract ~env type_ =
  let seen_paths = ref Path.Set.empty in
  let rec result type_ =
    match get_desc type_ with
    | Tpoly (type_, _) -> result type_
    | Tarrow (_, _, type_, _) -> result type_
    | Trefine refinement -> unit_carrier refinement.ref_skeleton
    | Tconstr (path, _, _) ->
      if Path.Set.mem path !seen_paths then false
      else begin
        seen_paths := Path.Set.add path !seen_paths;
        match expand_head_for_refinement ~env type_ with
        | expanded -> result expanded
        | exception _ -> false
      end
    | _ -> false
  and unit_carrier type_ =
    match get_desc (expand_head_for_refinement ~env type_) with
    | Tconstr (path, [], _) -> Path.same path Predef.path_unit
    | _ -> false
    | exception _ -> false
  in
  result type_

let collect_semantic_token state ~role expression =
  match expression.exp_desc with
  | Texp_ident { path; desc; mode; _ }
    when valid_local_span expression.exp_loc ->
    let env = expression.exp_env in
    let totality = semantic_totality_string expression mode in
    let logicality = semantic_logicality_string mode in
    let total_definition =
      Types.Uid.Tbl.mem state.total_functions desc.val_uid
    in
    let arrow = semantic_arrow_type ~env expression.exp_type in
    (* The occurrence's [exp_type] can be an unrefined instance, so consult the
       resolved value's declared type as well.  A proof contract is narrower
       than merely containing a refinement: after following arrows and type
       aliases its result must be a refined unit law.  Refined computational
       results and refined argument domains remain ordinary program code. *)
    let proof_contract =
      semantic_proof_contract ~env expression.exp_type
      || semantic_proof_contract ~env desc.val_type
    in
    let known_total =
      total_definition
      || expression_has_total_mode expression
      || String.equal totality "total"
    in
    let logical = String.equal logicality "logical" in
    (* Totality or logicality is necessary but not sufficient.  Only a refined
       unit result is proof-only; [int{p}], [bool{p}], and refinements confined
       to argument domains describe computations rather than lemma evidence. *)
    let classification =
      match role with
      | Semantic_head | Semantic_statement ->
        if (known_total || logical) && proof_contract then "proof-call"
        else "ordinary"
      | Semantic_use ->
        if (known_total || logical) && proof_contract && arrow
        then "proof-use"
        else "ordinary"
    in
    let token =
      Json.object_
        [ Json.field "location" (json_span expression.exp_loc);
          Json.field "name" (json_string (Path.name path));
          Json.field "role" (json_string (semantic_role_string role));
          Json.field "totality" (json_string totality);
          Json.field "logicality" (json_string logicality);
          Json.field "total_definition" (string_of_bool total_definition);
          Json.field "classification" (json_string classification);
        ]
    in
    semantic_identifier_tokens := token :: !semantic_identifier_tokens;
    if not (String.equal classification "ordinary") then begin
      let mode_text =
        "@"
        ^ (if known_total then " total" else "")
        ^ (if logical then " logical" else "")
      in
      dumped_identifier_modes :=
        Json.object_
          [ Json.field "location" (json_span expression.exp_loc);
            Json.field "mode" (json_string mode_text);
          ]
        :: !dumped_identifier_modes
    end
  | _ -> ()

(* Runs before the fact walk so tokens survive a verification abort; total
   definitions are pre-registered here through the same [register_definition]
   the fact walk uses (re-registration there is idempotent). *)
let collect_semantic_tokens state structure =
  let consumed = ref [] in
  let consume expression =
    match expression.exp_desc with
    | Texp_ident _ -> consumed := expression.exp_loc :: !consumed
    | _ -> ()
  in
  let super = Tast_iterator.default_iterator in
  let iterator =
    { super with
      value_bindings =
        (fun sub (rec_flag, bindings) ->
          (* Match the fact walk's dependency order: recursive names are
             available in their right-hand sides, while a nonrecursive alias
             is registered only after its right-hand side (including nested
             aliases) has been traversed. *)
          if rec_flag = Recursive then
            List.iter (register_definition state) bindings;
          super.value_bindings sub (rec_flag, bindings);
          if rec_flag = Nonrecursive then
            List.iter (register_definition state) bindings);
      expr =
        (fun sub expression ->
          (match expression.exp_desc with
           | Texp_apply (function_, _, _, _, _) ->
             consume function_;
             collect_semantic_token state ~role:Semantic_head function_
           | Texp_sequence (first, _, _) ->
             consume first;
             collect_semantic_token state ~role:Semantic_statement first
           | Texp_ident _ ->
             if not (List.exists (same_span expression.exp_loc) !consumed)
             then collect_semantic_token state ~role:Semantic_use expression
           | _ -> ());
          super.expr sub expression);
    }
  in
  List.iter (iterator.structure_item iterator) structure.str_items

let finish_dump () =
  if !Clflags.vox_dump_vc then begin
    Format.eprintf "Error: VCs dumped, not discharged.@.";
    raise Location.Already_displayed_error
  end

let verify_structure ?(toplevel = false) structure =
  with_fresh_refinement_alias_cache (fun () ->
  let state =
    if toplevel
    then
      { facts = !toplevel_facts;
        total_functions = toplevel_total_functions;
        call_subjects = Hashtbl.create 16;
      }
    else
      { facts = Facts.empty;
        total_functions = Types.Uid.Tbl.create 16;
        call_subjects = Hashtbl.create 16;
      }
  in
  let walk_root () =
    if Option.is_some !Clflags.vox_dump_vc_json then begin
      let imposition_locations = collect_imposition_type_judgments () in
      collect_checked_expression_types ~imposition_locations structure;
      collect_refinement_types structure;
      collect_semantic_tokens state structure;
    end else
      ignore (Typecore.take_refinement_imposition_judgments ());
    let iterator = iterator state in
    List.iter
      (Tast_iterator.default_iterator.structure_item iterator)
      structure.str_items;
    if toplevel then toplevel_facts := state.facts
  in
  begin try walk_root () with
  | Unsupported_subject (loc, what) ->
    Location.raise_errorf ~loc
      "Refinement verification failed: %s cannot yet be represented in a \
       verification condition"
      what
  end)
