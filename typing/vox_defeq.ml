(* Definitional equations for total functions.

   A [let[@vox.def] f p1 ... pn = rhs] binding is expanded, before type
   checking, into two structure items:

   - the original binding for [f], forced [@ total] (so a body using a partial
     operation -- integer [/] or [mod], [raise], [List.hd], array indexing --
     is rejected at the totality mode; recursion is admitted only when
     typecore's structural-termination analysis proves the recursive group);

   - a companion binding
       [let f_def p1 ... pn = (() : unit{ f p1 ... pn = rhs })]
     whose refinement asserts [f]'s definitional equation.  [f] stays an
     uninterpreted solver symbol: nothing about its definition reaches the
     solver except through this lemma.  Writing [let () = f_def a1 ... an] then
     deposits the ground fact [f a1 ... an = rhs[a1,...,an]] via the existing
     refined-application-becomes-a-fact mechanism ([Vox_verify.check_application]).

   The lemma's refinement is a TRUSTED axiom: the compiler asserts it because it
   was generated from [f]'s own (checked, total, partial-op-free) body, not
   because the unit body [()] proves it.  The companion binding therefore
   carries [@vox.def.axiom], which tells [Vox_verify] to skip verifying its
   body.  Faithfulness is the trust anchor -- the predicate's right-hand side is
   [f]'s source body retyped in the same scope, with the parameters pinned to
   [f]'s types through the [f p1 ... pn] application, so it denotes exactly
   [f]'s body. *)

open Parsetree

let def_attribute = "vox.def"
let refinement_type_extension = "vox2.refinement.type"

(* Provenance of generated companion-lemma bindings, tracked out-of-band so the
   verification-skip can NEVER be triggered by user-written surface syntax.  The
   expander mints a fresh ghost location for each lemma and records the physical
   location object; [Vox_verify] recognises a generated lemma by PHYSICAL
   identity of its [vb_loc] against this set.  A hand-written binding cannot
   carry one of these objects, so it is always verified normally.  A false
   negative (identity lost to a copy) would only over-reject a genuine lemma --
   never admit an unverified one -- so the channel fails closed. *)
let generated_lemma_locations : Location.t list ref = ref []

let is_generated_lemma_loc loc =
  List.exists (fun recorded -> recorded == loc) !generated_lemma_locations

let has_attribute name attributes =
  List.exists
    (fun (attribute : attribute) -> String.equal attribute.attr_name.txt name)
    attributes

let strip_def_attribute attributes =
  List.filter
    (fun (attribute : attribute) ->
      not (String.equal attribute.attr_name.txt def_attribute))
    attributes

let binding_has_def_attribute binding =
  has_attribute def_attribute binding.pvb_attributes

let error ~loc message =
  Location.raise_errorf ~loc "vox: %s" message

(* The variable name bound by a plain parameter [Pparam_val (Nolabel, None, x)].
   Anything else -- labelled, optional, or a non-variable pattern -- is outside
   the first cut and fails closed. *)
let rec pattern_name (pattern : pattern) =
  match pattern.ppat_desc with
  | Ppat_var name -> Some name
  | Ppat_constraint (pattern, _, _) -> pattern_name pattern
  | _ -> None

let simple_parameter_name (parameter : function_param) =
  match parameter.pparam_desc with
  | Pparam_val (Nolabel, None, pattern) -> pattern_name pattern
  | Pparam_val _ | Pparam_newtype _ -> None

let no_function_constraint =
  { mode_annotations = [];
    ret_mode_annotations = [];
    ret_type_constraint = None;
  }

(* Build the trusted-lemma binding for [f]: [f_def p1 ... pn], whose result is
   [()] refined by [f p1 ... pn = rhs]. *)
let make_lemma_binding ~loc ~function_name ~parameters ~parameter_names ~body =
  let ident name = Ast_helper.Exp.ident ~loc { txt = Longident.Lident name; loc } in
  let application =
    Ast_helper.Exp.apply ~loc (ident function_name)
      (List.map
         (fun (name : string Location.loc) ->
           Nolabel, Ast_helper.Exp.ident ~loc:name.loc
                      { txt = Longident.Lident name.txt; loc = name.loc })
         parameter_names)
  in
  let equation =
    Ast_helper.Exp.apply ~loc (ident "=")
      [ Nolabel, application; Nolabel, body ]
  in
  let unit_type = Ast_helper.Typ.constr ~loc { txt = Longident.Lident "unit"; loc } [] in
  let refined_unit =
    Ast_helper.Typ.extension ~loc
      ( { txt = refinement_type_extension; loc },
        PStr
          [ Ast_helper.Str.eval ~loc
              (Ast_helper.Exp.constraint_ ~loc equation (Some unit_type) []) ] )
  in
  let unit_value =
    Ast_helper.Exp.construct ~loc { txt = Longident.Lident "()"; loc } None
  in
  let lemma_body = Ast_helper.Exp.constraint_ ~loc unit_value (Some refined_unit) [] in
  let lemma_function =
    Ast_helper.Exp.function_ ~loc parameters no_function_constraint
      (Pfunction_body lemma_body)
  in
  (* Mint a fresh ghost location for the lemma binding and record it, so the
     verification-skip is keyed on expander provenance rather than any
     user-writable attribute. *)
  let lemma_loc = { loc with Location.loc_ghost = true } in
  generated_lemma_locations := lemma_loc :: !generated_lemma_locations;
  let binding =
    Ast_helper.Vb.mk ~loc:lemma_loc
      (Ast_helper.Pat.var ~loc { txt = function_name ^ "_def"; loc })
      lemma_function
  in
  { binding with
    pvb_modes = [Location.mkloc (Mode "total") lemma_loc];
  }

(* Expand a single [let[@vox.def] ...] binding into the [f] item (forced
   [@ total]) and the companion [f_def] item.  Raises on anything the first cut
   does not support. *)
let expand_binding ~item_loc ~rec_flag binding =
  let loc = binding.pvb_loc in
  let function_name =
    match binding.pvb_pat.ppat_desc with
    | Ppat_var name -> name.txt
    | _ ->
      error ~loc
        "[@vox.def] requires a binding of a single named function"
  in
  let parameters, body =
    match binding.pvb_expr.pexp_desc with
    | Pexp_function (parameters, _constraint, Pfunction_body body) ->
      parameters, body
    | Pexp_function (_, _, Pfunction_cases _) ->
      error ~loc
        "[@vox.def] does not support [function] syntax; write explicit \
         parameters"
    | _ ->
      error ~loc
        "[@vox.def] requires a function binding with explicit parameters"
  in
  let parameter_names =
    List.map
      (fun parameter ->
        match simple_parameter_name parameter with
        | Some name -> name
        | None ->
          error ~loc:parameter.pparam_loc
            "[@vox.def] parameters must be plain variables (no labelled, \
             optional, or pattern parameters)")
      parameters
  in
  if parameter_names = [] then
    error ~loc "[@vox.def] requires a function with at least one parameter";
  let total_mode = Location.mkloc (Mode "total") loc in
  let function_binding =
    { binding with
      pvb_modes = binding.pvb_modes @ [total_mode];
      pvb_attributes = strip_def_attribute binding.pvb_attributes;
    }
  in
  let lemma_binding =
    make_lemma_binding ~loc ~function_name ~parameters ~parameter_names ~body
  in
  if rec_flag = Asttypes.Recursive then
    Vox_vc.Recursive_binding.request_defeq binding.pvb_loc;
  [ { pstr_desc = Pstr_value (rec_flag, [function_binding]);
      pstr_loc = item_loc;
    };
    { pstr_desc = Pstr_value (Nonrecursive, [lemma_binding]);
      pstr_loc = item_loc;
    } ]

let expand_item (item : structure_item) =
  match item.pstr_desc with
  | Pstr_value (rec_flag, bindings)
    when List.exists binding_has_def_attribute bindings ->
    (match rec_flag, bindings with
     | (Nonrecursive | Recursive), [binding] ->
       expand_binding ~item_loc:item.pstr_loc ~rec_flag binding
     | Nonrecursive, _ ->
       error ~loc:item.pstr_loc
         "[@vox.def] must be the sole binding of its [let]"
     | Recursive, _ ->
       error ~loc:item.pstr_loc
         "[@vox.def] must be the sole binding of its recursive [let]")
  | _ -> [item]

let expand_structure structure = List.concat_map expand_item structure
