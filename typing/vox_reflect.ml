(* vox: partial translation of pure typedtree expressions into
   refinement predicates -- the compiler-attached logical meaning of the
   built-in int/bool operations.

   [translate e] returns the logic term denoting [e]'s value when [e]
   is built from variables, int/bool literals, immutable field reads
   of simple records, and the primitive operations the predicate
   language models (+ - * ~- succ pred on int; && || not; comparisons
   at int or bool); [None] otherwise.
   Recognition is keyed on the PRIMITIVE ([Val_prim]), never the source
   name, so shadowing [(+)] cannot be mistaken for integer addition.
   Comparisons are polymorphic primitives and are translated only at
   int or bool operands: at other types the logic's equality is
   uninterpreted, and [compare] does not denote it.

   The translatable fragment is pure, so naming a subexpression never
   duplicates or reorders effects.

   CAVEAT (DESIGN.md): the logic's ints are unbounded while the
   machine's wrap, so the translation of + - * equates modular with
   ideal arithmetic; overflow is outside the model. *)

open Typedtree

let is_int_or_bool env ty =
  let ty =
    (* Expansion can fail on exotic types; falling back to no expansion
       just makes the translation more conservative. *)
    match Ctype.expand_head env ty with
    | ty -> ty
    | exception _ -> ty
  in
  match Types.get_desc ty with
  | Tconstr (p, [], _) ->
    Path.same p Predef.path_int || Path.same p Predef.path_bool
  | _ -> false
;;

(* TOTAL (reflected) functions ([let rec total_ f ... = ...]): program
   functions whose definitions are translated into the logic
   (translate_def below) and emitted as solver-side definitions
   (vox_verify).  Registered by stamp when the binding is typed
   (Typecore.type_let), so a SATURATED application of a reflected
   function translates like a primitive: [Pfun (name, args)].  Stamps
   are process-unique, so entries from other units in the same process
   can never be confused with the current unit's identifiers. *)
let reflected : (Ident.t, string * int) Hashtbl.t = Hashtbl.create 16

let register_reflected id ~arity =
  Hashtbl.replace reflected id (Ident.name id, arity)
;;

let rec translate (e : expression) : Refinement.pred option =
  match e.exp_desc with
  | Texp_ident { path = Path.Pident id; _ } -> Some (Refinement.Pvar id)
  | Texp_constant (Const_int n) -> Some (Refinement.Pint n)
  | Texp_construct ({ txt = Longident.Lident "true"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool true)
  | Texp_construct ({ txt = Longident.Lident "false"; _ }, _, _, [], _) ->
    Some (Refinement.Pbool false)
  | Texp_field { record; label; _ } ->
    (* A field read of a simple record is the structure projection the
       predicate language writes as [_.px].  [vox_simple_record]
       requires every field immutable, so the read is pure and its
       value is stable -- a mutable field (which disqualifies the whole
       record) must stay a fresh unknown at each read. *)
    let path = Data_types.lbl_res_type_path label in
    (match Ctype.vox_simple_record record.exp_env path with
     | Some _ ->
       Option.map
         (fun base -> Refinement.Pfield (path, label.lbl_name, base))
         (translate record)
     | None -> None)
  | Texp_apply
      ({ exp_desc = Texp_ident { path = Path.Pident id; _ }; _ }, args, _, _, _)
    when Hashtbl.mem reflected id ->
    let name, arity = Hashtbl.find reflected id in
    let args =
      List.map
        (fun (lbl, arg) ->
          match (lbl : Types.arg_label), arg with
          | Nolabel, Arg (a, _) -> translate a
          | _ -> None)
        args
    in
    if List.length args = arity && List.for_all Option.is_some args
    then Some (Refinement.Pfun (name, List.map Option.get args))
    else None
  | Texp_apply
      ( { exp_desc = Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }
        ; _
        }
      , args
      , _
      , _
      , _ ) ->
    let args =
      List.map
        (fun (lbl, arg) ->
          match (lbl : Types.arg_label), arg with
          | Nolabel, Arg (a, _) -> Some a
          | _ -> None)
        args
    in
    let unary k =
      match args with
      | [ Some a ] -> Option.map k (translate a)
      | _ -> None
    in
    let binary k =
      match args with
      | [ Some a; Some b ] ->
        (match translate a, translate b with
         | Some pa, Some pb -> Some (k pa pb)
         | _ -> None)
      | _ -> None
    in
    let intop op = binary (fun a b -> Refinement.Pbinop (op, a, b)) in
    let cmp op =
      (* Both operands have the same type; checking one suffices. *)
      match args with
      | Some a :: _ when is_int_or_bool a.exp_env a.exp_type ->
        binary (fun a b -> Refinement.Pbinop (op, a, b))
      | _ -> None
    in
    (match prim.prim_name with
     | "%addint" -> intop Refinement.Add
     | "%subint" -> intop Refinement.Sub
     | "%mulint" -> intop Refinement.Mul
     | "%negint" ->
       unary (fun a -> Refinement.Pbinop (Sub, Refinement.Pint 0, a))
     | "%succint" ->
       unary (fun a -> Refinement.Pbinop (Add, a, Refinement.Pint 1))
     | "%predint" ->
       unary (fun a -> Refinement.Pbinop (Sub, a, Refinement.Pint 1))
     | "%sequand" -> binary (fun a b -> Refinement.Pand (a, b))
     | "%sequor" -> binary (fun a b -> Refinement.Por (a, b))
     | "%boolnot" -> unary (fun a -> Refinement.Pnot a)
     | "%equal" -> cmp Refinement.Eq
     | "%notequal" -> cmp Refinement.Neq
     | "%lessthan" -> cmp Refinement.Lt
     | "%lessequal" -> cmp Refinement.Le
     | "%greaterthan" -> cmp Refinement.Gt
     | "%greaterequal" -> cmp Refinement.Ge
     | _ -> None)
  | _ -> None
;;

(* ------------------------------------------------------------------ *)
(* Reflected DEFINITIONS: the translation of a [total_] binding's
   body into an equation-style logical definition.

   The reflectable fragment is deliberately small (sharp edges, not
   bugs): a function whose parameters are plain variables of int, bool
   or simple-variant sort, whose body is built from [if] on translatable
   conditions, exhaustive one-level [match] on a variable in scope
   (constructor patterns over variables or wildcards, simple variants
   only), and right-hand sides in the [translate] fragment extended
   with constructor applications and saturated calls to reflected
   functions (self-calls included).  The definition must be CLOSED:
   every variable it mentions is one of its own parameters or match
   fields -- a local function capturing enclosing variables would
   otherwise smuggle activation-local stamps into a global definition.

   Termination is checked by the solver (the Lean backend emits an
   honest [def]); int-indexed recursion carries a [@@vox.decreases e]
   metric from which [termination_by (e).toNat] is synthesized. *)

type rsort =
  | Rint
  | Rbool
  | Rdata of Path.t

type def_body =
  | Bpred of Refinement.pred
  | Bite of Refinement.pred * def_body * def_body
  | Bcase of Ident.t * def_clause list

and def_clause =
  { dc_path : Path.t (* the scrutinee's datatype *)
  ; dc_cstr : string
  ; dc_fields : Ident.t list (* wildcards get fresh idents *)
  ; dc_rhs : def_body
  }

type spec_def =
  { sd_name : string (* solver-side name; the source name in v0 *)
  ; sd_id : Ident.t
  ; sd_params : (Ident.t * rsort) list
  ; sd_ret : rsort
  ; sd_body : def_body
  ; sd_decreases : Refinement.pred option
  ; sd_loc : Location.t
  }

let safe_expand_head env ty =
  match Ctype.expand_head env ty with
  | ty' -> ty'
  | exception _ -> ty
;;

let rsort_of_type env ~loc ~what ty =
  match Types.get_desc (safe_expand_head env ty) with
  | Tconstr (p, [], _) ->
    if Path.same p Predef.path_int
    then Rint
    else if Path.same p Predef.path_bool
    then Rbool
    else (
      match Ctype.vox_simple_variant env p with
      | Some _ -> Rdata p
      | None ->
        Location.raise_errorf ~loc
          "vox: %s of a reflected function must be int, bool, or a simple \
           variant"
          what)
  | _ ->
    Location.raise_errorf ~loc
      "vox: %s of a reflected function must be int, bool, or a simple variant"
      what
;;

(* Right-hand sides: the [translate] fragment, extended with
   applications of simple-variant constructors. *)
let rec translate_rhs (e : expression) : Refinement.pred option =
  match translate e with
  | Some p -> Some p
  | None ->
    (match e.exp_desc with
     | Texp_construct (_, cstr, _, args, _) ->
       let path = Data_types.cstr_res_type_path cstr in
       (match Ctype.vox_simple_variant e.exp_env path with
        | None -> None
        | Some _ ->
          let args = List.map (fun (_, a) -> translate_rhs a) args in
          if List.for_all Option.is_some args
          then
            Some
              (Refinement.Pconstr
                 (path, cstr.cstr_name, List.map Option.get args))
          else None)
     | _ -> None)
;;

let def_unsupported loc =
  Location.raise_errorf ~loc
    "vox: this expression cannot be reflected into the logic (reflected \
     bodies are built from int/bool operations, constructors of simple \
     variants, saturated calls to reflected functions, [if] on \
     translatable conditions, and exhaustive one-level [match] on a \
     variable)"
;;

let rec translate_body (e : expression) : def_body =
  match e.exp_desc with
  | Texp_ifthenelse (c, e_then, Some e_else) ->
    (match translate c with
     | Some p -> Bite (p, translate_body e_then, translate_body e_else)
     | None -> def_unsupported c.exp_loc)
  | Texp_match (scrut, _sort, comp_cases, val_cases, partial) ->
    let scrut_id =
      match scrut.exp_desc with
      | Texp_ident { path = Path.Pident id; _ } -> id
      | _ ->
        Location.raise_errorf ~loc:scrut.exp_loc
          "vox: a reflected [match] must scrutinize a variable"
    in
    (* Ordinary clauses arrive as COMPUTATION cases (value patterns
       wrapped in [Tpat_value]); [val_cases] holds effect handlers. *)
    if val_cases <> [] || partial <> Total
    then
      Location.raise_errorf ~loc:e.exp_loc
        "vox: a reflected [match] must be exhaustive, without effect \
         handlers";
    Bcase (scrut_id, List.map translate_clause comp_cases)
  | _ ->
    (match translate_rhs e with
     | Some p -> Bpred p
     | None -> def_unsupported e.exp_loc)

and translate_clause : type k. k case -> def_clause =
  fun c ->
  (match c.c_guard with
   | None -> ()
   | Some g ->
     Location.raise_errorf ~loc:g.exp_loc
       "vox: a reflected [match] cannot have when-guards");
  let rec constructor_clause : type k2. k2 general_pattern -> def_clause =
    fun pat ->
    match pat.pat_desc with
    | Tpat_value p -> constructor_clause (p :> value general_pattern)
    | Tpat_construct (_, cstr, _, args, _) ->
      let path = Data_types.cstr_res_type_path cstr in
      (match Ctype.vox_simple_variant pat.pat_env path with
       | Some _ -> ()
       | None ->
         Location.raise_errorf ~loc:pat.pat_loc
           "vox: a reflected [match] is limited to simple variants");
      let field (_, (p : value general_pattern)) =
        match p.pat_desc with
        | Tpat_var { id; _ } -> id
        | Tpat_any -> Ident.create_local "*vox-reflect-wild*"
        | _ ->
          Location.raise_errorf ~loc:p.pat_loc
            "vox: a reflected [match] is limited to one-level constructor \
             patterns over variables or wildcards"
      in
      { dc_path = path
      ; dc_cstr = cstr.cstr_name
      ; dc_fields = List.map field args
      ; dc_rhs = translate_body c.c_rhs
      }
    | _ ->
      Location.raise_errorf ~loc:pat.pat_loc
        "vox: a reflected [match] is limited to one-level constructor \
         patterns over variables or wildcards"
  in
  constructor_clause c.c_lhs
;;

(* The [@@vox.decreases e] metric: an int-valued expression over the
   parameters, in a tiny surface fragment (parameters, int literals,
   [+ - *]). *)
let rec translate_metric params (e : Parsetree.expression) : Refinement.pred =
  let unsupported () =
    Location.raise_errorf ~loc:e.pexp_loc
      "vox: a [@@vox.decreases] metric may mention only the function's \
       parameters, int literals, and + - *"
  in
  match e.pexp_desc with
  | Pexp_ident { txt = Longident.Lident name; _ } ->
    (match
       List.find_opt (fun (id, _) -> String.equal (Ident.name id) name) params
     with
     | Some (id, _) -> Refinement.Pvar id
     | None -> unsupported ())
  | Pexp_constant { pconst_desc = Pconst_integer (s, None); _ } ->
    (match int_of_string_opt s with
     | Some n -> Refinement.Pint n
     | None -> unsupported ())
  | Pexp_apply
      ( { pexp_desc =
            Pexp_ident { txt = Longident.Lident (("+" | "-" | "*") as op); _ }
        ; _
        }
      , [ (Nolabel, a); (Nolabel, b) ] ) ->
    let binop =
      match op with
      | "+" -> Refinement.Add
      | "-" -> Refinement.Sub
      | _ -> Refinement.Mul
    in
    Refinement.Pbinop (binop, translate_metric params a, translate_metric params b)
  | _ -> unsupported ()
;;

let find_attr name (attrs : Parsetree.attributes) =
  List.find_opt
    (fun (a : Parsetree.attribute) -> String.equal a.attr_name.txt name)
    attrs
;;

let has_total_attr attrs = find_attr "vox.total" attrs <> None

(* The [total_] marker rides the binder pattern (parser); the
   [@@vox.total] attribute spelling on the binding also works. *)
let is_total_binding (vb : Typedtree.value_binding) =
  has_total_attr vb.vb_attributes || has_total_attr vb.vb_pat.pat_attributes
;;

let rec body_preds acc = function
  | Bpred p -> p :: acc
  | Bite (c, a, b) -> body_preds (body_preds (c :: acc) a) b
  | Bcase (_, clauses) ->
    List.fold_left (fun acc cl -> body_preds acc cl.dc_rhs) acc clauses
;;

let rec body_bound acc = function
  | Bpred _ -> acc
  | Bite (_, a, b) -> body_bound (body_bound acc a) b
  | Bcase (_, clauses) ->
    List.fold_left
      (fun acc cl -> body_bound (cl.dc_fields @ acc) cl.dc_rhs)
      acc
      clauses
;;

let rec body_scrutinees acc = function
  | Bpred _ -> acc
  | Bite (_, a, b) -> body_scrutinees (body_scrutinees acc a) b
  | Bcase (x, clauses) ->
    List.fold_left
      (fun acc cl -> body_scrutinees acc cl.dc_rhs)
      (x :: acc)
      clauses
;;

(* Datatype paths a definition depends on (for solver-side registration):
   scrutinized datatypes plus constructor applications in the preds. *)
let def_datatype_paths (d : spec_def) =
  let clause_paths =
    let rec go acc = function
      | Bpred _ -> acc
      | Bite (_, a, b) -> go (go acc a) b
      | Bcase (_, clauses) ->
        List.fold_left (fun acc cl -> go (cl.dc_path :: acc) cl.dc_rhs) acc clauses
    in
    go [] d.sd_body
  in
  let sort_paths =
    List.filter_map
      (fun s ->
        match s with
        | Rdata p -> Some p
        | Rint | Rbool -> None)
      (d.sd_ret :: List.map snd d.sd_params)
  in
  clause_paths
  @ sort_paths
  @ List.concat_map Refinement.constr_paths (body_preds [] d.sd_body)
;;

(* Translate a typed [total_] binding into a definition.  Raises
   with a source location on anything outside the fragment. *)
let translate_def (vb : Typedtree.value_binding) : spec_def =
  let loc = vb.vb_loc in
  let id =
    match vb.vb_pat.pat_desc with
    | Tpat_var { id; _ } -> id
    | _ ->
      Location.raise_errorf ~loc
        "vox: total_ requires a binding of a single variable"
  in
  match vb.vb_expr.exp_desc with
  | Texp_function { params; body; _ } ->
    let env = vb.vb_expr.exp_env in
    let param (fp : function_param) =
      match fp.fp_arg_label, fp.fp_kind with
      | Nolabel, Tparam_pat ({ pat_desc = Tpat_var { id; _ }; _ } as pat) ->
        ( id
        , rsort_of_type env ~loc:pat.pat_loc ~what:"each parameter"
            pat.pat_type )
      | _ ->
        Location.raise_errorf ~loc:fp.fp_loc
          "vox: a reflected function's parameters must be plain variables"
    in
    let params = List.map param params in
    let params, def_body, ret_ty =
      match body with
      | Tfunction_body e -> params, translate_body e, e.exp_type
      | Tfunction_cases fc ->
        (match fc.fc_cases with
         | [] -> def_unsupported loc
         | c0 :: _ ->
           if fc.fc_partial <> Total
           then
             Location.raise_errorf ~loc
               "vox: a reflected [function] must be exhaustive";
           let scrut_sort =
             rsort_of_type env ~loc ~what:"each parameter" c0.c_lhs.pat_type
           in
           ( params @ [ fc.fc_param, scrut_sort ]
           , Bcase (fc.fc_param, List.map translate_clause fc.fc_cases)
           , c0.c_rhs.exp_type ))
    in
    if params = [] then def_unsupported loc;
    let ret = rsort_of_type env ~loc ~what:"the result" ret_ty in
    let decreases =
      match find_attr "vox.decreases" vb.vb_attributes with
      | None -> None
      | Some { attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]; _ }
        -> Some (translate_metric params e)
      | Some a ->
        Location.raise_errorf ~loc:a.attr_loc
          "vox: [@@vox.decreases] expects an expression payload"
    in
    (* Closedness: every variable in the definition -- in a
       right-hand side, an [if] condition, the metric, or as a [match]
       scrutinee -- is a parameter or a match field.  (A local function
       could otherwise capture an enclosing activation's variable in a
       global definition.) *)
    let bound = List.map fst params @ body_bound [] def_body in
    let check_closed v =
      if not (List.exists (Ident.same v) bound)
      then
        Location.raise_errorf ~loc
          "vox: a reflected definition must be closed, but this one mentions \
           %s; reflected functions may only be defined at the module level, \
           over their own parameters"
          (Ident.name v)
    in
    List.iter
      (fun p -> List.iter check_closed (Refinement.free_vars p))
      (Option.to_list decreases @ body_preds [] def_body);
    List.iter check_closed (body_scrutinees [] def_body);
    { sd_name = Ident.name id
    ; sd_id = id
    ; sd_params = params
    ; sd_ret = ret
    ; sd_body = def_body
    ; sd_decreases = decreases
    ; sd_loc = loc
    }
  | _ ->
    Location.raise_errorf ~loc
      "vox: total_ requires a function binding"
;;
