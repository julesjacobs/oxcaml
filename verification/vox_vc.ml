open Types
open Typedtree
open Vox_smt

type state =
  { values : term option Path.Map.t;
    facts : labelled_term list;
    omitted_premises : (Location.t * Location.error) list
  }

type context =
  { mutable symbols : Symbol.t list;
    mutable free : term option Path.Map.t;
    prove : Location.t -> query -> unit
  }

let empty = { values = Path.Map.empty; facts = []; omitted_premises = [] }

let bind s id value =
  { s with values = Path.Map.add (Path.Pident id) value s.values }

let fact s label term =
  match term with
  | Boolean true -> s
  | _ -> { s with facts = { label; term } :: s.facts }

let not_ = function Boolean b -> Boolean (not b) | t -> App (Not, [t])

let branch s t = fact s "branch" t

let both op a b = App (op, [a; b])

let unsupported loc =
  Location.raise_errorf ~loc "Unsupported refinement predicate in VC generation"

let required loc = function Some t -> t | None -> unsupported loc

let impossible s =
  List.exists
    (fun f -> match f.term with Boolean false -> true | _ -> false)
    s.facts

let paths outcomes f =
  List.concat_map (fun (s, v) -> if impossible s then [] else f s v) outcomes

let rec arguments_right_to_left eval s = function
  | [] -> [s, []]
  | arg :: args ->
    paths (arguments_right_to_left eval s args) (fun s values ->
        paths (eval s arg) (fun s value -> [s, value :: values]))

let short_circuit eval loc ~is_and s a b =
  paths (eval s a) (fun s a ->
      let a = required loc a in
      eval (branch s (if is_and then a else not_ a)) b
      @ [branch s (if is_and then not_ a else a), Some (Boolean (not is_and))])

let guarded_case eval loc s (matched, condition) guard body rest =
  let guards =
    match guard with
    | None -> [branch matched condition, Some (Boolean true)]
    | Some g -> eval (branch matched condition) g
  in
  paths guards (fun s g ->
      let g = required loc g in
      eval (branch s g) body @ rest (branch s (not_ g)))
  @ rest (branch s (not_ condition))

let rec sort env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r -> sort env r.ref_payload
  | Tconstr (p, [], _) when Path.same p Predef.path_int -> Some Int63
  | Tconstr (p, [], _) when Path.same p Predef.path_bool -> Some Bool
  | _ -> None

let fresh ctx env ty label =
  Option.map
    (fun sort ->
      let symbol = Symbol.create ~label sort in
      ctx.symbols <- symbol :: ctx.symbols;
      Var symbol)
    (sort env ty)

let lookup ctx s env ty path =
  match Path.Map.find_opt path s.values with
  | Some value -> value
  | None -> (
    match Path.Map.find_opt path ctx.free with
    | Some value -> value
    | None ->
      let value = fresh ctx env ty (Path.name path) in
      ctx.free <- Path.Map.add path value ctx.free;
      value)

let primitive env path =
  match (Env.find_value path env).val_kind with
  | Val_prim p -> Some (p.Primitive.prim_name, p.prim_arity)
  | _ -> None
  | exception Not_found -> None

let operation env ty name args =
  let unary sort op =
    match args with
    | [Some x] when term_sort x = sort -> Some (App (op, [x]))
    | _ -> None
  in
  let binary sort op =
    match args with
    | [Some x; Some y] when term_sort x = sort && term_sort y = sort ->
      Some (App (op, [x; y]))
    | _ -> None
  in
  let equality op =
    match args with
    | [Some x; Some y] when term_sort x = term_sort y -> Some (App (op, [x; y]))
    | _ -> None
  in
  let result =
    match name with
    | "%addint" -> binary Int63 Add
    | "%subint" -> binary Int63 Sub
    | "%mulint" -> binary Int63 Mul
    | "%divint" | "%modint" ->
      begin match args with
      | [_; Some (Integer divisor)] when divisor <> 0L ->
        binary Int63 (if name = "%divint" then Div else Rem)
      | _ -> None
      end
    | "%negint" -> unary Int63 Neg
    | "%equal" | "%eq" -> equality Eq
    | "%notequal" | "%noteq" -> equality Ne
    | "%lessthan" | "%ltint" -> binary Int63 Lt
    | "%lessequal" | "%leint" -> binary Int63 Le
    | "%greaterthan" | "%gtint" -> binary Int63 Gt
    | "%greaterequal" | "%geint" -> binary Int63 Ge
    | "%boolnot" -> unary Bool Not
    | "%sequand" -> binary Bool And
    | "%sequor" -> binary Bool Or
    | "%identity" -> ( match args with [v] -> v | _ -> None)
    | _ -> None
  in
  match result with
  | Some value when Some (term_sort value) = sort env ty -> result
  | _ -> None

let constant = function
  | Typedtree.Const_int n -> Some (Integer (Int64.of_int n))
  | _ -> None

let rconstant c =
  match c.Parsetree.pconst_desc with
  | Parsetree.Pconst_integer (n, None) -> Some (Integer (Int64.of_string n))
  | _ -> None

let constructor env ty name =
  match sort env ty, name with
  | Some Bool, "true" -> Some (Boolean true)
  | Some Bool, "false" -> Some (Boolean false)
  | _ -> None

let rconstructor env ty = function
  | Path.Pextra_ty (_, Path.Pcstr_ty name) -> constructor env ty name
  | _ -> None

let refinement env ty loc =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r -> r
  | _ ->
    Misc.fatal_errorf "VC: refinement expected at %a" Location.print_loc loc

let rec predicate ctx env s e =
  if impossible s
  then []
  else
    let eval = predicate ctx env in
    let return v = [s, v] in
    match e.rexp_desc with
    | Rexp_var id -> return (lookup ctx s env e.rexp_type (Path.Pident id))
    | Rexp_ident path ->
      begin match primitive env path with
      | Some (_, 0) -> unsupported e.rexp_loc
      | _ -> return (lookup ctx s env e.rexp_type path)
      end
    | Rexp_constant c -> return (Some (required e.rexp_loc (rconstant c)))
    | Rexp_construct (p, []) -> return (rconstructor env e.rexp_type p)
    | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, args) ->
      begin match primitive env path, args with
      | Some ((("%sequand" | "%sequor") as name), 2), [(_, a); (_, b)] ->
        short_circuit eval e.rexp_loc ~is_and:(name = "%sequand") s a b
      | Some (name, arity), _ when arity = List.length args ->
        paths
          (arguments_right_to_left (fun s (_, e) -> eval s e) s args)
          (fun s args ->
            [s, Some (required e.rexp_loc (operation env e.rexp_type name args))])
      | _ -> unsupported e.rexp_loc
      end
    | Rexp_ifthenelse (c, t, Some f) ->
      paths (eval s c) (fun s c ->
          let c = required e.rexp_loc c in
          eval (branch s c) t @ eval (branch s (not_ c)) f)
    | Rexp_sequence (a, b) -> paths (eval s a) (fun s _ -> eval s b)
    | Rexp_let (binding, body) ->
      paths (eval s binding.rb_expr) (fun s value ->
          let states =
            match binding.rb_kind with
            | Rbind_value -> [s, value]
            | Rbind_refine ->
              expose ctx env s binding.rb_expr.rexp_type value
                binding.rb_expr.rexp_loc
          in
          paths states (fun s value ->
              eval (bind s binding.rb_ident value) body))
    | Rexp_match (scrutinee, cases) ->
      paths (eval s scrutinee) (fun s value ->
          predicate_cases ctx env s value cases)
    | _ -> unsupported e.rexp_loc

and expose ctx env s ty value loc =
  let r = refinement env ty loc in
  paths
    (predicate ctx env (bind s r.ref_binder value) r.ref_pred)
    (fun s predicate -> [fact s "refinement" (required loc predicate), value])

and predicate_pattern env s value p =
  match p.rpat_desc with
  | Rpat_any -> s, Boolean true
  | Rpat_var id -> bind s id value, Boolean true
  | Rpat_alias (p, id) -> predicate_pattern env (bind s id value) value p
  | Rpat_constant c ->
    s, both Eq (required p.rpat_loc value) (required p.rpat_loc (rconstant c))
  | Rpat_construct (path, []) ->
    ( s,
      both Eq
        (required p.rpat_loc value)
        (required p.rpat_loc (rconstructor env p.rpat_type path)) )
  | _ -> unsupported p.rpat_loc

and predicate_cases ctx env s value cases =
  if impossible s
  then []
  else
    match cases with
    | [] -> []
    | case :: cases ->
      let matched = predicate_pattern env s value case.rc_lhs in
      let rest s = predicate_cases ctx env s value cases in
      guarded_case (predicate ctx env) case.rc_rhs.rexp_loc s matched
        case.rc_guard case.rc_rhs rest

let rec pattern : type k.
    context -> state -> term option -> k general_pattern -> state * term =
 fun ctx s value p ->
  match p.pat_desc with
  | Tpat_any -> s, Boolean true
  | Tpat_var { id; _ } -> bind s id value, Boolean true
  | Tpat_alias { pattern = p; id; _ } -> pattern ctx (bind s id value) value p
  | Tpat_value p -> pattern ctx s value (p :> Typedtree.pattern)
  | Tpat_constant c ->
    begin match value, constant c with
    | Some x, Some c -> s, both Eq x c
    | _ ->
      s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")
    end
  | Tpat_construct (_, c, _, [], _) ->
    begin match value, constructor p.pat_env p.pat_type c.cstr_name with
    | Some x, Some c -> s, both Eq x c
    | _ ->
      s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")
    end
  | _ ->
    let s =
      List.fold_left
        (fun s (id, _, ty, _, _) ->
          bind s id (fresh ctx p.pat_env ty (Ident.name id)))
        s (pat_bound_idents_full p)
    in
    s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")

let intro_loc e =
  List.find_map
    (function Texp_refine, loc, _ -> Some loc | _ -> None)
    e.exp_extra

let expose_fact ctx env s ty value loc =
  (* Dropping an unsupported premise is conservative; goals remain strict. *)
  try expose ctx env s ty value loc
  with Location.Error error ->
    [{ s with omitted_premises = (loc, error) :: s.omitted_premises }, value]

let omitted_premise_messages s =
  List.concat_map
    (fun (loc, (error : Location.error)) ->
      Location.msg ~loc
        "This refinement premise was omitted because it could not be \
         translated to SMT"
      :: error.main :: error.sub)
    (List.rev s.omitted_premises)

let has_elim e =
  List.exists
    (function Texp_let_refine _, _, _ -> true | _ -> false)
    e.exp_extra

let rec expression ctx s e =
  if impossible s
  then []
  else
    let outcomes = expression_desc ctx s e in
    match intro_loc e with
    | Some loc ->
      paths outcomes (fun s value ->
          let r = refinement e.exp_env e.exp_type e.exp_loc in
          let goals =
            try predicate ctx e.exp_env (bind s r.ref_binder value) r.ref_pred
            with Location.Error error ->
              raise
                (Location.Error
                   { error with
                     sub =
                       error.sub
                       @ [ Location.msg ~loc
                             "Required by this refinement introduction" ]
                   })
          in
          List.iter
            (fun (s, goal) ->
              try
                ctx.prove loc
                  { symbols = List.rev ctx.symbols;
                    facts = List.rev s.facts;
                    goal =
                      { label = "refine_";
                        term = required r.ref_pred.rexp_loc goal
                      }
                  }
              with Location.Error error ->
                raise
                  (Location.Error
                     { error with sub = error.sub @ omitted_premise_messages s }))
            goals;
          [s, value])
    | None -> outcomes

and expression_desc ctx s e =
  let eval = expression ctx in
  let opaque () = fresh ctx e.exp_env e.exp_type "result" in
  match e.exp_desc with
  | Texp_ident { path; desc; _ } ->
    let value =
      match desc.val_kind with
      | Val_mut _ | Val_ivar _ | Val_prim _ -> opaque ()
      | _ -> lookup ctx s e.exp_env e.exp_type path
    in
    [s, value]
  | Texp_constant c -> [s, constant c]
  | Texp_construct (_, c, _, [], _) ->
    [s, constructor e.exp_env e.exp_type c.cstr_name]
  | Texp_let (rec_flag, bindings, body) ->
    paths
      (value_bindings ctx s rec_flag bindings (has_elim e))
      (fun s _ -> eval s body)
  | Texp_assume (binding, _, _) ->
    paths (eval s binding.vb_expr) (fun s value ->
        expose_fact ctx e.exp_env s e.exp_type value e.exp_loc)
  | Texp_sequence (a, _, b) -> paths (eval s a) (fun s _ -> eval s b)
  | Texp_ifthenelse (c, t, f) ->
    paths (eval s c) (fun s c ->
        let c =
          match c with
          | Some c -> c
          | None ->
            required e.exp_loc
              (fresh ctx e.exp_env Predef.type_bool "condition")
        in
        eval (branch s c) t
        @
        match f with
        | None -> [branch s (not_ c), None]
        | Some f -> eval (branch s (not_ c)) f)
  | Texp_apply (fn, args, _, _, _, _) ->
    let prim =
      match fn.exp_desc with
      | Texp_ident { path; _ } -> primitive fn.exp_env path
      | _ -> None
    in
    begin match prim, args with
    | ( Some ((("%sequand" | "%sequor") as name), 2),
        [(_, Arg (a, _)); (_, Arg (b, _))] ) ->
      short_circuit eval e.exp_loc ~is_and:(name = "%sequand") s a b
    | _ ->
      let argument s (_, arg) =
        match arg with Omitted _ -> [s, None] | Arg (e, _) -> eval s e
      in
      paths (arguments_right_to_left argument s args) (fun s args ->
          paths (eval s fn) (fun s _ ->
              let value =
                match prim with
                | Some (name, arity) when arity = List.length args ->
                  operation e.exp_env e.exp_type name args
                | _ -> None
              in
              match prim with
              | Some (("%raise" | "%reraise" | "%raise_notrace"), 1) -> []
              | _ ->
                [(s, match value with Some _ -> value | None -> opaque ())]))
    end
  | Texp_function { params; body; _ } ->
    let captured = s in
    let s =
      List.fold_left
        (fun s p ->
          let pat =
            match p.fp_kind with
            | Tparam_pat pat -> pat
            | Tparam_optional_default (pat, default, _) ->
              ignore (eval s default);
              pat
          in
          let value =
            fresh ctx pat.pat_env pat.pat_type (Ident.name p.fp_param)
          in
          let s, condition = pattern ctx (bind s p.fp_param value) value pat in
          branch s condition)
        s params
    in
    begin match body with
    | Tfunction_body body -> ignore (eval s body)
    | Tfunction_cases cases ->
      begin match cases.fc_cases with
      | [] -> ()
      | c :: _ ->
        let value = fresh ctx c.c_lhs.pat_env c.c_lhs.pat_type "argument" in
        ignore
          (value_cases ctx (bind s cases.fc_param value) value cases.fc_cases)
      end
    end;
    [captured, None]
  | Texp_match (scrutinee, _, cases, [], _)
    when List.for_all (fun c -> snd (split_pattern c.c_lhs) = None) cases ->
    paths (eval s scrutinee) (fun s value ->
        computation_cases ctx s value cases)
  | _ ->
    (* Unknown evaluation/control-flow forms lose outgoing facts, but cannot
       hide obligations in their children or delayed bodies. *)
    let iterator = iterator ctx s in
    Tast_iterator.default_iterator.expr iterator e;
    [s, opaque ()]

and value_bindings ctx s rec_flag bindings eliminate =
  let s =
    match rec_flag with
    | Asttypes.Nonrecursive -> s
    | Asttypes.Recursive ->
      List.iter
        (fun vb ->
          match vb.vb_expr.exp_desc with
          | Texp_function _ -> ()
          | _ ->
            Location.raise_errorf ~loc:vb.vb_expr.exp_loc
              "Refinement verification does not support recursive value \
               initialization")
        bindings;
      List.fold_left
        (fun s vb ->
          fst
            (pattern ctx s
               (fresh ctx vb.vb_pat.pat_env vb.vb_pat.pat_type "recursive")
               vb.vb_pat))
        s bindings
  in
  let rec loop s = function
    | [] -> [s, None]
    | vb :: rest ->
      paths (expression ctx s vb.vb_expr) (fun s value ->
          let states =
            if eliminate
            then
              expose_fact ctx vb.vb_expr.exp_env s vb.vb_expr.exp_type value
                vb.vb_expr.exp_loc
            else [s, value]
          in
          paths states (fun s value ->
              let s, condition = pattern ctx s value vb.vb_pat in
              loop (branch s condition) rest))
  in
  loop s bindings

and value_cases ctx s value cases = cases_with_pattern ctx s value cases

and computation_cases ctx s value cases = cases_with_pattern ctx s value cases

and cases_with_pattern : type k.
    context -> state -> term option -> k case list -> (state * term option) list
    =
 fun ctx s value cases ->
  if impossible s
  then []
  else
    match cases with
    | [] -> []
    | c :: cases ->
      let matched = pattern ctx s value c.c_lhs in
      let rest s = cases_with_pattern ctx s value cases in
      guarded_case (expression ctx) c.c_rhs.exp_loc s matched c.c_guard c.c_rhs
        rest

and structure ctx s str =
  List.fold_left
    (fun states item ->
      paths states (fun s _ ->
          match item.str_desc with
          | Tstr_value (rec_flag, bindings) ->
            value_bindings ctx s rec_flag bindings false
          | Tstr_eval (e, _, _) -> expression ctx s e
          | _ ->
            let iterator = iterator ctx s in
            Tast_iterator.default_iterator.structure_item iterator item;
            [s, None]))
    [s, None]
    str.str_items

and iterator ctx s =
  { Tast_iterator.default_iterator with
    expr = (fun _ e -> ignore (expression ctx s e));
    value_bindings =
      (fun _ (rec_flag, bindings) ->
        ignore (value_bindings ctx s rec_flag bindings false));
    structure = (fun _ str -> ignore (structure ctx s str))
  }

let generate ~prove str =
  let exception Has_obligation in
  let scan =
    { Tast_iterator.default_iterator with
      expr =
        (fun self e ->
          if Option.is_some (intro_loc e) then raise Has_obligation;
          Tast_iterator.default_iterator.expr self e)
    }
  in
  match scan.structure scan str with
  | () -> ()
  | exception Has_obligation ->
    let ctx = { symbols = []; free = Path.Map.empty; prove } in
    ignore (structure ctx empty str)
