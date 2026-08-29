open Types
open Typedtree
open Vox_smt
open Vox_encoding

type function_value =
  { label : string;
    instances : Function.t list ref;
    total : bool
  }

type value =
  | Scalar of term
  | Function of function_value

let scalar = function Some (Scalar t) -> Some t | _ -> None

let scalar_value t = Some (Scalar t)

let scalar_option = Option.map (fun t -> Scalar t)

type state =
  { values : value option Path.Map.t;
    facts : labelled_term list;
    omitted_premises : (Location.t * Location.error) list
  }

module Symbolic_keys = Hashtbl.Make (struct
  type t = Path.t * sort

  let equal (path1, sort1) (path2, sort2) =
    Path.same path1 path2 && sort1 = sort2

  let hash (path, sort) = Hashtbl.hash (Path.hash path, sort)
end)

type context =
  { mutable symbols : Symbol.t list;
    mutable functions : Function.t list;
    mutable free : value option Path.Map.t;
    symbolic : value option Symbolic_keys.t;
    prove : Location.t -> query -> unit;
    verify_introductions : bool;
    mutable check_call :
      context -> state -> expression -> value option list -> unit
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

let required loc value =
  match scalar value with Some t -> t | None -> unsupported loc

let logical_function_mode mode =
  Mode.Totality.is_total (Mode.Value.proj_comonadic Mode.Axis.Totality mode)
  && Mode.Statefulness.is_stateless
       (Mode.Value.proj_comonadic Mode.Axis.Statefulness mode)

let at_mode mode = function
  | Some (Function f) when logical_function_mode mode ->
    Some (Function { f with total = true })
  | value -> value

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
      @ [ ( branch s (if is_and then not_ a else a),
            scalar_value (Boolean (not is_and)) ) ])

let guarded_case eval loc s (matched, condition) guard body rest =
  let guards =
    match guard with
    | None -> [branch matched condition, scalar_value (Boolean true)]
    | Some g -> eval (branch matched condition) g
  in
  paths guards (fun s g ->
      let g = required loc g in
      eval (branch s g) body @ rest (branch s (not_ g)))
  @ rest (branch s (not_ condition))

let fresh ctx env ty label =
  match sort env ty with
  | Some sort ->
    let symbol = Symbol.create ~label sort in
    ctx.symbols <- symbol :: ctx.symbols;
    scalar_value (Var symbol)
  | None ->
    begin match get_desc (Ctype.expand_head env ty) with
    | Tarrow _ -> Some (Function { label; instances = ref []; total = false })
    | _ -> None
    end

let symbolic_path ctx env ty path =
  let path = Env.normalize_value_path None env path in
  match sort env ty with
  | None -> fresh ctx env ty (Path.name path)
  | Some sort ->
    let key = path, sort in
    begin match Symbolic_keys.find_opt ctx.symbolic key with
    | Some value -> value
    | None ->
      let value = fresh ctx env ty (Path.name path) in
      Symbolic_keys.add ctx.symbolic key value;
      value
    end

let instantiate_path ctx env ty path value =
  match value, sort env ty with
  | Some (Scalar term), Some expected when term_sort term <> expected ->
    symbolic_path ctx env ty path
  | _ -> value

let lookup ctx s env ty path =
  let path = Env.normalize_value_path None env path in
  match Path.Map.find_opt path s.values with
  | Some value -> instantiate_path ctx env ty path value
  | None -> (
    match Path.Map.find_opt path ctx.free with
    | Some value -> instantiate_path ctx env ty path value
    | None ->
      let value = fresh ctx env ty (Path.name path) in
      ctx.free <- Path.Map.add path value ctx.free;
      value)

let operation env function_type result_type name args =
  scalar_option
    (Vox_encoding.operation env ~function_type ~result_type name
       (List.map scalar args))

let function_call ctx env ty fn args =
  match fn, signature env ty (List.length args) with
  | Some (Function fn), Some (arguments, ((Bool | Int63) as result)) ->
    let args = List.filter_map scalar args in
    if List.map term_sort args <> arguments
    then None
    else
      let f =
        match
          List.find_opt
            (fun f ->
              Function.arguments f = arguments && Function.result f = result)
            !(fn.instances)
        with
        | Some f -> f
        | None ->
          let f = Function.create ~label:fn.label ~arguments ~result in
          fn.instances := f :: !(fn.instances);
          ctx.functions <- f :: ctx.functions;
          f
      in
      scalar_value (Call (f, args))
  | _ -> None

let apply_function ctx env fn_type result_type prim fn args ~total =
  let value =
    match prim with
    | Some (name, arity) when arity = List.length args ->
      operation env fn_type result_type name args
    | _ -> None
  in
  match value with
  | Some _ -> value
  | None when total -> function_call ctx env fn_type fn args
  | None -> None

let constant c = scalar_option (Vox_encoding.constant c)

let rconstant c = scalar_option (Vox_encoding.rconstant c)

let constructor env ty name =
  scalar_option (Vox_encoding.constructor env ty name)

let rconstructor ctx env ty path =
  match scalar_option (Vox_encoding.rconstructor env ty path) with
  | Some _ as value -> value
  | None -> symbolic_path ctx env ty path

let expression_constructor ctx env ty (c : Data_types.constructor_description) =
  match constructor env ty c.cstr_name with
  | Some _ as value -> value
  | None ->
    let path =
      match c.cstr_tag with
      | Extension path -> path
      | Ordinary _ | Null ->
        Path.Pextra_ty
          (Data_types.cstr_res_type_path c, Path.Pcstr_ty c.cstr_name)
    in
    symbolic_path ctx env ty path

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
    | Rexp_constant c ->
      return (scalar_value (required e.rexp_loc (rconstant c)))
    | Rexp_construct (p, []) -> return (rconstructor ctx env e.rexp_type p)
    | Rexp_apply (fn, args) ->
      let prim =
        match fn.rexp_desc with
        | Rexp_ident path -> primitive env path
        | _ -> None
      in
      begin match prim, args with
      | Some ((("%sequand" | "%sequor") as name), 2), [(_, a); (_, b)] ->
        short_circuit eval e.rexp_loc ~is_and:(name = "%sequand") s a b
      | _ ->
        paths
          (arguments_right_to_left (fun s (_, e) -> eval s e) s args)
          (fun s args ->
            paths (eval s fn) (fun s value ->
                let result =
                  apply_function ctx env fn.rexp_type e.rexp_type prim value
                    args ~total:true
                in
                [s, scalar_value (required e.rexp_loc result)]))
      end
    | Rexp_logical_equal (left, right) ->
      paths (eval s right) (fun s right ->
          paths (eval s left) (fun s left ->
              [ ( s,
                  scalar_value
                    (both Eq (required e.rexp_loc left)
                       (required e.rexp_loc right)) ) ]))
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

and predicate_pattern ctx env s value p =
  match p.rpat_desc with
  | Rpat_any -> s, Boolean true
  | Rpat_var id -> bind s id value, Boolean true
  | Rpat_alias (p, id) -> predicate_pattern ctx env (bind s id value) value p
  | Rpat_constant c ->
    s, both Eq (required p.rpat_loc value) (required p.rpat_loc (rconstant c))
  | Rpat_construct (path, []) ->
    ( s,
      both Eq
        (required p.rpat_loc value)
        (required p.rpat_loc (rconstructor ctx env p.rpat_type path)) )
  | _ -> unsupported p.rpat_loc

and predicate_cases ctx env s value cases =
  if impossible s
  then []
  else
    match cases with
    | [] -> []
    | case :: cases ->
      let matched = predicate_pattern ctx env s value case.rc_lhs in
      let rest s = predicate_cases ctx env s value cases in
      guarded_case (predicate ctx env) case.rc_rhs.rexp_loc s matched
        case.rc_guard case.rc_rhs rest

let rec pattern : type k.
    context -> state -> value option -> k general_pattern -> state * term =
 fun ctx s value p ->
  match p.pat_desc with
  | Tpat_any -> s, Boolean true
  | Tpat_var { id; mode; _ } -> bind s id (at_mode mode value), Boolean true
  | Tpat_alias { pattern = p; id; _ } -> pattern ctx (bind s id value) value p
  | Tpat_value p -> pattern ctx s value (p :> Typedtree.pattern)
  | Tpat_constant c ->
    begin match scalar value, scalar (constant c) with
    | Some x, Some c -> s, both Eq x c
    | _ ->
      s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")
    end
  | Tpat_construct (_, c, _, [], _) ->
    begin match
      scalar value, scalar (expression_constructor ctx p.pat_env p.pat_type c)
    with
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

let rec module_structure m =
  match m.mod_desc with
  | Tmod_structure str -> Some str
  | Tmod_constraint (m, _, _, _) -> module_structure m
  | _ -> None

let export_module ctx id str s =
  let fields = Hashtbl.create 8 in
  List.iter
    (function
      | Sig_value (id, _, Exported) | Sig_module (id, _, _, _, Exported) ->
        Hashtbl.replace fields (Ident.name id) id
      | _ -> ())
    str.str_type;
  let exports =
    Hashtbl.fold (fun _ id ids -> Ident.Set.add id ids) fields Ident.Set.empty
  in
  let rec exported = function
    | Path.Pident field when Ident.Set.mem field exports ->
      Some (Path.Pdot (Path.Pident id, Ident.name field))
    | Path.Pdot (prefix, field) ->
      Option.map (fun p -> Path.Pdot (p, field)) (exported prefix)
    | _ -> None
  in
  let values = Path.Map.union (fun _ inner _ -> Some inner) s.values ctx.free in
  let values =
    Path.Map.fold
      (fun path value values ->
        match exported path with
        | None -> values
        | Some path -> Path.Map.add path value values)
      values s.values
  in
  { s with values }

let rec expression ctx s e =
  if impossible s
  then []
  else
    let outcomes = expression_desc ctx s e in
    match if ctx.verify_introductions then intro_loc e else None with
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
                    functions = List.rev ctx.functions;
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
  | Texp_ident { path; desc; mode; _ } ->
    let value =
      match desc.val_kind with
      | Val_mut _ | Val_ivar _ -> opaque ()
      | Val_prim p when p.prim_arity = 0 -> opaque ()
      | _ -> at_mode mode (lookup ctx s e.exp_env e.exp_type path)
    in
    [s, value]
  | Texp_constant c -> [s, constant c]
  | Texp_construct (_, c, _, [], _) ->
    [s, expression_constructor ctx e.exp_env e.exp_type c]
  | Texp_let (rec_flag, bindings, body) ->
    paths
      (value_bindings ctx s rec_flag bindings (has_elim e))
      (fun s _ -> eval s body)
  | Texp_letmodule (Some id, _, _, m, body)
    when Option.is_some (module_structure m) ->
    let str = Option.get (module_structure m) in
    paths (structure ctx s str) (fun s _ ->
        eval (export_module ctx id str s) body)
  | Texp_assume (binding, _, _) ->
    paths (eval s binding.vb_expr) (fun s value ->
        expose_fact ctx e.exp_env s e.exp_type value e.exp_loc)
  | Texp_logical_equal (left, right) ->
    paths (eval s right) (fun s right ->
        paths (eval s left) (fun s left ->
            match scalar left, scalar right with
            | Some left, Some right when term_sort left = term_sort right ->
              [s, scalar_value (both Eq left right)]
            | _ -> [s, opaque ()]))
  | Texp_sequence (a, _, b) -> paths (eval s a) (fun s _ -> eval s b)
  | Texp_ifthenelse (c, t, f) ->
    paths (eval s c) (fun s c ->
        let c =
          match scalar c with
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
          ctx.check_call ctx s e args;
          paths (eval s fn) (fun s fn_value ->
              let total =
                match fn_value with
                | Some (Function { total; _ }) -> total
                | _ -> false
              in
              let value =
                apply_function ctx e.exp_env fn.exp_type e.exp_type prim
                  fn_value args ~total
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
    [captured, opaque ()]
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
    context ->
    state ->
    value option ->
    k case list ->
    (state * value option) list =
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
          | Tstr_module { mb_id = Some id; mb_expr; _ }
            when Option.is_some (module_structure mb_expr) ->
            let str = Option.get (module_structure mb_expr) in
            paths (structure ctx s str) (fun s _ ->
                [export_module ctx id str s, None])
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

let context ~prove ~verify_introductions =
  { symbols = [];
    functions = [];
    free = Path.Map.empty;
    symbolic = Symbolic_keys.create 16;
    prove;
    verify_introductions;
    check_call = (fun _ _ _ _ -> ())
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
    let ctx = context ~prove ~verify_introductions:true in
    ignore (structure ctx empty str)

let check_termination ~prove ~self ~fn ~measure =
  let params, body = Recursive_function.parameters fn in
  let ctx = context ~prove ~verify_introductions:false in
  let reject e =
    Location.raise_errorf ~loc:e.exp_loc
      "Unsupported decreases expression: expected scalar primitive operations"
  in
  let rec check e =
    if Option.is_some (intro_loc e) || sort e.exp_env e.exp_type = None
    then reject e;
    match e.exp_desc with
    | Texp_ident { desc = { val_kind = Val_reg _; _ }; _ }
    | Texp_constant (Const_int _) ->
      ()
    | Texp_construct (_, c, _, [], _)
      when Option.is_some (constructor e.exp_env e.exp_type c.cstr_name) ->
      ()
    | Texp_apply
        (({ exp_desc = Texp_ident { path; _ }; _ } as f), args, _, _, _, _) ->
      let args =
        List.map
          (function
            | Nolabel, Arg (e, _) ->
              check e;
              e
            | _ -> reject e)
          args
      in
      begin match primitive f.exp_env path with
      | Some (name, arity) when arity = List.length args ->
        let values =
          List.map
            (fun arg ->
              match sort arg.exp_env arg.exp_type with
              | Some Bool -> scalar_value (Boolean false)
              | Some Int63 -> scalar_value (Integer 0L)
              | Some (Opaque _) -> reject arg
              | None -> reject arg)
            args
        in
        if operation e.exp_env f.exp_type e.exp_type name values = None
        then reject e
      | _ -> reject e
      end
    | Texp_let (Asttypes.Nonrecursive, bindings, body) ->
      List.iter
        (fun vb ->
          begin match vb.vb_pat.pat_desc with
          | Tpat_var _ | Tpat_any -> ()
          | _ -> reject vb.vb_expr
          end;
          check vb.vb_expr)
        bindings;
      check body
    | Texp_ifthenelse (c, t, Some f) -> List.iter check [c; t; f]
    | Texp_sequence (a, _, b) ->
      check a;
      check b
    | _ -> reject e
  in
  check measure;
  let entry =
    List.fold_left
      (fun s (id, pat) ->
        bind s id (fresh ctx pat.pat_env pat.pat_type (Ident.name id)))
      empty params
  in
  List.iter
    (fun (entry, entry_measure) ->
      let entry_measure = required measure.exp_loc entry_measure in
      let check_call ctx s call args =
        match call.exp_desc with
        | Texp_apply
            ( { exp_desc = Texp_ident { path = Path.Pident id; _ }; _ },
              _,
              _,
              _,
              _,
              _ )
          when Ident.same self id ->
          let call_state =
            List.fold_left2
              (fun s (id, _) value -> bind s id value)
              s params args
          in
          List.iter
            (fun (s, value) ->
              prove call.exp_loc
                { symbols = List.rev ctx.symbols;
                  functions = List.rev ctx.functions;
                  facts = List.rev s.facts;
                  goal =
                    { label = "decreases";
                      term =
                        both Lt (required measure.exp_loc value) entry_measure
                    }
                })
            (expression ctx call_state measure)
        | _ -> ()
      in
      (* Parameter function values share their instance caches across paths. *)
      ctx.check_call <- check_call;
      ignore (expression ctx entry body))
    (expression ctx entry measure)
