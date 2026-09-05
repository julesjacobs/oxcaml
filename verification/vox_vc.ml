open Types
open Typedtree
open Vox_smt

type obligation =
  { loc : Location.t;
    goal : term;
    omitted_premises : (Location.t * Location.error) list
  }

(* Command lists are stored in reverse execution order. Define binds a fresh
   symbol to a total SMT expression; it does not restrict reachability. Check
   verifies a nested computation without exporting its assumptions. *)
type command =
  | Assume of term
  | Define of term
  | Assert of obligation
  | Choice of command list * command list
  | Check of command list

type state =
  { values : term option Path.Map.t;
    code : command list;
    dead : bool;
    omitted_premises : (Location.t * Location.error) list
  }

type context =
  { mutable free : term option Path.Map.t;
    mutable batches : command list list
  }

let empty =
  { values = Path.Map.empty; code = []; dead = false; omitted_premises = [] }

let bind s id value =
  { s with values = Path.Map.add (Path.Pident id) value s.values }

let not_ = function Boolean b -> Boolean (not b) | t -> App (Not, [t])

let both op a b =
  match op, a, b with
  | And, Boolean true, x | And, x, Boolean true -> x
  | And, Boolean false, _ | And, _, Boolean false -> Boolean false
  | Implies, Boolean false, _ | Implies, _, Boolean true -> Boolean true
  | Implies, Boolean true, x -> x
  | Implies, x, Boolean false -> not_ x
  | _ -> App (op, [a; b])

let branch s term =
  match term with
  | Boolean true -> s
  | _ ->
    { s with
      code = Assume term :: s.code;
      dead = s.dead || term = Boolean false
    }

let impossible s = s.dead

let unsupported loc =
  Location.raise_errorf ~loc "Unsupported refinement predicate in VC generation"

let required loc = function Some t -> t | None -> unsupported loc

let fresh_symbol sort label = Var (Symbol.create ~label sort)

let name s = function
  | Some (App (op, args) as term) ->
    let s =
      match op, args with
      | (Div | Rem), [_; divisor] -> branch s (App (Ne, [divisor; Integer 0L]))
      | _ -> s
    in
    let value = fresh_symbol (term_sort term) "value" in
    { s with code = Define (both Eq value term) :: s.code }, Some value
  | value -> s, value

let rec added_prefix ~base = function
  | current when current == base -> []
  | item :: rest -> item :: added_prefix ~base rest
  | [] -> Misc.fatal_error "VC: state does not extend its input"

let choose s condition ifso ifnot =
  if s.dead
  then s, None
  else
    match condition with
    | Boolean true -> ifso s
    | Boolean false -> ifnot s
    | _ ->
      let left, a = ifso (branch s condition) in
      let right, b = ifnot (branch s (not_ condition)) in
      let value =
        if left.dead
        then b
        else if right.dead
        then a
        else if a = b
        then a
        else
          match a, b with
          | Some a, Some b -> Some (App (Ite, [condition; a; b]))
          | _ -> None
      in
      let s =
        { s with
          code =
            Choice
              ( added_prefix ~base:s.code left.code,
                added_prefix ~base:s.code right.code )
            :: s.code;
          dead = left.dead && right.dead;
          omitted_premises =
            added_prefix ~base:s.omitted_premises left.omitted_premises
            @ added_prefix ~base:s.omitted_premises right.omitted_premises
            @ s.omitted_premises
        }
      in
      name s value

let rec arguments_right_to_left eval s = function
  | [] -> s, []
  | arg :: args ->
    let s, values = arguments_right_to_left eval s args in
    let s, value = eval s arg in
    s, value :: values

let short_circuit eval loc ~is_and s a b =
  let s, a = eval s a in
  if s.dead
  then s, None
  else
    let condition = required loc a in
    if is_and
    then
      choose s condition (fun s -> eval s b) (fun s -> s, Some (Boolean false))
    else
      choose s condition (fun s -> s, Some (Boolean true)) (fun s -> eval s b)

let guarded_case eval loc s (matched, condition) guard body rest =
  let values = s.values in
  let s, accepted =
    match guard with
    | None -> matched, condition
    | Some g ->
      let state, value =
        choose matched condition
          (fun s -> eval s g)
          (fun s -> s, Some (Boolean false))
      in
      state, if state.dead then Boolean false else required loc value
  in
  choose s accepted
    (fun s -> eval s body)
    (fun state -> rest { state with values })

let rec erase_assertions code =
  List.filter_map
    (function
      | Assert _ | Check _ -> None
      | (Assume _ | Define _) as c -> Some c
      | Choice (a, b) -> Some (Choice (erase_assertions a, erase_assertions b)))
    code

let rec sort env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r -> sort env r.ref_payload
  | Tconstr (p, [], _) when Path.same p Predef.path_int -> Some Int63
  | Tconstr (p, [], _) when Path.same p Predef.path_bool -> Some Bool
  | _ -> None

let fresh _ctx env ty label =
  Option.map (fun sort -> fresh_symbol sort label) (sort env ty)

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
      | [_; Some (Integer 0L)] -> None
      | _ -> binary Int63 (if name = "%divint" then Div else Rem)
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
  then s, None
  else
    let eval = predicate ctx env in
    match e.rexp_desc with
    | Rexp_var id -> s, lookup ctx s env e.rexp_type (Path.Pident id)
    | Rexp_ident path ->
      begin match primitive env path with
      | Some (_, 0) -> unsupported e.rexp_loc
      | _ -> s, lookup ctx s env e.rexp_type path
      end
    | Rexp_constant c -> s, Some (required e.rexp_loc (rconstant c))
    | Rexp_construct (p, []) -> s, rconstructor env e.rexp_type p
    | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, args) ->
      begin match primitive env path, args with
      | Some ((("%sequand" | "%sequor") as op), 2), [(_, a); (_, b)] ->
        short_circuit eval e.rexp_loc ~is_and:(op = "%sequand") s a b
      | Some (op, arity), _ when arity = List.length args ->
        let s, args =
          arguments_right_to_left (fun s (_, e) -> eval s e) s args
        in
        if s.dead
        then s, None
        else
          name s
            (Some (required e.rexp_loc (operation env e.rexp_type op args)))
      | _ -> unsupported e.rexp_loc
      end
    | Rexp_logical_equal (left, right) ->
      let s, right = eval s right in
      let s, left = eval s left in
      if s.dead
      then s, None
      else
        name s
          (Some (both Eq (required e.rexp_loc left) (required e.rexp_loc right)))
    | Rexp_ifthenelse (c, t, Some f) ->
      let s, c = eval s c in
      choose s
        (if s.dead then Boolean false else required e.rexp_loc c)
        (fun s -> eval s t)
        (fun s -> eval s f)
    | Rexp_sequence (a, b) ->
      let s, _ = eval s a in
      eval s b
    | Rexp_let (binding, body) ->
      let s, value = eval s binding.rb_expr in
      let s, value =
        match binding.rb_kind with
        | Rbind_value -> s, value
        | Rbind_refine ->
          expose ctx env s binding.rb_expr.rexp_type value
            binding.rb_expr.rexp_loc
      in
      eval (bind s binding.rb_ident value) body
    | Rexp_match (scrutinee, cases) ->
      let s, value = eval s scrutinee in
      predicate_cases ctx env s value cases
    | _ -> unsupported e.rexp_loc

and expose ctx env s ty value loc =
  if impossible s
  then s, value
  else
    let r = refinement env ty loc in
    let s, predicate =
      predicate ctx env (bind s r.ref_binder value) r.ref_pred
    in
    (if s.dead then s else branch s (required loc predicate)), value

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
  then s, None
  else
    match cases with
    | [] -> branch s (Boolean false), None
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
    { s with omitted_premises = (loc, error) :: s.omitted_premises }, value

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
  then s, None
  else
    let s, value = expression_desc ctx s e in
    match intro_loc e with
    | Some loc when not s.dead ->
      let r = refinement e.exp_env e.exp_type e.exp_loc in
      let goals, goal =
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
      let assertion =
        Assert
          { loc;
            goal =
              (if goals.dead
               then Boolean true
               else required r.ref_pred.rexp_loc goal);
            omitted_premises = goals.omitted_premises
          }
      in
      let check = assertion :: added_prefix ~base:s.code goals.code in
      { s with code = Check check :: s.code }, value
    | _ -> s, value

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
    s, value
  | Texp_constant c -> s, constant c
  | Texp_construct (_, c, _, [], _) ->
    s, constructor e.exp_env e.exp_type c.cstr_name
  | Texp_let (rec_flag, bindings, body) ->
    let s, _ = value_bindings ctx s rec_flag bindings (has_elim e) in
    eval s body
  | Texp_assume (binding, _, _) ->
    let s, value = eval s binding.vb_expr in
    expose_fact ctx e.exp_env s e.exp_type value e.exp_loc
  | Texp_logical_equal (left, right) -> (
    let s, right = eval s right in
    let s, left = eval s left in
    match left, right with
    | Some left, Some right when term_sort left = term_sort right ->
      name s (Some (both Eq left right))
    | _ -> s, opaque ())
  | Texp_sequence (a, _, b) ->
    let s, _ = eval s a in
    eval s b
  | Texp_ifthenelse (c, t, f) ->
    let s, c = eval s c in
    let c =
      match c with
      | Some c -> c
      | None ->
        required e.exp_loc (fresh ctx e.exp_env Predef.type_bool "condition")
    in
    choose s c
      (fun s -> eval s t)
      (fun s -> match f with None -> s, None | Some f -> eval s f)
  | Texp_apply (fn, args, _, _, _, _) ->
    let prim =
      match fn.exp_desc with
      | Texp_ident { path; _ } -> primitive fn.exp_env path
      | _ -> None
    in
    begin match prim, args with
    | ( Some ((("%sequand" | "%sequor") as op), 2),
        [(_, Arg (a, _)); (_, Arg (b, _))] ) ->
      short_circuit eval e.exp_loc ~is_and:(op = "%sequand") s a b
    | _ -> (
      let argument s (_, arg) =
        match arg with Omitted _ -> s, None | Arg (e, _) -> eval s e
      in
      let s, args = arguments_right_to_left argument s args in
      let s, _ = eval s fn in
      let value =
        match prim with
        | Some (op, arity) when arity = List.length args ->
          operation e.exp_env e.exp_type op args
        | _ -> None
      in
      match prim with
      | Some (("%raise" | "%reraise" | "%raise_notrace"), 1) ->
        branch s (Boolean false), None
      | _ -> name s (match value with Some _ -> value | None -> opaque ()))
    end
  | Texp_function { params; body; _ } ->
    let captured = s in
    let s = { s with code = erase_assertions s.code } in
    let s =
      List.fold_left
        (fun s p ->
          let s, pat =
            match p.fp_kind with
            | Tparam_pat pat -> s, pat
            | Tparam_optional_default (pat, default, _) ->
              let checked, _ = eval s default in
              ( { s with
                  code =
                    Check (added_prefix ~base:s.code checked.code) :: s.code
                },
                pat )
          in
          let value =
            fresh ctx pat.pat_env pat.pat_type (Ident.name p.fp_param)
          in
          let s, condition = pattern ctx (bind s p.fp_param value) value pat in
          branch s condition)
        s params
    in
    let s, _ =
      match body with
      | Tfunction_body body -> eval s body
      | Tfunction_cases cases ->
        begin match cases.fc_cases with
        | [] -> s, None
        | c :: _ ->
          let value = fresh ctx c.c_lhs.pat_env c.c_lhs.pat_type "argument" in
          value_cases ctx (bind s cases.fc_param value) value cases.fc_cases
        end
    in
    ctx.batches <- s.code :: ctx.batches;
    captured, None
  | Texp_match (scrutinee, _, cases, [], _)
    when List.for_all (fun c -> snd (split_pattern c.c_lhs) = None) cases ->
    let s, value = eval s scrutinee in
    computation_cases ctx s value cases
  | _ ->
    (* Unknown evaluation/control-flow forms lose outgoing facts, but cannot
       hide obligations in their children or delayed bodies. *)
    let state = ref s in
    let iterator = iterator ctx state in
    Tast_iterator.default_iterator.expr iterator e;
    !state, opaque ()

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
    | [] -> s, None
    | _ when impossible s -> s, None
    | vb :: rest ->
      let s, value = expression ctx s vb.vb_expr in
      let s, value =
        if eliminate
        then
          expose_fact ctx vb.vb_expr.exp_env s vb.vb_expr.exp_type value
            vb.vb_expr.exp_loc
        else s, value
      in
      let s, condition = pattern ctx s value vb.vb_pat in
      loop (branch s condition) rest
  in
  loop s bindings

and value_cases ctx s value cases = cases_with_pattern ctx s value cases

and computation_cases ctx s value cases = cases_with_pattern ctx s value cases

and cases_with_pattern : type k.
    context -> state -> term option -> k case list -> state * term option =
 fun ctx s value cases ->
  if impossible s
  then s, None
  else
    match cases with
    | [] -> branch s (Boolean false), None
    | c :: cases ->
      let matched = pattern ctx s value c.c_lhs in
      let rest s = cases_with_pattern ctx s value cases in
      guarded_case (expression ctx) c.c_rhs.exp_loc s matched c.c_guard c.c_rhs
        rest

and structure ctx s str =
  List.fold_left
    (fun (s, _) item ->
      if impossible s
      then s, None
      else
        match item.str_desc with
        | Tstr_value (rec_flag, bindings) ->
          value_bindings ctx s rec_flag bindings false
        | Tstr_eval (e, _, _) -> expression ctx s e
        | _ ->
          let state = ref s in
          let iterator = iterator ctx state in
          Tast_iterator.default_iterator.structure_item iterator item;
          !state, None)
    (s, None) str.str_items

and iterator ctx state =
  let checked f =
    let s = !state in
    let result, _ = f s in
    state
      := { s with
           code = Check (added_prefix ~base:s.code result.code) :: s.code
         }
  in
  { Tast_iterator.default_iterator with
    expr = (fun _ e -> checked (fun s -> expression ctx s e));
    value_bindings =
      (fun _ (rec_flag, bindings) ->
        checked (fun s -> value_bindings ctx s rec_flag bindings false));
    structure = (fun _ str -> checked (fun s -> structure ctx s str))
  }

let query code =
  let definitions = ref [] in
  let share = function
    | App _ as term ->
      let value = fresh_symbol Bool "reachable" in
      definitions
        := { label = "reachable"; term = both Eq value term } :: !definitions;
      value
    | term -> term
  in
  let goals = ref [] in
  let rec forward code reachable =
    List.fold_left
      (fun reachable -> function
        | Assume p -> share (both And reachable p)
        | Define term ->
          definitions := { label = "value"; term } :: !definitions;
          reachable
        | Assert o ->
          goals := (o, both Implies reachable o.goal) :: !goals;
          reachable
        | Choice (a, b) ->
          let a = forward a reachable in
          let b = forward b reachable in
          share (both Or a b)
        | Check code ->
          ignore (forward code reachable);
          reachable)
      reachable (List.rev code)
  in
  ignore (forward code (Boolean true));
  let goals = List.rev !goals in
  let goal =
    { label = "refine_";
      term =
        List.fold_left (fun q (_, goal) -> both And q goal) (Boolean true) goals
    }
  in
  let facts = List.rev !definitions in
  let seen = Hashtbl.create 16 and symbols = ref [] in
  let rec visit = function
    | Var s when not (Hashtbl.mem seen s) ->
      Hashtbl.add seen s ();
      symbols := s :: !symbols
    | App (_, args) -> List.iter visit args
    | _ -> ()
  in
  List.iter (fun f -> visit f.term) facts;
  visit goal.term;
  { symbols = List.rev !symbols; facts; goal }, goals

let verify_batch prove code =
  let query, goals = query code in
  let prove_one (o : obligation) q =
    try prove o.loc q
    with Location.Error error ->
      let s = { empty with omitted_premises = o.omitted_premises } in
      raise
        (Location.Error
           { error with sub = error.sub @ omitted_premise_messages s })
  in
  match goals with
  | [] -> ()
  | [(o, _)] -> prove_one o query
  | (first, _) :: _ -> (
    try prove first.loc query
    with Location.Error _ ->
      List.iter
        (fun (o, term) ->
          prove_one o { query with goal = { label = "refine_"; term } })
        goals)

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
    let ctx = { free = Path.Map.empty; batches = [] } in
    let result, _ = structure ctx empty str in
    List.iter (verify_batch prove) (List.rev ctx.batches);
    verify_batch prove result.code
