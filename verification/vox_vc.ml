open Types
open Typedtree
open Vox_smt
open Vox_encoding

type function_value =
  { label : string;
    instances : Function.t list ref;
    primitive : (string * int) option;
    choice : (term * function_value * function_value) option;
    total : bool
  }

type value =
  | Scalar of term
  | Function of function_value

let scalar = function Some (Scalar t) -> Some t | _ -> None

let scalar_value t = Some (Scalar t)

let scalar_option = Option.map (fun t -> Scalar t)

let equal_value a b =
  match a, b with
  | None, None -> true
  | Some (Scalar a), Some (Scalar b) -> a = b
  | Some (Function a), Some (Function b) ->
    a.instances == b.instances && a.total = b.total
  | _ -> false

let join_value condition a b =
  if equal_value a b
  then a
  else
    match a, b with
    | Some (Scalar a), Some (Scalar b) ->
      Some (Scalar (App (Ite, [condition; a; b])))
    | Some (Function a), Some (Function b) ->
      Some
        (Function
           { label = "choice";
             instances = ref [];
             choice = Some (condition, a, b);
             primitive = (if a.primitive = b.primitive then a.primitive else None);
             total = a.total && b.total
           })
    | _ -> None

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
  { values : value option Path.Map.t;
    code : command list;
    dead : bool;
    omitted_premises : (Location.t * Location.error) list
  }

module Symbolic_keys = Hashtbl.Make (struct
  type t = Path.t * sort

  let equal (path1, sort1) (path2, sort2) =
    Path.same path1 path2 && sort1 = sort2

  let hash (path, sort) = Hashtbl.hash (Path.hash path, sort)
end)

type context =
  { mutable functions : Function.t list;
    mutable free : value option Path.Map.t;
    mutable batches : command list list;
    symbolic : value option Symbolic_keys.t;
    prove : Location.t -> query -> unit;
    verify_introductions : bool;
    mutable check_call : context -> state -> expression -> value option list -> unit
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

let fresh_symbol sort label = Var (Symbol.create ~label sort)

let name s = function
  | Some (Scalar ((App _ | Call _) as term)) ->
    let s =
      match term with
      | App ((Div | Rem), [_; divisor]) ->
        branch s (App (Ne, [divisor; Integer 0L]))
      | _ -> s
    in
    let value = fresh_symbol (term_sort term) "value" in
    { s with code = Define (both Eq value term) :: s.code }, scalar_value value
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
        else join_value condition a b
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
      choose s condition
        (fun s -> eval s b)
        (fun s -> s, scalar_value (Boolean false))
    else
      choose s condition
        (fun s -> s, scalar_value (Boolean true))
        (fun s -> eval s b)

let guarded_case eval loc s (matched, condition) guard body rest =
  let values = s.values in
  let s, accepted =
    match guard with
    | None -> matched, condition
    | Some g ->
      let state, value =
        choose matched condition
          (fun s -> eval s g)
          (fun s -> s, scalar_value (Boolean false))
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

let fresh ?primitive _ctx env ty label =
  match sort env ty with
  | Some sort -> scalar_value (fresh_symbol sort label)
  | None -> (
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow _ ->
      Some
        (Function { label; instances = ref []; choice = None; primitive; total = false })
    | _ -> None)

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
  match value_constant env ty path with
  | Some value -> scalar_value value
  | None -> (
    match Path.Map.find_opt path s.values with
    | Some value -> instantiate_path ctx env ty path value
    | None -> (
      match Path.Map.find_opt path ctx.free with
      | Some value -> instantiate_path ctx env ty path value
      | None ->
        let value =
          fresh ?primitive:(primitive env path) ctx env ty (Path.name path)
        in
        ctx.free <- Path.Map.add path value ctx.free;
        value))

let operation env function_type result_type name args =
  scalar_option (Vox_encoding.operation env ~function_type ~result_type name (List.map scalar args))

let rec function_call ctx env ty fn args =
  match fn with
  | Some (Function { choice = Some (condition, a, b); _ }) ->
    join_value condition
      (function_call ctx env ty (Some (Function a)) args)
      (function_call ctx env ty (Some (Function b)) args)
  | _ -> (
    match fn, signature env ty (List.length args) with
    | Some (Function fn), Some (arguments, result) ->
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
    | _ -> None)

let apply_function ctx env fn_type result_type prim fn args ~total =
  let value =
    match prim with
    | Some (name, arity) when arity = List.length args ->
      operation env fn_type result_type name args
    | _ -> None
  in
  match value with
  | Some _ -> value
  | None when total ->
    (* Trusted total declarations must respect the scalar encoding: equal bigint
       numbers are indistinguishable, regardless of allocation identity. *)
    function_call ctx env fn_type fn args
  | None -> None

let stored_primitive syntax = function
  | Some (Function { primitive = Some _ as primitive; _ }) -> primitive
  | _ -> syntax

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
    | Rexp_constant c -> s, scalar_value (required e.rexp_loc (rconstant c))
    | Rexp_construct (p, []) -> s, rconstructor ctx env e.rexp_type p
    | Rexp_apply (fn, args) ->
      let prim =
        match fn.rexp_desc with
        | Rexp_ident path -> primitive env path
        | _ -> None
      in
      begin match prim, args with
      | Some ((("%sequand" | "%sequor") as op), 2), [(_, a); (_, b)] ->
        short_circuit eval e.rexp_loc ~is_and:(op = "%sequand") s a b
      | _ ->
        let s, args =
          arguments_right_to_left (fun s (_, e) -> eval s e) s args
        in
        let s, value = eval s fn in
        let prim = stored_primitive prim value in
        if s.dead
        then s, None
        else
          name s
            (scalar_value
               (required e.rexp_loc
                  (apply_function ctx env fn.rexp_type e.rexp_type prim value
                     args ~total:true)))
      end
    | Rexp_logical_equal (left, right) ->
      let s, right = eval s right in
      let s, left = eval s left in
      if s.dead
      then s, None
      else
        name s
          (scalar_value
             (both Eq (required e.rexp_loc left) (required e.rexp_loc right)))
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
  then s, None
  else
    match cases with
    | [] -> branch s (Boolean false), None
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
  then s, None
  else
    let s, value = expression_desc ctx s e in
    match if ctx.verify_introductions then intro_loc e else None with
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
  | Texp_ident { path; desc; mode; _ } ->
    let value =
      match desc.val_kind with
      | Val_mut _ | Val_ivar _ -> opaque ()
      | Val_prim p when p.prim_arity = 0 -> opaque ()
      | _ -> at_mode mode (lookup ctx s e.exp_env e.exp_type path)
    in
    s, value
  | Texp_constant c -> s, constant c
  | Texp_construct (_, c, _, [], _) ->
    s, expression_constructor ctx e.exp_env e.exp_type c
  | Texp_open ({ open_expr = { mod_desc = Tmod_ident _; _ }; _ }, body) ->
    eval s body
  | Texp_letmodule (Some id, _, _, m, body) when Option.is_some (module_structure m) ->
    let str = Option.get (module_structure m) in
    let s, _ = structure ctx s str in
    eval (export_module ctx id str s) body
  | Texp_let (rec_flag, bindings, body) ->
    let s, _ = value_bindings ctx s rec_flag bindings (has_elim e) in
    eval s body
  | Texp_assume (binding, _, _) ->
    let s, value = eval s binding.vb_expr in
    expose_fact ctx e.exp_env s e.exp_type value e.exp_loc
  | Texp_logical_equal (left, right) -> (
    let s, right = eval s right in
    let s, left = eval s left in
    match scalar left, scalar right with
    | Some left, Some right when term_sort left = term_sort right ->
      name s (scalar_value (both Eq left right))
    | _ -> s, opaque ())
  | Texp_sequence (a, _, b) ->
    let s, _ = eval s a in
    eval s b
  | Texp_ifthenelse (c, t, f) ->
    let s, c = eval s c in
    let c =
      match scalar c with
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
      if not s.dead then ctx.check_call ctx s e args;
      let s, fn_value = eval s fn in
      let prim = stored_primitive prim fn_value in
      let total =
        match fn_value with Some (Function { total; _ }) -> total | _ -> false
      in
      let value =
        apply_function ctx e.exp_env fn.exp_type e.exp_type prim fn_value args
          ~total
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
    captured, opaque ()
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
    context -> state -> value option -> k case list -> state * value option =
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
        | Tstr_module { mb_id = Some id; mb_expr; _ }
          when Option.is_some (module_structure mb_expr) ->
          let str = Option.get (module_structure mb_expr) in
          let s, _ = structure ctx s str in
          export_module ctx id str s, None
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

let query ctx code =
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
    | App (_, args) | Call (_, args) -> List.iter visit args
    | _ -> ()
  in
  List.iter (fun f -> visit f.term) facts;
  visit goal.term;
  ( { symbols = List.rev !symbols;
      functions = List.rev ctx.functions;
      facts;
      goal
    },
    goals )

let verify_batch ctx prove code =
  let query, goals = query ctx code in
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

let context ~prove ~verify_introductions =
  { functions = []; free = Path.Map.empty; batches = []; symbolic = Symbolic_keys.create 16; prove;
    verify_introductions; check_call = (fun _ _ _ _ -> ()) }

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
    let result, _ = structure ctx empty str in
    List.iter (verify_batch ctx ctx.prove) (List.rev ctx.batches);
    verify_batch ctx prove result.code

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
              | Some Int -> scalar_value (Big_integer "0")
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
    | Texp_open ({ open_expr = { mod_desc = Tmod_ident _; _ }; _ }, body) ->
      check body
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
  let entry, entry_measure = expression ctx entry measure in
  if not entry.dead then begin
    let entry_measure = required measure.exp_loc entry_measure in
    let check_call ctx s call args =
      match call.exp_desc with
      | Texp_apply ({ exp_desc = Texp_ident { path = Path.Pident id; _ }; _ }, _, _, _, _, _)
        when Ident.same self id ->
        let call_state = List.fold_left2 (fun s (id, _) value -> bind s id value) s params args in
        let checked, value = expression ctx call_state measure in
        if not checked.dead then
          verify_batch ctx prove (Assert { loc = call.exp_loc;
            goal = (let value = required measure.exp_loc value in
              match term_sort entry_measure with
              | Int63 -> both Lt value entry_measure
              | Int -> both And (both Int_ge value (Big_integer "0")) (both Int_lt value entry_measure)
              | Bool | Opaque _ -> reject measure);
            omitted_premises = checked.omitted_premises } :: checked.code)
      | _ -> ()
    in
    ctx.check_call <- check_call;
    ignore (expression ctx entry body)
  end
