open Types
open Typedtree
open Vox_smt
open Vox_encoding

exception Unproved of Location.error

module Term_table = Hashtbl.Make (struct
  type t = term

  let equal left right = compare left right = 0

  (* The default hash only inspects ten meaningful nodes. Large obligations
     share that prefix, causing expensive comparisons in the visited table. *)
  let hash term = Hashtbl.hash_param 100 1000 term
end)

type logical_lambda =
  { parameters : Symbol.t list;
    body : term;
    definitions : (Symbol.t, term) Hashtbl.t;
    observations : (term, term) Hashtbl.t
  }

type function_value =
  { label : string;
    instances : Function.t list ref;
    specializations :
      ((int * function_value * sort list * sort * sort list) list * Function.t)
      list
      ref;
    primitive : (string * int) option;
    choice : (term * function_value * function_value) option;
    total : bool;
    (* Shared by aliases and choices until deferred body checking completes. *)
    lambda : logical_lambda option ref;
    application : (type_expr * function_value * value option list) option
  }

and value =
  | Scalar of term
  | Function of function_value
  | Record of (string * value option) list

type set_origin =
  | Set_empty
  | Set_singleton
  | Set_add
  | Set_remove
  | Set_union
  | Set_inter
  | Set_diff

type map_origin =
  | Map_empty
  | Map_singleton
  | Map_add
  | Map_remove

type iarray_origin =
  | Iarray_literal of term option list
  | Iarray_append of term * term
  | Iarray_sub of term * term * term
  | Iarray_set of term * term * term

let scalar = function Some (Scalar t) -> Some t | _ -> None

let scalar_value t = Some (Scalar t)

let scalar_option = Option.map (fun t -> Scalar t)

let rec equal_value a b =
  match a, b with
  | None, None -> true
  | Some (Scalar a), Some (Scalar b) -> a = b
  | Some (Function a), Some (Function b) ->
    a.instances == b.instances && a.total = b.total
  | Some (Record a), Some (Record b) ->
    List.length a = List.length b
    && List.for_all2 (fun (x, a) (y, b) -> x = y && equal_value a b) a b
  | _ -> false

let rec join_value condition a b =
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
             specializations = ref [];
             choice = Some (condition, a, b);
             primitive =
               (if a.primitive = b.primitive then a.primitive else None);
             total = a.total && b.total;
             lambda = ref None;
             application = None
           })
    | Some (Record a), Some (Record b) when List.map fst a = List.map fst b ->
      Some
        (Record
           (List.map2
              (fun (label, a) (_, b) -> label, join_value condition a b)
              a b))
    | _ -> None

type obligation =
  { loc : Location.t;
    origin : Location.t;  (** where the required refinement is written *)
    goal : term;
    omitted_premises : (Location.t * Location.error) list;
    group : int;
        (** The conjuncts of one refinement share a group, which is proved as
            one query; the conjuncts are proved alone only to name a failure. *)
    note : Location.msg option;
        (** Explains an obligation that no written refinement states. *)
    headline : string option;  (** printed before the solver's message *)
    context : Location.msg list  (** printed after the refinement's origin *)
  }

let next_group = ref 0

let fresh_group () =
  incr next_group;
  !next_group

(* Command lists are stored in reverse execution order. Define binds a fresh
   symbol to a total SMT expression; it does not restrict reachability. Check
   verifies a nested computation without exporting its assumptions. *)
type command =
  | Assume of term
  | Define of term
  | Assert of obligation
  | Choice of command list * command list
  | Check of command list

module Exposed = Set.Make (struct
  type t = int * term

  let compare = compare
end)

module Unfolded = Set.Make (struct
  type t = Path.t * term

  let compare (path, term) (path', term') =
    match Path.compare path path' with 0 -> compare term term' | c -> c
end)

type state =
  { values : value option Path.Map.t;
    code : command list;
    dead : bool;
    omitted_premises : (Location.t * Location.error) list;
    (* Refinements already assumed on this path, by type node and value. A
       branch's additions are dropped at the join, which rebuilds the state from
       the one before the branch. *)
    exposed : Exposed.t;
    (* Applications of transparent definitions already unfolded on this path. *)
    unfolded : Unfolded.t
  }

type deferred_check =
  { scope : state;
    check : state -> state
  }

module Symbolic_keys = Hashtbl.Make (struct
  type t = Path.t * sort

  let equal (path1, sort1) (path2, sort2) =
    Path.same path1 path2 && sort1 = sort2

  let hash (path, sort) = Hashtbl.hash (Path.hash path, sort)
end)

type logical_map =
  { key_sort : sort;
    some : Constructor.t;
    at : Function.t;
    equal : term -> term -> term option
  }

type context =
  { poll : unit -> unit;
    encoding : Vox_encoding.context;
    mutable datatypes : datatype_declaration list;
    mutable functions : Function.t list;
    function_cache : (string * sort list * sort, Function.t) Hashtbl.t;
    string_literals : (Function.t, int) Hashtbl.t;
    set_origins : (Function.t, set_origin) Hashtbl.t;
    set_class_sorts : (sort, sort) Hashtbl.t;
    set_membership : (sort * term * term, term) Hashtbl.t;
    observation_definitions : (Symbol.t, term) Hashtbl.t;
    shared_observations : term Term_table.t;
    map_origins : (Function.t, map_origin) Hashtbl.t;
    iarray_origins : (Symbol.t, iarray_origin) Hashtbl.t;
    iarray_constructors :
      (Function.t, [`Literal | `Append | `Sub | `Set]) Hashtbl.t;
    observation_equations : (term, term) Hashtbl.t;
    iarray_lengths : (term, term) Hashtbl.t;
    iarray_reads : (sort * term * term, term) Hashtbl.t;
    map_class_sorts : (sort, sort) Hashtbl.t;
    pref_heaps : (sort, unit) Hashtbl.t;
    finite_maps : (sort, logical_map) Hashtbl.t;
    pref_constructors :
      ( Function.t,
        [`Empty | `Put | `Remove | `Union | `Restrict | `Exclude] )
      Hashtbl.t;
    pref_observers :
      (Function.t, (Constructor.t * Constructor.t) option) Hashtbl.t;
    mutable free : value option Path.Map.t;
    (* Module aliases inside structures ([module B = Base]), by the local path
       and by the paths that export it. Signatures do not keep them. *)
    mutable module_aliases : Path.t Path.Map.t;
    (* Whether the predicate being evaluated is a goal. *)
    mutable in_goal : bool;
    mutable argument_values : value option Path.Map.t;
    (* Each function body's commands, with the warning settings in effect where
       it was written ([@warning] attributes apply to its proofs). *)
    mutable batches : (Warnings.state * command list) list;
    named_terms : (Symbol.t, term) Hashtbl.t;
    symbolic : value option Symbolic_keys.t;
    prove : batch:bool -> Location.t -> query -> unit;
    verify_introductions : bool;
    mutable check_call :
      context -> state -> expression -> value option list -> unit;
    (* Transparent definitions being unfolded, innermost first. *)
    mutable unfolding : Path.t list
  }

let empty =
  { values = Path.Map.empty;
    code = [];
    dead = false;
    omitted_premises = [];
    exposed = Exposed.empty;
    unfolded = Unfolded.empty
  }

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

(* The assumptions made by each proof step (warning 227), by physical identity
   of the command; see [Vox_proof_steps]. *)
module Step_assumptions = Hashtbl.Make (struct
  type t = command

  let equal = ( == )

  let hash = Hashtbl.hash
end)

let step_assumptions : Vox_proof_steps.step list Step_assumptions.t =
  Step_assumptions.create 16

(* Observation symbols and equations that the evaluation of proof steps recorded
   in the context: an expansion that uses them depends on those steps. *)
let observation_steps : (Symbol.t, Vox_proof_steps.step list) Hashtbl.t =
  Hashtbl.create 16

let equation_steps : Vox_proof_steps.step list Term_table.t =
  Term_table.create 16

let record_steps add key =
  match !Vox_proof_steps.current with [] -> () | steps -> add key steps

(* The values of refined parameters that are proof steps. Wherever such a
   value's refinement is exposed again, its facts belong to the step. *)
let value_steps : (Symbol.t, Vox_proof_steps.step) Hashtbl.t = Hashtbl.create 16

let register_value step value =
  match step, value with
  | Some step, Some (Scalar (Var symbol)) ->
    Hashtbl.replace value_steps symbol step
  | _ -> ()

let branch s term =
  match term with
  | Boolean true -> s
  | _ ->
    let assumption = Assume term in
    (match !Vox_proof_steps.current with
    | [] -> ()
    | steps ->
      Step_assumptions.replace step_assumptions assumption steps;
      (* Obligations on an impossible path are not generated, so no core can
         show that they need the step. *)
      if term = Boolean false then List.iter Vox_proof_steps.use steps);
    { s with
      code = assumption :: s.code;
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

(* Totality is relative to the arguments: a total, stateless [apply f x = f x]
   runs whatever effects [f] has. A call is therefore a function of its
   arguments only when each argument is itself total and stateless at the call:
   its type crosses both axes (data without closures), it is a function value
   seen at such a mode, it is an identifier (or an immutable field of one) whose
   use-site mode is, or it fills a dependent parameter, whose mode is total and
   stateless by construction. Other arguments, such as inline closures and
   applications returning closures or data holding them, make the call opaque.

   The verifier runs after typing, when every type is known, so crossing ignores
   [-principal]: under it, [Ctype.cross_left] refuses to cross types that were
   not principal during inference. *)
let rec use_mode (e : expression) =
  (* An immutable field has its record's mode after the field's modalities, as
     in [Typecore]; a mutable field may since have been overwritten. *)
  let field record modalities =
    Option.map (Mode.Modality.Const.apply_left modalities) (use_mode record)
  in
  match e.exp_desc with
  | Texp_ident { mode; _ } -> Some mode
  | Texp_field { record; label = { lbl_mut = Immutable; _ } as label; _ } ->
    field record label.lbl_modalities
  | Texp_unboxed_field { record; label; _ } -> field record label.lbl_modalities
  | _ -> None

let logical_argument ~dependent (e : expression) value =
  dependent
  || (match value with Some (Function { total; _ }) -> total | _ -> false)
  ||
  let mode =
    match use_mode e with
    | Some mode -> mode
    | None -> Mode.Value.(disallow_right max)
  in
  logical_function_mode
    (Misc.protect_refs
       [Misc.R (Clflags.principal, false)]
       (fun () -> Ctype.cross_left e.exp_env e.exp_type mode))

(* Whether each parameter of a function type, in order, is dependent. *)
let rec dependent_parameters env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tarrow ((_, _, _, binder), _, result, _) ->
    Option.is_some binder :: dependent_parameters env result
  | Tpoly (ty, []) | Trefine { ref_payload = ty; _ } ->
    dependent_parameters env ty
  | _ -> []

let fresh_symbol sort label = Var (Symbol.create ~label sort)

let name ctx s = function
  | Some (Scalar (Construct (_, []))) as value -> s, value
  | Some (Scalar ((App _ | Call _ | Construct _ | Is _ | Select _) as term)) ->
    let s =
      match term with
      | App ((Div | Rem), [_; divisor]) ->
        branch s (App (Ne, [divisor; Integer 0L]))
      | _ -> s
    in
    let symbol = Symbol.create ~label:"value" (term_sort term) in
    Hashtbl.add ctx.named_terms symbol term;
    let value = Var symbol in
    let definition = Define (both Eq value term) in
    record_steps (Step_assumptions.replace step_assumptions) definition;
    { s with code = definition :: s.code }, scalar_value value
  | value -> s, value

let rec expose_head ctx = function
  | Var symbol as value -> (
    match Hashtbl.find_opt ctx.named_terms symbol with
    | Some term -> expose_head ctx term
    | None -> value)
  | value -> value

let rec added_prefix ~base = function
  | current when current == base -> []
  | item :: rest -> item :: added_prefix ~base rest
  | [] -> Misc.fatal_error "VC: state does not extend its input"

let choose ctx s condition ifso ifnot =
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
      name ctx s value

let rec arguments_right_to_left eval s = function
  | [] -> s, []
  | arg :: args ->
    let s, values = arguments_right_to_left eval s args in
    let s, value = eval s arg in
    s, value :: values

let short_circuit ctx eval loc ~is_and s a b =
  let s, a = eval s a in
  if s.dead
  then s, None
  else
    let condition = required loc a in
    if is_and
    then
      choose ctx s condition
        (fun s -> eval s b)
        (fun s -> s, scalar_value (Boolean false))
    else
      choose ctx s condition
        (fun s -> s, scalar_value (Boolean true))
        (fun s -> eval s b)

let disjunction terms = List.fold_left (both Or) (Boolean false) terms

let merge_patterns base outcomes =
  let values =
    List.fold_right
      (fun (s, condition) values ->
        Path.Map.merge
          (fun _ left right ->
            match left, right with
            | None, value | value, None -> value
            | Some a, Some b -> Some (join_value condition a b))
          s.values values)
      outcomes Path.Map.empty
  in
  let code, omitted_premises =
    List.fold_left
      (fun (code, omitted) (s, condition) ->
        let facts = added_prefix ~base:base.code s.code in
        let code =
          match facts with
          | [] -> code
          | _ when condition = Boolean true -> facts @ code
          | _ ->
            Choice (facts @ [Assume condition], [Assume (not_ condition)])
            :: code
        in
        ( code,
          added_prefix ~base:base.omitted_premises s.omitted_premises @ omitted
        ))
      (base.code, base.omitted_premises)
      outcomes
  in
  ( { base with values; code; omitted_premises },
    disjunction (List.map snd outcomes) )

let guarded_case ctx eval_guard eval_body loc s matched guard body rest =
  let matched, condition = merge_patterns s matched in
  let values = s.values in
  let s, accepted =
    match guard with
    | None -> matched, condition
    | Some g ->
      let state, value =
        choose ctx matched condition
          (fun s -> eval_guard s g)
          (fun s -> s, scalar_value (Boolean false))
      in
      state, if state.dead then Boolean false else required loc value
  in
  choose ctx s accepted
    (fun s -> eval_body s body)
    (fun state -> rest { state with values })

let register_declarations ctx declarations =
  List.iter
    (fun declaration ->
      if
        not
          (List.exists
             (fun existing -> existing.datatype = declaration.datatype)
             ctx.datatypes)
      then ctx.datatypes <- declaration :: ctx.datatypes)
    declarations

let register_data ctx data =
  register_declarations ctx (declarations ctx.encoding data)

let register_sort ctx sort =
  register_declarations ctx (declarations_of_sort ctx.encoding sort)

let data_of_type ctx env ty =
  match data ctx.encoding env ty with
  | Some data ->
    register_data ctx data;
    Some data
  | None -> None

let data_constructor data name =
  match data.kind with
  | Tuple_data constructor | Record_data constructor -> Some constructor
  | Variant_data constructors -> List.assoc_opt name constructors

let path_constructor_name = function
  | Path.Pextra_ty (_, Path.Pcstr_ty name) -> Some name
  | _ -> None

let rec select ctx constructor index value =
  match expose_head ctx value with
  | Construct (actual, fields) when actual = constructor ->
    List.nth fields index
  | App (Ite, [condition; left; right]) ->
    App
      ( Ite,
        [ condition;
          select ctx constructor index left;
          select ctx constructor index right ] )
  | _ -> Select (constructor, index, value)

let construct ctx env ty name values =
  match data_of_type ctx env ty with
  | None -> None
  | Some data ->
    begin match data_constructor data name with
    | None -> None
    | Some constructor ->
      begin match Misc.Stdlib.List.map_option scalar values with
      | Some values
        when List.map term_sort values
             = List.map snd (Constructor.fields constructor) ->
        scalar_value (Construct (constructor, values))
      | Some _ | None -> None
      end
    end

let select_field ctx env ty name value =
  match value with
  | Some (Record fields) -> Option.join (List.assoc_opt name fields)
  | _ -> (
    match data_of_type ctx env ty, scalar value with
    | Some { kind = Record_data constructor; _ }, Some value ->
      begin match
        List.find_mapi
          (fun index (label, _) -> if label = name then Some index else None)
          (Constructor.fields constructor)
      with
      | Some index -> scalar_value (select ctx constructor index value)
      | None -> None
      end
    | _ -> None)

let record_value ctx env ty base fields =
  match data_of_type ctx env ty with
  | Some { kind = Record_data constructor; _ } ->
    let field name =
      match List.assoc_opt name fields with
      | Some value -> scalar value
      | None ->
        Option.bind base (fun (ty, value) ->
            scalar (select_field ctx env ty name value))
    in
    let values =
      List.map (fun (name, _) -> field name) (Constructor.fields constructor)
    in
    begin match Misc.Stdlib.List.map_option Fun.id values with
    | Some values -> scalar_value (Construct (constructor, values))
    | None -> None
    end
  | _ ->
    Option.map
      (fun labels ->
        Record
          (List.map
             (fun (name, _) ->
               let value =
                 match List.assoc_opt name fields with
                 | Some value -> value
                 | None ->
                   Option.bind base (fun (ty, value) ->
                       select_field ctx env ty name value)
               in
               name, value)
             labels))
      (immutable_record_fields env ty)

let rec erase_assertions code =
  List.filter_map
    (function
      | Assert _ | Check _ -> None
      | (Assume _ | Define _) as c -> Some c
      | Choice (a, b) -> Some (Choice (erase_assertions a, erase_assertions b)))
    code

let fact s _label term = branch s term

let intern_function ctx label arguments result =
  List.iter (register_sort ctx) (result :: arguments);
  let key = label, arguments, result in
  match Hashtbl.find_opt ctx.function_cache key with
  | Some function_ -> function_
  | None ->
    let function_ = Function.create ~label ~arguments ~result in
    Hashtbl.add ctx.function_cache key function_;
    ctx.functions <- function_ :: ctx.functions;
    function_

let share_observation ctx term =
  match term with
  | Boolean _ | Integer _ | Var _ -> term
  | _ -> (
    match Term_table.find_opt ctx.shared_observations term with
    | Some value -> value
    | None ->
      let symbol = Symbol.create ~label:"observation" (term_sort term) in
      Hashtbl.add ctx.observation_definitions symbol term;
      record_steps (Hashtbl.replace observation_steps) symbol;
      Hashtbl.add ctx.named_terms symbol term;
      let value = Var symbol in
      Term_table.add ctx.shared_observations term value;
      value)

let map_term_children f = function
  | App (op, args) -> App (op, List.map f args)
  | Call (fn, args) -> Call (fn, List.map f args)
  | Construct (constructor, args) -> Construct (constructor, List.map f args)
  | Is (constructor, arg) -> Is (constructor, f arg)
  | Select (constructor, field, arg) -> Select (constructor, field, f arg)
  | (Boolean _ | Integer _ | Big_integer _ | Var _) as term -> term

(* Unresolved body results are not captures: keeping them free would identify
   different applications of a function whose body we cannot model. *)
let logical_lambda ctx captured argument_values parameters body =
  let retained = Hashtbl.create 16 in
  let rec retain term =
    match term with
    | Var symbol when not (Hashtbl.mem retained symbol) ->
      Hashtbl.add retained symbol ();
      Option.iter retain (Hashtbl.find_opt ctx.named_terms symbol)
    | _ ->
      ignore
        (map_term_children
           (fun term ->
             retain term;
             term)
           term)
  in
  let retain_value _ value = Option.iter retain (scalar value) in
  Path.Map.iter retain_value captured.values;
  Path.Map.iter retain_value argument_values;
  Path.Map.iter retain_value ctx.free;
  Symbolic_keys.iter retain_value ctx.symbolic;
  List.iter (fun symbol -> Hashtbl.replace retained symbol ()) parameters;
  let definitions = Hashtbl.create 16 in
  let observations = Hashtbl.create 16 in
  let visited = Hashtbl.create 16 in
  let rec visit term =
    if not (Hashtbl.mem visited term)
    then begin
      Hashtbl.add visited term ();
      Option.iter
        (fun equation ->
          Hashtbl.add observations term equation;
          visit equation)
        (Hashtbl.find_opt ctx.observation_equations term);
      match term with
      | Var symbol when not (Hashtbl.mem retained symbol) ->
        begin match Hashtbl.find_opt ctx.named_terms symbol with
        | None -> raise Exit
        | Some term ->
          Hashtbl.add definitions symbol term;
          visit term
        end
      | _ ->
        ignore
          (map_term_children
             (fun term ->
               visit term;
               term)
             term)
    end
  in
  match visit body with
  | () -> Some { parameters; body; definitions; observations }
  | exception Exit -> None

let instantiate_lambda ctx lambda args =
  let values = Hashtbl.create 16 in
  let terms = Hashtbl.create 16 in
  List.iter2 (Hashtbl.add values) lambda.parameters args;
  let rec instantiate term =
    match Hashtbl.find_opt terms term with
    | Some value -> value
    | None ->
      let definition =
        match term with
        | Var symbol ->
          begin match Hashtbl.find_opt values symbol with
          | Some value -> value
          | None ->
            begin match Hashtbl.find_opt lambda.definitions symbol with
            | None -> term
            | Some definition -> instantiate definition
            end
          end
        | term -> map_term_children instantiate term
      in
      let value = share_observation ctx definition in
      Hashtbl.add terms term value;
      Option.iter
        (fun equation ->
          Hashtbl.replace ctx.observation_equations definition
            (instantiate equation);
          record_steps (Term_table.replace equation_steps) definition)
        (Hashtbl.find_opt lambda.observations term);
      value
  in
  instantiate lambda.body

let iarray_origin ctx array =
  match expose_head ctx array with
  | Var symbol -> Hashtbl.find_opt ctx.iarray_origins symbol
  | Call (fn, args) ->
    begin match Hashtbl.find_opt ctx.iarray_constructors fn, args with
    | Some `Literal, elements ->
      Some (Iarray_literal (List.map Option.some elements))
    | Some `Append, [left; right] -> Some (Iarray_append (left, right))
    | Some `Sub, [source; position; length] ->
      Some (Iarray_sub (source, position, length))
    | Some `Set, [source; index; value] ->
      Some (Iarray_set (source, index, value))
    | _ -> None
    end
  | _ -> None

let observe_iarray ctx call value =
  if call <> value
  then begin
    Hashtbl.replace ctx.observation_equations call (share_observation ctx value);
    record_steps (Term_table.replace equation_steps) call
  end;
  call

let rec iarray_length ctx iarray_sort array =
  match Hashtbl.find_opt ctx.iarray_lengths array with
  | Some value -> value
  | None ->
    let value = expand_iarray_length ctx iarray_sort array in
    let call =
      Call (intern_function ctx "Iarray.length" [iarray_sort] Int63, [array])
    in
    let value = observe_iarray ctx call value in
    Hashtbl.add ctx.iarray_lengths array value;
    value

and expand_iarray_length ctx iarray_sort array =
  match iarray_origin ctx array with
  | Some (Iarray_literal elements) ->
    Integer (Int64.of_int (List.length elements))
  | Some (Iarray_append (left, right)) ->
    App
      ( Add,
        [iarray_length ctx iarray_sort left; iarray_length ctx iarray_sort right]
      )
  | Some (Iarray_sub (_, _, length)) -> length
  | Some (Iarray_set (source, _, _)) -> iarray_length ctx iarray_sort source
  | None -> (
    match expose_head ctx array with
    | App (Ite, [condition; left; right]) ->
      App
        ( Ite,
          [ condition;
            iarray_length ctx iarray_sort left;
            iarray_length ctx iarray_sort right ] )
    | _ ->
      let function_ = intern_function ctx "Iarray.length" [iarray_sort] Int63 in
      Call (function_, [array]))

let rec iarray_get_with_budget ctx budget iarray_sort element_sort array index =
  let key = element_sort, array, index in
  match Hashtbl.find_opt ctx.iarray_reads key with
  | Some value -> value
  | None when !budget = 0 ->
    let function_ =
      intern_function ctx "Iarray.get" [iarray_sort; Int63] element_sort
    in
    Call (function_, [array; index])
  | None ->
    decr budget;
    let value =
      expand_iarray_get_with_budget ctx budget iarray_sort element_sort array
        index
    in
    let call =
      Call
        ( intern_function ctx "Iarray.get" [iarray_sort; Int63] element_sort,
          [array; index] )
    in
    let value = observe_iarray ctx call value in
    Hashtbl.add ctx.iarray_reads key value;
    value

and expand_iarray_get_with_budget ctx budget iarray_sort element_sort array
    index =
  let unknown () =
    let function_ =
      intern_function ctx "Iarray.get" [iarray_sort; Int63] element_sort
    in
    Call (function_, [array; index])
  in
  let bounded value =
    App
      ( Ite,
        [ both And
            (both Le (Integer 0L) index)
            (both Lt index (iarray_length ctx iarray_sort array));
          value;
          unknown () ] )
  in
  match iarray_origin ctx array with
  | Some (Iarray_literal elements) ->
    List.fold_right
      (fun (position, element) rest ->
        match element with
        | Some value when term_sort value = element_sort ->
          App
            (Ite, [both Eq index (Integer (Int64.of_int position)); value; rest])
        | _ -> rest)
      (List.mapi (fun position element -> position, element) elements)
      (unknown ())
  | Some (Iarray_append (left, right)) ->
    let length = iarray_length ctx iarray_sort left in
    let shifted = share_observation ctx (App (Sub, [index; length])) in
    if left = right
    then
      let index =
        share_observation ctx
          (App (Ite, [both Lt index length; index; shifted]))
      in
      bounded
        (iarray_get_with_budget ctx budget iarray_sort element_sort left index)
    else
      bounded
        (App
           ( Ite,
             [ both Lt index length;
               iarray_get_with_budget ctx budget iarray_sort element_sort left
                 index;
               iarray_get_with_budget ctx budget iarray_sort element_sort right
                 shifted ] ))
  | Some (Iarray_set (source, changed, value)) ->
    bounded
      (App
         ( Ite,
           [ both Eq index changed;
             value;
             iarray_get_with_budget ctx budget iarray_sort element_sort source
               index ] ))
  | Some (Iarray_sub (source, position, _)) ->
    bounded
      (iarray_get_with_budget ctx budget iarray_sort element_sort source
         (share_observation ctx (App (Add, [position; index]))))
  | None -> (
    match expose_head ctx array with
    | App (Ite, [condition; left; right]) ->
      App
        ( Ite,
          [ condition;
            iarray_get_with_budget ctx budget iarray_sort element_sort left
              index;
            iarray_get_with_budget ctx budget iarray_sort element_sort right
              index ] )
    | _ -> unknown ())

(* Branching copy histories may expose exponentially many source indices. Beyond
   this budget, reads retain their uninterpreted meaning. *)
let iarray_get ctx iarray_sort element_sort array index =
  iarray_get_with_budget ctx (ref 256) iarray_sort element_sort array index

let iarray_copy ctx iarray_sort origin =
  let constructor =
    match origin with
    | Iarray_literal elements when List.for_all Option.is_some elements ->
      Some (`Literal, "%vox.iarray.literal", List.map Option.get elements)
    | Iarray_append (left, right) ->
      Some (`Append, "%vox.iarray.append", [left; right])
    | Iarray_sub (source, position, length) ->
      Some (`Sub, "%vox.iarray.sub", [source; position; length])
    | Iarray_set (source, index, value) ->
      Some (`Set, "%vox.iarray.set", [source; index; value])
    | Iarray_literal _ -> None
  in
  match constructor with
  | Some (kind, label, args) ->
    let fn = intern_function ctx label (List.map term_sort args) iarray_sort in
    Hashtbl.replace ctx.iarray_constructors fn kind;
    scalar_value (share_observation ctx (Call (fn, args)))
  | None ->
    let symbol = Symbol.create ~label:"iarray copy" iarray_sort in
    register_sort ctx iarray_sort;
    Hashtbl.add ctx.iarray_origins symbol origin;
    scalar_value (Var symbol)

let set_constructor ctx origin label arguments set_sort terms =
  let function_ = intern_function ctx label arguments set_sort in
  Hashtbl.replace ctx.set_origins function_ origin;
  Call (function_, terms)

let set_empty ctx set_sort =
  set_constructor ctx Set_empty "Set.empty" [] set_sort []

let comparison_class ctx class_sorts label container_sort element =
  let element_sort = term_sort element in
  let class_sort =
    match Hashtbl.find_opt class_sorts container_sort with
    | Some sort -> sort
    | None ->
      let sort = fresh_opaque_sort ctx.encoding in
      Hashtbl.add class_sorts container_sort sort;
      sort
  in
  let function_ = intern_function ctx label [element_sort] class_sort in
  Call (function_, [element])

let set_class ctx set_sort element =
  comparison_class ctx ctx.set_class_sorts "Set.comparison_class" set_sort
    element

let set_same_element ctx set_sort left right =
  both Eq (set_class ctx set_sort left) (set_class ctx set_sort right)

let rec set_mem ctx set_sort element set =
  let key = set_sort, element, set in
  match Hashtbl.find_opt ctx.set_membership key with
  | Some value -> value
  | None ->
    let term = expand_set_mem ctx set_sort element set in
    let value = share_observation ctx term in
    Hashtbl.add ctx.set_membership key value;
    value

and expand_set_mem ctx set_sort element set =
  let class_ = set_class ctx set_sort element in
  let unknown () =
    let function_ =
      intern_function ctx "Set.mem" [term_sort class_; set_sort] Bool
    in
    Call (function_, [class_; set])
  in
  match expose_head ctx set with
  | App (Ite, [condition; left; right]) ->
    App
      ( Ite,
        [ condition;
          set_mem ctx set_sort element left;
          set_mem ctx set_sort element right ] )
  | Call (function_, arguments) ->
    begin match Hashtbl.find_opt ctx.set_origins function_, arguments with
    | Some Set_empty, [] -> Boolean false
    | Some Set_singleton, [member] ->
      set_same_element ctx set_sort element member
    | Some Set_add, [member; set] ->
      both Or
        (set_same_element ctx set_sort element member)
        (set_mem ctx set_sort element set)
    | Some Set_remove, [member; set] ->
      both And
        (not_ (set_same_element ctx set_sort element member))
        (set_mem ctx set_sort element set)
    | Some Set_union, [left; right] ->
      both Or
        (set_mem ctx set_sort element left)
        (set_mem ctx set_sort element right)
    | Some Set_inter, [left; right] ->
      both And
        (set_mem ctx set_sort element left)
        (set_mem ctx set_sort element right)
    | Some Set_diff, [left; right] ->
      both And
        (set_mem ctx set_sort element left)
        (not_ (set_mem ctx set_sort element right))
    | _ -> unknown ()
    end
  | _ -> unknown ()

let set_find ctx set_sort element set =
  let class_ = set_class ctx set_sort element in
  let function_ =
    intern_function ctx "Set.find"
      [term_sort class_; set_sort]
      (term_sort element)
  in
  Call (function_, [class_; set])

let map_constructor ctx origin label arguments map_sort terms =
  let function_ = intern_function ctx label arguments map_sort in
  Hashtbl.replace ctx.map_origins function_ origin;
  Call (function_, terms)

let map_empty ctx map_sort =
  map_constructor ctx Map_empty "Map.empty" [] map_sort []

let map_class ctx map_sort key =
  comparison_class ctx ctx.map_class_sorts "Map.comparison_class" map_sort key

let map_same_key ctx map_sort left right =
  both Eq (map_class ctx map_sort left) (map_class ctx map_sort right)

let rec map_mem ctx map_sort key map =
  let class_ = map_class ctx map_sort key in
  let unknown () =
    let function_ =
      intern_function ctx "Map.mem" [term_sort class_; map_sort] Bool
    in
    Call (function_, [class_; map])
  in
  match expose_head ctx map with
  | App (Ite, [condition; left; right]) ->
    App
      ( Ite,
        [ condition;
          map_mem ctx map_sort key left;
          map_mem ctx map_sort key right ] )
  | Call (function_, arguments) ->
    begin match Hashtbl.find_opt ctx.map_origins function_, arguments with
    | Some Map_empty, [] -> Boolean false
    | Some Map_singleton, [bound; _] -> map_same_key ctx map_sort key bound
    | Some Map_add, [bound; _; map] ->
      both Or
        (map_same_key ctx map_sort key bound)
        (map_mem ctx map_sort key map)
    | Some Map_remove, [bound; map] ->
      both And
        (not_ (map_same_key ctx map_sort key bound))
        (map_mem ctx map_sort key map)
    | _ -> unknown ()
    end
  | _ -> unknown ()

let rec map_find ctx map_sort value_sort key map =
  let unknown () =
    let class_ = map_class ctx map_sort key in
    let function_ =
      intern_function ctx "Map.find" [term_sort class_; map_sort] value_sort
    in
    Call (function_, [class_; map])
  in
  match expose_head ctx map with
  | App (Ite, [condition; left; right]) ->
    App
      ( Ite,
        [ condition;
          map_find ctx map_sort value_sort key left;
          map_find ctx map_sort value_sort key right ] )
  | Call (function_, arguments) ->
    begin match Hashtbl.find_opt ctx.map_origins function_, arguments with
    | Some Map_singleton, [_; data] when term_sort data = value_sort -> data
    | Some Map_add, [bound; data; map] when term_sort data = value_sort ->
      App
        ( Ite,
          [ map_same_key ctx map_sort key bound;
            data;
            map_find ctx map_sort value_sort key map ] )
    | Some Map_remove, [bound; rest] ->
      App
        ( Ite,
          [ map_same_key ctx map_sort key bound;
            unknown ();
            map_find ctx map_sort value_sort key rest ] )
    | Some Map_empty, [] | _ -> unknown ()
    end
  | _ -> unknown ()

let iarray_value ctx env ty s values =
  match iarray ctx.encoding env ty with
  | Some (iarray_sort, _) ->
    s, iarray_copy ctx iarray_sort (Iarray_literal (List.map scalar values))
  | None -> s, None

let fresh_function ?primitive label =
  Function
    { label;
      instances = ref [];
      specializations = ref [];
      choice = None;
      primitive;
      total = false;
      lambda = ref None;
      application = None
    }

let fresh ?primitive ctx env ty label =
  let is_function ty =
    match get_desc (Ctype.expand_head env ty) with
    | Tarrow _ -> true
    | _ -> false
  in
  let field (name, ty) =
    let label = label ^ "." ^ name in
    let value =
      if is_function ty
      then Some (fresh_function label)
      else
        Option.map
          (fun sort ->
            register_sort ctx sort;
            Scalar (fresh_symbol sort label))
          (sort ctx.encoding env ty)
    in
    name, value
  in
  match immutable_record_fields env ty with
  | Some fields when List.exists (fun (_, ty) -> is_function ty) fields ->
    Some (Record (List.map field fields))
  | _ -> (
    match sort ctx.encoding env ty with
    | Some sort ->
      register_sort ctx sort;
      scalar_value (fresh_symbol sort label)
    | None when is_function ty -> Some (fresh_function ?primitive label)
    | None -> None)

let symbolic_path ctx env ty path =
  let path = Env.normalize_value_path None env path in
  match sort ctx.encoding env ty with
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

(* A term built only from constructors and literals has the same shape at every
   instance of a polymorphic value (parametricity), so it is rebuilt at the sort
   of the use. Positions are fixed by the value's type scheme, so a constructor
   is found by its label in the datatype at the same position. *)
let rec rebuild_constructor_term ctx term target =
  if term_sort term = target
  then Some term
  else
    match expose_head ctx term, target with
    | Construct (constructor, args), Datatype datatype ->
      let label = Constructor.label constructor in
      let target_constructor =
        List.find_map
          (fun (declaration : datatype_declaration) ->
            if declaration.datatype = datatype
            then
              List.find_opt
                (fun candidate -> Constructor.label candidate = label)
                declaration.constructors
            else None)
          (declarations_of_sort ctx.encoding target)
      in
      begin match target_constructor with
      | Some target_constructor
        when List.compare_lengths (Constructor.fields target_constructor) args
             = 0 ->
        Option.map
          (fun args -> Construct (target_constructor, args))
          (Misc.Stdlib.List.map_option Fun.id
             (List.map2
                (fun arg (_, sort) -> rebuild_constructor_term ctx arg sort)
                args
                (Constructor.fields target_constructor)))
      | _ -> None
      end
    | _ -> None

let reinstantiate_constructor_term ctx env ty path term expected =
  match Subst.Lazy.force_value_description (Env.find_value path env) with
  | source when same_nominal_data_type env source.val_type ty ->
    register_sort ctx expected;
    Option.map
      (fun term -> Scalar term)
      (rebuild_constructor_term ctx term expected)
  | _ -> None
  | exception Not_found -> None

let instantiate_path ctx env ty path value =
  match value, sort ctx.encoding env ty with
  | Some (Scalar term), Some expected when term_sort term <> expected ->
    begin match expose_head ctx term with
    | Construct _ ->
      begin match
        reinstantiate_constructor_term ctx env ty path term expected
      with
      | Some _ as value -> value
      | None -> symbolic_path ctx env ty path
      end
    | _ -> symbolic_path ctx env ty path
    end
  | _ -> value

let rec resolve_module_alias ctx = function
  | Path.Pdot (prefix, name) -> (
    match Path.Map.find_opt prefix ctx.module_aliases with
    | Some target -> Some (Path.Pdot (target, name))
    | None ->
      Option.map
        (fun prefix -> Path.Pdot (prefix, name))
        (resolve_module_alias ctx prefix))
  | _ -> None

let rec lookup ctx s env ty path =
  let path = Env.normalize_value_path None env path in
  match resolve_module_alias ctx path with
  | Some path -> lookup ctx s env ty path
  | None -> lookup_normalized ctx s env ty path

and lookup_normalized ctx s env ty path =
  match sort ctx.encoding env ty with
  | Some set_sort
    when is_set_sort ctx.encoding set_sort && is_set_empty env path ->
    scalar_value (set_empty ctx set_sort)
  | Some map_sort
    when is_map_sort ctx.encoding map_sort && is_map_empty env path ->
    scalar_value (map_empty ctx map_sort)
  | _ -> (
    match value_constant ctx.encoding env ty path with
    | Some value -> scalar_value value
    | None -> (
      match Path.Map.find_opt path s.values with
      | Some value -> instantiate_path ctx env ty path value
      | None -> (
        match Path.Map.find_opt path ctx.argument_values with
        | Some value -> instantiate_path ctx env ty path value
        | None -> (
          match Path.Map.find_opt path ctx.free with
          | Some value -> instantiate_path ctx env ty path value
          | None ->
            let value =
              fresh ?primitive:(primitive env path) ctx env ty (Path.name path)
            in
            ctx.free <- Path.Map.add path value ctx.free;
            value))))

let iarray_call ctx value =
  match scalar value with
  | None -> None
  | Some value ->
    let sort = term_sort value in
    if is_iarray_sort ctx.encoding sort then Some (sort, value) else None

let vox_sequence_length ctx values =
  Call
    (intern_function ctx "Vox_sequence.length" [term_sort values] Int, [values])

let first_argument_type env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tarrow (_, argument, _, _) -> Some (Btype.tpoly_get_mono argument)
  | _ -> None

let borrow_projection ctx name result handle =
  Call (intern_function ctx name [term_sort handle] result, [handle])

let borrow_extent ctx handle =
  borrow_projection ctx "Borrow.extent" Int63 handle

let borrow_model_sort ctx env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (_, [_; model], _) -> sort ctx.encoding env model
  | Tconstr (_, [element], _) ->
    sort ctx.encoding env
      (Btype.newgenty (Tconstr (Predef.path_list, [element], ref Mnil)))
  | _ -> None

let borrow_projections =
  [ "caml_borrow_contents";
    "caml_borrow_current";
    "caml_borrow_final";
    "caml_borrow_frame_final";
    "caml_borrow_frame_left";
    "caml_borrow_frame_right" ]

let normal_borrow_projection ctx name args value s =
  match args, scalar value with
  | [handle], Some model
    when List.mem name
           [ "caml_borrow_contents";
             "caml_borrow_current";
             "caml_borrow_final";
             "caml_borrow_frame_final" ] ->
    begin match scalar handle with
    | None -> s
    | Some handle ->
      let extent = borrow_extent ctx handle in
      let size = App (Int_of_int63, [extent]) in
      fact
        (fact s "borrow extent" (both Le (Integer 0L) extent))
        "borrow model length"
        (if is_iarray_sort ctx.encoding (term_sort model)
         then both Eq (iarray_length ctx (term_sort model) model) extent
         else both Eq (vox_sequence_length ctx model) size)
    end
  | _ -> s

let normal_borrow_length ctx args value s =
  match args, scalar value with
  | [handle], Some length ->
    begin match scalar handle with
    | Some handle ->
      fact
        (fact s "borrow length" (both Eq length (borrow_extent ctx handle)))
        "borrow extent"
        (both Le (Integer 0L) length)
    | None -> s
    end
  | _ -> s

let tuple_fields ctx env ty value =
  match data_of_type ctx env ty, scalar value with
  | ( Some { kind = Tuple_data constructor | Record_data constructor; _ },
      Some value ) ->
    Some
      (List.mapi
         (fun i _ -> select ctx constructor i value)
         (Constructor.fields constructor))
  | _ -> None

let normal_borrow_transition ctx env fn_type result_type name args value s =
  match first_argument_type env fn_type, args with
  | Some receiver_type, receiver :: _ ->
    begin match borrow_model_sort ctx env receiver_type, scalar receiver with
    | Some model_sort, Some receiver ->
      let project name x = borrow_projection ctx name model_sort x in
      let current = project "caml_borrow_current" in
      let final = project "caml_borrow_final" in
      let contents = project "caml_borrow_contents" in
      let frame_final = project "caml_borrow_frame_final" in
      let frame_left = project "caml_borrow_frame_left" in
      let frame_right = project "caml_borrow_frame_right" in
      let same_size x y = both Eq (borrow_extent ctx x) (borrow_extent ctx y) in
      let facts =
        match name, scalar value, tuple_fields ctx env result_type value with
        | "caml_borrow_open", _, Some [frame; loan] ->
          [ both Eq (current loan) (contents receiver);
            both Eq (final loan) (frame_final frame);
            same_size loan receiver;
            same_size frame receiver ]
        | "caml_borrow_restore", Some owner, _ ->
          [ both Eq (contents owner) (frame_final receiver);
            same_size owner receiver ]
        | "caml_borrow_split", _, Some [frame; left; right] ->
          begin match args with
          | [_; index] ->
            begin match scalar index with
            | Some index ->
              [ both Eq (frame_final frame) (final receiver);
                both Eq (final left) (frame_left frame);
                both Eq (final right) (frame_right frame);
                same_size frame receiver;
                both Eq (borrow_extent ctx left) index;
                both Eq (borrow_extent ctx right)
                  (App (Sub, [borrow_extent ctx receiver; index])) ]
            | None -> []
            end
          | _ -> []
          end
        | "caml_borrow_recombine", Some loan, _ ->
          [both Eq (final loan) (frame_final receiver); same_size loan receiver]
        | "caml_borrow_finish", _, _ ->
          [both Eq (final receiver) (current receiver)]
        | "caml_borrow_transfer", Some loan, _ ->
          [ both Eq (current loan) (current receiver);
            both Eq (final loan) (final receiver);
            same_size loan receiver ]
        | _ -> []
      in
      List.fold_left (fun s fact_ -> fact s "borrow transition" fact_) s facts
    | _ -> s
    end
  | _ -> s

let normal_vox_sequence_length ctx env function_type args s =
  match first_argument_type env function_type, args with
  | Some ty, [values] ->
    begin match data_of_type ctx env ty, scalar values with
    | Some { kind = Variant_data constructors; _ }, Some values ->
      begin match
        List.assoc_opt "[]" constructors, List.assoc_opt "::" constructors
      with
      | Some nil, Some cons when List.length (Constructor.fields cons) = 2 ->
        let length = vox_sequence_length ctx values in
        let tail = select ctx cons 1 values in
        let equation =
          both Eq length
            (App
               ( Ite,
                 [ Is (nil, values);
                   Big_integer "0";
                   App (Int_add, [Big_integer "1"; vox_sequence_length ctx tail])
                 ] ))
        in
        fact
          (fact s "sequence length" equation)
          "sequence length nonnegative"
          (App (Int_le, [Big_integer "0"; length]))
      | _ -> s
      end
    | _ -> s
    end
  | _ -> s

let pref_location ctx heap pointer =
  comparison_class ctx ctx.map_class_sorts "Pref.location" (term_sort heap)
    pointer

let pref_observe ctx budget fn heap key =
  (* Each call records its own proof-step provenance; the cache also preserves
     the remaining expansion budget. *)
  let expanded = Hashtbl.create 16 in
  let rec observe budget fn heap key =
    let call = Call (fn, [heap; key]) in
    if budget = 0 || Hashtbl.mem expanded (budget, call)
    then call
    else begin
      Hashtbl.add expanded (budget, call) ();
      let value =
        match expose_head ctx heap with
        | App (Ite, [condition; left; right])
          when Hashtbl.mem ctx.finite_maps (term_sort heap) ->
          App
            ( Ite,
              [ condition;
                observe (budget - 1) fn left key;
                observe (budget - 1) fn right key ] )
        | Call (update, [source; changed; value])
          when Hashtbl.find_opt ctx.pref_constructors update = Some `Put ->
          let old = observe (budget - 1) fn source key in
          let changed_value =
            match Hashtbl.find_opt ctx.pref_observers fn with
            | Some None -> Boolean true
            | Some (Some (some, _)) ->
              begin match Constructor.fields some with
              | [(_, sort)] when sort = term_sort value ->
                Construct (some, [value])
              | _ -> call
              end
            | None -> call
          in
          App (Ite, [both Eq key changed; changed_value; old])
        | Call (update, [source; changed])
          when Hashtbl.find_opt ctx.pref_constructors update = Some `Remove ->
          let old = observe (budget - 1) fn source key in
          let removed =
            match Hashtbl.find_opt ctx.pref_observers fn with
            | Some None -> Boolean false
            | Some (Some (_, none)) -> Construct (none, [])
            | None -> call
          in
          App (Ite, [both Eq key changed; removed; old])
        | Call (op, [left; right])
          when List.mem
                 (Hashtbl.find_opt ctx.pref_constructors op)
                 [Some `Union; Some `Restrict; Some `Exclude] ->
          let mem heap =
            let mem =
              intern_function ctx "Pref.mem"
                [term_sort heap; term_sort key]
                Bool
            in
            Hashtbl.replace ctx.pref_observers mem None;
            observe (budget - 1) mem heap key
          in
          let empty =
            match Hashtbl.find_opt ctx.pref_observers fn with
            | Some None -> Boolean false
            | Some (Some (_, none)) -> Construct (none, [])
            | None -> call
          in
          let left_value = observe (budget - 1) fn left key in
          begin match Hashtbl.find_opt ctx.pref_constructors op with
          | Some `Union ->
            App (Ite, [mem left; left_value; observe (budget - 1) fn right key])
          | Some `Restrict -> App (Ite, [mem right; left_value; empty])
          | Some `Exclude -> App (Ite, [mem right; empty; left_value])
          | _ -> call
          end
        | Call (empty, [])
          when Hashtbl.find_opt ctx.pref_constructors empty = Some `Empty ->
          begin match Hashtbl.find_opt ctx.pref_observers fn with
          | Some None -> Boolean false
          | Some (Some (_, none)) -> Construct (none, [])
          | None -> call
          end
        | _ -> call
      in
      observe_iarray ctx call value
    end
  in
  observe budget fn heap key

let rec pref_disjoint ctx budget left right =
  let fn =
    intern_function ctx "Pref.disjoint" [term_sort left; term_sort right] Bool
  in
  let call = Call (fn, [left; right]) in
  let expand heap other =
    match expose_head ctx heap with
    | Call (fn, []) when Hashtbl.find_opt ctx.pref_constructors fn = Some `Empty
      ->
      Some (Boolean true)
    | Call (fn, [source; key; _])
      when Hashtbl.find_opt ctx.pref_constructors fn = Some `Put ->
      let mem =
        intern_function ctx "Pref.mem" [term_sort other; term_sort key] Bool
      in
      Hashtbl.replace ctx.pref_observers mem None;
      Some
        (both And
           (App (Not, [pref_observe ctx budget mem other key]))
           (pref_disjoint ctx (budget - 1) source other))
    | Call (fn, [a; b])
      when Hashtbl.find_opt ctx.pref_constructors fn = Some `Union ->
      Some
        (both And
           (pref_disjoint ctx (budget - 1) a other)
           (pref_disjoint ctx (budget - 1) b other))
    | _ -> None
  in
  if budget = 0
  then call
  else
    let value =
      match expand left right with
      | Some value -> value
      | None -> Option.value (expand right left) ~default:call
    in
    observe_iarray ctx call value

(* Extensionality of heaps, instantiated at one location: if [left] and [right]
   agree at [Pref.diff left right], they are equal. In the model, a heap is a
   partial map from locations to payloads, [at] is lookup and [mem] is
   membership in the domain, and [diff] picks a location where two different
   heaps differ. This is Z3's array extensionality lemma. *)
let pref_extensionality ctx env heap_type left right =
  let heap_sort = term_sort left in
  let payload =
    match get_desc (Ctype.expand_head env heap_type) with
    | Tconstr (_, [payload], _) -> Some payload
    | _ -> None
  in
  let option_type = Option.map Predef.type_option payload in
  match
    ( Option.bind option_type (sort ctx.encoding env),
      Option.bind option_type (data_of_type ctx env) )
  with
  | Some option_sort, Some data
    when Hashtbl.mem ctx.pref_heaps heap_sort && term_sort right = heap_sort
    -> (
    let key_sort =
      match Hashtbl.find_opt ctx.map_class_sorts heap_sort with
      | Some sort -> sort
      | None ->
        let sort = fresh_opaque_sort ctx.encoding in
        Hashtbl.add ctx.map_class_sorts heap_sort sort;
        sort
    in
    match data_constructor data "Some", data_constructor data "None" with
    | Some some, Some none ->
      let label name =
        (if Hashtbl.mem ctx.finite_maps heap_sort
         then "Logical_map."
         else "Pref.")
        ^ name
      in
      let diff =
        intern_function ctx (label "diff") [heap_sort; heap_sort] key_sort
      in
      let key =
        match Hashtbl.find_opt ctx.finite_maps heap_sort with
        | None -> Call (diff, [left; right])
        | Some map ->
          let diff =
            intern_function ctx "Logical_map.difference_key"
              [heap_sort; heap_sort] map.key_sort
          in
          comparison_class ctx ctx.map_class_sorts "Logical_map.key" heap_sort
            (Call (diff, [left; right]))
      in
      let at =
        intern_function ctx (label "at") [heap_sort; key_sort] option_sort
      in
      Hashtbl.replace ctx.pref_observers at (Some (some, none));
      let mem = intern_function ctx (label "mem") [heap_sort; key_sort] Bool in
      Hashtbl.replace ctx.pref_observers mem None;
      let observe fn heap = pref_observe ctx 128 fn heap key in
      let domain heap =
        both Eq (observe mem heap) (Is (some, observe at heap))
      in
      Some
        (both And
           (both And (domain left) (domain right))
           (both Implies
              (both Eq (observe at left) (observe at right))
              (both Eq left right)))
    | _ -> None)
  | _ -> None

let logical_map_cardinal ctx budget map =
  let expanded = Hashtbl.create 16 in
  let rec cardinal budget map =
    let fn = intern_function ctx "Logical_map.cardinal" [term_sort map] Int in
    let call = Call (fn, [map]) in
    if budget = 0 || Hashtbl.mem expanded (budget, call)
    then call
    else begin
      Hashtbl.add expanded (budget, call) ();
      let value =
        match expose_head ctx map with
        | Call (op, [])
          when Hashtbl.find_opt ctx.pref_constructors op = Some `Empty ->
          Big_integer "0"
        | Call (op, source :: key :: _)
          when List.mem
                 (Hashtbl.find_opt ctx.pref_constructors op)
                 [Some `Put; Some `Remove] ->
          let info = Hashtbl.find ctx.finite_maps (term_sort map) in
          let present =
            Is (info.some, pref_observe ctx 128 info.at source key)
          in
          let size = cardinal (budget - 1) source in
          let adding = Hashtbl.find ctx.pref_constructors op = `Put in
          let changed =
            App ((if adding then Int_add else Int_sub), [size; Big_integer "1"])
          in
          App
            ( Ite,
              [ present;
                (if adding then size else changed);
                (if adding then changed else size) ] )
        | App (Ite, [condition; left; right]) ->
          App
            ( Ite,
              [ condition;
                cardinal (budget - 1) left;
                cardinal (budget - 1) right ] )
        | _ -> call
      in
      observe_iarray ctx call value
    end
  in
  cardinal budget map

let logical_map_key ctx map key =
  comparison_class ctx ctx.map_class_sorts "Logical_map.key" (term_sort map) key

let operation ctx env function_type result_type name args =
  match name, args with
  | "caml_logical_map_empty", [_] ->
    Option.bind (sort ctx.encoding env result_type) (fun map_sort ->
        if not (Hashtbl.mem ctx.finite_maps map_sort)
        then None
        else
          let fn = intern_function ctx "Logical_map.empty" [] map_sort in
          Hashtbl.replace ctx.pref_constructors fn `Empty;
          scalar_value (Call (fn, [])))
  | "caml_logical_map_add", [key; value; map] ->
    begin match scalar key, scalar value, scalar map with
    | Some key, Some value, Some map
      when Hashtbl.mem ctx.finite_maps (term_sort map) ->
      let terms = [map; logical_map_key ctx map key; value] in
      let fn =
        intern_function ctx "Logical_map.add" (List.map term_sort terms)
          (term_sort map)
      in
      Hashtbl.replace ctx.pref_constructors fn `Put;
      scalar_value (Call (fn, terms))
    | _ -> None
    end
  | "caml_logical_map_remove", [key; map] ->
    begin match scalar key, scalar map with
    | Some key, Some map when Hashtbl.mem ctx.finite_maps (term_sort map) ->
      let terms = [map; logical_map_key ctx map key] in
      let fn =
        intern_function ctx "Logical_map.remove" (List.map term_sort terms)
          (term_sort map)
      in
      Hashtbl.replace ctx.pref_constructors fn `Remove;
      scalar_value (Call (fn, terms))
    | _ -> None
    end
  | (("caml_logical_map_find_opt" | "caml_logical_map_mem") as name), [key; map]
    ->
    begin match scalar key, scalar map with
    | Some key, Some map ->
      Option.bind
        (Hashtbl.find_opt ctx.finite_maps (term_sort map))
        (fun info ->
          let key = logical_map_key ctx map key in
          let value = pref_observe ctx 128 info.at map key in
          scalar_value
            (if name = "caml_logical_map_mem"
             then Is (info.some, value)
             else value))
    | _ -> None
    end
  | "caml_logical_map_cardinal", [map] ->
    Option.bind (scalar map) (fun map ->
        if Hashtbl.mem ctx.finite_maps (term_sort map)
        then scalar_value (logical_map_cardinal ctx 128 map)
        else None)
  | "caml_pref_heap_disjoint", [left; right] ->
    begin match scalar left, scalar right with
    | Some left, Some right -> scalar_value (pref_disjoint ctx 64 left right)
    | _ -> None
    end
  | "caml_pref_heap_same_domain", [left; right] ->
    begin match scalar left, scalar right with
    | Some left, Some right ->
      scalar_value
        (Call
           ( intern_function ctx "Pref.same_domain"
               [term_sort left; term_sort right]
               Bool,
             [left; right] ))
    | _ -> None
    end
  | "caml_pref_own_bytecode", [token] ->
    begin match scalar token, sort ctx.encoding env result_type with
    | Some token, Some heap_sort ->
      Hashtbl.replace ctx.pref_heaps heap_sort ();
      scalar_value
        (borrow_projection ctx "Pref.own" heap_sort (expose_head ctx token))
    | _ -> None
    end
  | "caml_pref_heap_empty", [_] ->
    Option.bind (sort ctx.encoding env result_type) (fun heap_sort ->
        Hashtbl.replace ctx.pref_heaps heap_sort ();
        let fn = intern_function ctx "Pref.empty" [] heap_sort in
        Hashtbl.replace ctx.pref_constructors fn `Empty;
        scalar_value (Call (fn, [])))
  | ( (( "caml_pref_heap_union" | "caml_pref_heap_restrict"
       | "caml_pref_heap_exclude" ) as name),
      [left; right] ) ->
    begin match scalar left, scalar right with
    | Some left, Some right ->
      let kind, label =
        match name with
        | "caml_pref_heap_union" -> `Union, "Pref.union"
        | "caml_pref_heap_restrict" -> `Restrict, "Pref.restrict"
        | _ -> `Exclude, "Pref.exclude"
      in
      let fn =
        intern_function ctx label
          [term_sort left; term_sort right]
          (term_sort left)
      in
      Hashtbl.replace ctx.pref_heaps (term_sort left) ();
      Hashtbl.replace ctx.pref_constructors fn kind;
      scalar_value (Call (fn, [left; right]))
    | _ -> None
    end
  | "caml_pref_heap_put", [heap; pointer; value] ->
    begin match scalar heap, scalar pointer, scalar value with
    | Some heap, Some pointer, Some value ->
      let key = pref_location ctx heap pointer in
      let fn =
        intern_function ctx "Pref.put"
          [term_sort heap; term_sort key; term_sort value]
          (term_sort heap)
      in
      Hashtbl.replace ctx.pref_heaps (term_sort heap) ();
      Hashtbl.replace ctx.pref_constructors fn `Put;
      scalar_value (Call (fn, [heap; key; value]))
    | _ -> None
    end
  | (("caml_pref_heap_mem" | "caml_pref_heap_at") as name), [heap; pointer] ->
    begin match
      scalar heap, scalar pointer, sort ctx.encoding env result_type
    with
    | Some heap, Some pointer, Some result ->
      Hashtbl.replace ctx.pref_heaps (term_sort heap) ();
      let key = pref_location ctx heap pointer in
      let label =
        if name = "caml_pref_heap_mem" then "Pref.mem" else "Pref.at"
      in
      let fn =
        intern_function ctx label [term_sort heap; term_sort key] result
      in
      let constructors =
        if name = "caml_pref_heap_mem"
        then Some None
        else
          Option.bind (data_of_type ctx env result_type) (fun data ->
              match
                data_constructor data "Some", data_constructor data "None"
              with
              | Some some, Some none -> Some (Some (some, none))
              | _ -> None)
      in
      Option.bind constructors (fun constructors ->
          Hashtbl.replace ctx.pref_observers fn constructors;
          scalar_value (pref_observe ctx 128 fn heap key))
    | _ -> None
    end
  | "caml_bigint_to_int_opt", [value] -> (
    match scalar value with
    | Some value when term_sort value = Int -> (
      let in_range =
        App
          ( And,
            [ App (Int_le, [Big_integer "-4611686018427387904"; value]);
              App (Int_le, [value; Big_integer "4611686018427387903"]) ] )
      in
      match
        ( construct ctx env result_type "Some"
            [scalar_value (App (Int63_of_int, [value]))],
          construct ctx env result_type "None" [] )
      with
      | Some (Scalar some), Some (Scalar none) ->
        scalar_value (App (Ite, [in_range; some; none]))
      | _ -> None)
    | _ -> None)
  | "caml_vox_sequence_length", [values] ->
    Option.bind (scalar values) (fun values ->
        scalar_value (vox_sequence_length ctx values))
  | name, [handle] when List.mem name borrow_projections ->
    begin match scalar handle, sort ctx.encoding env result_type with
    | Some handle, Some result ->
      scalar_value (borrow_projection ctx name result handle)
    | _ -> None
    end
  | "%set_singleton", [element] ->
    begin match scalar element, sort ctx.encoding env result_type with
    | Some element, Some set_sort when is_set_sort ctx.encoding set_sort ->
      scalar_value
        (set_constructor ctx Set_singleton "Set.singleton"
           [term_sort element]
           set_sort [element])
    | _ -> None
    end
  | (("%set_add" | "%set_remove") as name), [element; set] ->
    begin match scalar element, scalar set with
    | Some element, Some set when is_set_sort ctx.encoding (term_sort set) ->
      let origin, label =
        if name = "%set_add"
        then Set_add, "Set.add"
        else Set_remove, "Set.remove"
      in
      scalar_value
        (set_constructor ctx origin label
           [term_sort element; term_sort set]
           (term_sort set) [element; set])
    | _ -> None
    end
  | (("%set_union" | "%set_inter" | "%set_diff") as name), [left; right] ->
    begin match scalar left, scalar right with
    | Some left, Some right
      when term_sort left = term_sort right
           && is_set_sort ctx.encoding (term_sort left) ->
      let origin, label =
        match name with
        | "%set_union" -> Set_union, "Set.union"
        | "%set_inter" -> Set_inter, "Set.inter"
        | _ -> Set_diff, "Set.diff"
      in
      scalar_value
        (set_constructor ctx origin label
           [term_sort left; term_sort right]
           (term_sort left) [left; right])
    | _ -> None
    end
  | "%set_mem", [element; set] ->
    begin match scalar element, scalar set with
    | Some element, Some set when is_set_sort ctx.encoding (term_sort set) ->
      scalar_value (set_mem ctx (term_sort set) element set)
    | _ -> None
    end
  | "%set_find", [element; set] | "%set_refined_find", [set; element] ->
    begin match scalar element, scalar set with
    | Some element, Some set when is_set_sort ctx.encoding (term_sort set) ->
      scalar_value (set_find ctx (term_sort set) element set)
    | _ -> None
    end
  | "%map_empty", [_unit] ->
    begin match sort ctx.encoding env result_type with
    | Some map_sort when is_map_sort ctx.encoding map_sort ->
      scalar_value (map_empty ctx map_sort)
    | _ -> None
    end
  | "%map_singleton", [key; data] ->
    begin match scalar key, scalar data, sort ctx.encoding env result_type with
    | Some key, Some data, Some map_sort when is_map_sort ctx.encoding map_sort
      ->
      scalar_value
        (map_constructor ctx Map_singleton "Map.singleton"
           [term_sort key; term_sort data]
           map_sort [key; data])
    | _ -> None
    end
  | "%map_add", [key; data; map] ->
    begin match scalar key, scalar data, scalar map with
    | Some key, Some data, Some map
      when is_map_sort ctx.encoding (term_sort map) ->
      scalar_value
        (map_constructor ctx Map_add "Map.add"
           [term_sort key; term_sort data; term_sort map]
           (term_sort map) [key; data; map])
    | _ -> None
    end
  | "%map_remove", [key; map] ->
    begin match scalar key, scalar map with
    | Some key, Some map when is_map_sort ctx.encoding (term_sort map) ->
      scalar_value
        (map_constructor ctx Map_remove "Map.remove"
           [term_sort key; term_sort map]
           (term_sort map) [key; map])
    | _ -> None
    end
  | "%map_mem", [key; map] ->
    begin match scalar key, scalar map with
    | Some key, Some map when is_map_sort ctx.encoding (term_sort map) ->
      scalar_value (map_mem ctx (term_sort map) key map)
    | _ -> None
    end
  | "%map_find", [key; map] | "%map_refined_find", [map; key] ->
    begin match scalar key, scalar map, sort ctx.encoding env result_type with
    | Some key, Some map, Some value_sort
      when is_map_sort ctx.encoding (term_sort map) ->
      scalar_value (map_find ctx (term_sort map) value_sort key map)
    | _ -> None
    end
  | "caml_array_append", [left; right] ->
    begin match
      ( iarray_call ctx left,
        iarray_call ctx right,
        iarray ctx.encoding env result_type )
    with
    | Some (sort, left), Some (_, right), Some _ ->
      iarray_copy ctx sort (Iarray_append (left, right))
    | _ -> None
    end
  | ("%iarray_sub" | "caml_vox_iarray_sub"), [source; position; length] ->
    begin match
      ( iarray_call ctx source,
        scalar position,
        scalar length,
        iarray ctx.encoding env result_type )
    with
    | Some (sort, source), Some position, Some length, Some _
      when term_sort position = Int63 && term_sort length = Int63 ->
      iarray_copy ctx sort (Iarray_sub (source, position, length))
    | _ -> None
    end
  | "caml_vox_iarray_set", [source; index; value] ->
    begin match iarray_call ctx source, scalar index, scalar value with
    | Some (sort, source), Some index, Some value ->
      iarray_copy ctx sort (Iarray_set (source, index, value))
    | _ -> None
    end
  | "%array_length", [array] ->
    begin match iarray_call ctx array with
    | Some (iarray_sort, array) ->
      scalar_value (iarray_length ctx iarray_sort array)
    | None -> None
    end
  | "%array_safe_get", [array; index] ->
    begin match
      iarray_call ctx array, scalar index, sort ctx.encoding env result_type
    with
    | Some (iarray_sort, array), Some index, Some element_sort
      when term_sort index = Int63 ->
      scalar_value (iarray_get ctx iarray_sort element_sort array index)
    | _ -> None
    end
  | _ ->
    scalar_option
      (Vox_encoding.operation ctx.encoding env ~function_type ~result_type name
         (List.map scalar args))

let normal_iarray_copy ctx value s =
  match scalar value with
  | None -> s
  | Some array -> (
    let length source = iarray_length ctx (term_sort array) source in
    match iarray_origin ctx array with
    | Some (Iarray_append (left, right)) ->
      let left = length left and right = length right in
      let sum = App (Add, [left; right]) in
      List.fold_left
        (fun s term -> fact s "iarray copy" term)
        s
        [ both Le (Integer 0L) left;
          both Le (Integer 0L) right;
          both Le left sum;
          both Le right sum ]
    | Some (Iarray_sub (source, position, size)) ->
      let length = length source in
      List.fold_left
        (fun s term -> fact s "iarray copy" term)
        s
        [ both Le (Integer 0L) position;
          both Le (Integer 0L) size;
          both Le position length;
          both Le size (App (Sub, [length; position])) ]
    | Some (Iarray_literal _ | Iarray_set _) | None -> s)

let normal_iarray_length ctx args s =
  match args with
  | [array] ->
    begin match iarray_call ctx array with
    | Some (iarray_sort, array) ->
      fact s "iarray length"
        (both Le (Integer 0L) (iarray_length ctx iarray_sort array))
    | None -> s
    end
  | _ -> s

let normal_iarray_get ctx args s =
  match args with
  | [array; index] ->
    begin match iarray_call ctx array, scalar index with
    | Some (iarray_sort, array), Some index when term_sort index = Int63 ->
      let length = iarray_length ctx iarray_sort array in
      fact s "normal return"
        (both And (both Le (Integer 0L) index) (both Lt index length))
    | _ -> s
    end
  | _ -> s

let normal_set_find ctx name args value s =
  let element, set =
    match name, args with
    | "%set_find", [element; set] -> element, set
    | "%set_refined_find", [set; element] -> element, set
    | _ -> None, None
  in
  match scalar element, scalar set, scalar value with
  | Some element, Some set, Some result
    when is_set_sort ctx.encoding (term_sort set) ->
    let set_sort = term_sort set in
    fact
      (fact s "normal return" (set_mem ctx set_sort element set))
      "set representative"
      (both And
         (set_mem ctx set_sort result set)
         (set_same_element ctx set_sort result element))
  | _ -> s

let normal_map_find ctx name args s =
  let key, map =
    match name, args with
    | "%map_find", [key; map] -> key, map
    | "%map_refined_find", [map; key] -> key, map
    | _ -> None, None
  in
  match scalar key, scalar map with
  | Some key, Some map when is_map_sort ctx.encoding (term_sort map) ->
    fact s "normal return" (map_mem ctx (term_sort map) key map)
  | _ -> s

let rec function_call ctx env ty fn args =
  match fn with
  | Some (Function { application = Some (original, fn, prefix); _ }) ->
    function_call ctx env original (Some (Function fn)) (prefix @ args)
  | Some (Function { choice = Some (condition, a, b); _ }) ->
    join_value condition
      (function_call ctx env ty (Some (Function a)) args)
      (function_call ctx env ty (Some (Function b)) args)
  | Some (Function { lambda = { contents = Some lambda }; _ })
    when List.length lambda.parameters = List.length args ->
    begin match
      ( Misc.Stdlib.List.map_option scalar args,
        signature ctx.encoding env ty (List.length args) )
    with
    | Some args, Some (_, result)
      when List.map term_sort args = List.map Symbol.sort lambda.parameters
           && term_sort lambda.body = result ->
      scalar_value (instantiate_lambda ctx lambda args)
    | _ ->
      let fn =
        Option.map
          (function
            | Function fn -> Function { fn with lambda = ref None }
            | (Scalar _ | Record _) as value -> value)
          fn
      in
      function_call ctx env ty fn args
    end
  | Some (Function fn)
    when let rec remaining ty n =
           match get_desc (Ctype.expand_head env ty), n with
           | Tarrow _, 0 -> true
           | Tarrow (_, _, ret, _), n when n > 0 -> remaining ret (n - 1)
           | _ -> false
         in
         remaining ty (List.length args) ->
    Some
      (Function
         { fn with
           instances = ref [];
           specializations = ref [];
           choice = None;
           lambda = ref None;
           application = Some (ty, fn, args)
         })
  | _ -> (
    let rec lift fn =
      match fn.application with
      | Some (_, original, prefix) ->
        begin match Misc.Stdlib.List.map_option scalar prefix with
        | Some prefix ->
          let original, captured = lift original in
          original, captured @ prefix
        | None -> fn, []
        end
      | None -> fn, []
    in
    let rec arguments position ty args =
      match args with
      | [] ->
        Option.map (fun result -> [], [], result) (sort ctx.encoding env ty)
      | value :: rest -> (
        match get_desc (Ctype.expand_head env ty) with
        | Tarrow ((Nolabel, _, _, _), arg, ret, _) ->
          Option.bind
            (arguments (position + 1) ret rest)
            (fun (terms, functions, result) ->
              match value, sort ctx.encoding env arg with
              | Some (Scalar term), Some expected when term_sort term = expected
                ->
                Some (term :: terms, functions, result)
              | Some (Function function_), None ->
                let arg = Btype.tpoly_get_mono arg in
                let rec arity ty =
                  match get_desc (Ctype.expand_head env ty) with
                  | Tarrow (_, _, ret, _) -> 1 + arity ret
                  | _ -> 0
                in
                Option.map
                  (fun (domain, range) ->
                    let function_, captured = lift function_ in
                    ( captured @ terms,
                      ( position,
                        function_,
                        domain,
                        range,
                        List.map term_sort captured )
                      :: functions,
                      result ))
                  (signature ctx.encoding env arg (arity arg))
              | _ -> None)
        | _ -> None)
    in
    match fn, arguments 0 ty args with
    | Some (Function fn), Some (args, higher, result) ->
      let arguments = List.map term_sort args in
      List.iter (register_sort ctx) (result :: arguments);
      let same_functions left right =
        List.length left = List.length right
        && List.for_all2
             (fun (i, a, args, result, captures)
                  (j, b, args', result', captures') ->
               i = j && a.instances == b.instances && args = args'
               && result = result' && captures = captures')
             left right
      in
      let matches f =
        Function.arguments f = arguments && Function.result f = result
      in
      let existing =
        if higher = []
        then List.find_opt matches !(fn.instances)
        else
          Option.map snd
            (List.find_opt
               (fun (keys, f) -> same_functions keys higher && matches f)
               !(fn.specializations))
      in
      let f =
        match existing with
        | Some f -> f
        | None ->
          let f = Function.create ~label:fn.label ~arguments ~result in
          if higher = []
          then fn.instances := f :: !(fn.instances)
          else fn.specializations := (higher, f) :: !(fn.specializations);
          ctx.functions <- f :: ctx.functions;
          f
      in
      scalar_value (Call (f, args))
    | _ -> None)

let apply_function ctx env fn_type result_type prim fn args ~total =
  let value =
    match prim with
    | Some (name, arity) when arity = List.length args ->
      operation ctx env fn_type result_type name args
    | _ -> None
  in
  match value with
  | Some _ -> value
  | None when total ->
    (* Trusted total declarations must respect the scalar encoding: equal bigint
       numbers are indistinguishable, regardless of allocation identity. *)
    function_call ctx env fn_type fn args
  | None -> (
    (* A partially applied shift is kept, so that its count is checked where the
       shift is performed. *)
    match prim, fn with
    | ( Some (("%lslint" | "%lsrint" | "%asrint"), arity),
        Some (Function { application = None; _ }) )
      when List.length args < arity ->
      function_call ctx env fn_type fn args
    | _ -> None)

let rec register_logical_maps ctx env s ty =
  let ty = Ctype.expand_head env ty in
  match get_desc ty with
  | Tarrow (_, arg, result, _) ->
    register_logical_maps ctx env s arg;
    register_logical_maps ctx env s result
  | Tpoly (ty, _) | Trefine { ref_payload = ty; _ } ->
    register_logical_maps ctx env s ty
  | Tconstr (_, [payload], _) ->
    let model =
      Option.bind (Vox_encoding.logical_map_key env ty) (fun path ->
          Option.map (fun map_sort -> path, map_sort) (sort ctx.encoding env ty))
    in
    begin match model with
    | Some (path, map_sort) when not (Hashtbl.mem ctx.finite_maps map_sort) ->
      let eq_type =
        (Subst.Lazy.force_value_description (Env.find_value path env)).val_type
      in
      let eq = lookup ctx s env eq_type path in
      let option_type = Predef.type_option payload in
      begin match
        ( first_argument_type env eq_type,
          data_of_type ctx env option_type,
          sort ctx.encoding env option_type )
      with
      | Some key_type, Some data, Some option_sort ->
        begin match
          ( sort ctx.encoding env key_type,
            data_constructor data "Some",
            data_constructor data "None" )
        with
        | Some key_sort, Some some, Some none ->
          let key = fresh_symbol key_sort "map key" in
          let class_ =
            comparison_class ctx ctx.map_class_sorts "Logical_map.key" map_sort
              key
          in
          let at =
            intern_function ctx "Logical_map.at"
              [map_sort; term_sort class_]
              option_sort
          in
          let equal left right =
            scalar
              (apply_function ctx env eq_type Predef.type_bool
                 (primitive env path) eq
                 [scalar_value left; scalar_value right]
                 ~total:true)
          in
          Hashtbl.replace ctx.pref_observers at (Some (some, none));
          Hashtbl.replace ctx.pref_heaps map_sort ();
          Hashtbl.add ctx.finite_maps map_sort { key_sort; some; at; equal }
        | _ -> ()
        end
      | _ -> ()
      end
    | _ -> ()
    end
  | _ -> ()

(* OCaml leaves a shift by a count outside [0, 63] unspecified, and compiled
   code really differs: constant folding and the hardware give different
   results, and inlining decides which applies at each call. So every shift that
   code performs must have a count in range, whatever its declared type; only
   then is the value the encoding gives it the value the program computes. *)
let shift_count fn prim args =
  let is_shift = function
    | "%lslint" | "%lsrint" | "%asrint" -> true
    | _ -> false
  in
  let rec saturated fn args =
    match fn with
    | Some (Function { application = Some (_, original, prefix); _ }) ->
      saturated (Some (Function original)) (prefix @ args)
    | Some (Function { primitive = Some (name, 2); _ }) when is_shift name ->
      Some args
    | _ -> None
  in
  match prim, args with
  | Some (name, 2), [_; count] when is_shift name -> Some count
  | _ -> (
    match saturated fn args with Some [_; count] -> Some count | _ -> None)

let require_shift_count ctx s ~loc ~count_loc count =
  if s.dead || not ctx.verify_introductions
  then s
  else
    let goal =
      match scalar count with
      | Some n -> both And (both Le (Integer 0L) n) (both Le n (Integer 63L))
      | None -> Boolean false
    in
    let obligation =
      { loc = count_loc;
        origin = count_loc;
        goal;
        omitted_premises = s.omitted_premises;
        group = fresh_group ();
        note =
          Some
            (Location.msg ~loc
               "A shift count must be between 0 and 63: outside that range \
                OCaml leaves the result unspecified.");
        headline = None;
        context = []
      }
    in
    branch { s with code = Assert obligation :: s.code } goal

let stored_primitive syntax = function
  | Some (Function { primitive = Some _ as primitive; _ }) -> primitive
  | _ -> syntax

let string_literal ctx env text =
  match sort ctx.encoding env Predef.type_string with
  | None -> None
  | Some sort ->
    let fn = intern_function ctx ("string literal:" ^ text) [] sort in
    if not (Hashtbl.mem ctx.string_literals fn)
    then Hashtbl.add ctx.string_literals fn (Hashtbl.length ctx.string_literals);
    scalar_value (Call (fn, []))

let constant ctx env c =
  match c with
  | Const_string (text, _, _) -> string_literal ctx env text
  | _ -> scalar_option (Vox_encoding.constant c)

let rconstant ctx env c =
  match c.Parsetree.pconst_desc with
  | Parsetree.Pconst_string (text, _, _) -> string_literal ctx env text
  | _ -> scalar_option (Vox_encoding.rconstant c)

let constructor ctx env ty name =
  scalar_option (Vox_encoding.constructor ctx.encoding env ty name)

let rconstructor ctx env ty path =
  match scalar_option (Vox_encoding.rconstructor ctx.encoding env ty path) with
  | Some _ as value -> value
  | None ->
    begin match path_constructor_name path with
    | Some name ->
      begin match construct ctx env ty name [] with
      | Some _ as value -> value
      | None -> symbolic_path ctx env ty path
      end
    | None -> symbolic_path ctx env ty path
    end

let expression_constructor ctx env ty (c : Data_types.constructor_description) =
  match constructor ctx env ty c.cstr_name with
  | Some _ as value -> value
  | None ->
    begin match construct ctx env ty c.cstr_name [] with
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
    end

(* A definition marked [@def transparent] is unfolded at each full application
   of its name: the application assumes the refinement of the definition lemma
   [f_def] at the same arguments, as if the lemma were called there. The lemma
   must be total, so its refinement holds for all arguments that satisfy its
   parameters' refinements; those become premises of the assumed fact. *)
let transparent_definition env path =
  match Env.find_value path env with
  | description ->
    Builtin_attributes.is_transparent_definition
      description.Subst.Lazy.val_attributes
  | exception Not_found -> false

let refinement_free env ty =
  let visited = Hashtbl.create 8 in
  let rec visit ty =
    let ty = Ctype.expand_head env ty in
    if not (Hashtbl.mem visited (get_id ty))
    then begin
      Hashtbl.add visited (get_id ty) ();
      match get_desc ty with
      | Trefine _ -> raise Exit
      | _ -> Btype.iter_type_expr visit ty
    end
  in
  match visit ty with () -> true | exception Exit -> false

(* [scheme] with its type variables instantiated so that each pattern, a part of
   [scheme], matches its target. With [~refined:false], [None] when a variable
   would be instantiated with a type that has refinements. *)
let scheme_instance ?(refined = true) env scheme pairs =
  let variables = ref [] in
  let rec matching pattern target =
    match get_desc pattern, get_desc target with
    | Tvar _, _ when get_level pattern = Btype.generic_level ->
      if not (List.exists (fun (v, _) -> eq_type v pattern) !variables)
      then variables := (pattern, target) :: !variables
    | Trefine { ref_payload; _ }, _ | Tpoly (ref_payload, []), _ ->
      matching ref_payload target
    | _, (Trefine { ref_payload; _ } | Tpoly (ref_payload, [])) ->
      matching pattern ref_payload
    | Tconstr (p, ps, _), Tconstr (p', ts, _)
      when Path.same
             (Env.normalize_type_path None env p)
             (Env.normalize_type_path None env p')
           && List.compare_lengths ps ts = 0 ->
      List.iter2 matching ps ts
    | Tconstr _, _ | _, Tconstr _ ->
      let pattern' = Ctype.expand_head env pattern in
      let target' = Ctype.expand_head env target in
      if not (eq_type pattern pattern' && eq_type target target')
      then matching pattern' target'
    | Ttuple ps, Ttuple ts when List.compare_lengths ps ts = 0 ->
      List.iter2 (fun (_, p) (_, t) -> matching p t) ps ts
    | Tarrow (_, p, p', _), Tarrow (_, t, t', _) ->
      matching p t;
      matching p' t'
    | _ -> ()
  in
  List.iter (fun (pattern, target) -> matching pattern target) pairs;
  match List.split (List.rev !variables) with
  | [], _ -> Some scheme
  | _, targets
    when (not refined) && not (List.for_all (refinement_free env) targets) ->
    None
  | variables, targets -> (
    try Some (Ctype.apply env variables scheme targets)
    with Ctype.Cannot_apply -> None)

exception Unusable_lemma

let unfolding_equation env loc path fn_type arity =
  let fail () = raise Unusable_lemma in
  let lemma =
    match path with
    | Path.Pident id -> (
      match
        Env.find_value_by_name (Longident.Lident (Ident.name id ^ "_def")) env
      with
      | lemma, _ -> lemma
      | exception Not_found -> fail ())
    | Path.Pdot (parent, name) -> Path.Pdot (parent, name ^ "_def")
    | Path.Papply _ | Path.Pextra_ty _ -> fail ()
  in
  let _, description, (mode, _) =
    match Env.lookup_value_path ~use:false ~loc lemma env with
    | found -> found
    | exception Not_found -> fail ()
  in
  if not (logical_function_mode mode) then fail ();
  let rec parameters ty n =
    if n = 0
    then [], ty
    else
      match get_desc (Ctype.expand_head env ty) with
      | Tarrow ((Nolabel, _, _, Some binder), argument, result, _) ->
        let rest, result = parameters result (n - 1) in
        (binder, argument) :: rest, result
      | _ -> fail ()
  in
  let rec arguments ty n =
    if n = 0
    then []
    else
      match get_desc (Ctype.expand_head env ty) with
      | Tarrow (_, argument, result, _) -> argument :: arguments result (n - 1)
      | _ -> fail ()
  in
  (* Top-level parameter refinements become premises; deeper ones cannot. *)
  let rec refinements ty =
    match get_desc (Ctype.expand_head env ty) with
    | Tpoly (ty, []) -> refinements ty
    | Trefine r ->
      let rest, payload = refinements r.ref_payload in
      r :: rest, payload
    | _ -> [], ty
  in
  let scheme = description.val_type in
  let generic, _ = parameters scheme arity in
  if
    not
      (List.for_all
         (fun (_, ty) -> refinement_free env (snd (refinements ty)))
         generic)
  then fail ();
  (* Instantiate the lemma's type variables at the application's types. *)
  let ty =
    match
      scheme_instance env scheme
        (List.map2
           (fun (_, pattern) target -> pattern, target)
           generic (arguments fn_type arity))
    with
    | Some ty -> ty
    | None -> fail ()
  in
  let parameters, result = parameters ty arity in
  let same_path p =
    Path.same
      (Env.normalize_value_path None env p)
      (Env.normalize_value_path None env path)
  in
  let rec variable e =
    match e.rexp_desc with
    | Rexp_var id -> Some id
    | Rexp_refinement (_, e) | Rexp_ghost e -> variable e
    | _ -> None
  in
  match get_desc (Ctype.expand_head env result) with
  | Trefine
      ({ ref_pred =
           { rexp_desc =
               Rexp_logical_equal
                 ( { rexp_desc =
                       Rexp_apply ({ rexp_desc = Rexp_ident p; _ }, args);
                     _
                   },
                   _ );
             _
           };
         _
       } as refinement)
    when same_path p
         && List.compare_lengths args parameters = 0
         && List.for_all2
              (fun (label, arg) (binder, _) ->
                label = Asttypes.Nolabel
                &&
                match variable arg with
                | Some id -> Ident.same id binder
                | None -> false)
              args parameters ->
    ( List.map (fun (binder, ty) -> binder, fst (refinements ty)) parameters,
      refinement )
  | _ -> fail ()

(* A local definition is not unfolded while its own lemma is checked, or when
   its lemma is shadowed; a missing or unusable exported lemma is an error. *)
let transparent_equation env loc path fn_type arity =
  match unfolding_equation env loc path fn_type arity with
  | equation -> Some equation
  | exception Unusable_lemma -> (
    match path with
    | Path.Pident _ -> None
    | _ ->
      Location.raise_errorf ~loc
        "The transparent definition %s cannot be unfolded: %s_def must be a \
         total lemma stating its definition, with no refinement nested inside \
         a parameter type"
        (Path.name path) (Path.last path))

(* The type the verifier knows a predicate subexpression has, when it can tell
   without evaluating it: the declared type of a value in the environment, or of
   a field of a record. It does not depend on the refinements of the types
   recorded in the predicate, which predicate equality compares only by
   skeleton. *)
let rec known_type env e =
  match e.rexp_desc with
  | Rexp_ghost e | Rexp_refinement (_, e) -> known_type env e
  | Rexp_var id -> declared_type env (Path.Pident id) e.rexp_type
  | Rexp_ident path -> declared_type env path e.rexp_type
  | Rexp_field (record, _, name) | Rexp_unboxed_field (record, _, name) -> (
    match known_type env record with
    | Some ty -> field_type env ty record name
    | None ->
      let rec payload ty =
        match get_desc (Ctype.expand_head env ty) with
        | Trefine r -> payload r.ref_payload
        | _ -> ty
      in
      let ty = payload record.rexp_type in
      if refinement_free env ty then field_type env ty record name else None)
  | _ -> None

(* The declared type of [path], instantiated at the skeleton [ty]. *)
and declared_type env path ty =
  match Env.find_value path env with
  | description ->
    let scheme = (Subst.Lazy.force_value_description description).val_type in
    scheme_instance ~refined:false env scheme [scheme, ty]
  | exception Not_found -> None

(* The declared type of field [name] of a record of type [ty]. *)
and field_type env ty record name =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r -> field_type env r.ref_payload record name
  | Tconstr (path, args, _) -> (
    match Env.find_type path env with
    | { type_kind =
          Type_record (labels, _, _) | Type_record_unboxed_product (labels, _, _);
        type_params;
        _
      } -> (
      match
        List.find_opt
          (fun (l : Types.label_declaration) ->
            String.equal (Ident.name l.ld_id) name)
          labels
      with
      | Some label -> (
        try
          Some
            (List.fold_left
               (fun ty (previous : Types.label_declaration) ->
                 if not (Ctype.refinement_ident_occurs previous.ld_id ty)
                 then ty
                 else
                   let field =
                     { record with
                       rexp_desc =
                         Rexp_field (record, path, Ident.name previous.ld_id);
                       rexp_type =
                         Ctype.apply env type_params previous.ld_type args;
                       rexp_type_constraint = false
                     }
                   in
                   Ctype.substitute_refinement_expression previous.ld_id field
                     ty)
               (Ctype.apply env type_params label.ld_type args)
               labels)
        with Ctype.Cannot_apply -> None)
      | None -> None)
    | _ -> None
    | exception Not_found -> None)
  | _ -> None

let rec predicate ctx env s e =
  ctx.poll ();
  if impossible s
  then s, None
  else
    let eval = predicate ctx env in
    match e.rexp_desc with
    | Rexp_var id -> s, lookup ctx s env e.rexp_type (Path.Pident id)
    | Rexp_ident path ->
      begin match primitive env path with
      | Some (("%set_empty" | "%map_empty"), 0) ->
        s, lookup ctx s env e.rexp_type path
      | Some (_, 0) -> unsupported e.rexp_loc
      | _ -> s, lookup ctx s env e.rexp_type path
      end
    | Rexp_constant c ->
      s, scalar_value (required e.rexp_loc (rconstant ctx env c))
    | Rexp_tuple components ->
      let s, values =
        arguments_right_to_left (fun s (_, e) -> eval s e) s components
      in
      name ctx s
        (scalar_value
           (required e.rexp_loc (construct ctx env e.rexp_type "" values)))
    | Rexp_construct (path, args) ->
      let s, values = arguments_right_to_left eval s args in
      let value =
        match values with
        | [] -> rconstructor ctx env e.rexp_type path
        | _ -> (
          match path_constructor_name path with
          | Some name -> construct ctx env e.rexp_type name values
          | None -> None)
      in
      name ctx s (scalar_value (required e.rexp_loc value))
    | Rexp_record (fields, extended)
    | Rexp_record_unboxed_product (fields, extended) -> (
      let s, base =
        match extended with
        | None -> s, None
        | Some e ->
          let s, value = eval s e in
          s, Some (e.rexp_type, value)
      in
      let s, values =
        arguments_right_to_left (fun s (_, _, e) -> eval s e) s fields
      in
      let fields =
        List.map2 (fun (_, name, _) value -> name, value) fields values
      in
      let value = record_value ctx env e.rexp_type base fields in
      match value with
      | Some (Record _) -> name ctx s value
      | _ -> name ctx s (scalar_value (required e.rexp_loc value)))
    | Rexp_field (record_exp, _, field_name)
    | Rexp_unboxed_field (record_exp, _, field_name) -> (
      let s, record = eval s record_exp in
      let value = select_field ctx env record_exp.rexp_type field_name record in
      match value with
      | Some (Function _ | Record _) -> name ctx s value
      | _ -> name ctx s (scalar_value (required e.rexp_loc value)))
    | Rexp_apply (fn, args) ->
      let s, value, _ = application ctx env s e fn args in
      s, value
    | Rexp_refinement (_, body) ->
      (* [body] is used at a less refined type. Typing records the type it had,
         but that record is not trusted, and predicates are compared up to it:
         the refinements assumed are those of the type the verifier itself knows
         [body] has. *)
      known ctx env s body
    | Rexp_ghost body -> eval s body
    | Rexp_logical_equal (left_exp, right) ->
      register_logical_maps ctx env s left_exp.rexp_type;
      let s, right = eval s right in
      let s, left = eval s left_exp in
      if s.dead
      then s, None
      else
        let left = required e.rexp_loc left in
        let right = required e.rexp_loc right in
        if sort_has_unsupported_logical_equality ctx.encoding (term_sort left)
        then unsupported e.rexp_loc
        else
          let s =
            match
              if ctx.in_goal
              then pref_extensionality ctx env left_exp.rexp_type left right
              else None
            with
            | Some lemma -> fact s "heap extensionality" lemma
            | None -> s
          in
          name ctx s (scalar_value (both Eq left right))
    | Rexp_ifthenelse (c, t, Some f) ->
      let s, c = eval s c in
      choose ctx s
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
    | Rexp_fun _ -> (
      let rec parameters acc body =
        match body.rexp_desc with
        | Rexp_fun (id, ty, _, body) -> parameters ((id, ty) :: acc) body
        | _ -> List.rev acc, body
      in
      let params, body = parameters [] e in
      let captured_arguments = ctx.argument_values in
      let scoped, symbols =
        List.fold_left
          (fun (scoped, symbols) (id, ty) ->
            let value = fresh ctx env ty (Ident.name id) in
            match scalar value with
            | Some (Var symbol) ->
              let scoped, _ =
                expose_outer ctx env (bind scoped id value) ty value e.rexp_loc
              in
              scoped, symbol :: symbols
            | _ -> unsupported e.rexp_loc)
          (s, []) params
      in
      let scoped, result = eval scoped body in
      let lambda =
        if impossible scoped
        then None
        else
          Option.bind (scalar result)
            (logical_lambda ctx s captured_arguments (List.rev symbols))
      in
      match lambda, fresh_function "refinement_function" with
      | Some lambda, Function fn ->
        fn.lambda := Some lambda;
        s, Some (Function fn)
      | _ ->
        Location.raise_errorf ~loc:e.rexp_loc
          "This local function cannot be represented in a refinement \
           predicate. Use an explicit total function witness and a pointwise \
           lemma.")
    | _ -> unsupported e.rexp_loc

(* Also returns the values of the arguments, unless the application
   short-circuits. *)
and application ctx env s e fn args =
  let eval = predicate ctx env in
  let prim =
    match fn.rexp_desc with Rexp_ident path -> primitive env path | _ -> None
  in
  match prim, args with
  | Some ((("%sequand" | "%sequor") as op), 2), [(_, a); (_, b)] ->
    let s, value =
      short_circuit ctx eval e.rexp_loc ~is_and:(op = "%sequand") s a b
    in
    s, value, None
  | _ ->
    let s, args = arguments_right_to_left (fun s (_, e) -> eval s e) s args in
    let s, value = eval s fn in
    let prim = stored_primitive prim value in
    if s.dead
    then s, None, None
    else
      let () = register_logical_maps ctx env s fn.rexp_type in
      let result =
        apply_function ctx env fn.rexp_type e.rexp_type prim value args
          ~total:true
      in
      let s =
        match fn.rexp_desc with
        | Rexp_ident path ->
          unfold_transparent ctx env s path fn.rexp_type args result e.rexp_loc
        | _ -> s
      in
      let s =
        match prim with
        | Some ("caml_vox_sequence_length", 1) ->
          normal_vox_sequence_length ctx env fn.rexp_type args s
        | Some (name, 1) when List.mem name borrow_projections ->
          normal_borrow_projection ctx name args result s
        | Some ("%array_length", 1) -> normal_iarray_length ctx args s
        | Some ((("%set_find" | "%set_refined_find") as op_name), 2) ->
          normal_set_find ctx op_name args result s
        | Some ((("%map_find" | "%map_refined_find") as op_name), 2) ->
          normal_map_find ctx op_name args s
        | _ -> s
      in
      let s, value =
        match result with
        | Some (Function _) -> s, result
        | _ -> name ctx s (scalar_value (required e.rexp_loc result))
      in
      s, value, Some args

(* Evaluates [e] and assumes the refinements of the type the verifier knows it
   has, whatever types typing recorded in the predicate: the declared type of a
   value or of a record field ([known_type]), or the result type of the function
   applied (predicate equality compares the types of applied functions). *)
and known ctx env s e =
  match e.rexp_desc with
  | Rexp_ghost body | Rexp_refinement (_, body) -> known ctx env s body
  | Rexp_apply (fn, args) -> (
    let s, value, arguments = application ctx env s e fn args in
    match arguments with
    | Some arguments when not s.dead ->
      applied_result ctx env s fn.rexp_type (List.map fst args) arguments value
        e.rexp_loc
    | _ -> s, value)
  | _ -> (
    let s, value = predicate ctx env s e in
    match known_type env e with
    | Some ty -> expose_outer ctx env s ty value e.rexp_loc
    | None -> s, value)

(* The refinements of the result of a function of type [fn_type] applied to
   arguments with these labels and values, its parameters bound to them. They
   hold when the arguments satisfy the parameters' refinements: as [if premises
   then result], like the lemma of a transparent definition. Nothing is assumed
   when a parameter has a refinement below its top level. *)
and applied_result ctx env s fn_type labels arguments value loc =
  let same_label (label : Asttypes.arg_label) (label' : Types.arg_label) =
    match label, label' with
    | Nolabel, Nolabel -> true
    | Labelled l, (Labelled l' | Position l') | Optional l, Optional l' ->
      String.equal l l'
    | (Nolabel | Labelled _ | Optional _), _ -> false
  in
  let rec refinements ty =
    match get_desc (Ctype.expand_head env ty) with
    | Tpoly (ty, []) -> refinements ty
    | Trefine r ->
      let rest, payload = refinements r.ref_payload in
      r :: rest, payload
    | _ -> [], ty
  in
  let rec result bound premises ty labels arguments =
    match labels, arguments, get_desc (Ctype.expand_head env ty) with
    | [], [], _ -> Some (bound, List.rev premises, ty)
    | ( label :: labels,
        argument :: arguments,
        Tarrow ((label', _, _, binder), parameter, ty, _) )
      when same_label label label' ->
      let parameter_refinements, payload = refinements parameter in
      if not (refinement_free env payload)
      then None
      else
        let premises =
          List.fold_left
            (fun premises r -> (r, argument) :: premises)
            premises parameter_refinements
        in
        let bound =
          match binder with
          | Some binder -> bind bound binder argument
          | None -> bound
        in
        result bound premises ty labels arguments
    | _ -> None
  in
  match result s [] fn_type labels arguments with
  | None -> s, value
  | Some (bound, premises, ty) ->
    let assume s = fst (expose_outer ctx env s ty value loc) in
    let assumed =
      match premises with
      | [] -> assume bound
      | _ -> (
        try
          let bound, premises =
            List.fold_left
              (fun (s, terms) (r, argument) ->
                let s, premise =
                  predicate ctx env (bind s r.ref_binder argument) r.ref_pred
                in
                s, required loc premise :: terms)
              (bound, []) premises
          in
          fst
            (choose ctx bound
               (List.fold_left (both And) (Boolean true) premises)
               (fun s -> assume s, None)
               (fun s -> s, None))
        with Location.Error _ -> bound)
    in
    { assumed with values = s.values }, value

and unfold_transparent ctx env s path fn_type args result loc =
  match result with
  | Some (Scalar (Call _ as call)) when not (impossible s) -> (
    let path = Env.normalize_value_path None env path in
    if
      List.exists (Path.same path) ctx.unfolding
      || Unfolded.mem (path, call) s.unfolded
      || not (transparent_definition env path)
    then s
    else
      match transparent_equation env loc path fn_type (List.length args) with
      | None -> s
      | Some (parameters, refinement) ->
        let bound = List.fold_left2 bind s (List.map fst parameters) args in
        let premises =
          List.concat
            (List.map2
               (fun (_, refinements) arg ->
                 List.map (fun r -> r, arg) refinements)
               parameters args)
        in
        let assume s =
          match premises with
          | [] ->
            let s = bind s refinement.ref_binder None in
            assume_fact ctx env s refinement.ref_pred loc
          | _ -> (
            (* As [if premises then f_def args]: evaluating the premises is the
               check an explicit call makes, and the lemma's refinement, with
               every fact found while evaluating it, holds only when they do. *)
            try
              let s, premises =
                List.fold_left
                  (fun (s, terms) (r, arg) ->
                    let s, premise =
                      predicate ctx env (bind s r.ref_binder arg) r.ref_pred
                    in
                    s, required loc premise :: terms)
                  (s, []) premises
              in
              fst
                (choose ctx s
                   (List.fold_left (both And) (Boolean true) premises)
                   (fun s ->
                     ( assume_fact ctx env
                         (bind s refinement.ref_binder None)
                         refinement.ref_pred loc,
                       None ))
                   (fun s -> s, None))
            with Location.Error error ->
              { bound with
                omitted_premises = (loc, error) :: bound.omitted_premises
              })
        in
        let outer = ctx.unfolding in
        ctx.unfolding <- path :: outer;
        let unfolded =
          Fun.protect
            ~finally:(fun () -> ctx.unfolding <- outer)
            (fun () -> assume bound)
        in
        { unfolded with
          values = s.values;
          unfolded = Unfolded.add (path, call) unfolded.unfolded
        })
  | _ -> s

and expose ctx env s ty value loc =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r when not (impossible s) ->
    let s, predicate =
      predicate ctx env (bind s r.ref_binder value) r.ref_pred
    in
    (if s.dead then s else branch s (required loc predicate)), value
  | _ -> s, value

and predicate_pattern ctx env s value p =
  let s, value =
    List.fold_left
      (fun (s, value) ty -> expose_outer ctx env s ty value p.rpat_loc)
      (s, value)
      (p.rpat_type :: p.rpat_refinements)
  in
  match p.rpat_desc with
  | Rpat_any -> [s, Boolean true]
  | Rpat_var id -> [bind s id value, Boolean true]
  | Rpat_alias (p, id) -> predicate_pattern ctx env (bind s id value) value p
  | Rpat_constant c ->
    [ ( s,
        both Eq
          (required p.rpat_loc value)
          (required p.rpat_loc (rconstant ctx env c)) ) ]
  | Rpat_tuple components ->
    begin match data_of_type ctx env p.rpat_type, scalar value with
    | Some { kind = Tuple_data constructor; _ }, Some value ->
      predicate_pattern_fields ctx env s value constructor
        (List.map snd components)
    | _ -> unsupported p.rpat_loc
    end
  | Rpat_construct (path, patterns) ->
    begin match
      patterns, scalar value, scalar (rconstructor ctx env p.rpat_type path)
    with
    | [], Some value, Some constructor -> [s, both Eq value constructor]
    | _ ->
      begin match
        ( data_of_type ctx env p.rpat_type,
          path_constructor_name path,
          scalar value )
      with
      | Some data, Some name, Some value ->
        begin match data_constructor data name with
        | Some constructor ->
          predicate_pattern_fields ctx env s value constructor patterns
        | None -> unsupported p.rpat_loc
        end
      | _ -> unsupported p.rpat_loc
      end
    end
  | Rpat_record (_, fields) ->
    begin match data_of_type ctx env p.rpat_type, scalar value with
    | Some { kind = Record_data constructor; _ }, Some value ->
      let fields =
        List.map
          (fun (_, name, pattern) ->
            match
              List.find_mapi
                (fun index (field, _) ->
                  if String.equal field name then Some index else None)
                (Constructor.fields constructor)
            with
            | Some index -> index, pattern
            | None -> unsupported pattern.rpat_loc)
          fields
      in
      predicate_pattern_selected_fields ctx env s value constructor fields
    | _ -> unsupported p.rpat_loc
    end
  | Rpat_or (left, right) ->
    let left = predicate_pattern ctx env s value left in
    let left_condition = disjunction (List.map snd left) in
    let right =
      List.map
        (fun (s, condition) -> s, both And (not_ left_condition) condition)
        (predicate_pattern ctx env s value right)
    in
    left @ right

and predicate_pattern_fields ctx env s value constructor patterns =
  if List.length patterns <> List.length (Constructor.fields constructor)
  then unsupported Location.none;
  predicate_pattern_selected_fields ctx env s value constructor
    (List.mapi (fun index pattern -> index, pattern) patterns)

and predicate_pattern_selected_fields ctx env s value constructor patterns =
  List.fold_left
    (fun outcomes (index, pattern) ->
      List.concat_map
        (fun (s, condition) ->
          List.map
            (fun (s, field_condition) -> s, both And condition field_condition)
            (predicate_pattern ctx env s
               (scalar_value (select ctx constructor index value))
               pattern))
        outcomes)
    [s, Is (constructor, value)]
    patterns

and predicate_cases ctx env s value cases =
  if impossible s
  then s, None
  else
    match cases with
    | [] -> branch s (Boolean false), None
    | case :: cases ->
      let matched = predicate_pattern ctx env s value case.rc_lhs in
      let rest s = predicate_cases ctx env s value cases in
      guarded_case ctx (predicate ctx env) (predicate ctx env)
        case.rc_rhs.rexp_loc s matched case.rc_guard case.rc_rhs rest

and assume_fact ctx env s p loc =
  (* Dropping an unsupported premise is conservative; goals remain strict. *)
  let rec assume s p =
    try
      match p.rexp_desc with
      | Rexp_ghost p -> assume s p
      | Rexp_let (binding, body) ->
        let s, value = predicate ctx env s binding.rb_expr in
        let s, value =
          match binding.rb_kind with
          | Rbind_value -> s, value
          | Rbind_refine ->
            expose_fact ctx env s binding.rb_expr.rexp_type value
              binding.rb_expr.rexp_loc
        in
        assume (bind s binding.rb_ident value) body
      | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, [(_, a); (_, b)])
        when primitive env path = Some ("%sequand", 2) ->
        assume (assume s a) b
      | _ ->
        let s, predicate = predicate ctx env s p in
        if s.dead then s else branch s (required loc predicate)
    with Location.Error error ->
      { s with omitted_premises = (loc, error) :: s.omitted_premises }
  in
  assume s p

and expose_fact ctx env s ty value loc =
  match value with
  | Some (Scalar (Var symbol)) when Hashtbl.mem value_steps symbol ->
    (* The refinement exposed may be the parameter's or one assumed later (by
       [assume_]): the facts belong to both steps. *)
    Vox_proof_steps.also_step (Hashtbl.find value_steps symbol) (fun () ->
        expose_value_fact ctx env s ty value loc)
  | _ -> expose_value_fact ctx env s ty value loc

and expose_value_fact ctx env s ty value loc =
  let assume s p = assume_fact ctx env s p loc in
  let ty = Ctype.expand_head env ty in
  match get_desc ty, scalar value with
  | Trefine _, Some term when Exposed.mem (get_id ty, term) s.exposed ->
    (* Uses of a refined value re-expose its type; the facts are already on this
       path. *)
    s, value
  | Trefine r, term when not (impossible s) ->
    let s = assume (bind s r.ref_binder value) r.ref_pred in
    let s =
      match term with
      | Some term ->
        { s with exposed = Exposed.add (get_id ty, term) s.exposed }
      | None -> s
    in
    s, value
  | _ -> s, value

and expose_outer ctx env s ty value loc =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r ->
    let s, value = expose_fact ctx env s ty value loc in
    expose_outer ctx env s r.ref_payload value loc
  | _ -> s, value

let rec pattern : type k.
    context -> state -> value option -> k general_pattern -> (state * term) list
    =
 fun ctx s value p ->
  let s, value = expose_outer ctx p.pat_env s p.pat_type value p.pat_loc in
  let s, value =
    List.fold_left
      (fun (s, value) -> function
        | Tpat_refinement source, loc, _ ->
          expose_outer ctx p.pat_env s source value loc
        | _ -> s, value)
      (s, value) p.pat_extra
  in
  match p.pat_desc with
  | Tpat_any -> [s, Boolean true]
  | Tpat_var { id; mode; _ } -> [bind s id (at_mode mode value), Boolean true]
  | Tpat_alias { pattern = p; id; _ } -> pattern ctx (bind s id value) value p
  | Tpat_value p -> pattern ctx s value (p :> Typedtree.pattern)
  | Tpat_constant c ->
    begin match scalar value, scalar (constant ctx p.pat_env c) with
    | Some x, Some c -> [s, both Eq x c]
    | _ ->
      [s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")]
    end
  | Tpat_tuple components ->
    begin match data_of_type ctx p.pat_env p.pat_type, scalar value with
    | Some { kind = Tuple_data constructor; _ }, Some value ->
      pattern_fields ctx s value constructor (List.map snd components)
    | _ -> pattern_fallback ctx s p
    end
  | Tpat_construct (_, c, _, args, _) ->
    begin match data_of_type ctx p.pat_env p.pat_type, scalar value with
    | Some data, Some value ->
      begin match data_constructor data c.cstr_name with
      | Some constructor ->
        pattern_fields ctx s value constructor (List.map snd args)
      | None -> pattern_fallback ctx s p
      end
    | _ ->
      begin match
        ( scalar value,
          scalar (expression_constructor ctx p.pat_env p.pat_type c),
          args )
      with
      | Some x, Some c, [] -> [s, both Eq x c]
      | _ ->
        (* Even without a datatype encoding, argument patterns recover their
           declared refinements. Share a value across an inline record's fields
           so dependencies refer to the fields actually bound. *)
        let condition =
          required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")
        in
        List.fold_left
          (fun outcomes (_, pat) ->
            let value = fresh ctx pat.pat_env pat.pat_type "argument" in
            List.concat_map
              (fun (s, condition) ->
                List.map
                  (fun (s, matched) -> s, both And condition matched)
                  (pattern ctx s value pat))
              outcomes)
          [s, condition]
          args
      end
    end
  | Tpat_record (fields, _, _) -> record_pattern ctx s value p fields
  | Tpat_record_unboxed_product (fields, _, _) ->
    record_pattern ctx s value p fields
  | Tpat_or (left, right, _) ->
    let left = pattern ctx s value left in
    let left_condition = disjunction (List.map snd left) in
    let right =
      List.map
        (fun (s, condition) -> s, both And (not_ left_condition) condition)
        (pattern ctx s value right)
    in
    left @ right
  | _ -> pattern_fallback ctx s p

and record_pattern :
    'rep.
    context ->
    state ->
    value option ->
    pattern ->
    (Longident.t Location.loc * 'rep Data_types.gen_label_description * pattern)
    list ->
    (state * term) list =
 fun ctx s value p fields ->
  let labels =
    match fields with
    | [] -> [||]
    | (_, label, _) :: _ -> label.Data_types.lbl_all
  in
  (* Nonbinding patterns keep declaration-level refinements. Interpret their
     field names against this record, restoring the scope after nested
     matches. *)
  let initial = s in
  let s =
    Array.fold_left
      (fun s label ->
        bind s label.Data_types.lbl_id
          (select_field ctx p.pat_env p.pat_type label.Data_types.lbl_name value))
      s labels
  in
  let outcomes =
    List.fold_left
      (fun outcomes (_, label, pat) ->
        List.concat_map
          (fun (s, condition) ->
            let field =
              select_field ctx p.pat_env p.pat_type label.Data_types.lbl_name
                value
            in
            let field =
              match field with
              | Some _ -> field
              | None ->
                fresh ctx pat.pat_env pat.pat_type label.Data_types.lbl_name
            in
            List.map
              (fun (s, field_condition) ->
                s, both And condition field_condition)
              (pattern ctx s field pat))
          outcomes)
      [s, Boolean true]
      fields
  in
  List.map
    (fun (s, condition) ->
      let values =
        Array.fold_left
          (fun values label ->
            let path = Path.Pident label.Data_types.lbl_id in
            match Path.Map.find_opt path initial.values with
            | None -> Path.Map.remove path values
            | Some value -> Path.Map.add path value values)
          s.values labels
      in
      { s with values }, condition)
    outcomes

and pattern_fields ctx s value constructor patterns =
  pattern_selected_fields ctx s value constructor
    (List.mapi (fun index pattern -> index, pattern) patterns)

and pattern_selected_fields ctx s value constructor patterns =
  List.fold_left
    (fun outcomes (index, pat) ->
      List.concat_map
        (fun (s, condition) ->
          List.map
            (fun (s, field_condition) -> s, both And condition field_condition)
            (pattern ctx s
               (scalar_value (select ctx constructor index value))
               pat))
        outcomes)
    [s, Is (constructor, value)]
    patterns

and pattern_fallback : type k.
    context -> state -> k general_pattern -> (state * term) list =
 fun ctx s p ->
  let s =
    List.fold_left
      (fun s (id, _, ty, _, _) ->
        bind s id (fresh ctx p.pat_env ty (Ident.name id)))
      s (pat_bound_idents_full p)
  in
  [s, required p.pat_loc (fresh ctx p.pat_env Predef.type_bool "pattern")]

(* Asserts the predicate of refinement [r] of [value]. Each conjunct of a
   top-level [&&] chain is an obligation of its own, proved under the conjuncts
   before it, as [&&] evaluates; a failure then names the conjunct.
   [required_by] explains a predicate that cannot be translated. *)
let assert_refinement ctx env s r value ~loc ~headline ~context ~required_by =
  let rec conjuncts p =
    match p.rexp_desc with
    | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, [(_, a); (_, b)])
      when primitive env path = Some ("%sequand", 2) ->
      conjuncts a @ conjuncts b
    | _ -> [p]
  in
  let evaluate goals p =
    let in_goal = ctx.in_goal in
    ctx.in_goal <- true;
    try
      Fun.protect
        ~finally:(fun () -> ctx.in_goal <- in_goal)
        (fun () -> predicate ctx env goals p)
    with Location.Error error ->
      raise (Location.Error { error with sub = error.sub @ required_by })
  in
  let group = fresh_group () in
  let prove goals p =
    if goals.dead
    then goals
    else
      let goals, goal = evaluate goals p in
      let goal =
        if goals.dead then Boolean true else required p.rexp_loc goal
      in
      let assertion =
        Assert
          { loc;
            origin = p.rexp_loc;
            goal;
            omitted_premises = goals.omitted_premises;
            group;
            note = None;
            headline;
            context
          }
      in
      branch { goals with code = assertion :: goals.code } goal
  in
  let goals =
    List.fold_left prove (bind s r.ref_binder value) (conjuncts r.ref_pred)
  in
  { s with code = Check (added_prefix ~base:s.code goals.code) :: s.code }

(* Refinement subsumption: every value of [source] is a value of [target].
   Typing compared the two types up to refinements ([Ctype.moregen],
   [Ctype.subtype]) and found refinements that must be derived; this walks the
   pair again, assuming the source's refinements and asserting the target's.
   Both types were compared structurally, so the walk only meets pairs that
   typing decomposed the same way; any other pair is an error, never a silently
   skipped proof. *)
type subsumption =
  { sub_loc : Location.t;  (** where a failure is reported *)
    sub_headline : string option;
    sub_context : Location.msg list
  }

let refinement_binder env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r -> Some (Ident.name r.ref_binder)
  | _ -> None

(* Counterexamples should use the declaration's names. *)
let argument_name env ~source_binder ~target_binder ~source ~target =
  let first = List.find_map Fun.id in
  Option.value ~default:"argument"
    (first
       [ Option.map Ident.name target_binder;
         Option.map Ident.name source_binder;
         refinement_binder env target;
         refinement_binder env source ])

let bind_binder s binder value =
  match binder with Some id -> bind s id value | None -> s

let subsumption_unsupported (site : subsumption) =
  Location.raise_errorf ~loc:site.sub_loc ~sub:site.sub_context
    "Refinement subsumption is not supported for these types"

let rec subsume ctx env site s value ~source ~target =
  subsume_walk ctx env site [] s value source target

and subsume_walk ctx env site visited s value source target =
  ctx.poll ();
  let src = Ctype.expand_head env source
  and tgt = Ctype.expand_head env target in
  if
    impossible s
    || List.exists (fun (a, b) -> eq_type a src && eq_type b tgt) visited
    || Ctype.is_equal env false [src] [tgt]
  then s
  else
    let visited = (src, tgt) :: visited in
    let walk = subsume_walk ctx env site visited in
    let value =
      match value with Some _ -> value | None -> fresh ctx env src "value"
    in
    (* Each nested value is checked in its own scope. *)
    let scoped s f =
      let inner = f s in
      { s with code = Check (added_prefix ~base:s.code inner.code) :: s.code }
    in
    match get_desc src, get_desc tgt with
    | Trefine r1, Trefine r2 when refinements_equal env r1 r2 ->
      let s, value = expose_fact ctx env s src value site.sub_loc in
      walk s value r1.ref_payload r2.ref_payload
    | Trefine r1, _ ->
      let s, value = expose_fact ctx env s src value site.sub_loc in
      walk s value r1.ref_payload tgt
    | _, Trefine r2 ->
      let s = walk s value src r2.ref_payload in
      assert_subsumed ctx env site s tgt r2 value
    | Tarrow ((l1, _, _, b1), p1, u1, _), Tarrow ((l2, _, _, b2), p2, u2, _)
      when l1 = l2 ->
      scoped s (fun s ->
          (* The argument has the caller's type, whose refinements hold of it
             (even when the parameter types are equal: a later refinement may
             depend on them); both binders name it. A polymorphic parameter is
             only supported when the two are equal. *)
          let mono ty =
            match get_desc ty with
            | Tpoly (ty, []) -> Some ty
            | Tpoly _ -> None
            | _ -> Some ty
          in
          let x source target =
            fresh ctx env target
              (argument_name env ~source_binder:b1 ~target_binder:b2 ~source
                 ~target)
          in
          let s, x =
            match mono p1, mono p2 with
            | Some p1, Some p2 ->
              let s, x = expose_outer ctx env s p2 (x p1 p2) site.sub_loc in
              walk s x p2 p1, x
            | _ when Ctype.is_equal env false [p1] [p2] -> s, x p1 p2
            | _ -> subsumption_unsupported site
          in
          let s = bind_binder (bind_binder s b1 x) b2 x in
          (* The result of a primitive or a total function is known. *)
          let result =
            match l1, value with
            | Nolabel, Some (Function f) ->
              apply_function ctx env src u1 f.primitive value [x] ~total:f.total
            | _ -> None
          in
          let result =
            match result with
            | Some _ -> result
            | None ->
              fresh ctx env u1
                (Option.value ~default:"result" (refinement_binder env u2))
          in
          walk s result u1 u2)
    | (Ttuple c1, Ttuple c2 | Tunboxed_tuple c1, Tunboxed_tuple c2)
      when List.compare_lengths c1 c2 = 0 ->
      let fields =
        match get_desc src with
        | Ttuple _ -> tuple_fields ctx env src value
        | _ -> None
      in
      let fields =
        match fields with
        | Some fields when List.compare_lengths fields c1 = 0 ->
          List.map (fun term -> Some (Scalar term)) fields
        | _ -> List.map (fun _ -> None) c1
      in
      List.fold_left2
        (fun s field ((_, t1), (_, t2)) -> walk s field t1 t2)
        s fields (List.combine c1 c2)
    | Tconstr (p1, a1, _), Tconstr (p2, a2, _)
      when Path.same p1 p2 && List.compare_lengths a1 a2 = 0 ->
      (* Values of a parameter are fresh elements. Invariant and phantom
         parameters were compared syntactically. *)
      let variances =
        match Env.find_type p1 env with
        | decl -> decl.type_variance
        | exception Not_found -> List.map (fun _ -> Types.Variance.full) a1
      in
      List.fold_left2
        (fun s v (t1, t2) ->
          match Types.Variance.get_upper v with
          | true, false ->
            scoped s (fun s -> walk s (fresh ctx env t1 "element") t1 t2)
          | false, true ->
            scoped s (fun s -> walk s (fresh ctx env t2 "element") t2 t1)
          | true, true | false, false -> s)
        s variances (List.combine a1 a2)
    | Tpoly (t1, []), Tpoly (t2, []) -> walk s value t1 t2
    | Tpoly (t1, vs1), Tpoly (t2, vs2) when List.compare_lengths vs1 vs2 = 0 ->
      walk s value t1 t2
    | Trepr (t1, _), Trepr (t2, _) | Tbox t1, Tbox t2 -> walk s value t1 t2
    | Tconstr _, _ when not (eq_type (Ctype.expand_head_opt env src) src) ->
      (* [Ctype.subtype] opens private abbreviations of the source. *)
      walk s value (Ctype.expand_head_opt env src) tgt
    | ( ( Tvar _ | Tunivar _ | Tvariant _ | Tobject _ | Tfield _ | Tnil
        | Tpackage _ | Tquote _ | Tsplice _ | Tquote_eval _ | Tof_kind _ ),
        _ ) ->
      (* Refinements in these types were compared syntactically. *)
      s
    | _ -> subsumption_unsupported site

and refinements_equal env r1 r2 =
  match
    Ctype.refinement_predicate_types env
      ~pairs:[r1.ref_binder, r2.ref_binder]
      r1.ref_pred r2.ref_pred
  with
  | Some types -> (
    let exception Different in
    match
      Ctype.relate_predicate_types env
        (fun ty1 ty2 ->
          if not (Ctype.is_equal env false [ty1] [ty2]) then raise Different)
        types
    with
    | () -> true
    | exception Different -> false)
  | None -> false

and assert_subsumed ctx env site s ty r value =
  if impossible s
  then s
  else
    let s =
      assert_refinement ctx env s r value ~loc:site.sub_loc
        ~headline:site.sub_headline ~context:site.sub_context
        ~required_by:site.sub_context
    in
    fst (expose_fact ctx env s ty value site.sub_loc)

(* How a failed obligation of an inclusion is reported: at the value when it is
   written inside the checked module, else at the site. *)
let obligation_subsumption ?qualifier (site : refinement_site)
    (o : refinement_obligation) =
  let within (outer : Location.t) (inner : Location.t) =
    (not inner.loc_ghost)
    && inner.loc_start.pos_fname = outer.loc_start.pos_fname
    && inner.loc_start.pos_cnum >= outer.loc_start.pos_cnum
    && inner.loc_end.pos_cnum <= outer.loc_end.pos_cnum
  in
  let at_value =
    match site.rs_kind with
    | Rsite_interface _ ->
      (not o.ro_value_loc.loc_ghost)
      && o.ro_value_loc.loc_start.pos_fname = !Location.input_name
    | Rsite_constraint | Rsite_functor_argument ->
      within site.rs_loc o.ro_value_loc
  in
  let name =
    String.concat "."
      ((match qualifier with
         | Some qualifier when not at_value -> [Path.name qualifier]
         | _ -> [])
      @ o.ro_modules @ [o.ro_name])
  in
  let headline =
    match site.rs_kind with
    | Rsite_interface file ->
      Printf.sprintf
        "The value \"%s\" does not satisfy its declaration in \"%s\"." name
        (Filename.basename file)
    | Rsite_constraint ->
      Printf.sprintf
        "The value \"%s\" does not satisfy its declaration in the signature."
        name
    | Rsite_functor_argument ->
      Printf.sprintf
        "The value \"%s\" does not satisfy the functor's parameter." name
  in
  let context =
    match site.rs_kind with
    | Rsite_functor_argument when at_value ->
      [Location.msg ~loc:site.rs_loc "Required by this functor application."]
    | Rsite_interface _ | Rsite_constraint | Rsite_functor_argument -> []
  in
  { sub_loc = (if at_value then o.ro_value_loc else site.rs_loc);
    sub_headline = Some headline;
    sub_context = context
  }

(* The module whose inclusion a site checks. Its values are found by their own
   idents only when it is a structure: a signature can carry the idents of
   another module ([module type of]). A module given by a path is known by paths
   in that module; any other module's values are unknown. *)
type site_root =
  | Root_structure of structure
  | Root_path of Path.t
  | Root_opaque

let site_root m =
  match m.mod_desc with
  | Tmod_structure str -> Root_structure str
  | Tmod_ident (path, _) -> Root_path path
  | _ -> Root_opaque

(* The exported submodule [name] of a structure, which an inclusion compares;
   hidden ones ([open struct ... end]) are not. *)
let own_module str name =
  List.fold_left
    (fun found -> function
      | Sig_module (id, _, _, _, Exported) when Ident.name id = name -> Some id
      | _ -> found)
    None str.str_type

let intro_loc e =
  List.find_map
    (function
      | Texp_refine, loc, _ -> Some loc
      | Texp_refinement { target; _ }, loc, _ -> (
        match get_desc (Ctype.expand_head e.exp_env target) with
        | Trefine _ -> Some loc
        | _ -> None)
      | _ -> None)
    e.exp_extra

(* The proof step whose facts evaluating [e] at type [ty] assumes: an [assume_],
   or a lemma call (an application whose result is a refinement of [unit]). *)
let step_kind e ty : Vox_proof_steps.kind option =
  match e.exp_desc with
  | Texp_assume _ -> Some Assume
  | Texp_apply (fn, _, _, _, _, _) -> (
    match get_desc (Ctype.expand_head e.exp_env ty) with
    | Trefine r -> (
      match get_desc (Ctype.expand_head e.exp_env r.ref_payload) with
      | Tconstr (path, [], _) when Path.same path Predef.path_unit ->
        Some
          (Lemma_call
             (match fn.exp_desc with
             | Texp_ident { path; _ } -> Path.name path
             | _ -> "this function"))
      | _ -> None)
    | _ -> None)
  | _ -> None

(* A step's refinement can also be needed by the typer, which accepts a value of
   a refined type where that same type is expected without a proof. So a step is
   only checked when its refined value is not used that way: when a conversion
   drops its refinement where it is computed, or when it is bound by a [let]
   pattern without variables ([discarded]). *)
let converted e =
  List.exists
    (function Texp_refinement _, _, _ -> true | _ -> false)
    e.exp_extra

let discarded : expression option ref = ref None

(* Checked refined parameters: a use of one exposes its refinement again, as
   part of the same step. *)
let argument_steps : (Ident.t, Vox_proof_steps.step) Hashtbl.t =
  Hashtbl.create 16

let expression_step e ty =
  if not (Vox_proof_steps.enabled ())
  then None
  else
    match e.exp_desc with
    | Texp_ident { path = Path.Pident id; _ } ->
      Hashtbl.find_opt argument_steps id
    | _ ->
      if converted e || match !discarded with Some d -> d == e | None -> false
      then
        Option.bind (step_kind e ty) (fun kind ->
            Vox_proof_steps.create kind e.exp_loc)
      else None

(* The locations of functions bound by [let]. The parameters of an anonymous
   function passed as an argument have the refinements its callee requires,
   which are not proof steps of this function. *)
let let_bound_functions : (Location.t, unit) Hashtbl.t = Hashtbl.create 16

(* A refined parameter, whose refinement the function assumes, when it is a
   variable that [fn] never uses at a refined type without a conversion. *)
let argument_step fn (pat : pattern) =
  if
    (not (Vox_proof_steps.enabled ()))
    || not (Hashtbl.mem let_bound_functions fn.exp_loc)
  then None
  else
    match get_desc pat.pat_type, pat_bound_idents pat with
    | Trefine _, [id]
    (* A refinement written on the parameter: a refinement named by a type
       abbreviation belongs to that type. The parameters of a generated
       definition lemma have the location of the whole definition. *)
      when pat.pat_loc <> fn.exp_loc -> (
      let exception Refined_use in
      let default = Tast_iterator.default_iterator in
      let uses =
        { default with
          expr =
            (fun it e ->
              (match e.exp_desc with
              | Texp_ident { path = Path.Pident id'; _ }
                when Ident.same id id' && not (converted e) -> (
                match get_desc (Ctype.expand_head e.exp_env e.exp_type) with
                | Trefine _ -> raise Refined_use
                | _ -> ())
              | _ -> ());
              default.expr it e)
        }
      in
      match uses.expr uses fn with
      | () ->
        let step =
          Vox_proof_steps.create (Argument (Ident.name id)) pat.pat_loc
        in
        Option.iter (Hashtbl.replace argument_steps id) step;
        step
      | exception Refined_use -> None)
    | _ -> None

(* Evaluate [f] from [s] as part of [step]. A step that makes its path
   impossible is used: no obligation is generated on that path, so no core could
   show that it needs the step. *)
let in_step step s f =
  let ((s', _) as result) = Vox_proof_steps.with_step step f in
  (match step with
  | Some step when s'.dead && not s.dead -> Vox_proof_steps.use step
  | _ -> ());
  result

let omitted_premise_messages s =
  List.concat_map
    (fun (loc, (error : Location.error)) ->
      Location.msg ~loc
        "This refinement premise was omitted because it could not be \
         translated to SMT"
      :: error.main :: error.sub)
    (List.rev s.omitted_premises)

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
  ctx.module_aliases
    <- Path.Map.fold
         (fun path target aliases ->
           match exported path with
           | None -> aliases
           | Some path -> Path.Map.add path target aliases)
         ctx.module_aliases ctx.module_aliases;
  { s with values }

let rec module_alias m =
  match m.mod_desc with
  | Tmod_ident (path, _) -> Some path
  | Tmod_constraint (m, _, _, _) -> module_alias m
  | _ -> None

let forwards_result e =
  match e.exp_desc with
  | Texp_let _ | Texp_sequence _ | Texp_ifthenelse _ | Texp_match _
  | Texp_open _ | Texp_letmodule _ | Texp_exclave _ ->
    true
  | _ -> false

let defer checks scope check = checks := { scope; check } :: !checks

(* Re-enter the saved branch only for its proof. Its assumptions must not leak
   into checks for another branch or into evaluation of the enclosing call. *)
let complete_checks entry s checks =
  List.fold_left
    (fun s { scope; check } ->
      if s.dead
      then s
      else
        let prefix =
          erase_assertions (added_prefix ~base:entry.code scope.code)
        in
        let scoped =
          { s with
            values =
              Path.Map.union
                (fun _ current _ -> Some current)
                s.values scope.values;
            code = prefix @ s.code
          }
        in
        let checked = check scoped in
        { s with
          code = Check (added_prefix ~base:s.code checked.code) :: s.code
        })
    s (List.rev checks)

let rec expression ?deferred ctx s e =
  ctx.poll ();
  if impossible s
  then s, None
  else
    let deferred =
      if
        List.exists
          (function Texp_refine, _, _ -> true | _ -> false)
          e.exp_extra
      then None
      else deferred
    in
    expression_extras ?deferred ctx s e e.exp_type e.exp_extra

and expression_extras ?deferred ctx s e ty = function
  | [] ->
    (* A proof step's arguments are part of it: its terms can request
       observations. *)
    let step = if forwards_result e then None else expression_step e ty in
    in_step step s (fun () ->
        let s, value =
          expression_desc ?deferred ctx s { e with exp_type = ty }
        in
        (* The returned child's facts are already present when checked eagerly,
           and its target must remain unavailable when checking is deferred. *)
        if forwards_result e
        then s, value
        else expose_outer ctx e.exp_env s ty value e.exp_loc)
  | (extra, loc, _) :: rest -> (
    let source =
      match extra with
      | Texp_refinement { source; _ } | Texp_subsumption { source; _ } -> source
      | _ -> ty
    in
    let s, value = expression_extras ?deferred ctx s e source rest in
    match extra with
    | Texp_refinement { target; _ } ->
      begin match deferred with
      | Some checks
        when match get_desc (Ctype.expand_head e.exp_env target) with
             | Trefine _ -> true
             | _ -> false ->
        defer checks s (fun s ->
            fst (introduce_outer ctx e.exp_env s target value loc));
        s, value
      | None | Some _ -> introduce ctx e.exp_env s target value loc
      end
    | Texp_subsumption { source; target } -> (
      let site = { sub_loc = loc; sub_headline = None; sub_context = [] } in
      let check s = subsume ctx e.exp_env site s value ~source ~target in
      match deferred with
      | None -> check s, value
      | Some checks ->
        defer checks s check;
        s, value)
    | Texp_value_name id ->
      let path = Path.Pident id in
      ctx.argument_values <- Path.Map.add path value ctx.argument_values;
      ctx.free <- Path.Map.remove path ctx.free;
      bind s id value, value
    | _ -> s, value)

and introduce ctx env s ty value loc =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r when ctx.verify_introductions && not s.dead ->
    let s =
      assert_refinement ctx env s r value ~loc ~headline:None ~context:[]
        ~required_by:
          [Location.msg ~loc "Required by this refinement introduction"]
    in
    expose_outer ctx env s ty value loc
  | _ -> s, value

and introduce_outer ctx env s ty value loc =
  match get_desc (Ctype.expand_head env ty) with
  | Trefine r ->
    let s, value = introduce_outer ctx env s r.ref_payload value loc in
    introduce ctx env s ty value loc
  | _ -> s, value

and expression_desc ?deferred ctx s e =
  let eval = expression ctx in
  let result = expression ?deferred ctx in
  let opaque () = fresh ctx e.exp_env e.exp_type "result" in
  let opaque_if_unsupported = function
    | Some _ as value -> value
    | None -> opaque ()
  in
  match e.exp_desc with
  | Texp_ident { path; desc; mode; _ } ->
    let value =
      match desc.val_kind with
      | Val_mut _ | Val_ivar _ -> opaque ()
      | Val_prim p when p.prim_arity = 0 -> opaque ()
      | _ -> at_mode mode (lookup ctx s e.exp_env e.exp_type path)
    in
    s, value
  | Texp_constant c -> s, constant ctx e.exp_env c
  | Texp_tuple (components, _) ->
    let s, values =
      arguments_right_to_left (fun s (_, e) -> result s e) s components
    in
    name ctx s
      (opaque_if_unsupported (construct ctx e.exp_env e.exp_type "" values))
  | Texp_construct (_, c, _, args, _) ->
    let s, values =
      arguments_right_to_left (fun s (_, e) -> result s e) s args
    in
    let value =
      match values with
      | [] -> expression_constructor ctx e.exp_env e.exp_type c
      | _ -> construct ctx e.exp_env e.exp_type c.cstr_name values
    in
    name ctx s (opaque_if_unsupported value)
  | Texp_record { fields; extended_expression; _ } ->
    let s, base =
      match extended_expression with
      | None -> s, None
      | Some (e, _, _) ->
        let s, value = eval s e in
        s, Some (e.exp_type, value)
    in
    let fields = Array.to_list fields in
    let s, values = record_fields ?deferred ctx s fields in
    let fields =
      List.filter_map Fun.id
        (List.map2
           (fun (label, _, field) value ->
             match field with
             | Kept _ -> None
             | Overridden _ -> Some (label.Data_types.lbl_name, value))
           fields values)
    in
    name ctx s
      (opaque_if_unsupported
         (record_value ctx e.exp_env e.exp_type base fields))
  | Texp_record_unboxed_product { fields; extended_expression; _ } ->
    let s, base =
      match extended_expression with
      | None -> s, None
      | Some (e, _) ->
        let s, value = eval s e in
        s, Some (e.exp_type, value)
    in
    let fields = Array.to_list fields in
    let s, values = record_fields ?deferred ctx s fields in
    let fields =
      List.filter_map Fun.id
        (List.map2
           (fun (label, _, field) value ->
             match field with
             | Kept _ -> None
             | Overridden _ -> Some (label.Data_types.lbl_name, value))
           fields values)
    in
    name ctx s
      (opaque_if_unsupported
         (record_value ctx e.exp_env e.exp_type base fields))
  | Texp_array (Immutable, _, elements, _) ->
    let s, values = arguments_right_to_left result s elements in
    let s, value = iarray_value ctx e.exp_env e.exp_type s values in
    s, opaque_if_unsupported value
  | Texp_field { record; label; _ } ->
    let s, value = eval s record in
    name ctx s
      (opaque_if_unsupported
         (select_field ctx e.exp_env record.exp_type label.Data_types.lbl_name
            value))
  | Texp_unboxed_field { record; label; _ } ->
    let s, value = eval s record in
    name ctx s
      (opaque_if_unsupported
         (select_field ctx e.exp_env record.exp_type label.Data_types.lbl_name
            value))
  | Texp_open ({ open_expr = { mod_desc = Tmod_ident _; _ }; _ }, body) ->
    result s body
  | Texp_letmodule (Some id, _, _, m, body)
    when Option.is_some (module_structure m) ->
    let str = Option.get (module_structure m) in
    let s, _ = structure ctx s str in
    discharge_constraints ctx s m;
    result (export_module ctx id str s) body
  | Texp_let (rec_flag, bindings, body) ->
    let s, _ = value_bindings ctx s rec_flag bindings in
    result s body
  | Texp_assume (binding, _, _) -> eval s binding.vb_expr
  | Texp_logical_equal (left, right) -> (
    let s, right = eval s right in
    let s, left = eval s left in
    match scalar left, scalar right with
    | Some left, Some right
      when term_sort left = term_sort right
           && not
                (sort_has_unsupported_logical_equality ctx.encoding
                   (term_sort left)) ->
      name ctx s (scalar_value (both Eq left right))
    | _ -> s, opaque ())
  | Texp_sequence (a, _, b) ->
    let s, _ = eval s a in
    result s b
  | Texp_ifthenelse (c, t, f) ->
    let s, c = eval s c in
    let c =
      match scalar c with
      | Some c -> c
      | None ->
        required e.exp_loc (fresh ctx e.exp_env Predef.type_bool "condition")
    in
    choose ctx s c
      (fun s -> result s t)
      (fun s -> match f with None -> s, None | Some f -> result s f)
  | Texp_apply (fn, args, _, _, _, _) ->
    let prim =
      match fn.exp_desc with
      | Texp_ident { path; _ } -> primitive fn.exp_env path
      | _ -> None
    in
    begin match prim, args with
    | ( Some ((("%sequand" | "%sequor") as op), 2),
        [(_, Arg (a, _)); (_, Arg (b, _))] ) ->
      short_circuit ctx eval e.exp_loc ~is_and:(op = "%sequand") s a b
    | _ -> (
      let args = Array.of_list args in
      let args_expressions = args in
      let values = Array.make (Array.length args) None in
      let checks = Array.init (Array.length args) (fun _ -> ref []) in
      let entries = Array.make (Array.length args) s in
      let s = ref s in
      for index = Array.length args - 1 downto 0 do
        match snd args.(index) with
        | Omitted _ -> ()
        | Arg (e, _) ->
          entries.(index) <- !s;
          let evaluated, value = expression ~deferred:checks.(index) ctx !s e in
          values.(index) <- value;
          s := evaluated
      done;
      let evaluated, fn_value = eval !s fn in
      s := evaluated;
      (* Only proofs of returned values are deferred. Argument computations have
         already been checked in runtime order; contracts follow dependency
         order. *)
      Array.iteri
        (fun index (_, arg) ->
          match arg with
          | Omitted _ -> ()
          | Arg (e, _) when !(checks.(index)) <> [] ->
            let checked =
              complete_checks entries.(index) !s !(checks.(index))
            in
            let checked, _ =
              expose_outer ctx e.exp_env checked e.exp_type values.(index)
                e.exp_loc
            in
            s := checked
          | Arg _ -> ())
        args;
      let complete =
        Array.for_all (function _, Arg _ -> true | _, Omitted _ -> false) args
      in
      let logical_arguments =
        lazy
          (let dependent =
             Array.of_list (dependent_parameters fn.exp_env fn.exp_type)
           in
           Array.for_all Fun.id
             (Array.mapi
                (fun index (_, arg) ->
                  match arg with
                  | Omitted _ -> true
                  | Arg (e, _) ->
                    let dependent =
                      index < Array.length dependent && dependent.(index)
                    in
                    logical_argument ~dependent e values.(index))
                args))
      in
      let s, args = !s, Array.to_list values in
      if not s.dead then ctx.check_call ctx s e args;
      let prim = stored_primitive prim fn_value in
      (* Set and map operations are modelled through the ordering, which the
         operation applies to the elements: an element holding an effectful
         closure can compare differently each time. *)
      let prim =
        match prim with
        | Some (name, _)
          when (String.starts_with ~prefix:"%set_" name
               || String.starts_with ~prefix:"%map_" name)
               && not (Lazy.force logical_arguments) ->
          None
        | _ -> prim
      in
      let s =
        match
          ( complete,
            shift_count fn_value prim args,
            List.rev
              (List.filter_map
                 (function _, Arg (e, _) -> Some e | _, Omitted _ -> None)
                 (Array.to_list args_expressions)) )
        with
        | true, Some count, count_expression :: _ ->
          require_shift_count ctx s ~loc:e.exp_loc
            ~count_loc:count_expression.exp_loc count
        | _ -> s
      in
      let total =
        (match fn_value with
          | Some (Function { total; _ }) -> total
          | _ -> false)
        && Lazy.force logical_arguments
      in
      register_logical_maps ctx e.exp_env s fn.exp_type;
      let value =
        apply_function ctx e.exp_env fn.exp_type e.exp_type prim fn_value args
          ~total
      in
      let s =
        match fn.exp_desc with
        | Texp_ident { path; _ } when complete ->
          unfold_transparent ctx e.exp_env s path fn.exp_type args value
            e.exp_loc
        | _ -> s
      in
      match prim with
      | Some ("caml_vox_sequence_length", 1) ->
        name ctx
          (normal_vox_sequence_length ctx e.exp_env fn.exp_type args s)
          value
      | Some (primitive_name, 1) when List.mem primitive_name borrow_projections
        ->
        name ctx
          (normal_borrow_projection ctx primitive_name args value s)
          value
      | Some ("caml_borrow_length", 1) ->
        let value = match value with Some _ -> value | None -> opaque () in
        name ctx (normal_borrow_length ctx args value s) value
      | Some (primitive_name, _)
        when List.mem primitive_name
               [ "caml_borrow_open";
                 "caml_borrow_restore";
                 "caml_borrow_split";
                 "caml_borrow_recombine";
                 "caml_borrow_finish";
                 "caml_borrow_transfer" ] ->
        let value = match value with Some _ -> value | None -> opaque () in
        name ctx
          (normal_borrow_transition ctx e.exp_env fn.exp_type e.exp_type
             primitive_name args value s)
          value
      | Some (("%raise" | "%reraise" | "%raise_notrace"), 1) ->
        branch s (Boolean false), None
      | Some ("%iarray_init", 2) ->
        let value = match value with Some _ -> value | None -> opaque () in
        let s =
          match args, iarray_call ctx value with
          | [length; _], Some (sort, array) -> (
            match scalar length with
            | Some length when term_sort length = Int63 ->
              branch s (both Eq (iarray_length ctx sort array) length)
            | _ -> s)
          | _ -> s
        in
        name ctx s value
      | Some ("caml_array_append", 2) | Some ("%iarray_sub", 3) ->
        name ctx
          (normal_iarray_copy ctx value s)
          (match value with Some _ -> value | None -> opaque ())
      | Some ("%array_length", 1) ->
        name ctx
          (normal_iarray_length ctx args s)
          (match value with Some _ -> value | None -> opaque ())
      | Some ("%array_safe_get", 2) ->
        name ctx
          (normal_iarray_get ctx args s)
          (match value with Some _ -> value | None -> opaque ())
      | Some ((("%set_find" | "%set_refined_find") as op_name), 2) ->
        name ctx
          (normal_set_find ctx op_name args value s)
          (match value with Some _ -> value | None -> opaque ())
      | Some ((("%map_find" | "%map_refined_find") as op_name), 2) ->
        name ctx
          (normal_map_find ctx op_name args s)
          (match value with Some _ -> value | None -> opaque ())
      | _ -> name ctx s (match value with Some _ -> value | None -> opaque ()))
    end
  | Texp_function { params; body; _ } ->
    let value = opaque () in
    let check s =
      check_function ctx s e params body value;
      s
    in
    begin match deferred with
    | None -> ignore (check s)
    | Some checks -> defer checks s check
    end;
    s, value
  | Texp_match (scrutinee, _, cases, [], _)
    when List.for_all (fun c -> snd (split_pattern c.c_lhs) = None) cases ->
    let s, value = eval s scrutinee in
    computation_cases ?deferred ctx s scrutinee.exp_type value cases
  | Texp_for { for_id; for_from; for_to; for_dir; for_body; _ } ->
    let s, first = eval s for_from in
    let s, last = eval s for_to in
    let index = scalar_value (Var (Symbol.create ~label:"loop index" Int63)) in
    let body_state = bind s for_id index in
    let body_state =
      match scalar first, scalar last, scalar index with
      | Some first, Some last, Some index ->
        let low, high =
          match for_dir with
          | Asttypes.Upto -> first, last
          | Asttypes.Downto -> last, first
        in
        branch body_state (both And (both Le low index) (both Le index high))
      | _ -> body_state
    in
    let checked, _ = eval body_state for_body in
    let s =
      { s with code = Check (added_prefix ~base:s.code checked.code) :: s.code }
    in
    s, opaque ()
  | Texp_exclave body -> result s body
  | Texp_assert
      ( { exp_desc = Texp_construct (_, { cstr_name = "false"; _ }, _, _, _); _ },
        _ ) ->
    (* Compiled to a raise, even under [-noassert]. *)
    branch s (Boolean false), None
  | Texp_assert (condition, _) when not !Clflags.noassert ->
    (* The continuation runs only if the condition evaluated to true. Under
       [-noassert] the condition is not evaluated at all. *)
    let s, condition = eval s condition in
    let s =
      match scalar condition with
      | Some condition when not s.dead -> branch s condition
      | _ -> s
    in
    s, opaque ()
  | _ ->
    (* Unknown evaluation/control-flow forms lose outgoing facts, but cannot
       hide obligations in their children or delayed bodies. *)
    let state = ref s in
    let iterator = iterator ctx state in
    Tast_iterator.default_iterator.expr iterator e;
    !state, opaque ()

and record_fields :
    'rep.
    ?deferred:deferred_check list ref ->
    context ->
    state ->
    ('rep Data_types.gen_label_description * _ * record_label_definition) list ->
    state * value option list =
 fun ?deferred ctx initial fields ->
  let fields = Array.of_list fields in
  let values = Array.make (Array.length fields) None in
  let entries = Array.make (Array.length fields) initial in
  let checks = Array.init (Array.length fields) (fun _ -> ref []) in
  let s = ref initial in
  for index = Array.length fields - 1 downto 0 do
    match fields.(index) with
    | _, _, Kept _ -> ()
    | _, _, Overridden (_, e) ->
      entries.(index) <- !s;
      let next, value = expression ~deferred:checks.(index) ctx !s e in
      s := next;
      values.(index) <- value
  done;
  let check s =
    let s = ref s in
    Array.iteri
      (fun index (_, _, field) ->
        match field with
        | Kept _ -> ()
        | Overridden (_, e) ->
          let checked = complete_checks entries.(index) !s !(checks.(index)) in
          s
            := fst
                 (expose_outer ctx e.exp_env checked e.exp_type values.(index)
                    e.exp_loc))
      fields;
    !s
  in
  let s =
    match deferred with
    | None -> check !s
    | Some checks ->
      defer checks !s check;
      !s
  in
  s, Array.to_list values

and check_function ctx s e params body value =
  let captured = s in
  let captured_arguments = ctx.argument_values in
  let s = { s with code = erase_assertions s.code } in
  let s =
    List.fold_left
      (fun s p ->
        let s, pat =
          match p.fp_kind with
          | Tparam_pat pat -> s, pat
          | Tparam_optional_default (pat, default, _) ->
            let checked, _ = expression ctx s default in
            ( { s with
                code = Check (added_prefix ~base:s.code checked.code) :: s.code
              },
              pat )
        in
        let value =
          fresh ctx pat.pat_env pat.pat_type (Ident.name p.fp_param)
        in
        let step = argument_step e pat in
        register_value step value;
        let s, condition =
          in_step step s (fun () ->
              merge_patterns s (pattern ctx (bind s p.fp_param value) value pat))
        in
        branch s condition)
      s params
  in
  let s = assume_lemma_premise ctx s body in
  let arguments =
    Misc.Stdlib.List.map_option
      (fun p ->
        match
          ( p.fp_arg_label,
            p.fp_kind,
            Path.Map.find_opt (Path.Pident p.fp_param) s.values )
        with
        | Nolabel, Tparam_pat _, Some (Some (Scalar (Var symbol))) ->
          Some symbol
        | _ -> None)
      params
  in
  let s, result =
    match body with
    | Tfunction_body body -> expression ctx s body
    | Tfunction_cases cases ->
      begin match cases.fc_cases with
      | [] -> s, None
      | c :: _ ->
        let value = fresh ctx c.c_lhs.pat_env c.c_lhs.pat_type "argument" in
        value_cases ctx (bind s cases.fc_param value) value cases.fc_cases
      end
  in
  ctx.batches <- (Warnings.backup (), s.code) :: ctx.batches;
  if
    (not s.dead)
    && List.exists
         (function Texp_ghost, _, _ -> true | _ -> false)
         e.exp_extra
  then
    match value, arguments, scalar result with
    | Some (Function fn), Some parameters, Some body ->
      fn.lambda
        := logical_lambda ctx captured captured_arguments parameters body
    | _ -> ()

(* An erased lemma whose conclusion is [{u : unit | if p then q else true}] is
   proved under [p]. Its body never runs, and under [not p] its conclusion holds
   trivially; a recursive call still has to establish the callee's own premise
   to use its conclusion. *)
and assume_lemma_premise ctx s body =
  match body with
  | Tfunction_body body
    when (not s.dead)
         && List.exists
              (function Texp_ghost, _, _ -> true | _ -> false)
              body.exp_extra -> (
    let env = body.exp_env in
    (* Only a body whose single refinement is this conclusion: another one (an
       inner annotation) would be checked under [p] as well. *)
    let conclusions =
      List.filter_map
        (function
          | Texp_refinement { target; _ }, _, _ -> (
            match get_desc (Ctype.expand_head env target) with
            | Trefine r -> Some r
            | _ -> None)
          | _ -> None)
        body.exp_extra
    in
    match conclusions with
    | [ ({ ref_pred =
             { rexp_desc =
                 Rexp_ifthenelse
                   ( premise,
                     _,
                     Some
                       { rexp_desc =
                           Rexp_construct
                             (Path.Pextra_ty (_, Path.Pcstr_ty "true"), []);
                         _
                       } );
               _
             };
           _
         } as r) ]
      when match get_desc (Ctype.expand_head env r.ref_payload) with
           | Tconstr (path, [], _) -> Path.same path Predef.path_unit
           | _ -> false -> (
      let unit = fresh ctx env r.ref_payload "result" in
      match predicate ctx env (bind s r.ref_binder unit) premise with
      | assumed, premise when not assumed.dead -> (
        match scalar premise with
        | Some premise -> branch { assumed with values = s.values } premise
        | None -> s)
      | _ -> s
      | exception Location.Error _ -> s)
    | _ -> s)
  | _ -> s

and value_bindings ctx s rec_flag bindings =
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
          let value =
            fresh ctx vb.vb_pat.pat_env vb.vb_pat.pat_type "recursive"
          in
          let s, condition = merge_patterns s (pattern ctx s value vb.vb_pat) in
          branch s condition)
        s bindings
  in
  let rec loop s = function
    | [] -> s, None
    | _ when impossible s -> s, None
    | vb :: rest ->
      if Vox_proof_steps.enabled ()
      then Hashtbl.replace let_bound_functions vb.vb_expr.exp_loc ();
      (* A proof step whose value is discarded: the pattern's refinement is the
         step's too. *)
      let discarded_step =
        if pat_bound_idents vb.vb_pat = [] then Some vb.vb_expr else None
      in
      let s, value, step =
        Builtin_attributes.warning_scope ~ppwarning:false vb.vb_attributes
          (fun () ->
            let saved = !discarded in
            discarded := discarded_step;
            Fun.protect
              ~finally:(fun () -> discarded := saved)
              (fun () ->
                let s, value = expression ctx s vb.vb_expr in
                ( s,
                  value,
                  Option.bind discarded_step (fun e ->
                      expression_step e e.exp_type) )))
      in
      let value =
        match rec_flag, value with
        | Asttypes.Recursive, Some (Function fn) ->
          Some (Function { fn with lambda = ref None })
        | _ -> value
      in
      let s, condition =
        in_step step s (fun () ->
            merge_patterns s (pattern ctx s value vb.vb_pat))
      in
      loop (branch s condition) rest
  in
  loop s bindings

and value_cases ctx s value cases = cases_with_pattern ctx s value cases

and computation_cases ?deferred ctx s scrutinee_type value cases =
  cases_with_pattern ?deferred ~scrutinee_type ctx s value cases

and cases_with_pattern : type k.
    ?scrutinee_type:Types.type_expr ->
    ?deferred:deferred_check list ref ->
    context ->
    state ->
    value option ->
    k case list ->
    state * value option =
 fun ?scrutinee_type ?deferred ctx s value cases ->
  if impossible s
  then s, None
  else
    match cases with
    | [] -> branch s (Boolean false), None
    | c :: cases ->
      (* A generalized nullary constructor can have a different datatype
         instance from the pattern after refinement elaboration. *)
      let matched_value =
        match scrutinee_type, scalar value with
        | Some source, Some term
          when Some (term_sort term)
               <> sort ctx.encoding c.c_lhs.pat_env c.c_lhs.pat_type ->
          begin match expose_head ctx term with
          | Construct (constructor, [])
            when same_nominal_data_type c.c_lhs.pat_env source c.c_lhs.pat_type
            ->
            construct ctx c.c_lhs.pat_env c.c_lhs.pat_type
              (Constructor.label constructor)
              []
          | _ -> None
          end
        | _ -> value
      in
      let matched = pattern ctx s matched_value c.c_lhs in
      let rest s =
        cases_with_pattern ?scrutinee_type ?deferred ctx s value cases
      in
      guarded_case ctx (expression ctx) (expression ?deferred ctx)
        c.c_rhs.exp_loc s matched c.c_guard c.c_rhs rest

and structure ctx s str =
  List.fold_left
    (fun (s, _) item ->
      if impossible s
      then s, None
      else
        match item.str_desc with
        | Tstr_value (rec_flag, bindings) ->
          value_bindings ctx s rec_flag bindings
        | Tstr_eval (e, _, _) -> expression ctx s e
        | Tstr_attribute attribute ->
          Builtin_attributes.warning_attribute ~ppwarning:false attribute;
          s, None
        | Tstr_module { mb_id = Some id; mb_expr; mb_attributes; _ }
          when Option.is_some (module_structure mb_expr) ->
          let str = Option.get (module_structure mb_expr) in
          let s, _ =
            Builtin_attributes.warning_scope ~ppwarning:false mb_attributes
              (fun () ->
                let s, _ = structure ctx s str in
                discharge_constraints ctx s mb_expr;
                s, None)
          in
          export_module ctx id str s, None
        | Tstr_module { mb_id = Some id; mb_expr; _ }
          when Option.is_some (module_alias mb_expr) ->
          (* The alias and its target are the same module at run time. *)
          let target = Option.get (module_alias mb_expr) in
          discharge_constraints ctx s mb_expr;
          ctx.module_aliases
            <- Path.Map.add (Path.Pident id) target ctx.module_aliases;
          s, None
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
  let module_expr self m =
    match m.mod_desc with
    | Tmod_constraint (arg, _, _, _) | Tmod_apply (_, arg, _, _, _) -> (
      (* Termination checks run the verifier on bodies during typing; the sites
         are left for the verification of the unit. *)
      match
        if ctx.verify_introductions
        then Verification.find_refinement_site m.mod_desc
        else None
      with
      | None -> Tast_iterator.default_iterator.module_expr self m
      | Some site ->
        (match m.mod_desc with
        | Tmod_apply (funct, _, _, _, _) ->
          self.Tast_iterator.module_expr self funct
        | _ -> ());
        (* The site's obligations are discharged in the state after the checked
           module. *)
        checked (fun s ->
            let s =
              match module_structure arg with
              | Some str ->
                let s, _ = structure ctx s str in
                discharge_constraints ctx s arg;
                s
              | None ->
                let state = ref s in
                let iterator = iterator ctx state in
                iterator.Tast_iterator.module_expr iterator arg;
                !state
            in
            discharge_site ~root:(site_root arg) ctx s site;
            s, None))
    | _ -> Tast_iterator.default_iterator.module_expr self m
  in
  { Tast_iterator.default_iterator with
    expr = (fun _ e -> checked (fun s -> expression ctx s e));
    value_bindings =
      (fun _ (rec_flag, bindings) ->
        checked (fun s -> value_bindings ctx s rec_flag bindings));
    structure = (fun _ str -> checked (fun s -> structure ctx s str));
    module_expr
  }

(* The obligations of the signature constraints around a structure whose state
   is [s]. *)
and discharge_constraints ctx s m =
  match m.mod_desc with
  | Tmod_constraint (inner, _, _, _) when ctx.verify_introductions ->
    Option.iter
      (discharge_site ~root:(site_root inner) ctx s)
      (Verification.find_refinement_site m.mod_desc);
    discharge_constraints ctx s inner
  | _ -> ()

(* Proves the obligations of an inclusion check in the state [s] of its site, as
   a batch of its own. The declared refinements follow from facts already in
   [s], so [s] itself is unchanged. *)
and discharge_site ~root ctx s (site : refinement_site) =
  if not (impossible s)
  then begin
    let checked =
      List.fold_left
        (fun checked (o : refinement_obligation) ->
          let env = o.ro_env in
          let find path = lookup ctx checked env o.ro_source path in
          let inside prefix names =
            List.fold_left
              (fun path name -> Path.Pdot (path, name))
              prefix names
          in
          let value =
            match o.ro_value, root with
            | Some (Pident id), Root_structure str -> (
              match o.ro_modules with
              | [] -> find (Pident id)
              | first :: rest ->
                Option.bind (own_module str first) (fun first ->
                    find (inside (Pident first) (rest @ [Ident.name id]))))
            | Some (Pident id), Root_path prefix ->
              find (inside prefix (o.ro_modules @ [Ident.name id]))
            | _ -> None
          in
          let value =
            match value with
            | Some _ -> value
            | None -> fresh ctx env o.ro_source o.ro_name
          in
          let inner =
            subsume ctx env
              (obligation_subsumption
                 ?qualifier:
                   (match root with Root_path path -> Some path | _ -> None)
                 site o)
              checked value ~source:o.ro_source ~target:o.ro_target
          in
          { checked with
            code =
              Check (added_prefix ~base:checked.code inner.code) :: checked.code
          })
        { s with code = erase_assertions s.code }
        site.rs_obligations
    in
    ctx.batches <- (Warnings.backup (), checked.code) :: ctx.batches
  end

(* Keep the SSA definitions needed by the selected obligations. Definitions from
   later branches or postconditions can otherwise dominate a query. Dropping
   facts only weakens the premises; no new equality is introduced. *)
let slice_goal ?(rename = Fun.id) ctx query term =
  let used = Hashtbl.create 32 in
  let pending = Queue.create () in
  let definitions = Hashtbl.create 32 in
  let rec visit = function
    | Var symbol ->
      if not (Hashtbl.mem used symbol)
      then begin
        Hashtbl.add used symbol ();
        Queue.add symbol pending;
        (* Observation expansion can expose additional SSA dependencies. *)
        Option.iter
          (fun definition -> visit (rename definition))
          (Hashtbl.find_opt ctx.observation_definitions symbol)
      end
    | App (_, args) | Call (_, args) | Construct (_, args) ->
      List.iter visit args
    | Is (_, arg) | Select (_, _, arg) -> visit arg
    | Boolean _ | Integer _ | Big_integer _ -> ()
  in
  let defined_symbol (fact : labelled_term) =
    match fact.label, fact.term with
    | ("value" | "reachable" | "observation"), App (Eq, [Var symbol; _]) ->
      Some symbol
    | _ -> None
  in
  List.iter
    (fun fact ->
      match defined_symbol fact with
      | None -> visit fact.term
      | Some symbol ->
        let previous =
          Option.value (Hashtbl.find_opt definitions symbol) ~default:[]
        in
        Hashtbl.replace definitions symbol (fact.term :: previous))
    query.facts;
  visit term;
  while not (Queue.is_empty pending) do
    let symbol = Queue.take pending in
    List.iter visit
      (Option.value (Hashtbl.find_opt definitions symbol) ~default:[])
  done;
  { query with
    symbols = List.filter (Hashtbl.mem used) query.symbols;
    facts =
      List.filter
        (fun fact ->
          match defined_symbol fact with
          | None -> true
          | Some symbol -> Hashtbl.mem used symbol)
        query.facts;
    goal = { label = "refine_"; term }
  }

(* With [indicators], each assumption and definition made by a proof step is
   guarded by a fresh Boolean indicator, recorded there with its step (warning
   227). So are the facts that expansion derives from the terms of guarded facts
   only: a step can also be needed for the observations its terms request. *)
let query ?indicators ctx code =
  ctx.poll ();
  let indicated = Hashtbl.create 16 in
  let fresh_indicators steps =
    match indicators with
    | None -> []
    | Some indicators ->
      List.map
        (fun step ->
          let indicator = Symbol.create ~label:"proof step" Bool in
          indicators := (indicator, step) :: !indicators;
          Hashtbl.replace indicated indicator ();
          indicator)
        steps
  in
  let indicator command =
    match indicators, Step_assumptions.find_opt step_assumptions command with
    | Some _, Some steps -> fresh_indicators steps
    | _ -> []
  in
  (* Nested implications, one per indicator, which expansion recognizes. *)
  let guarded indicators p =
    List.fold_left (fun p u -> both Implies (Var u) p) p indicators
  in
  let guard command p = guarded (indicator command) p in
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
  (* A refined value's predicate is re-derived, with fresh names, each time the
     value is used. Definitions are total and unconditional, so a symbol defined
     like an earlier one is replaced by it; an assumption already on the current
     path is then recognized and dropped. *)
  let renamed = Hashtbl.create 64 and defined = Term_table.create 64 in
  let rec rename = function
    | Var symbol as term -> (
      match Hashtbl.find_opt renamed symbol with
      | Some existing -> existing
      | None -> term)
    | term -> map_term_children rename term
  in
  let rename term = if Hashtbl.length renamed = 0 then term else rename term in
  let assumed = Term_table.create 64 in
  (* A path is reachable when [prefix] and [reachable] hold. Branches start from
     [true] under the enclosing path, and a join yields [reachable && (a || b)]
     rather than [(reachable && a') || (reachable && b')]: facts from before a
     branch then remain top-level conjuncts of the negated goal, which the
     solver's preprocessing can substitute instead of rediscovering them in
     every case split. *)
  let rec forward ~prefix code reachable =
    ctx.poll ();
    let added = ref [] in
    let reachable =
      List.fold_left
        (fun reachable -> function
          | Assume p as command ->
            let p = rename p in
            if Term_table.mem assumed p
            then reachable
            else begin
              Term_table.add assumed p ();
              added := p :: !added;
              share (both And reachable (guard command p))
            end
          | Define term as command ->
            (match rename term with
            | App (Eq, [Var symbol; value]) as term
              when not (Hashtbl.mem ctx.observation_definitions symbol) -> (
              match Term_table.find_opt defined value with
              | Some existing -> Hashtbl.replace renamed symbol existing
              | None -> (
                match indicator command with
                | [] ->
                  Term_table.add defined value (Var symbol);
                  definitions := { label = "value"; term } :: !definitions
                | indicators ->
                  (* Not shared with later definitions, which then do not depend
                     on the step. *)
                  definitions
                    := { label = "value"; term = guarded indicators term }
                       :: !definitions))
            | term ->
              definitions
                := { label = "value"; term = guard command term }
                   :: !definitions);
            reachable
          | Assert o ->
            goals
              := (o, both Implies (both And prefix reachable) (rename o.goal))
                 :: !goals;
            reachable
          | Choice (a, b) ->
            let prefix = share (both And prefix reachable) in
            let a = forward ~prefix a (Boolean true) in
            let b = forward ~prefix b (Boolean true) in
            let joined =
              match a, b with
              | Boolean true, _ | _, Boolean true -> Boolean true
              | Boolean false, x | x, Boolean false -> x
              | _ -> both Or a b
            in
            share (both And reachable joined)
          | Check code ->
            ignore (forward ~prefix code reachable);
            reachable)
        reachable (List.rev code)
    in
    List.iter (Term_table.remove assumed) !added;
    reachable
  in
  ignore (forward ~prefix:(Boolean true) code (Boolean true));
  let goals = List.rev !goals in
  let goal =
    { label = "refine_";
      term =
        List.fold_left (fun q (_, goal) -> both And q goal) (Boolean true) goals
    }
  in
  let facts = List.rev !definitions in
  (* Observation equations recorded during VC generation are keyed by terms
     written before renaming; those added during expansion already use the
     renamed terms. *)
  let generated_equations =
    lazy
      (let table = Hashtbl.create (Hashtbl.length ctx.observation_equations) in
       Hashtbl.iter
         (fun key value -> Hashtbl.replace table (rename key) value)
         ctx.observation_equations;
       table)
  in
  let observation_equation term =
    match Hashtbl.find_opt ctx.observation_equations term with
    | Some _ as equation -> equation
    | None when Hashtbl.length renamed = 0 -> None
    | None -> Hashtbl.find_opt (Lazy.force generated_equations) term
  in
  let renamed_equation_steps =
    lazy
      (let table = Term_table.create (Term_table.length equation_steps) in
       Term_table.iter
         (fun key steps -> Term_table.replace table (rename key) steps)
         equation_steps;
       table)
  in
  (* The steps whose evaluation recorded an observation equation or symbol. *)
  let equation_steps_of term =
    match Term_table.find_opt equation_steps term with
    | Some steps -> steps
    | None when Hashtbl.length renamed = 0 -> []
    | None ->
      Option.value ~default:[]
        (Term_table.find_opt (Lazy.force renamed_equation_steps) term)
  in
  let expand ~slice goal =
    let raw = { datatypes = []; symbols = []; functions = []; facts; goal } in
    let facts =
      if slice then (slice_goal ~rename ctx raw goal.term).facts else facts
    in
    let definitions = ref (List.rev facts) in
    (* With indicators, a fact derived while visiting a guarded fact is guarded
       by the same indicators, since only that step requested it. Unguarded
       facts are visited first, so what they request is not guarded. Without
       indicators, every guard is empty. *)
    let guard = ref [] in
    let with_guard g f =
      let saved = !guard in
      guard := g;
      Fun.protect ~finally:(fun () -> guard := saved) f
    in
    let union a b = List.sort_uniq compare (a @ b) in
    let add label term =
      let term =
        match !guard with
        | [] -> term
        | g ->
          both Implies
            (List.fold_left (fun c u -> both And c (Var u)) (Boolean true) g)
            term
      in
      definitions := { label; term } :: !definitions
    in
    (* These edges request observations; they do not assert array equality.
       Equality still follows from the original, possibly guarded facts. *)
    let array_equalities = Hashtbl.create 16 in
    let array_observations = Hashtbl.create 16 in
    let pending = Queue.create () in
    (* Equalities through subarrays can revisit an array at shifted indices.
       Stop expanding at a fixed budget and retain uninterpreted
       observations. *)
    let observation_budget = ref 1024 in
    let propagated = Term_table.create 64 in
    let propagate g observe alias =
      if !observation_budget > 0
      then begin
        let term = with_guard g (fun () -> observe alias) in
        if not (Term_table.mem propagated term)
        then begin
          Term_table.add propagated term ();
          decr observation_budget;
          Queue.add (term, g) pending
        end
      end
    in
    let entries table key =
      Option.value (Hashtbl.find_opt table key) ~default:[]
    in
    let relate left right =
      let previous = entries array_equalities left in
      let same_origin =
        rename (expose_head ctx left) = rename (expose_head ctx right)
        && Option.is_some (iarray_origin ctx left)
      in
      if
        left <> right && (not same_origin)
        && not (List.exists (fun (known, _) -> known = right) previous)
      then begin
        let g = !guard in
        Hashtbl.replace array_equalities left ((right, g) :: previous);
        List.iter
          (fun (observe, g') -> propagate (union g g') observe right)
          (entries array_observations left)
      end
    in
    let observation_function label fn =
      Hashtbl.find_opt ctx.function_cache
        (label, Function.arguments fn, Function.result fn)
      = Some fn
    in
    let logical_keys = Hashtbl.create 8 in
    let seen = Term_table.create 16 and symbols = ref [] in
    let first_pass = ref (Option.is_some indicators) and deferred = ref [] in
    let rec visit term =
      if not (Term_table.mem seen term)
      then begin
        Term_table.add seen term ();
        match term with
        | App (Implies, [(Var indicator as var); fact])
          when Hashtbl.mem indicated indicator ->
          visit var;
          if !first_pass
          then deferred := (indicator, fact) :: !deferred
          else with_guard (union !guard [indicator]) (fun () -> visit fact)
        | _ -> expand_term term
      end
    and expand_term term =
      begin match term with
      | Call (fn, []) when Hashtbl.mem ctx.string_literals fn ->
        let tag =
          intern_function ctx "string literal identity" [term_sort term] Int
        in
        let id = Hashtbl.find ctx.string_literals fn in
        let axiom =
          both Eq (Call (tag, [term])) (Big_integer (string_of_int id))
        in
        add "string literal" axiom;
        Queue.add (axiom, !guard) pending
      | Call (fn, [key]) when observation_function "Logical_map.key" fn ->
        let info =
          Hashtbl.fold
            (fun map_sort info found ->
              if
                Hashtbl.find_opt ctx.map_class_sorts map_sort
                = Some (Function.result fn)
              then Some info
              else found)
            ctx.finite_maps None
        in
        Option.iter
          (fun info ->
            let previous = entries logical_keys fn in
            Hashtbl.replace logical_keys fn (key :: previous);
            List.iter
              (fun other ->
                let equivalent = both Eq term (Call (fn, [other])) in
                List.iter
                  (fun (left, right) ->
                    Option.iter
                      (fun equality ->
                        let axiom = both Eq equality equivalent in
                        add "logical map key equality" axiom;
                        Queue.add (axiom, !guard) pending)
                      (info.equal left right))
                  [key, other; other, key])
              (key :: previous))
          info
      | Call (fn, [_]) when observation_function "Logical_map.cardinal" fn ->
        let axiom = App (Int_le, [Big_integer "0"; term]) in
        add "logical map cardinality" axiom;
        Queue.add (axiom, !guard) pending
      | Call (fn, [pointer]) when observation_function "Pref.location" fn ->
        (* A left inverse enforces injectivity with one equation per pointer. *)
        let inverse =
          intern_function ctx "Pref.pointer"
            [Function.result fn]
            (term_sort pointer)
        in
        let axiom = both Eq (Call (inverse, [term])) pointer in
        add "pref identity" axiom;
        Queue.add (axiom, !guard) pending
      | Call (fn, [array])
        when observation_function "Iarray.length" fn
             && Option.is_none (iarray_origin ctx array)
             &&
             match expose_head ctx array with
             | App (Ite, _) -> false
             | _ -> true ->
        (* Every iarray, ghost or real, is built by a literal, a partial
           allocation that raises above [Sys.max_array_length], a total
           operation that keeps or shrinks a length, or a view of a real array
           or string, so its length is at most 2^57. Arrays built from others
           get their lengths from those, and are not bounded here: their
           construction may not have returned. *)
        let axiom =
          both And
            (both Le (Integer 0L) term)
            (both Le term (Integer 1152921504606846975L))
        in
        add "iarray length bound" axiom;
        Queue.add (axiom, !guard) pending
      | _ -> ()
      end;
      begin match term with
      | App (Eq, [left; right])
        when is_iarray_sort ctx.encoding (term_sort left)
             || Hashtbl.mem ctx.pref_heaps (term_sort left) ->
        relate left right;
        relate right left
      | _ -> ()
      end;
      let observations =
        match term with
        | Call (fn, [heap; key]) when Hashtbl.mem ctx.pref_observers fn ->
          [(heap, fun alias -> pref_observe ctx 128 fn alias key)]
        | Call (fn, [left; right]) when observation_function "Pref.disjoint" fn
          ->
          [ (left, fun alias -> pref_disjoint ctx 64 alias right);
            (right, fun alias -> pref_disjoint ctx 64 left alias) ]
        | Call (fn, [array; index]) when observation_function "Iarray.get" fn ->
          [ ( array,
              fun alias ->
                iarray_get ctx (term_sort alias) (Function.result fn) alias
                  index ) ]
        | Call (fn, [array]) when observation_function "Iarray.length" fn ->
          [(array, fun alias -> iarray_length ctx (term_sort alias) alias)]
        | Call (fn, [map]) when observation_function "Logical_map.cardinal" fn
          ->
          [(map, fun alias -> logical_map_cardinal ctx 128 alias)]
        | _ -> []
      in
      List.iter
        (fun (array, read) ->
          let observe alias =
            let value = read alias in
            if is_iarray_sort ctx.encoding (term_sort term)
            then begin
              relate term value;
              relate value term
            end;
            value
          in
          let g = !guard in
          Hashtbl.replace array_observations array
            ((observe, g) :: entries array_observations array);
          List.iter
            (fun (right, g') -> propagate (union g g') observe right)
            (entries array_equalities array))
        observations;
      Option.iter
        (fun value ->
          with_guard
            (union !guard (declared (equation_steps_of term)))
            (fun () -> define "iarray observation" term value))
        (observation_equation term);
      match term with
      | Var symbol ->
        symbols := symbol :: !symbols;
        Option.iter
          (fun value ->
            with_guard
              (union !guard
                 (declared
                    (Option.value ~default:[]
                       (Hashtbl.find_opt observation_steps symbol))))
              (fun () -> define "observation" term value))
          (Hashtbl.find_opt ctx.observation_definitions symbol)
      | App (_, args) | Call (_, args) | Construct (_, args) ->
        List.iter visit args
      | Is (_, arg) | Select (_, _, arg) -> visit arg
      | _ -> ()
    (* Fresh indicators for the steps that recorded an observation, declared in
       the query. *)
    and declared steps =
      let indicators = fresh_indicators steps in
      List.iter (fun u -> visit (Var u)) indicators;
      indicators
    and define label term value =
      let equation = both Eq term (rename value) in
      add label equation;
      visit equation
    in
    let drain () =
      while not (Queue.is_empty pending) do
        let term, g = Queue.take pending in
        with_guard g (fun () -> visit term)
      done
    in
    List.iter (fun f -> visit f.term) facts;
    visit goal.term;
    drain ();
    first_pass := false;
    List.iter
      (fun (indicator, fact) ->
        with_guard [indicator] (fun () -> visit fact);
        drain ())
      (List.rev !deferred);
    { datatypes = List.rev ctx.datatypes;
      symbols = List.rev !symbols;
      functions = List.rev ctx.functions;
      facts = List.rev !definitions;
      goal
    }
  in
  (* Preserve explicit observation hints in the initial batch. Early slicing is
     only needed when regenerating smaller individual retry queries. *)
  let slice query term = slice_goal ~rename ctx query term in
  ( slice (expand ~slice:false goal) goal.term,
    goals,
    fun term -> slice (expand ~slice:true { label = "refine_"; term }) term )

(* [proved] receives the obligations of each query that was proved. *)
let verify_batch ?(proved = fun _ -> ()) ctx prove code =
  let query, goals, expand = query ctx code in
  let prove_one (o : obligation) q =
    match prove ~batch:false o.loc q with
    | () -> proved [o]
    | exception Unproved error ->
      let s = { empty with omitted_premises = o.omitted_premises } in
      let origin =
        if o.origin.loc_ghost || o.origin = o.loc
        then []
        else
          (* A refinement from another unit is named by its file alone; the
             directory it was compiled in means nothing to the reader. *)
          let loc =
            if o.origin.loc_start.pos_fname = o.loc.loc_start.pos_fname
            then o.origin
            else
              let base (p : Lexing.position) =
                { p with pos_fname = Filename.basename p.pos_fname }
              in
              { o.origin with
                loc_start = base o.origin.loc_start;
                loc_end = base o.origin.loc_end
              }
          in
          [Location.msg ~loc "The refinement is stated here."]
      in
      let main =
        match o.headline with
        | None -> error.main
        | Some headline ->
          Location.msg ~loc:error.main.loc "@[<v>%s@,%a@]" headline
            Format_doc.pp_doc error.main.txt
      in
      raise
        (Location.Error
           { error with
             main;
             sub =
               error.sub @ origin @ Option.to_list o.note @ o.context
               @ omitted_premise_messages s
           })
  in
  (* The conjuncts of a refinement are consecutive goals of one group. A group
     is proved as one query, which is as cheap as proving the whole refinement;
     only if it fails is each conjunct proved alone, to name the one that
     fails. *)
  let rec groups = function
    | [] -> []
    | ((o : obligation), _) :: _ as goals ->
      let members, rest =
        List.partition (fun ((m : obligation), _) -> m.group = o.group) goals
      in
      members :: groups rest
  in
  let prove_group members query =
    match members with
    | [(o, _)] -> prove_one o (Lazy.force query)
    | (first, _) :: _ -> (
      match prove ~batch:false first.loc (Lazy.force query) with
      | () -> proved (List.map fst members)
      | exception Unproved _ ->
        (* If every conjunct is proved alone, the refinement holds. *)
        List.iter (fun (o, term) -> prove_one o (expand term)) members)
    | [] -> ()
  in
  let conjunction members =
    List.fold_left (fun q (_, goal) -> both And q goal) (Boolean true) members
  in
  match groups goals with
  | [] -> ()
  | [members] -> prove_group members (lazy query)
  | ((first, _) :: _) :: _ as groups -> (
    match prove ~batch:true first.loc query with
    | () -> proved (List.map fst goals)
    | exception Unproved _ ->
      List.iter
        (fun members ->
          prove_group members (lazy (expand (conjunction members))))
        groups)
  | [] :: _ -> ()

(* Whether [code] assumes facts of a proof step not yet known to be used. *)
let rec has_unused_step code =
  List.exists
    (function
      | (Assume _ | Define _) as command -> (
        match Step_assumptions.find_opt step_assumptions command with
        | Some steps ->
          List.exists (fun step -> not step.Vox_proof_steps.used) steps
        | None -> false)
      | Choice (a, b) -> has_unused_step a || has_unused_step b
      | Check code -> has_unused_step code
      | Assert _ -> false)
    code

(* Prove [code]; with warning 227 enabled, then, after the pass's other proofs,
   prove each query that was proved again with indicators on the proof steps'
   assumptions, to find the steps that its proof used. *)
let verify_steps ctx prove code =
  match !Vox_proof_steps.active with
  | None -> verify_batch ctx prove code
  | Some checker ->
    let proved = ref [] in
    verify_batch
      ~proved:(fun obligations -> proved := obligations :: !proved)
      ctx prove code;
    Vox_proof_steps.defer @@ fun () ->
    if has_unused_step code
    then begin
      let indicators = ref [] in
      let query, goals, expand = query ~indicators ctx code in
      List.iter
        (fun obligations ->
          let selected =
            List.filter (fun (o, _) -> List.memq o obligations) goals
          in
          let query =
            if List.compare_lengths selected goals = 0
            then query
            else
              expand
                (List.fold_left
                   (fun q (_, goal) -> both And q goal)
                   (Boolean true) selected)
          in
          let loc =
            match obligations with
            | (o : obligation) :: _ -> o.loc
            | [] -> Location.none
          in
          Vox_proof_steps.check checker loc query !indicators)
        (List.rev !proved)
    end

let steps_pass ?unused_steps ~report f =
  Fun.protect
    ~finally:(fun () ->
      Step_assumptions.reset step_assumptions;
      Hashtbl.reset observation_steps;
      Term_table.reset equation_steps;
      Hashtbl.reset value_steps;
      Hashtbl.reset let_bound_functions;
      Hashtbl.reset argument_steps)
    (fun () -> Vox_proof_steps.pass ?checker:unused_steps ~report f)

let context ~poll ~prove ~verify_introductions =
  { poll;
    encoding = Vox_encoding.create_context ();
    datatypes = [];
    functions = [];
    function_cache = Hashtbl.create 32;
    string_literals = Hashtbl.create 16;
    set_origins = Hashtbl.create 16;
    set_class_sorts = Hashtbl.create 8;
    set_membership = Hashtbl.create 32;
    observation_definitions = Hashtbl.create 32;
    shared_observations = Term_table.create 32;
    map_origins = Hashtbl.create 16;
    iarray_origins = Hashtbl.create 16;
    iarray_constructors = Hashtbl.create 16;
    observation_equations = Hashtbl.create 32;
    iarray_lengths = Hashtbl.create 16;
    iarray_reads = Hashtbl.create 16;
    map_class_sorts = Hashtbl.create 8;
    pref_heaps = Hashtbl.create 8;
    finite_maps = Hashtbl.create 8;
    pref_constructors = Hashtbl.create 8;
    pref_observers = Hashtbl.create 8;
    free = Path.Map.empty;
    module_aliases = Path.Map.empty;
    in_goal = false;
    argument_values = Path.Map.empty;
    batches = [];
    named_terms = Hashtbl.create 32;
    symbolic = Symbolic_keys.create 16;
    prove;
    verify_introductions;
    check_call = (fun _ _ _ _ -> ());
    unfolding = []
  }

(* A total, stateless function is a function of its arguments, also in units
   that are never verified, so a shift declared total must have its count in [0,
   63] by its type, as in [Int.Refined]. The check is syntactic, so it needs no
   solver: some conjunct of the count's refinement bounds it below by a constant
   at least 0, and another above by a constant at most 63. *)
let check_total_shift (vd : value_description) =
  let total =
    match vd.val_modal_info with
    | Valmi_str_primitive modes ->
      modes.mode_modes.totality = Some Mode.Totality.Const.Total
    | Valmi_sig_value _ -> false
  in
  let shift =
    match vd.val_prim with
    | ("%lslint" | "%lsrint" | "%asrint") :: _ -> true
    | _ -> false
  in
  if total && shift
  then begin
    let env = vd.val_desc.ctyp_env in
    let rec desc ty =
      match get_desc (Ctype.expand_head env ty) with
      | Tpoly (ty, []) -> desc ty
      | d -> d
    in
    let count_refinement =
      match desc vd.val_val.val_type with
      | Tarrow (_, _, rest, _) -> (
        match desc rest with
        | Tarrow (_, count, _, _) -> (
          match desc count with Trefine r -> Some r | _ -> None)
        | _ -> None)
      | _ -> None
    in
    let bounded =
      match count_refinement with
      | None -> false
      | Some r ->
        let rec conjuncts p =
          match p.rexp_desc with
          | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, [(_, a); (_, b)])
            when primitive env path = Some ("%sequand", 2) ->
            conjuncts a @ conjuncts b
          | _ -> [p]
        in
        let constant p =
          match p.rexp_desc with
          | Rexp_constant c -> (
            match c.Parsetree.pconst_desc with
            | Parsetree.Pconst_integer (n, None) -> Int64.of_string_opt n
            | _ -> None)
          | _ -> None
        in
        let is_count p =
          match p.rexp_desc with
          | Rexp_var id -> Ident.same id r.ref_binder
          | _ -> false
        in
        (* [`Lower c] for [c <= n], [`Upper c] for [n <= c]. *)
        let bound p =
          match p.rexp_desc with
          | Rexp_apply ({ rexp_desc = Rexp_ident path; _ }, [(_, a); (_, b)])
            -> (
            let oriented =
              match primitive env path with
              | Some (("%lessthan" | "%ltint"), 2) -> Some (true, a, b)
              | Some (("%lessequal" | "%leint"), 2) -> Some (false, a, b)
              | Some (("%greaterthan" | "%gtint"), 2) -> Some (true, b, a)
              | Some (("%greaterequal" | "%geint"), 2) -> Some (false, b, a)
              | _ -> None
            in
            match oriented with
            | Some (strict, small, large) -> (
              match constant small, constant large with
              | Some c, None when is_count large ->
                Some (`Lower (if strict then Int64.succ c else c))
              | None, Some c when is_count small ->
                Some (`Upper (if strict then Int64.pred c else c))
              | _ -> None)
            | None -> None)
          | _ -> None
        in
        let bounds = List.filter_map bound (conjuncts r.ref_pred) in
        let lower = function `Lower c -> c >= 0L | `Upper _ -> false in
        let upper = function `Upper c -> c <= 63L | `Lower _ -> false in
        List.exists lower bounds && List.exists upper bounds
    in
    if not bounded
    then
      Location.raise_errorf ~loc:vd.val_loc
        "A shift declared total must refine its count to [0, 63], as in \
         Int.Refined: {n : int | 0 <= n && n <= 63}. OCaml leaves other counts \
         unspecified, and their results differ between evaluations."
  end

let generate ?(poll = fun () -> ()) ?unused_steps ?interface ~prove str =
  steps_pass ?unused_steps ~report:true @@ fun () ->
  poll ();
  let declarations =
    { Tast_iterator.default_iterator with
      value_description =
        (fun self vd ->
          check_total_shift vd;
          Tast_iterator.default_iterator.value_description self vd)
    }
  in
  declarations.structure declarations str;
  let exception Has_obligation in
  let scan =
    { Tast_iterator.default_iterator with
      expr =
        (fun self e ->
          poll ();
          if
            Option.is_some (intro_loc e)
            || List.exists
                 (function Texp_subsumption _, _, _ -> true | _ -> false)
                 e.exp_extra
          then raise Has_obligation;
          (* A unit without obligations uses none of its proof steps. *)
          if
            Vox_proof_steps.enabled ()
            &&
            let source =
              List.fold_left
                (fun ty -> function
                  | Texp_refinement { source; _ }, _, _ -> source | _ -> ty)
                e.exp_type e.exp_extra
            in
            Option.is_some (step_kind e source)
            ||
            match e.exp_desc with
            | Texp_function { params; _ } ->
              List.exists
                (fun p ->
                  match p.fp_kind with
                  | Tparam_pat pat | Tparam_optional_default (pat, _, _) -> (
                    match
                      get_desc (Ctype.expand_head pat.pat_env pat.pat_type)
                    with
                    | Trefine _ -> true
                    | _ -> false))
                params
            | _ -> false
          then raise Has_obligation;
          Tast_iterator.default_iterator.expr self e)
    }
  in
  match
    if Option.is_some interface || Verification.has_refinement_sites ()
    then raise Has_obligation;
    scan.structure scan str
  with
  | () -> ()
  | exception Has_obligation ->
    let ctx = context ~poll ~prove ~verify_introductions:true in
    let result, _, warnings =
      Builtin_attributes.warning_scope [] (fun () ->
          let result, value = structure ctx empty str in
          (* The unit's own interface is checked in its final state; sites the
             verifier did not reach are checked with no facts. *)
          Option.iter
            (discharge_site ~root:(Root_structure str) ctx result)
            interface;
          List.iter
            (discharge_site ~root:Root_opaque ctx empty)
            (Verification.unconsumed_refinement_sites ());
          result, value, Warnings.backup ())
    in
    let with_warnings state f =
      let saved = Warnings.backup () in
      Warnings.restore state;
      Fun.protect ~finally:(fun () -> Warnings.restore saved) f
    in
    List.iter
      (fun (state, code) ->
        with_warnings state (fun () -> verify_steps ctx ctx.prove code))
      (List.rev ctx.batches);
    with_warnings warnings (fun () -> verify_steps ctx prove result.code)

let check_termination ?unused_steps ~poll ~prove ~self ~fn ~measure () =
  steps_pass ?unused_steps ~report:false @@ fun () ->
  poll ();
  (* A recursive function is bound by [let]. *)
  if Vox_proof_steps.enabled ()
  then Hashtbl.replace let_bound_functions fn.exp_loc ();
  let params, body = Recursive_function.parameters fn in
  let ctx = context ~poll ~prove ~verify_introductions:false in
  (* The typer checked the measure as a total, stateless expression over
     immutable parameters, so it denotes a function of their values and may call
     total functions. Refinement introductions are not verified in this pass, so
     the measure may not contain any: a precondition assumed but not checked
     could make a callee's postcondition vacuous. *)
  let scan =
    { Tast_iterator.default_iterator with
      expr =
        (fun it e ->
          (match intro_loc e with
          | Some loc ->
            Location.raise_errorf ~loc
              "Unsupported decreases expression: a measure cannot contain a \
               refinement introduction"
          | None -> ());
          Tast_iterator.default_iterator.expr it e)
    }
  in
  scan.expr scan measure;
  (* A tuple is a lexicographic measure. *)
  let components =
    match measure.exp_desc with
    | Texp_tuple (components, _) -> List.map snd components
    | _ -> [measure]
  in
  List.iter
    (fun e ->
      match sort ctx.encoding e.exp_env e.exp_type with
      | Some (Int63 | Int) -> ()
      | Some (Bool | Opaque _ | Datatype _) | None ->
        Location.raise_errorf ~loc:e.exp_loc
          "Unsupported decreases expression: expected int or Bigint.t")
    components;
  let evaluate s =
    let s, values =
      List.fold_left
        (fun (s, values) e ->
          let s, value = expression ctx s e in
          s, (e.exp_loc, value) :: values)
        (s, []) components
    in
    ( s,
      if s.dead
      then []
      else List.rev_map (fun (loc, value) -> required loc value) values )
  in
  let entry =
    List.fold_left
      (fun s (id, pat) ->
        let value = fresh ctx pat.pat_env pat.pat_type (Ident.name id) in
        let step = argument_step fn pat in
        register_value step value;
        let s, condition =
          in_step step s (fun () ->
              merge_patterns s (pattern ctx (bind s id value) value pat))
        in
        branch s condition)
      empty params
  in
  let entry, entry_measure = evaluate entry in
  if not entry.dead
  then begin
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
          List.fold_left2 (fun s (id, _) value -> bind s id value) s params args
        in
        let checked, value = evaluate call_state in
        (* Each component is bounded below when it decreases: an int by its
           range, a Bigint.t by zero. *)
        let rec decreases = function
          | [] -> Boolean false
          | (value, entry) :: rest ->
            let smaller =
              match term_sort entry with
              | Int63 -> both Lt value entry
              | Int ->
                both And
                  (both Int_ge value (Big_integer "0"))
                  (both Int_lt value entry)
              | Bool | Opaque _ | Datatype _ -> assert false
            in
            if rest = []
            then smaller
            else
              App
                (Or, [smaller; both And (both Eq value entry) (decreases rest)])
        in
        if not checked.dead
        then
          verify_steps ctx prove
            (Assert
               { loc = call.exp_loc;
                 (* The error already points at the decreases attribute. *)
                 origin = call.exp_loc;
                 goal = decreases (List.combine value entry_measure);
                 omitted_premises = checked.omitted_premises;
                 group = fresh_group ();
                 note = None;
                 headline = None;
                 context = []
               }
            :: checked.code)
      | _ -> ()
    in
    ctx.check_call <- check_call;
    ignore (expression ctx entry body)
  end
