open Types
open Typedtree
open Vox_smt
open Vox_encoding

type function_value =
  { label : string;
    instances : Function.t list ref;
    primitive : (string * int) option;
    total : bool
  }

type value =
  | Scalar of term
  | Function of function_value

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
  { encoding : Vox_encoding.context;
    mutable datatypes : datatype_declaration list;
    mutable symbols : Symbol.t list;
    mutable functions : Function.t list;
    function_cache : (string * sort list * sort, Function.t) Hashtbl.t;
    set_origins : (Function.t, set_origin) Hashtbl.t;
    set_class_sorts : (sort, sort) Hashtbl.t;
    map_origins : (Function.t, map_origin) Hashtbl.t;
    map_class_sorts : (sort, sort) Hashtbl.t;
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

let rec added_prefix ~base = function
  | current when current == base -> []
  | item :: rest -> item :: added_prefix ~base rest
  | [] -> Misc.fatal_error "VC: state does not extend its input"

let guard_added_facts ~base guard s =
  List.map
    (fun fact -> { fact with term = both Implies guard fact.term })
    (added_prefix ~base:base.facts s.facts)

let added_omitted_premises ~base s =
  added_prefix ~base:base.omitted_premises s.omitted_premises

let merge_predicate_branches base branches =
  let facts =
    List.concat_map (fun (guard, s) -> guard_added_facts ~base guard s) branches
    @ base.facts
  in
  let omitted_premises =
    List.concat_map (fun (_, s) -> added_omitted_premises ~base s) branches
    @ base.omitted_premises
  in
  { base with facts; omitted_premises }

let predicate_short_circuit eval loc ~is_and s a b =
  paths (eval s a) (fun s a ->
      let a = required loc a in
      if a = Boolean (not is_and)
      then [s, scalar_value a]
      else
        match eval s b with
        | [(right, b)] ->
          let guard = if is_and then a else not_ a in
          let state = merge_predicate_branches s [guard, right] in
          let b = required loc b in
          [state, scalar_value (both (if is_and then And else Or) a b)]
        | rights ->
          List.map
            (fun (right, value) ->
              branch right (if is_and then a else not_ a), value)
            rights
          @ [ ( branch s (if is_and then not_ a else a),
                scalar_value (Boolean (not is_and)) ) ])

let predicate_if eval loc s condition ifso ifnot =
  paths (eval s condition) (fun s condition ->
      let condition = required loc condition in
      match condition with
      | Boolean true -> eval s ifso
      | Boolean false -> eval s ifnot
      | _ -> (
        let ifso_outcomes = eval s ifso in
        let ifnot_outcomes = eval s ifnot in
        match ifso_outcomes, ifnot_outcomes with
        | [(ifso_state, ifso)], [(ifnot_state, ifnot)] ->
          let state =
            merge_predicate_branches s
              [condition, ifso_state; not_ condition, ifnot_state]
          in
          let ifso = required loc ifso in
          let ifnot = required loc ifnot in
          [state, scalar_value (App (Ite, [condition; ifso; ifnot]))]
        | _ ->
          List.map (fun (s, value) -> branch s condition, value) ifso_outcomes
          @ List.map
              (fun (s, value) -> branch s (not_ condition), value)
              ifnot_outcomes))

let disjunction terms = List.fold_left (both Or) (Boolean false) terms

let guarded_case eval loc s matched guard body rest =
  let matched_condition = disjunction (List.map snd matched) in
  let selected =
    List.concat_map
      (fun (matched, condition) ->
        let guards =
          match guard with
          | None -> [branch matched condition, scalar_value (Boolean true)]
          | Some g -> eval (branch matched condition) g
        in
        paths guards (fun s g ->
            let g = required loc g in
            eval (branch s g) body @ rest (branch s (not_ g))))
      matched
  in
  selected @ rest (branch s (not_ matched_condition))

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
  match data_of_type ctx env ty, scalar value with
  | Some { kind = Record_data constructor; _ }, Some value ->
    begin match
      List.find_mapi
        (fun index (label, _) -> if label = name then Some index else None)
        (Constructor.fields constructor)
    with
    | Some index -> scalar_value (Select (constructor, index, value))
    | None -> None
    end
  | _ -> None

let record_value ctx env ty base fields =
  match data_of_type ctx env ty with
  | Some { kind = Record_data constructor; _ } ->
    let field name index =
      match List.assoc_opt name fields with
      | Some value -> scalar value
      | None ->
        Option.map (fun base -> Select (constructor, index, base)) (scalar base)
    in
    let values =
      List.mapi
        (fun index (name, _) -> field name index)
        (Constructor.fields constructor)
    in
    begin match Misc.Stdlib.List.map_option Fun.id values with
    | Some values -> scalar_value (Construct (constructor, values))
    | None -> None
    end
  | _ -> None

let fresh_sort ctx label sort =
  register_sort ctx sort;
  let symbol = Symbol.create ~label sort in
  ctx.symbols <- symbol :: ctx.symbols;
  Var symbol

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

let iarray_length ctx iarray_sort array =
  let function_ = intern_function ctx "Iarray.length" [iarray_sort] Int63 in
  Call (function_, [array])

let iarray_get ctx iarray_sort element_sort array index =
  let function_ =
    intern_function ctx "Iarray.get" [iarray_sort; Int63] element_sort
  in
  Call (function_, [array; index])

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
  let class_ = set_class ctx set_sort element in
  let unknown () =
    let function_ =
      intern_function ctx "Set.mem" [term_sort class_; set_sort] Bool
    in
    Call (function_, [class_; set])
  in
  match set with
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
  match map with
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
  match map with
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
  | Some (iarray_sort, element_sort) ->
    let array = fresh_sort ctx "iarray" iarray_sort in
    let s =
      fact s "iarray literal"
        (both Eq
           (iarray_length ctx iarray_sort array)
           (Integer (Int64.of_int (List.length values))))
    in
    let s =
      match element_sort with
      | None -> s
      | Some element_sort ->
        Misc.Stdlib.List.fold_lefti
          (fun index s value ->
            match scalar value with
            | Some element when term_sort element = element_sort ->
              fact s "iarray literal"
                (both Eq
                   (iarray_get ctx iarray_sort element_sort array
                      (Integer (Int64.of_int index)))
                   element)
            | Some _ | None -> s)
          s values
    in
    s, scalar_value array
  | None -> s, None

let fresh ?primitive ctx env ty label =
  match sort ctx.encoding env ty with
  | Some sort -> scalar_value (fresh_sort ctx label sort)
  | None ->
    begin match get_desc (Ctype.expand_head env ty) with
    | Tarrow _ ->
      Some (Function { label; instances = ref []; primitive; total = false })
    | _ -> None
    end

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

let reinstantiate_nullary_constructor ctx env ty path constructor =
  match Subst.Lazy.force_value_description (Env.find_value path env) with
  | source
    when Constructor.fields constructor = []
         && same_nominal_data_type env source.val_type ty ->
    construct ctx env ty (Constructor.label constructor) []
  | _ -> None
  | exception Not_found -> None

let instantiate_path ctx env ty path value =
  match value, sort ctx.encoding env ty with
  | Some (Scalar term), Some expected when term_sort term <> expected ->
    begin match term with
    | Construct (constructor, []) ->
      begin match
        reinstantiate_nullary_constructor ctx env ty path constructor
      with
      | Some _ as value -> value
      | None -> symbolic_path ctx env ty path
      end
    | _ -> symbolic_path ctx env ty path
    end
  | _ -> value

let lookup ctx s env ty path =
  let path = Env.normalize_value_path None env path in
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
        match Path.Map.find_opt path ctx.free with
        | Some value -> instantiate_path ctx env ty path value
        | None ->
          let value =
            fresh ?primitive:(primitive env path) ctx env ty (Path.name path)
          in
          ctx.free <- Path.Map.add path value ctx.free;
          value)))

let iarray_call ctx value =
  match scalar value with
  | None -> None
  | Some value ->
    let sort = term_sort value in
    if is_iarray_sort ctx.encoding sort then Some (sort, value) else None

let operation ctx env function_type result_type name args =
  match name, args with
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

let function_call ctx env ty fn args =
  match fn, signature ctx.encoding env ty (List.length args) with
  | Some (Function fn), Some (arguments, result) ->
    List.iter (register_sort ctx) (result :: arguments);
    begin match Misc.Stdlib.List.map_option scalar args with
    | Some args when List.map term_sort args = arguments ->
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
    | Some _ | None -> None
    end
  | _ -> None

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
  | None -> None

let stored_primitive syntax = function
  | Some (Function { primitive = Some _ as primitive; _ }) -> primitive
  | _ -> syntax

let constant c = scalar_option (Vox_encoding.constant c)

let rconstant c = scalar_option (Vox_encoding.rconstant c)

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
      | Some (("%set_empty" | "%map_empty"), 0) ->
        return (lookup ctx s env e.rexp_type path)
      | Some (_, 0) -> unsupported e.rexp_loc
      | None | Some (_, _) -> return (lookup ctx s env e.rexp_type path)
      end
    | Rexp_constant c ->
      return (scalar_value (required e.rexp_loc (rconstant c)))
    | Rexp_tuple components ->
      paths
        (arguments_right_to_left (fun s (_, e) -> eval s e) s components)
        (fun s values ->
          [ ( s,
              scalar_value
                (required e.rexp_loc (construct ctx env e.rexp_type "" values))
            ) ])
    | Rexp_construct (path, args) ->
      paths (arguments_right_to_left eval s args) (fun s values ->
          let value =
            match values with
            | [] -> rconstructor ctx env e.rexp_type path
            | _ ->
              begin match path_constructor_name path with
              | Some name -> construct ctx env e.rexp_type name values
              | None -> None
              end
          in
          [s, scalar_value (required e.rexp_loc value)])
    | Rexp_record (fields, extended) ->
      let bases =
        match extended with
        | None -> [s, None]
        | Some extended -> eval s extended
      in
      paths bases (fun s base ->
          paths
            (arguments_right_to_left
               (fun s (_, _, field) -> eval s field)
               s fields)
            (fun s values ->
              let fields =
                List.map2 (fun (_, name, _) value -> name, value) fields values
              in
              let value = record_value ctx env e.rexp_type base fields in
              [s, scalar_value (required e.rexp_loc value)]))
    | Rexp_field (record_exp, _, name) ->
      paths (eval s record_exp) (fun s record ->
          let value = select_field ctx env record_exp.rexp_type name record in
          [s, scalar_value (required e.rexp_loc value)])
    | Rexp_apply (fn, args) ->
      let prim =
        match fn.rexp_desc with
        | Rexp_ident path -> primitive env path
        | _ -> None
      in
      begin match prim, args with
      | Some ((("%sequand" | "%sequor") as name), 2), [(_, a); (_, b)] ->
        predicate_short_circuit eval e.rexp_loc ~is_and:(name = "%sequand") s a
          b
      | _ ->
        paths
          (arguments_right_to_left (fun s (_, e) -> eval s e) s args)
          (fun s args ->
            paths (eval s fn) (fun s value ->
                let prim = stored_primitive prim value in
                let result =
                  apply_function ctx env fn.rexp_type e.rexp_type prim value
                    args ~total:true
                in
                let s =
                  match prim with
                  | Some ("%array_length", 1) -> normal_iarray_length ctx args s
                  | Some ((("%set_find" | "%set_refined_find") as name), 2) ->
                    normal_set_find ctx name args result s
                  | Some ((("%map_find" | "%map_refined_find") as name), 2) ->
                    normal_map_find ctx name args s
                  | _ -> s
                in
                [s, scalar_value (required e.rexp_loc result)]))
      end
    | Rexp_logical_equal (left, right) ->
      paths (eval s right) (fun s right ->
          paths (eval s left) (fun s left ->
              let left = required e.rexp_loc left in
              let right = required e.rexp_loc right in
              if
                sort_has_unsupported_logical_equality ctx.encoding
                  (term_sort left)
              then unsupported e.rexp_loc
              else [s, scalar_value (both Eq left right)]))
    | Rexp_ifthenelse (c, t, Some f) -> predicate_if eval e.rexp_loc s c t f
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
  | Rpat_any -> [s, Boolean true]
  | Rpat_var id -> [bind s id value, Boolean true]
  | Rpat_alias (p, id) -> predicate_pattern ctx env (bind s id value) value p
  | Rpat_constant c ->
    [s, both Eq (required p.rpat_loc value) (required p.rpat_loc (rconstant c))]
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
               (scalar_value (Select (constructor, index, value)))
               pattern))
        outcomes)
    [s, Is (constructor, value)]
    patterns

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
    context -> state -> value option -> k general_pattern -> (state * term) list
    =
 fun ctx s value p ->
  match p.pat_desc with
  | Tpat_any -> [s, Boolean true]
  | Tpat_var { id; mode; _ } -> [bind s id (at_mode mode value), Boolean true]
  | Tpat_alias { pattern = p; id; _ } -> pattern ctx (bind s id value) value p
  | Tpat_value p -> pattern ctx s value (p :> Typedtree.pattern)
  | Tpat_constant c ->
    begin match scalar value, scalar (constant c) with
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
      | _ -> pattern_fallback ctx s p
      end
    end
  | Tpat_record (fields, _, _) ->
    begin match data_of_type ctx p.pat_env p.pat_type, scalar value with
    | Some { kind = Record_data constructor; _ }, Some value ->
      let patterns =
        List.map
          (fun (_, label, pattern) -> label.Data_types.lbl_pos, pattern)
          fields
      in
      pattern_selected_fields ctx s value constructor patterns
    | _ -> pattern_fallback ctx s p
    end
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
               (scalar_value (Select (constructor, index, value)))
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
                  { datatypes = List.rev ctx.datatypes;
                    symbols = List.rev ctx.symbols;
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
    [s, value]
  | Texp_constant c -> [s, constant c]
  | Texp_tuple (components, _) ->
    paths
      (arguments_right_to_left (fun s (_, e) -> eval s e) s components)
      (fun s values ->
        [s, opaque_if_unsupported (construct ctx e.exp_env e.exp_type "" values)])
  | Texp_construct (_, c, _, args, _) ->
    paths
      (arguments_right_to_left (fun s (_, e) -> eval s e) s args)
      (fun s values ->
        let value =
          match values with
          | [] -> expression_constructor ctx e.exp_env e.exp_type c
          | _ -> construct ctx e.exp_env e.exp_type c.cstr_name values
        in
        [s, opaque_if_unsupported value])
  | Texp_record { fields; extended_expression; _ } ->
    let bases =
      match extended_expression with
      | None -> [s, None]
      | Some (base, _, _) -> eval s base
    in
    let fields = Array.to_list fields in
    paths bases (fun s base ->
        paths
          (arguments_right_to_left
             (fun s (_, _, field) ->
               match field with
               | Kept _ -> [s, None]
               | Overridden (_, field) -> eval s field)
             s fields)
          (fun s values ->
            let fields =
              List.filter_map Fun.id
                (List.map2
                   (fun (label, _, field) value ->
                     match field with
                     | Kept _ -> None
                     | Overridden _ -> Some (label.Data_types.lbl_name, value))
                   fields values)
            in
            [ ( s,
                opaque_if_unsupported
                  (record_value ctx e.exp_env e.exp_type base fields) ) ]))
  | Texp_array (Immutable, _, elements, _) ->
    paths (arguments_right_to_left eval s elements) (fun s values ->
        let s, value = iarray_value ctx e.exp_env e.exp_type s values in
        [s, opaque_if_unsupported value])
  | Texp_field { record; label; _ } ->
    paths (eval s record) (fun s value ->
        [ ( s,
            opaque_if_unsupported
              (select_field ctx e.exp_env record.exp_type
                 label.Data_types.lbl_name value) ) ])
  | Texp_let (rec_flag, bindings, body) ->
    paths
      (value_bindings ctx s rec_flag bindings (has_elim e))
      (fun s _ -> eval s body)
  | Texp_open ({ open_expr = { mod_desc = Tmod_ident _; _ }; _ }, body) ->
    eval s body
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
            | Some left, Some right
              when term_sort left = term_sort right
                   && not
                        (sort_has_unsupported_logical_equality ctx.encoding
                           (term_sort left)) ->
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
              let prim = stored_primitive prim fn_value in
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
              | Some ("%array_length", 1) ->
                [ ( normal_iarray_length ctx args s,
                    match value with Some _ -> value | None -> opaque () ) ]
              | Some ("%array_safe_get", 2) ->
                [ ( normal_iarray_get ctx args s,
                    match value with Some _ -> value | None -> opaque () ) ]
              | Some ((("%set_find" | "%set_refined_find") as name), 2) ->
                [ ( normal_set_find ctx name args value s,
                    match value with Some _ -> value | None -> opaque () ) ]
              | Some ((("%map_find" | "%map_refined_find") as name), 2) ->
                [ ( normal_map_find ctx name args s,
                    match value with Some _ -> value | None -> opaque () ) ]
              | _ ->
                [(s, match value with Some _ -> value | None -> opaque ())]))
    end
  | Texp_function { params; body; _ } ->
    let captured = s in
    let states =
      List.fold_left
        (fun states p ->
          List.concat_map
            (fun s ->
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
              List.map
                (fun (s, condition) -> branch s condition)
                (pattern ctx (bind s p.fp_param value) value pat))
            states)
        [s] params
    in
    List.iter
      (fun s ->
        match body with
        | Tfunction_body body -> ignore (eval s body)
        | Tfunction_cases cases ->
          begin match cases.fc_cases with
          | [] -> ()
          | c :: _ ->
            let value = fresh ctx c.c_lhs.pat_env c.c_lhs.pat_type "argument" in
            ignore
              (value_cases ctx
                 (bind s cases.fc_param value)
                 value cases.fc_cases)
          end)
      states;
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
  let states =
    match rec_flag with
    | Asttypes.Nonrecursive -> [s]
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
        (fun states vb ->
          List.concat_map
            (fun s ->
              let value =
                fresh ctx vb.vb_pat.pat_env vb.vb_pat.pat_type "recursive"
              in
              List.map
                (fun (s, condition) -> branch s condition)
                (pattern ctx s value vb.vb_pat))
            states)
        [s] bindings
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
              List.concat_map
                (fun (s, condition) -> loop (branch s condition) rest)
                (pattern ctx s value vb.vb_pat)))
  in
  List.concat_map (fun s -> loop s bindings) states

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
  { encoding = Vox_encoding.create_context ();
    datatypes = [];
    symbols = [];
    functions = [];
    function_cache = Hashtbl.create 32;
    set_origins = Hashtbl.create 16;
    set_class_sorts = Hashtbl.create 8;
    map_origins = Hashtbl.create 16;
    map_class_sorts = Hashtbl.create 8;
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
    if
      Option.is_some (intro_loc e)
      || sort ctx.encoding e.exp_env e.exp_type = None
    then reject e;
    match e.exp_desc with
    | Texp_ident { desc = { val_kind = Val_reg _; _ }; _ }
    | Texp_constant (Const_int _) ->
      ()
    | Texp_construct (_, c, _, [], _)
      when Option.is_some (constructor ctx e.exp_env e.exp_type c.cstr_name) ->
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
              match sort ctx.encoding arg.exp_env arg.exp_type with
              | Some Bool -> scalar_value (Boolean false)
              | Some Int63 -> scalar_value (Integer 0L)
              | Some Int -> scalar_value (Big_integer "0")
              | Some (Opaque _ | Datatype _) -> reject arg
              | None -> reject arg)
            args
        in
        if operation ctx e.exp_env f.exp_type e.exp_type name values = None
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
                { datatypes = List.rev ctx.datatypes;
                  symbols = List.rev ctx.symbols;
                  functions = List.rev ctx.functions;
                  facts = List.rev s.facts;
                  goal =
                    { label = "decreases";
                      term =
                        (let value = required measure.exp_loc value in
                         match term_sort entry_measure with
                         | Int63 -> both Lt value entry_measure
                         | Int ->
                           both And
                             (both Int_ge value (Big_integer "0"))
                             (both Int_lt value entry_measure)
                         | Bool | Opaque _ | Datatype _ -> reject measure)
                    }
                })
            (expression ctx call_state measure)
        | _ -> ()
      in
      (* Parameter function values share their instance caches across paths. *)
      ctx.check_call <- check_call;
      ignore (expression ctx entry body))
    (expression ctx entry measure)
