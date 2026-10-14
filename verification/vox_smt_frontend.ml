module S = Vox_smt_sexp
module T = Vox_smt_term
module E = Vox_smt_engine
module M = Vox_smt_model
module Z = Vox_smt_integer.Z
module Resource = Vox_smt_resource
module SM = Map.Make (String)

exception Error of string

exception Ambiguous of string

type sort_constructor =
  { sort_identity : int;
    sort_name : string;
    sort_arity : int
  }

type sort_definition =
  | Ground of T.sort
  | Sort_constructor of sort_constructor

type allocation =
  | Datatype_instance of (int * T.sort list) * int
  | Sort_application of (int * T.sort list) * int

type sort_expression =
  | Parameter of int
  | Concrete of T.sort
  | Instance of datatype * sort_expression list
  | Application of sort_constructor * sort_expression list

and datatype =
  { identity : int;
    name : string;
    parameters : string list;
    mutable constructors : (string * (string * sort_expression) list) list
  }

type instance =
  { datatype : datatype;
    arguments : T.sort list;
    sort : T.sort;
    mutable symbols : T.symbol SM.t
  }

type polymorphic =
  | Constructor of datatype * int
  | Selector of datatype * int * int

type overload =
  | Declared of T.symbol
  | Datatype of polymorphic

type definition =
  | Function of T.symbol
  | Polymorphic of polymorphic
  | Overloaded of overload list
  | Macro of (string * T.sort) list * T.sort * S.t * definition SM.t

module Definition_maps = Hashtbl.Make (struct
  type t = definition SM.t
  let equal = ( == )
  let hash = Hashtbl.hash
end)

module Elaborations = Hashtbl.Make (struct
  type t = T.sort option * definition SM.t * T.term SM.t * S.t

  let hash (_, _, _, expression) = Hashtbl.hash expression

  let equal (expected, definitions, locals, expression)
      (other_expected, other_definitions, other_locals, other_expression) =
    expected = other_expected && definitions == other_definitions
    && locals == other_locals && expression == other_expression
end)

type named =
  { name : string;
    guard : T.term
  }

type annotation =
  { label : string;
    value : T.term;
    expression : S.t;
    body : S.t;
    epoch : int
  }

type annotations =
  { ordered : annotation list;
    by_name : annotation SM.t
  }

let no_annotations = { ordered = []; by_name = SM.empty }

type scope =
  { arena_terms : int;
    definitions : definition SM.t;
    sorts : sort_definition SM.t;
    datatypes : datatype SM.t;
    assertions : S.t list;
    named : named list;
    annotations : annotations
  }

type validation =
  { values : (T.sort, T.term) Hashtbl.t;
    mutable allocated_instances : allocation list
  }

type t =
  { clock : unit -> float;
    require_enabled_retrievals : bool;
    mutable resource : Resource.t option;
    mutable poll : unit -> unit;
    mutable busy : bool;
    mutable allocations : allocation list ref option;
    mutable terms : T.t;
    mutable solver : E.t;
    elaborations : T.term Elaborations.t;
    mutable cache_elaborations : bool;
    validation : validation option;
    mutable definitions : definition SM.t;
    mutable sorts : sort_definition SM.t;
    mutable datatypes : datatype SM.t;
    mutable global_datatypes : datatype SM.t;
    instances : (int * T.sort list, instance) Hashtbl.t;
    instantiated : (int, instance) Hashtbl.t;
    sort_applications : (int * T.sort list, T.sort) Hashtbl.t;
    applied_sorts : (int, sort_constructor * T.sort list) Hashtbl.t;
    sort_names : (int, string) Hashtbl.t;
    mutable next_datatype : int;
    mutable global_definitions : definition SM.t;
    mutable global_sorts : sort_definition SM.t;
    mutable scopes : scope list;
    mutable named : named list;
    mutable annotations : annotations;
    mutable global_annotations : annotations;
    mutable bind_annotations : bool;
    mutable annotation_epoch : int;
    mutable assertions : S.t list;
    mutable options : S.t SM.t;
    mutable result : E.result option;
    mutable model : M.t option;
    mutable core : string list;
    mutable assumption_core : string list;
    mutable next_sort : int;
    mutable next_guard : int;
    mutable print_success : bool;
    mutable global_declarations : bool;
    mutable budget : int;
    mutable timeout_ms : int;
    mutable reason : string;
    mutable resources : int;
    mutable reclaim_at : int;
    mutable reclaim_pending : bool
  }

let create ?(clock = Sys.time) ?(require_enabled_retrievals = false) () =
  let terms = T.create () in
  { clock; require_enabled_retrievals; resource = None;
    poll = (fun () -> ()); busy = false;
    allocations = None; terms;
    solver = E.create terms;
    elaborations = Elaborations.create 16;
    cache_elaborations = false;
    validation = None;
    definitions = SM.empty;
    sorts = SM.empty;
    datatypes = SM.empty;
    global_datatypes = SM.empty;
    instances = Hashtbl.create 16;
    instantiated = Hashtbl.create 16;
    sort_applications = Hashtbl.create 16;
    applied_sorts = Hashtbl.create 16;
    sort_names = Hashtbl.create 16;
    next_datatype = 0;
    global_definitions = SM.empty;
    global_sorts = SM.empty;
    scopes = [];
    named = [];
    annotations = no_annotations;
    global_annotations = no_annotations;
    bind_annotations = true;
    annotation_epoch = 0;
    assertions = [];
    options = SM.empty;
    result = None;
    model = None;
    core = [];
    assumption_core = [];
    next_sort = 0;
    next_guard = 0;
    print_success = false;
    global_declarations = false;
    budget = max_int;
    timeout_ms = 0;
    reason = "";
    resources = 0;
    reclaim_at = 8192;
    reclaim_pending = false
  }

let error message = raise (Error message)

let symbol_name name =
  let length = String.length name in
  if length >= 2 && name.[0] = '|' && name.[length - 1] = '|'
  then String.sub name 1 (length - 2)
  else name

let atom = function S.Atom s -> s | _ -> error "Expected a symbol"

let numeral s =
  String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

let natural s =
  if not (numeral s) then error "Expected a natural number";
  match int_of_string_opt s with
  | Some n when n >= 0 -> n
  | _ -> error "Expected a natural number"

let sort_string t = function
  | T.Bool -> "Bool"
  | T.Int -> "Int"
  | T.Bitvec width -> Printf.sprintf "(_ BitVec %d)" width
  | T.Named id -> (
    match Hashtbl.find_opt t.sort_names id with
    | Some name -> name
    | None -> error "Model sort unavailable")

let record_allocation t allocation =
  match t.validation with
  | None -> Option.iter (fun log -> log := allocation :: !log) t.allocations
  | Some validation ->
    validation.allocated_instances <- allocation :: validation.allocated_instances

let remove_allocation t = function
  | Datatype_instance (key, id) ->
    Hashtbl.remove t.instances key;
    Hashtbl.remove t.instantiated id;
    Hashtbl.remove t.sort_names id
  | Sort_application (key, id) ->
    Hashtbl.remove t.sort_applications key;
    Hashtbl.remove t.applied_sorts id;
    Hashtbl.remove t.sort_names id

let apply_sort t constructor arguments =
  t.poll ();
  if List.length arguments <> constructor.sort_arity then
    error ("Wrong sort arity: " ^ constructor.sort_name);
  let key = constructor.sort_identity, arguments in
  match Hashtbl.find_opt t.sort_applications key with
  | Some sort -> sort
  | None ->
    let id = t.next_sort in
    t.next_sort <- id + 1;
    let sort = T.Named id in
    record_allocation t (Sort_application (key, id));
    Hashtbl.add t.sort_applications key sort;
    Hashtbl.add t.applied_sorts id (constructor, arguments);
    let name = "(" ^ constructor.sort_name ^ " "
      ^ String.concat " " (List.map (sort_string t) arguments) ^ ")" in
    Hashtbl.add t.sort_names id name;
    sort

let instantiate t datatype arguments =
  let rec instance depth (datatype : datatype) arguments =
    t.poll ();
    if List.length arguments <> List.length datatype.parameters
    then error ("Wrong datatype arity: " ^ datatype.name);
    match Hashtbl.find_opt t.instances (datatype.identity, arguments) with
    | Some instance -> instance
    | None ->
      if depth > 128 then error "Nonregular datatype instantiation";
      let id = t.next_sort in
      t.next_sort <- id + 1;
      let result =
        { datatype; arguments; sort = T.Named id; symbols = SM.empty }
      in
      record_allocation t
        (Datatype_instance ((datatype.identity, arguments), id));
      Hashtbl.add t.instances (datatype.identity, arguments) result;
      Hashtbl.add t.instantiated id result;
      let name =
        if arguments = []
        then datatype.name
        else
          "(" ^ datatype.name ^ " "
          ^ String.concat " " (List.map (sort_string t) arguments)
          ^ ")"
      in
      Hashtbl.add t.sort_names id name;
      let rec field_sort expression =
        t.poll ();
        match expression with
        | Concrete sort -> sort
        | Parameter index -> List.nth arguments index
        | Instance (datatype, fields) ->
          (instance (depth + 1) datatype (List.map field_sort fields)).sort
        | Application (constructor, arguments) ->
          apply_sort t constructor (List.map field_sort arguments)
      in
      List.iter
        (fun (name, fields) ->
          t.poll ();
          let fields =
            List.map (fun (name, sort) -> name, field_sort sort) fields
          in
          let constructor =
            T.symbol t.terms
              { name;
                arguments = Array.of_list (List.map snd fields);
                result = result.sort;
                kind = T.Constructor id
              }
          in
          result.symbols
            <- SM.add (symbol_name name) constructor result.symbols;
          List.iteri
            (fun index (name, sort) ->
              let selector =
                T.symbol t.terms
                  { name;
                    arguments = [| result.sort |];
                    result = sort;
                    kind = T.Selector (constructor, index)
                  }
              in
              result.symbols
                <- SM.add (symbol_name name) selector result.symbols)
            fields)
        datatype.constructors;
      result
  in
  instance 0 datatype arguments

let rec sort_expression t parameters expression =
  t.poll ();
  match expression with
  | S.Atom name when List.mem (symbol_name name) parameters ->
    let rec index i = function
      | parameter :: _ when parameter = symbol_name name -> Parameter i
      | _ :: rest -> index (i + 1) rest
      | [] -> error "Datatype parameter invariant"
    in
    index 0 parameters
  | S.Atom name when symbol_name name = "Bool" -> Concrete T.Bool
  | S.Atom name when symbol_name name = "Int" -> Concrete T.Int
  | S.List [S.Atom "_"; S.Atom name; S.Atom n] when symbol_name name = "BitVec"
    ->
    let n = natural n in
    if n = 0 then error "Zero-width bitvector";
    Concrete (T.Bitvec n)
  | S.Atom name -> (
    match SM.find_opt (symbol_name name) t.sorts with
    | Some (Ground sort) -> Concrete sort
    | Some (Sort_constructor _) -> error ("Missing sort parameters: " ^ name)
    | None -> (
      match SM.find_opt (symbol_name name) t.datatypes with
      | Some datatype when datatype.parameters = [] -> Instance (datatype, [])
      | Some _ -> error ("Missing datatype parameters: " ^ name)
      | None -> error ("Unknown sort: " ^ name)))
  | S.List (S.Atom name :: args) -> (
    match SM.find_opt (symbol_name name) t.sorts with
    | Some (Sort_constructor constructor)
      when constructor.sort_arity = List.length args ->
      Application (constructor, List.map (sort_expression t parameters) args)
    | Some _ -> error ("Wrong sort arity: " ^ name)
    | None -> (match SM.find_opt (symbol_name name) t.datatypes with
    | Some datatype when List.length args = List.length datatype.parameters ->
      Instance (datatype, List.map (sort_expression t parameters) args)
    | Some _ -> error ("Wrong datatype arity: " ^ name)
    | None -> error ("Unknown sort: " ^ name)))
  | _ -> error "Unsupported sort"

let sort t expression =
  let rec concrete = function
    | Concrete sort -> sort
    | Instance (datatype, args) ->
      (instantiate t datatype (List.map concrete args)).sort
    | Application (constructor, args) ->
      apply_sort t constructor (List.map concrete args)
    | Parameter _ -> error "Unbound datatype parameter"
  in
  concrete (sort_expression t [] expression)

let instance_of_sort t datatype = function
  | T.Named id -> (
    match Hashtbl.find_opt t.instantiated id with
    | Some instance when instance.datatype.identity = datatype.identity ->
      instance
    | _ -> error "Datatype sort mismatch")
  | _ -> error "Expected datatype sort"

let inference t (datatype : datatype) expected =
  let parameters = Array.make (List.length datatype.parameters) None in
  Option.iter
    (fun expected ->
      let instance = instance_of_sort t datatype expected in
      List.iteri (fun i sort -> parameters.(i) <- Some sort) instance.arguments)
    expected;
  let rec unify expression actual =
    match expression with
    | Parameter index -> (
      match parameters.(index) with
      | Some expected when expected <> actual ->
        error "Datatype parameter mismatch"
      | Some _ -> ()
      | None -> parameters.(index) <- Some actual)
    | Concrete expected ->
      if expected <> actual then error "Datatype field sort mismatch"
    | Instance (datatype, expressions) ->
      let instance = instance_of_sort t datatype actual in
      List.iter2 unify expressions instance.arguments
    | Application (constructor, expressions) -> (
      let application = match actual with
        | T.Named id -> Hashtbl.find_opt t.applied_sorts id
        | T.Bool | T.Int | T.Bitvec _ -> None in
      match application with
      | Some (other, arguments)
        when constructor.sort_identity = other.sort_identity ->
        List.iter2 unify expressions arguments
      | _ -> error "Sort constructor mismatch")
  in
  let rec resolve = function
    | Concrete sort -> sort
    | Parameter index -> (
      match parameters.(index) with
      | Some sort -> sort
      | None -> raise (Ambiguous datatype.name))
    | Instance (datatype, expressions) ->
      (instantiate t datatype (List.map resolve expressions)).sort
    | Application (constructor, expressions) ->
      apply_sort t constructor (List.map resolve expressions)
  in
  parameters, unify, resolve

let polymorphic_values t polymorphic expected parse args =
  match polymorphic with
  | Selector _ -> Array.of_list (List.map (parse None) args)
  | Constructor (datatype, constructor) ->
    let name, fields = List.nth datatype.constructors constructor in
    if List.length fields <> List.length args then error "Constructor arity";
    let _, unify, resolve = inference t datatype expected in
    let fields = Array.of_list fields and args = Array.of_list args in
    let values = Array.make (Array.length args) None in
    let remaining = ref (Array.length args) in
    while !remaining > 0 do
      let previous = !remaining in
      Array.iteri
        (fun i (_, field) ->
          if values.(i) = None
          then
            let expected =
              try Some (resolve field) with Ambiguous _ -> None
            in
            match parse expected args.(i) with
            | value ->
              unify field (T.sort t.terms value);
              values.(i) <- Some value;
              decr remaining
            | exception Ambiguous _ -> ())
        fields;
      if !remaining = previous then raise (Ambiguous name)
    done;
    Array.map Option.get values

let polymorphic_symbol t polymorphic expected values =
  let datatype, constructor, field =
    match polymorphic with
    | Constructor (datatype, constructor) -> datatype, constructor, None
    | Selector (datatype, constructor, field) ->
      datatype, constructor, Some field
  in
  let name, fields = List.nth datatype.constructors constructor in
  let instance, name =
    match field with
    | Some field ->
      if Array.length values <> 1 then error "Selector arity";
      ( instance_of_sort t datatype (T.sort t.terms values.(0)),
        fst (List.nth fields field) )
    | None ->
      if Array.length values <> List.length fields
      then error "Constructor arity";
      let parameters, unify, _ = inference t datatype expected in
      List.iteri
        (fun i (_, expression) -> unify expression (T.sort t.terms values.(i)))
        fields;
      let arguments =
        Array.to_list parameters
        |> List.map (function
          | Some sort -> sort
          | None -> raise (Ambiguous name))
      in
      instantiate t datatype arguments, name
  in
  let symbol = SM.find (symbol_name name) instance.symbols in
  Option.iter
    (fun expected ->
      if (T.Unsafe.signature t.terms symbol).result <> expected
      then error "Qualified datatype result sort mismatch")
    expected;
  symbol

let require t expected xs =
  List.iter
    (fun x ->
      if T.sort t.terms x <> expected then error "Ill-sorted expression")
    xs

let width = function T.Bitvec n -> n | _ -> error "Expected a bitvector"

let add_width left right =
  if right > max_int - left then error "Bitvector width overflow";
  left + right

let multiply_width width count =
  if count > max_int / width then error "Bitvector width overflow";
  width * count

let same_sort t xs =
  match xs with
  | [] -> error "Missing operand"
  | x :: xs ->
    let s = T.sort t.terms x in
    require t s xs;
    s

let arity n xs = if List.length xs <> n then error "Wrong arity"

let all_pairs xs f =
  let rec loop = function
    | [] -> []
    | x :: xs -> List.map (f x) xs @ loop xs
  in
  loop xs

let distinct_bindings bindings =
  ignore (List.fold_left
    (fun names (name, _) ->
      if SM.mem name names then error ("Duplicate binding: " ^ name);
      SM.add name () names)
    SM.empty bindings)

let validation_value t values result =
  match Hashtbl.find_opt values result with
  | Some value -> value
  | None ->
    let symbol = T.symbol t.terms
      { name = "@validation"; arguments = [||]; result;
        kind = T.Uninterpreted } in
    let value = T.apply t.terms symbol [||] in
    Hashtbl.add values result value;
    value

let with_validation t ~cache_elaborations f =
  T.with_temporary t.terms (fun () ->
    let validation =
      { values = Hashtbl.create 4; allocated_instances = [] } in
    let context =
      { t with validation = Some validation;
        elaborations = Elaborations.create 16; cache_elaborations }
    in
    Fun.protect ~finally:(fun () ->
      List.iter (remove_allocation t) validation.allocated_instances)
      (fun () -> f context validation.values))

let datatype_of_polymorphic = function
  | Constructor (datatype, _) | Selector (datatype, _, _) -> datatype

let overloaded_tester t name choices value =
  let constructors = List.filter_map
    (function
      | Datatype (Constructor (datatype, index)) -> (
        match T.sort t.terms value with
        | T.Named id -> (
          match Hashtbl.find_opt t.instantiated id with
          | Some instance when instance.datatype.identity = datatype.identity ->
            let name, _ = List.nth datatype.constructors index in
            Some (SM.find (symbol_name name) instance.symbols)
          | _ -> None)
        | _ -> None)
      | Datatype (Selector _) | Declared _ -> None)
    choices in
  match constructors with
  | [constructor] -> T.apply t.terms (T.tester t.terms constructor) [|value|]
  | [] -> error ("No matching constructor: " ^ name)
  | _ -> raise (Ambiguous name)

let update_selector t definitions name value =
  let select polymorphic =
    t.poll ();
    match polymorphic with
    | Selector _ ->
      (try Some (polymorphic_symbol t polymorphic None [|value|],
                 Some polymorphic)
       with Error _ -> None)
    | Constructor _ -> None in
  match SM.find_opt (symbol_name name) definitions with
  | Some (Function symbol) ->
    let signature = T.Unsafe.signature t.terms symbol in
    (match signature.kind with
    | T.Selector _ when signature.arguments = [|T.sort t.terms value|] ->
      symbol, None
    | _ -> error "Expected a matching datatype selector")
  | Some (Polymorphic polymorphic) -> (
    match select polymorphic with
    | Some result -> result
    | None -> error "Expected a matching datatype selector")
  | Some (Overloaded choices) -> (
    match List.filter_map (function
      | Datatype polymorphic -> select polymorphic
      | Declared _ -> None) choices with
    | [result] -> result
    | [] -> error ("No matching selector: " ^ name)
    | _ -> raise (Ambiguous name))
  | _ -> error ("Unknown datatype selector: " ^ name)

let add_definition t key definition =
  t.definitions <- SM.add key definition t.definitions;
  if t.global_declarations
  then begin
    t.global_definitions <- SM.add key definition t.global_definitions;
    t.scopes
      <- List.map
           (fun (scope : scope) ->
             t.poll ();
             { scope with
               definitions = SM.add key definition scope.definitions
             })
           t.scopes
  end

let assertion_name = function
  | S.List (S.Atom "!" :: body :: attributes) ->
    let rec find found = function
      | [] -> found
      | S.Atom ":named" :: S.Atom name :: rest ->
        if found <> None then error "Duplicate :named attribute";
        find (Some name) rest
      | S.Atom ":named" :: _ -> error "Invalid :named attribute"
      | _ :: rest -> find found rest
    in
    body, find None attributes
  | body -> body, None

let bind_annotation t name expression body value =
  let key = symbol_name name in
  match SM.find_opt key t.annotations.by_name with
  | Some annotation when annotation.epoch = t.annotation_epoch
      && annotation.expression == expression -> ()
  | _ ->
    if SM.mem key t.definitions then error ("Duplicate symbol: " ^ name);
    add_definition t key
      (Macro ([], T.sort t.terms value, body, t.definitions));
    let annotation = { label = name; value; expression; body;
      epoch = t.annotation_epoch } in
    let add annotations =
      { ordered = annotation :: annotations.ordered;
        by_name = SM.add key annotation annotations.by_name } in
    t.annotations <- add t.annotations;
    if t.global_declarations then begin
      t.global_annotations <- add t.global_annotations;
      t.scopes <- List.map (fun (scope : scope) ->
        t.poll ();
        { scope with annotations = add scope.annotations }) t.scopes
    end

let without_annotations t f =
  let previous = t.bind_annotations in
  t.bind_annotations <- false;
  Fun.protect ~finally:(fun () -> t.bind_annotations <- previous) f

let closed_annotation t definitions locals expression =
  let is_constructor name =
    match SM.find_opt (symbol_name name) definitions with
    | Some (Function symbol) ->
      (match (T.Unsafe.signature t.terms symbol).kind with
      | T.Constructor _ -> true | _ -> false)
    | Some (Polymorphic (Constructor _)) -> true
    | Some (Overloaded choices) ->
      List.exists (function
        | Datatype (Constructor _) -> true
        | Datatype (Selector _) | Declared _ -> false) choices
    | _ -> false in
  let rec free locals expression =
    t.poll ();
    match expression with
    | S.Atom name -> SM.mem (symbol_name name) locals
    | S.String _ -> false
    | S.List [S.Atom "let"; S.List bindings; body] ->
      List.exists (function
        | S.List [_; value] -> free locals value
        | _ -> false) bindings
      || free (List.fold_left (fun locals -> function
        | S.List [S.Atom name; _] -> SM.remove (symbol_name name) locals
        | _ -> locals) locals bindings) body
    | S.List [S.Atom "match"; subject; S.List cases] ->
      free locals subject || List.exists (function
        | S.List [pattern; body] ->
          let names = match pattern with
            | S.Atom name when not (is_constructor name) -> [S.Atom name]
            | S.List (_ :: names) -> names
            | _ -> [] in
          let locals = List.fold_left (fun locals -> function
            | S.Atom name -> SM.remove (symbol_name name) locals
            | _ -> locals) locals names in
          free locals body
        | _ -> false) cases
    | S.List (S.Atom "!" :: body :: _) -> free locals body
    | S.List values -> List.exists (free locals) values in
  if not (SM.is_empty locals) && free locals expression then
    error "Named term must be closed"

let rec term ?expected t definitions locals expression =
  t.poll ();
  match expression with
  | S.Atom _ | S.String _ ->
    elaborate ?expected t definitions locals expression
  | S.List _ when t.cache_elaborations ->
    cached_term ?expected t definitions locals expression
  | S.List _ -> elaborate ?expected t definitions locals expression

and cached_term ?expected t definitions locals expression =
  let key = expected, definitions, locals, expression in
  match Elaborations.find_opt t.elaborations key with
  | Some value -> value
  | None ->
    let value = elaborate ?expected t definitions locals expression in
    Elaborations.add t.elaborations key value;
    value

and elaborate ?expected t definitions locals expression =
  let build name args = builtin t name args in
  let parse = term t definitions locals in
  match expression with
  | S.Atom "true" -> T.boolean t.terms true
  | S.Atom "false" -> T.boolean t.terms false
  | S.Atom s when numeral s -> T.integer t.terms (Z.of_string ~poll:t.poll s)
  | S.Atom s when String.length s > 2 && String.sub s 0 2 = "#b" ->
    let value = ref Z.zero in
    for i = 2 to String.length s - 1 do
      if s.[i] <> '0' && s.[i] <> '1' then error "Invalid binary literal";
      value
        := Z.add ~poll:t.poll
             (Z.mul ~poll:t.poll !value (Z.of_int 2))
             (Z.of_int (Char.code s.[i] - Char.code '0'))
    done;
    T.bits t.terms (String.length s - 2) !value
  | S.Atom s when String.length s > 2 && String.sub s 0 2 = "#x" ->
    let value = ref Z.zero in
    for i = 2 to String.length s - 1 do
      let digit =
        match s.[i] with
        | '0' .. '9' as c -> Char.code c - Char.code '0'
        | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
        | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
        | _ -> error "Invalid hexadecimal literal"
      in
      value := Z.add ~poll:t.poll
        (Z.mul ~poll:t.poll !value (Z.of_int 16)) (Z.of_int digit)
    done;
    T.bits t.terms (4 * (String.length s - 2)) !value
  | S.Atom name -> (
    match SM.find_opt (symbol_name name) locals with
    | Some term -> term
    | None -> application ?expected t definitions locals name [])
  | S.List [S.Atom "_"; S.Atom bv; S.Atom n]
    when String.length bv > 2 && String.sub bv 0 2 = "bv" ->
    let value = String.sub bv 2 (String.length bv - 2) in
    if not (numeral value) then error "Invalid bitvector numeral";
    T.bits t.terms (natural n) (Z.of_string ~poll:t.poll value)
  | S.List [S.Atom "let"; S.List bindings; body] ->
    let bindings =
      List.map
        (function
          | S.List [S.Atom name; value] -> symbol_name name, parse value
          | _ -> error "Invalid let binding")
        bindings
    in
    distinct_bindings bindings;
    let locals =
      List.fold_left
        (fun env (name, value) -> SM.add name value env)
        locals bindings
    in
    term ?expected t definitions locals body
  | S.List (S.Atom "!" :: _ :: _) ->
    let body, name = assertion_name expression in
    if t.bind_annotations then
      Option.iter (fun _ -> closed_annotation t definitions locals body) name;
    let value = term ?expected t definitions locals body in
    if t.bind_annotations then
      Option.iter
        (fun name -> bind_annotation t name expression body value) name;
    value
  | S.List [S.Atom "match"; subject; S.List cases] ->
    let subject = parse subject in
    let instance = match T.sort t.terms subject with
      | T.Named id -> Hashtbl.find_opt t.instantiated id
      | T.Bool | T.Int | T.Bitvec _ -> None in
    let instance = match instance with
      | Some instance -> instance
      | None -> error "Expected datatype match subject" in
    let covered = Hashtbl.create 8 and default = ref false in
    let constructor name =
      match SM.find_opt (symbol_name name) instance.symbols with
      | Some symbol ->
        (match (T.Unsafe.signature t.terms symbol).kind with
        | T.Constructor _ -> Some symbol
        | _ -> None)
      | None -> None in
    let branches = List.map (fun case ->
      t.poll ();
      if !default then error "Match default must be last";
      match case with
      | S.List [pattern; body] ->
        let name, bindings = match pattern with
          | S.Atom name -> name, None
          | S.List (S.Atom name :: bindings) ->
            name, Some (List.map (fun binding -> symbol_name (atom binding))
                          bindings)
          | _ -> error "Invalid match pattern" in
        (match constructor name, bindings with
        | None, None ->
          default := true;
          None, SM.add (symbol_name name) subject locals, body
        | None, Some _ -> error ("Unknown match constructor: " ^ name)
        | Some symbol, bindings ->
          if Hashtbl.mem covered symbol then error "Repeated match constructor";
          Hashtbl.add covered symbol ();
          let bindings = Option.value bindings ~default:[] in
          let signature = T.Unsafe.signature t.terms symbol in
          arity (Array.length signature.arguments) bindings;
          distinct_bindings (List.map (fun name -> name, ()) bindings);
          let rec bind_fields index locals = function
            | [] -> locals
            | name :: rest ->
              t.poll ();
              let selector = T.selector t.terms symbol index in
              let value = T.apply t.terms selector [|subject|] in
              bind_fields (index + 1) (SM.add name value locals) rest in
          let locals = bind_fields 0 locals bindings in
          Some (T.apply t.terms (T.tester t.terms symbol) [|subject|]),
          locals, body)
      | _ -> error "Invalid match case") cases in
    if branches = [] then error "Empty match";
    if not !default
       && Hashtbl.length covered <> List.length instance.datatype.constructors
    then error "Non-exhaustive match";
    let expected = match expected with
      | Some sort -> sort
      | None ->
        let rec discover = function
          | [] -> raise (Ambiguous "match result sort")
          | (_, locals, body) :: rest ->
            (match term t definitions locals body with
            | value -> T.sort t.terms value
            | exception Ambiguous _ -> discover rest) in
        discover branches in
    let branches = List.map (fun (condition, locals, body) ->
      let value = term ~expected t definitions locals body in
      require t expected [value];
      condition, value) branches in
    let rec lower = function
      | [_, value] -> value
      | (Some condition, value) :: rest ->
        T.ite t.terms condition value (lower rest)
      | _ -> error "Match lowering invariant" in
    lower branches
  | S.List [S.List [S.Atom "_"; S.Atom "is"; S.Atom name]; value] -> (
    let value = parse value in
    match SM.find_opt (symbol_name name) definitions with
    | Some (Function constructor) ->
      let signature = T.Unsafe.signature t.terms constructor in
      (match signature.kind with
      | T.Constructor _ -> ()
      | _ -> error "Expected a constructor");
      if T.sort t.terms value <> signature.result then error "Tester sort";
      let tester = T.tester t.terms constructor in
      T.apply t.terms tester [| value |]
    | Some (Overloaded choices) -> overloaded_tester t name choices value
    | Some (Polymorphic (Constructor (datatype, index))) ->
      let instance = instance_of_sort t datatype (T.sort t.terms value) in
      let name, _ = List.nth datatype.constructors index in
      let constructor = SM.find (symbol_name name) instance.symbols in
      T.apply t.terms (T.tester t.terms constructor) [| value |]
    | _ -> error "Unknown constructor")
  | S.List [S.List [S.Atom "_";
                   S.Atom (("update" | "update-field") as operation);
                   S.Atom name]; subject; replacement] ->
    let subject = term ?expected t definitions locals subject in
    let selector, polymorphic = update_selector t definitions name subject in
    let signature = T.Unsafe.signature t.terms selector in
    let constructor, field = match signature.kind with
      | T.Selector (constructor, field) -> constructor, field
      | _ -> error "Datatype updater selector invariant" in
    let condition = T.apply t.terms (T.tester t.terms constructor)
        [|subject|] in
    let inactive = T.node t.terms condition = T.Boolean false in
    let replacement =
      if inactive && operation = "update" then
        (try parse replacement with Ambiguous _ ->
          term ~expected:signature.result t definitions locals replacement)
      else term ~expected:signature.result t definitions locals replacement in
    if inactive && operation = "update" then begin
      (* cvc5 matches parametric updater arguments independently. The
         inactive update needs no constructor with mismatched fields. *)
      (match polymorphic with
      | Some (Selector (datatype, constructor, field)) ->
        let _, fields = List.nth datatype.constructors constructor in
        let _, unify, _ = inference t datatype None in
        unify (snd (List.nth fields field)) (T.sort t.terms replacement)
      | _ -> require t signature.result [replacement]);
      subject
    end else begin
      require t signature.result [replacement];
      let signature = T.Unsafe.signature t.terms constructor in
      let fields = Array.mapi (fun index _ ->
        t.poll ();
        if index = field then replacement
        else T.apply t.terms (T.selector t.terms constructor index)
            [|subject|]) signature.arguments in
      T.ite t.terms condition (T.apply t.terms constructor fields) subject
    end
  | S.List [S.List [S.Atom "_"; S.Atom "divisible"; S.Atom index]; value] ->
    if not (numeral index) then error "Expected a positive divisor";
    let divisor = Z.of_string index in
    if Z.is_zero divisor then error "Expected a positive divisor";
    let value = parse value in
    require t T.Int [value];
    T.equal t.terms
      (T.builtin t.terms "mod" T.Int
         [|value; T.integer t.terms divisor|])
      (T.integer t.terms Z.zero)
  | S.List [S.List [S.Atom "_"; S.Atom "iand"; S.Atom index]; x; y] ->
    let width = natural index in
    if width = 0 then error "Integer bitwise and needs a positive width";
    let x = parse x and y = parse y in
    require t T.Int [x; y];
    let convert value = T.builtin t.terms
        ("int_to_bv:" ^ string_of_int width) (T.Bitvec width) [|value|] in
    T.builtin t.terms "ubv_to_int" T.Int
      [|T.builtin t.terms "bvand" (T.Bitvec width)
          [|convert x; convert y|]|]
  | S.List [S.List (S.Atom "_" :: S.Atom op :: parameters); value] ->
    let value = parse value in
    let ps = List.map (fun x -> natural (atom x)) parameters in
    let name = op ^ ":" ^ String.concat ":" (List.map string_of_int ps) in
    let result =
      match op, ps with
      | ("int2bv" | "int_to_bv"), [n] ->
        if n = 0 then error "Zero-width bitvector";
        require t T.Int [value];
        T.Bitvec n
      | "extract", [high; low] ->
        let w = width (T.sort t.terms value) in
        if low > high || high >= w then error "Invalid extract";
        T.Bitvec (high - low + 1)
      | ("zero_extend" | "sign_extend"), [n] ->
        T.Bitvec (add_width (width (T.sort t.terms value)) n)
      | ("rotate_left" | "rotate_right"), [_] ->
        ignore (width (T.sort t.terms value));
        T.sort t.terms value
      | "repeat", [n] when n > 0 ->
        T.Bitvec (multiply_width (width (T.sort t.terms value)) n)
      | _ -> error "Unsupported indexed operator"
    in
    T.builtin t.terms name result [| value |]
  | S.List [S.Atom "as"; S.Atom name; expected] ->
    let value =
      application ~expected:(sort t expected) t definitions locals name []
    in
    require t (sort t expected) [value];
    value
  | S.List (S.List [S.Atom "as"; S.Atom name; expected] :: args) ->
    application ~expected:(sort t expected) t definitions locals name args
  | S.List [S.Atom (("ite" | "if_then_else") as name);
            condition; first; second]
    when name = "ite" || not (SM.mem name definitions
      || SM.mem name t.annotations.by_name) ->
    let condition = parse condition in
    let values =
      same_arguments ?expected t definitions locals [first; second]
    in
    T.ite t.terms condition (List.hd values) (List.hd (List.tl values))
  | S.List (S.Atom (("=" | "distinct") as name) :: args) ->
    build name (same_arguments t definitions locals args)
  | S.List (S.Atom name :: args) ->
    if SM.mem (symbol_name name) definitions
       || SM.mem (symbol_name name) t.annotations.by_name
    then application ?expected t definitions locals name args
    else if String.starts_with ~prefix:"is-" name && List.length args = 1
    then
      term t definitions locals
        (S.List
           [ S.List
               [ S.Atom "_";
                 S.Atom "is";
                 S.Atom (String.sub name 3 (String.length name - 3)) ];
             List.hd args ])
    else
      let name = symbol_name name in
      if name = "and" || name = "or" then begin
        let rec collect values pending = function
          | [] -> (match pending with
            | [] -> List.rev values
            | next :: pending -> collect values pending next)
          | expression :: rest ->
            t.poll ();
            match expression with
            | S.List (S.Atom nested :: children)
              when symbol_name nested = name
                && not (SM.mem name definitions)
                && not (SM.mem name t.annotations.by_name) ->
              collect values (rest :: pending) children
            | _ -> collect (parse expression :: values) pending rest in
        build name (collect [] [] args)
      end else build name (List.map parse args)
  | _ -> error "Unsupported expression"

and same_arguments ?expected t definitions locals args =
  match expected with
  | Some expected -> List.map (term ~expected t definitions locals) args
  | None ->
    let rec discover previous = function
      | [] -> raise (Ambiguous "expression sort")
      | expression :: rest ->
        match term t definitions locals expression with
        | value ->
          let expected = T.sort t.terms value in
          let parse = term ~expected t definitions locals in
          let previous = List.map parse (List.rev previous) in
          previous @ (value :: List.map parse rest)
        | exception Ambiguous _ -> discover (expression :: previous) rest
    in
    discover [] args

and application ?expected t definitions locals name args =
  let key = symbol_name name in
  let definition = match SM.find_opt key definitions with
    | Some _ as found -> found
    | None when SM.mem key t.annotations.by_name ->
      SM.find_opt key t.definitions
    | None -> None in
  match definition with
  | Some (Function symbol) ->
    function_application ?expected t definitions locals args symbol
  | Some (Polymorphic polymorphic) ->
    polymorphic_application ?expected t definitions locals args polymorphic
  | Some (Overloaded choices) ->
    let apply context = function
      | Declared symbol ->
        function_application ?expected context definitions locals args symbol
      | Datatype polymorphic ->
        polymorphic_application ?expected context definitions locals
          args polymorphic in
    let ambiguous = ref false in
    let matches = List.filter
      (fun choice ->
        try
          with_validation t ~cache_elaborations:t.cache_elaborations
            (fun context _ -> ignore (apply context choice));
          true
        with
        | Error _ | Invalid_argument _ -> false
        | Ambiguous _ -> ambiguous := true; false)
      choices in
    (match matches with
    | [choice] when not !ambiguous -> apply t choice
    | [] when not !ambiguous -> error ("No matching overload: " ^ name)
    | _ -> raise (Ambiguous name))
  | Some (Macro (parameters, result_sort, body, captured)) ->
    arity (List.length parameters) args;
    let args =
      List.map2
        (fun (_, expected) arg -> term ~expected t definitions locals arg)
        parameters args
    in
    let locals =
      List.fold_left2
        (fun env (name, sort) value ->
          require t sort [value];
          SM.add name value env)
        SM.empty parameters args
    in
    let result =
      match t.validation with
      | Some validation -> validation_value t validation.values result_sort
      | None -> without_annotations t (fun () ->
        if parameters = [] then begin
          t.poll ();
          cached_term ~expected:result_sort t captured locals body
        end else term ~expected:result_sort t captured locals body)
    in
    require t result_sort [result];
    Option.iter (fun expected -> require t expected [result]) expected;
    result
  | None when args = [] && symbol_name name = "true" -> T.boolean t.terms true
  | None when args = [] && symbol_name name = "false" ->
    T.boolean t.terms false
  | None -> error ("Unknown symbol: " ^ name)

and function_application ?expected t definitions locals args symbol =
  let signature = T.Unsafe.signature t.terms symbol in
  Option.iter (fun expected ->
    if signature.result <> expected then error "Function result sort mismatch")
    expected;
  arity (Array.length signature.arguments) args;
  let args = Array.of_list (List.mapi (fun i arg ->
    term ~expected:signature.arguments.(i) t definitions locals arg) args) in
  try T.apply t.terms symbol args
  with Invalid_argument message -> error message

and polymorphic_application ?expected t definitions locals args polymorphic =
  let values = polymorphic_values t polymorphic expected
    (fun expected -> term ?expected t definitions locals) args in
  let symbol = polymorphic_symbol t polymorphic expected values in
  T.apply t.terms symbol values

and builtin t name args =
  let bool () = require t T.Bool args in
  let int () = require t T.Int args in
  let make result = T.builtin t.terms name result (Array.of_list args) in
  let binary result =
    arity 2 args;
    make result
  in
  let left_associative result =
    match args with
    | first :: (_ :: _ as rest) ->
      List.fold_left
        (fun x y -> T.builtin t.terms name result [| x; y |])
        first rest
    | _ -> error (name ^ " needs two operands")
  in
  match name with
  | "not" ->
    bool ();
    arity 1 args;
    make T.Bool
  | "and" | "or" ->
    bool ();
    make T.Bool
  | "=>" | "implies" ->
    bool ();
    let rec imply = function
      | [] | [_] -> error "Implication needs two operands"
      | [a; b] -> T.or_ t.terms [| T.not_ t.terms a; b |]
      | a :: rest -> T.or_ t.terms [| T.not_ t.terms a; imply rest |]
    in
    imply args
  | "xor" -> (
    bool ();
    match args with
    | [] -> error "Empty xor"
    | x :: xs ->
      List.fold_left
        (fun x y -> T.builtin t.terms "xor" T.Bool [| x; y |])
        x xs)
  | "iff" ->
    bool ();
    builtin t "=" args
  | "=" ->
    ignore (same_sort t args);
    if List.length args < 2 then error "Equality needs two operands";
    let rec chain = function
      | x :: (y :: _ as rest) -> T.equal t.terms x y :: chain rest
      | _ -> []
    in
    T.and_ t.terms (Array.of_list (chain args))
  | "distinct" ->
    let sort = same_sort t args in
    if List.length args >= 16 && (match sort with
        | T.Named id -> T.constructors t.terms id = []
        | _ -> false)
    then T.builtin t.terms "distinct" T.Bool (Array.of_list args)
    else T.and_ t.terms
      (Array.of_list
         (all_pairs args (fun x y -> T.not_ t.terms (T.equal t.terms x y))))
  | "ite" -> (
    match args with
    | [c; x; y] -> T.ite t.terms c x y
    | _ -> error "Conditional needs three operands")
  | "+" | "*" | "-" ->
    int ();
    if args = [] then error "Missing operand";
    make T.Int
  | "~" ->
    int ();
    arity 1 args;
    T.builtin t.terms "-" T.Int (Array.of_list args)
  | "int.pow2" | "int.log2" ->
    int ();
    arity 1 args;
    make T.Int
  | "piand" ->
    int ();
    arity 3 args;
    make T.Int
  | "div" ->
    int ();
    left_associative T.Int
  | "mod" ->
    int ();
    binary T.Int
  | "mod_total" ->
    int ();
    arity 2 args;
    let dividend = List.hd args and divisor = List.nth args 1 in
    let zero = match T.node t.terms dividend with
      | T.Integer value -> Z.is_zero value
      | _ -> false in
    if zero || dividend = divisor
    then T.integer t.terms Z.zero
    else T.ite t.terms
      (T.equal t.terms divisor (T.integer t.terms Z.zero))
      dividend (T.builtin t.terms "mod" T.Int [|dividend; divisor|])
  | "rem" ->
    int ();
    arity 2 args;
    let remainder = T.builtin t.terms "mod" T.Int (Array.of_list args) in
    let divisor = List.nth args 1 in
    T.ite t.terms
      (T.builtin t.terms "<" T.Bool
        [|divisor; T.integer t.terms Z.zero|])
      (T.builtin t.terms "-" T.Int [|remainder|])
      remainder
  | "abs" ->
    int ();
    arity 1 args;
    let x = List.hd args in
    T.ite t.terms
      (T.builtin t.terms "<" T.Bool [| x; T.integer t.terms Z.zero |])
      (T.builtin t.terms "-" T.Int [| x |])
      x
  | "<" | "<=" | ">" | ">=" ->
    int ();
    if List.length args < 2 then error "Comparison needs two operands";
    let rec chain = function
      | x :: (y :: _ as rest) ->
        T.builtin t.terms name T.Bool [| x; y |] :: chain rest
      | _ -> []
    in
    T.and_ t.terms (Array.of_list (chain args))
  | "bv2nat" ->
    arity 1 args;
    ignore (width (T.sort t.terms (List.hd args)));
    T.builtin t.terms "ubv_to_int" T.Int (Array.of_list args)
  | "bv2int" | "ubv_to_int" | "sbv_to_int" ->
    arity 1 args;
    ignore (width (T.sort t.terms (List.hd args)));
    make T.Int
  | "concat" -> (
    match args with
    | first :: (_ :: _ as rest) ->
      List.fold_left
        (fun x y ->
          let w = add_width (width (T.sort t.terms x))
              (width (T.sort t.terms y)) in
          T.builtin t.terms name (T.Bitvec w) [| x; y |])
        first rest
    | _ -> error "Concatenation needs two operands")
  | "bvite" -> (
    match args with
    | [c; x; y] ->
      require t (T.Bitvec 1) [c];
      ignore (width (same_sort t [x; y]));
      T.ite t.terms (T.equal t.terms c (T.bits t.terms 1 Z.one)) x y
    | _ -> error "Bitvector conditional needs three operands")
  | "bvnot" | "bvneg" ->
    arity 1 args;
    let s = same_sort t args in
    ignore (width s);
    make s
  | "bvredand" | "bvredor" ->
    arity 1 args;
    ignore (width (same_sort t args));
    make (T.Bitvec 1)
  | "bvcomp" ->
    ignore (width (same_sort t args));
    binary (T.Bitvec 1)
  | "bvand" | "bvor" | "bvxor" | "bvxnor" | "bvadd" | "bvmul" ->
    let s = same_sort t args in
    ignore (width s);
    left_associative s
  | "bvnand" | "bvnor" | "bvsub" | "bvudiv" | "bvurem" | "bvsdiv" | "bvsrem"
  | "bvsmod" | "bvshl" | "bvlshr" | "bvashr" ->
    let s = same_sort t args in
    ignore (width s);
    binary s
  | "bvnego" ->
    arity 1 args;
    ignore (width (same_sort t args));
    make T.Bool
  | "bvult" | "bvule" | "bvugt" | "bvuge" | "bvslt" | "bvsle" | "bvsgt"
  | "bvsge" | "bvuaddo" | "bvsaddo" | "bvusubo" | "bvssubo" | "bvumulo"
  | "bvsmulo" | "bvsdivo" ->
    ignore (width (same_sort t args));
    binary T.Bool
  | _ -> error ("Unsupported operator: " ^ name)

exception Large_expression

let large_expression t expression =
  let rec count remaining expression =
    t.poll ();
    match expression with
    | _ when remaining = 0 -> raise Large_expression
    | S.Atom _ | S.String _ -> remaining - 1
    | S.List children -> List.fold_left count (remaining - 1) children
  in
  try ignore (count 1024 expression); false with Large_expression -> true

let parse_term t expression =
  (* Sort declarations cannot change during one elaboration. *)
  t.cache_elaborations <- large_expression t expression;
  Fun.protect
    ~finally:(fun () ->
      Elaborations.reset t.elaborations;
      t.cache_elaborations <- false)
    (fun () -> term t t.definitions SM.empty expression)

let parameters t args =
  let parameters = List.map
    (function
      | S.List [S.Atom name; s] -> symbol_name name, sort t s
      | _ -> error "Malformed function parameter")
    args in
  distinct_bindings parameters;
  parameters

let validate_definition t args result body =
  with_validation t ~cache_elaborations:(large_expression t body)
    (fun context values ->
      let parameters = parameters context args in
      let result = sort context result in
      let locals = List.fold_left
        (fun locals (name, sort) ->
          SM.add name (validation_value context values sort) locals)
        SM.empty parameters in
      let value = term ~expected:result context t.definitions locals body in
      require context result [value];
      let rec added annotations =
        if annotations == t.annotations.ordered then [] else
        match annotations with
        | annotation :: rest ->
          (annotation.label, sort_string context
             (T.sort context.terms annotation.value),
           annotation.expression, annotation.body) :: added rest
        | [] -> error "Named definition invariant" in
      List.rev (added context.annotations.ordered))

let install_annotations t annotations =
  List.iter (fun (name, result, expression, body) ->
    t.poll ();
    let result = match S.parse ?resource:t.resource result with
      | [result] -> sort t result
      | _ -> error "Named sort invariant" in
    let value = without_annotations t (fun () ->
      term ~expected:result t t.definitions SM.empty body) in
    bind_annotation t name expression body value) annotations

let add_polymorphic t name polymorphic =
  let key = symbol_name name in
  let add definitions =
    let extend choices =
      let datatype = datatype_of_polymorphic polymorphic in
      if List.exists (function
        | Datatype choice ->
          (datatype_of_polymorphic choice).identity = datatype.identity
        | Declared _ -> false) choices then error ("Duplicate symbol: " ^ name);
      Overloaded (choices @ [Datatype polymorphic])
    in
    let definition =
      match SM.find_opt key definitions with
      | None -> Polymorphic polymorphic
      | Some (Polymorphic previous) -> extend [Datatype previous]
      | Some (Overloaded choices) -> extend choices
      | Some (Function symbol) -> extend [Declared symbol]
      | Some (Macro _) -> error ("Duplicate symbol: " ^ name)
    in
    SM.add key definition definitions
  in
  let definitions = add t.definitions in
  if t.global_declarations then begin
    let global_definitions = add t.global_definitions in
    let scopes = List.map
      (fun (scope : scope) ->
        { scope with definitions = add scope.definitions }) t.scopes in
    t.global_definitions <- global_definitions;
    t.scopes <- scopes
  end;
  t.definitions <- definitions

let declare t name arguments result kind =
  let key = symbol_name name in
  let extend symbol definitions =
    let add choices =
      if List.exists (fun choice ->
        t.poll ();
        match choice with
        | Declared previous ->
          let signature = T.Unsafe.signature t.terms previous in
          signature.arguments = arguments && signature.result = result
        | Datatype _ -> false) choices
      then error ("Duplicate symbol: " ^ name);
      Overloaded (choices @ [Declared symbol]) in
    let definition = match SM.find_opt key definitions with
      | None -> Function symbol
      | Some (Function previous) -> add [Declared previous]
      | Some (Polymorphic previous) -> add [Datatype previous]
      | Some (Overloaded choices) -> add choices
      | Some (Macro _) -> error ("Duplicate symbol: " ^ name) in
    SM.add key definition definitions in
  let symbol = T.symbol t.terms { name; arguments; result; kind } in
  let definitions = extend symbol t.definitions in
  let globals, scopes =
    if t.global_declarations then
      extend symbol t.global_definitions,
      List.map (fun (scope : scope) ->
        t.poll ();
        { scope with definitions = extend symbol scope.definitions }) t.scopes
    else t.global_definitions, t.scopes in
  if arguments = [||] then
    E.register ?resource:t.resource t.solver (T.apply t.terms symbol [||]);
  t.definitions <- definitions;
  t.global_definitions <- globals;
  t.scopes <- scopes;
  symbol

let declare_sort t name arity =
  let key = symbol_name name in
  if SM.mem key t.sorts || SM.mem key t.datatypes
  then error ("Duplicate sort: " ^ name);
  let definition =
    if arity = 0 then begin
      let id = t.next_sort in
      t.next_sort <- id + 1;
      Hashtbl.add t.sort_names id name;
      Ground (T.Named id)
    end else begin
      let sort_identity = t.next_datatype in
      t.next_datatype <- sort_identity + 1;
      Sort_constructor { sort_identity; sort_name = name; sort_arity = arity }
    end in
  t.sorts <- SM.add key definition t.sorts;
  if t.global_declarations
  then begin
    t.global_sorts <- SM.add key definition t.global_sorts;
    t.scopes
      <- List.map
           (fun (scope : scope) ->
             { scope with sorts = SM.add key definition scope.sorts })
           t.scopes
  end

let define_sort t name sort =
  let key = symbol_name name in
  if SM.mem key t.sorts || SM.mem key t.datatypes
  then error ("Duplicate sort: " ^ name);
  t.sorts <- SM.add key (Ground sort) t.sorts;
  if t.global_declarations
  then begin
    t.global_sorts <- SM.add key (Ground sort) t.global_sorts;
    t.scopes
      <- List.map
           (fun (scope : scope) ->
             { scope with sorts = SM.add key (Ground sort) scope.sorts })
           t.scopes
  end

let declare_datatypes t headers bodies =
  let headers, bodies =
    if List.for_all (function S.Atom _ -> true | _ -> false) headers
    then
      let parameters = List.map (fun x -> symbol_name (atom x)) headers in
      let declarations =
        List.map
          (function
            | S.List (S.Atom name :: constructors) -> name, constructors
            | _ -> error "Malformed legacy datatype")
          bodies
      in
      let names = List.map (fun (name, _) -> symbol_name name) declarations in
      let rec qualify expression =
        t.poll ();
        match expression with
        | S.Atom name
          when List.mem (symbol_name name) names && parameters <> [] ->
          S.List (S.Atom name :: List.map (fun p -> S.Atom p) parameters)
        | S.List xs -> S.List (List.map qualify xs)
        | x -> x
      in
      ( List.map
          (fun (name, _) ->
            S.List
              [S.Atom name; S.Atom (string_of_int (List.length parameters))])
          declarations,
        List.map
          (fun (_, constructors) ->
            let constructors =
              List.map
                (function
                  | S.Atom name -> S.List [S.Atom name]
                  | S.List (S.Atom name :: fields) ->
                    S.List
                      (S.Atom name
                      :: List.map
                           (function
                             | S.List [name; sort] ->
                               S.List [name; qualify sort]
                             | _ -> error "Malformed datatype selector")
                           fields)
                  | _ -> error "Malformed datatype constructor")
                constructors
            in
            S.List [S.Atom "par"; S.List headers; S.List constructors])
          declarations )
    else headers, bodies
  in
  if List.length headers <> List.length bodies then error "Datatype arity";
  let declarations =
    List.map2
      (fun header body ->
        t.poll ();
        let name, arity =
          match header with
          | S.List [S.Atom name; S.Atom count] -> name, natural count
          | _ -> error "Malformed datatype declaration"
        in
        let parameters, constructors =
          match body with
          | S.List [S.Atom "par"; S.List parameters; S.List constructors] ->
            List.map (fun x -> symbol_name (atom x)) parameters, constructors
          | S.List constructors when arity = 0 -> [], constructors
          | _ -> error "Missing datatype parameters"
        in
        if List.length parameters <> arity
        then error "Datatype parameter arity";
        if List.length (List.sort_uniq String.compare parameters) <> arity
        then error "Duplicate datatype parameter";
        let key = symbol_name name in
        if SM.mem key t.sorts || SM.mem key t.datatypes
        then error ("Duplicate sort: " ^ name);
        let datatype =
          { identity = t.next_datatype; name; parameters; constructors = [] }
        in
        t.next_datatype <- t.next_datatype + 1;
        t.datatypes <- SM.add key datatype t.datatypes;
        if t.global_declarations
        then begin
          t.global_datatypes <- SM.add key datatype t.global_datatypes;
          t.scopes
            <- List.map
                 (fun (scope : scope) ->
                   { scope with
                     datatypes = SM.add key datatype scope.datatypes
                   })
                 t.scopes
        end;
        datatype, constructors)
      headers bodies
  in
  List.iter
    (fun (datatype, constructors) ->
      t.poll ();
      datatype.constructors
        <- List.map
             (function
               | S.List (S.Atom name :: fields) ->
                 ( name,
                   List.map
                     (function
                       | S.List [S.Atom name; sort] ->
                         name, sort_expression t datatype.parameters sort
                       | _ -> error "Malformed datatype field")
                     fields )
               | _ -> error "Malformed datatype constructor")
             constructors;
      if datatype.constructors = [] then error "Empty datatype";
      List.iteri
        (fun index (name, fields) ->
          let define = add_polymorphic t in
          define name (Constructor (datatype, index));
          List.iteri
            (fun field (name, _) ->
              define name (Selector (datatype, index, field)))
            fields)
        datatype.constructors)
    declarations;
  let productive = Hashtbl.create 16 in
  let changed = ref true in
  let rec inhabited parameters expression =
    t.poll ();
    match expression with
    | Parameter index -> List.nth parameters index
    | Concrete _ | Application _ -> true
    | Instance (datatype, arguments) ->
      datatype_inhabited datatype (List.map (inhabited parameters) arguments)
  and datatype_inhabited datatype parameters =
    t.poll ();
    let key = datatype.identity, parameters in
    match Hashtbl.find_opt productive key with
    | Some result -> result
    | None ->
      Hashtbl.add productive key false;
      let result =
        List.exists
          (fun (_, fields) ->
            List.for_all (fun (_, sort) -> inhabited parameters sort) fields)
          datatype.constructors
      in
      if result
      then begin
        Hashtbl.replace productive key true;
        changed := true
      end;
      result
  in
  while !changed do
    t.poll ();
    changed := false;
    let previous =
      Hashtbl.fold
        (fun key value xs -> if value then xs else key :: xs)
        productive []
    in
    List.iter (Hashtbl.remove productive) previous;
    List.iter
      (fun ((datatype : datatype), _) ->
        ignore
          (datatype_inhabited datatype
             (List.map (fun _ -> true) datatype.parameters)))
      declarations
  done;
  List.iter
    (fun ((datatype : datatype), _) ->
      if
        not
          (datatype_inhabited datatype
             (List.map (fun _ -> true) datatype.parameters))
      then error ("Datatype has no finite inhabitant: " ^ datatype.name))
    declarations;
  List.iter
    (fun ((datatype : datatype), _) ->
      if datatype.parameters = [] then ignore (instantiate t datatype []))
    declarations


let sort_name t id = sort_string t (T.Named id)

let model t =
  if t.result <> Some E.Sat then error "No model available";
  match t.model with
  | Some model -> model
  | None ->
    let model = match t.resource with
      | None -> M.create ~terms:t.terms ~solver:t.solver
      | Some resource ->
        M.create_with_resource resource ~terms:t.terms ~solver:t.solver in
    t.model <- Some model;
    model

let render t model =
  let qualify symbol =
    let signature = T.Unsafe.signature t.terms symbol in
    match SM.find_opt (symbol_name signature.name) t.definitions with
    | Some (Overloaded _) -> true
    | _ -> false
  in
  M.to_string ?resource:t.resource ~qualify model ~sort_name:(sort_name t)

let model_string t =
  let model = model t in
  let render = render t model in
  let definition symbol =
    t.poll ();
    let signature = T.Unsafe.signature t.terms symbol in
    let next_parameter = ref 0 in
    let rec fresh_parameter () =
      t.poll ();
      let name = "@arg" ^ string_of_int !next_parameter in
      incr next_parameter;
      if SM.mem name t.definitions then fresh_parameter () else name in
    let names = Array.map (fun _ -> fresh_parameter ()) signature.arguments in
    let parameters =
      Array.mapi
        (fun i sort -> Printf.sprintf "(%s %s)" names.(i) (sort_string t sort))
        signature.arguments
    in
    let body =
      if Array.length signature.arguments = 0
      then render
        (M.value ?resource:t.resource model (T.apply t.terms symbol [||]))
      else
        let entries, fallback =
          M.interpretation ?resource:t.resource model symbol in
        List.fold_left
          (fun body (arguments, result) ->
            if M.equal ?resource:t.resource model result fallback
            then body
            else
              let conditions =
                Array.mapi
                  (fun i value ->
                    Printf.sprintf "(= %s %s)" names.(i) (render value))
                  arguments
              in
              let condition =
                if Array.length conditions = 1
                then conditions.(0)
                else
                  "(and " ^ String.concat " " (Array.to_list conditions) ^ ")"
              in
              "(ite " ^ condition ^ " " ^ render result ^ " " ^ body ^ ")")
          (render fallback) entries
    in
    Printf.sprintf "(define-fun %s (%s) %s %s)" signature.name
      (String.concat " " (Array.to_list parameters))
      (sort_string t signature.result)
      body
  in
  let definitions =
    SM.bindings t.definitions
    |> List.concat_map (function
      | _, Function symbol -> [symbol]
      | _, Overloaded choices -> List.filter_map (function
        | Declared symbol -> Some symbol | Datatype _ -> None) choices
      | _, (Macro _ | Polymorphic _) -> [])
    |> List.filter_map (fun symbol ->
      if (T.Unsafe.signature t.terms symbol).kind = T.Uninterpreted
      then Some (definition symbol) else None)
  in
  "(" ^ String.concat "\n " definitions ^ ")"

let canonical_option = function
  | ":interactive-mode" -> ":produce-assertions"
  | ":global-decls" -> ":global-declarations"
  | ":reproducible-resource-limit" -> ":rlimit"
  | name -> name

let rec execute t command =
  let success () = if t.print_success then Some "success" else None in
  let require_option option =
    if t.require_enabled_retrievals
       && SM.find_opt option t.options <> Some (S.Atom "true")
    then error ("Command requires " ^ option ^ " true")
  in
  let invalidate () =
    t.result <- None;
    t.model <- None;
    t.reason <- "";
    t.core <- [];
    t.assumption_core <- []
  in
  let solve assumptions =
    let supplied =
      List.map
        (fun expression ->
          parse_term t expression, S.to_string ?resource:t.resource expression)
        assumptions
    in
    let named = List.rev t.named in
    let assumptions =
      List.map (fun named -> named.guard) named @ List.map fst supplied
    in
    invalidate ();
    let result =
      Fun.protect
        ~finally:(fun () ->
          let used = E.resource_count t.solver in
          t.resources <- t.resources + min used (max_int - t.resources))
        (fun () -> E.check ?resource:t.resource ~assumptions t.solver)
    in
    t.result <- Some result;
    Some
      (match result with
      | E.Sat -> "sat"
      | E.Unsat core ->
        let members = Hashtbl.create 16 in
        List.iter (fun term ->
          t.poll (); Hashtbl.replace members term ()) core;
        t.assumption_core
          <- List.filter_map
               (fun (term, source) ->
                 t.poll ();
                 if Hashtbl.mem members term then Some source else None)
               supplied;
        t.core
          <- List.filter_map
               (fun named ->
                 t.poll ();
                 if Hashtbl.mem members named.guard
                 then Some named.name else None)
               named;
        "unsat"
      | E.Unknown why ->
        t.reason <- why;
        "unknown")
  in
  let render_core names =
    let output = Buffer.create 64 in
    Buffer.add_char output '(';
    List.iteri (fun index name ->
      t.poll ();
      if index > 0 then Buffer.add_char output ' ';
      let offset = ref 0 in
      while !offset < String.length name do
        t.poll ();
        let count = min 1024 (String.length name - !offset) in
        Buffer.add_substring output name !offset count;
        offset := !offset + count
      done) names;
    Buffer.add_char output ')';
    Buffer.contents output
  in
  match command with
  | S.List [S.Atom "set-logic"; _] -> success ()
  | S.List [S.Atom "set-option"; S.Atom ":print-success"; S.Atom value] ->
    t.print_success <- (match value with
      | "true" -> true
      | "false" -> false
      | _ -> error "Expected a Boolean option");
    success ()
  | S.List
      [S.Atom "set-option";
       S.Atom (":rlimit" | ":reproducible-resource-limit"); S.Atom value] ->
    let n = natural value in
    t.budget <- (if n = 0 then max_int else n);
    success ()
  | S.List [S.Atom "set-option"; S.Atom ":timeout"; S.Atom value] ->
    t.timeout_ms <- natural value;
    success ()
  | S.List
      [S.Atom "set-option";
       S.Atom (":diagnostic-output-channel" | ":regular-output-channel");
       value] ->
    (match value with
    | S.String _ -> success ()
    | _ -> error "Expected a string output channel")
  | S.List
      [ S.Atom "set-option";
        S.Atom (":global-declarations" | ":global-decls");
        S.Atom value ] ->
    t.global_declarations
      <- (match value with
         | "true" -> true
         | "false" -> false
         | _ -> error "Expected a Boolean option");
    success ()
  | S.List
      [S.Atom "set-option";
       S.Atom (":produce-models" | ":produce-assignments"
               | ":produce-assertions" | ":interactive-mode"
               | ":produce-unsat-cores" | ":produce-unsat-assumptions"
               | ":incremental"); value] ->
    (match value with
    | S.Atom ("true" | "false") -> success ()
    | _ -> error "Expected a Boolean option")
  | S.List
      [S.Atom "set-option"; S.Atom (":random-seed" | ":verbosity");
       S.Atom value] ->
    ignore (natural value);
    success ()
  | S.List [S.Atom "set-option"; S.Atom name; S.Atom value] ->
    let normalized = String.map (fun c -> if c = '_' then '-' else c) name in
    let changes_semantics =
      match normalized with
      | ":rewriter.hi-div0" | ":smt.hi-div0" -> value <> "true"
      | ":global-negate" | ":smt.arith.ignore-int" | ":arith.ignore-int" ->
        value <> "false"
      | ":solve-int-as-bv" | ":solve-real-as-int" ->
        value <> "false" && value <> "0" && value <> "-1"
      | _ -> false
    in
    if changes_semantics then error ("Unsupported semantic option: " ^ name);
    (match normalized with
    | ":rewriter.hi-div0" | ":smt.hi-div0" | ":global-negate"
    | ":smt.arith.ignore-int" | ":arith.ignore-int"
    | ":solve-int-as-bv" | ":solve-real-as-int" -> success ()
    | _ -> Some "unsupported")
  | S.List (S.Atom "set-option" :: _) -> Some "unsupported"
  | S.List
      [S.Atom "set-info"; S.Atom ":status";
       S.Atom ("sat" | "unsat" | "unknown")] -> success ()
  | S.List (S.Atom "set-info" :: S.Atom ":status" :: _) ->
    error "Expected sat, unsat or unknown status"
  | S.List (S.Atom "set-info" :: S.Atom name :: values)
    when String.length name > 1 && name.[0] = ':'
         && (match values with [] | [_] -> true | _ -> false) ->
    success ()
  | S.List (S.Atom "set-info" :: _) -> error "Expected an information attribute"
  | S.List [S.Atom "declare-sort"; S.Atom name; S.Atom arity] ->
    declare_sort t name (natural arity);
    invalidate ();
    success ()
  | S.List [S.Atom "declare-sort"; S.Atom name] ->
    declare_sort t name 0;
    invalidate ();
    success ()
  | S.List [S.Atom "define-sort"; S.Atom name; S.List []; body] ->
    define_sort t name (sort t body);
    invalidate ();
    success ()
  | S.List [S.Atom "declare-fun"; S.Atom name; S.List args; result] ->
    ignore
      (declare t name
         (Array.of_list (List.map (sort t) args))
         (sort t result) T.Uninterpreted);
    invalidate ();
    success ()
  | S.List [S.Atom "declare-const"; S.Atom name; result] ->
    ignore (declare t name [||] (sort t result) T.Uninterpreted);
    invalidate ();
    success ()
  | S.List [S.Atom "define-fun"; S.Atom name; S.List args; result; body] ->
    let key = symbol_name name in
    if SM.mem key t.definitions then error ("Duplicate symbol: " ^ name);
    let annotations = validate_definition t args result body in
    install_annotations t annotations;
    if SM.mem key t.definitions then error ("Duplicate symbol: " ^ name);
    let parameters = parameters t args in
    add_definition t key
      (Macro (parameters, sort t result, body, t.definitions));
    invalidate ();
    success ()
  | S.List [S.Atom "define-const"; name; result; body] ->
    execute t (S.List
      [S.Atom "define-fun"; name; S.List []; result; body])
  | S.List [S.Atom "declare-datatypes"; S.List sorts; S.List bodies] ->
    declare_datatypes t sorts bodies;
    invalidate ();
    success ()
  | S.List [S.Atom "declare-datatype"; S.Atom name; body] ->
    execute t
      (S.List
         [ S.Atom "declare-datatypes";
           S.List
             [ S.List
                 [ S.Atom name;
                   S.Atom
                     (match body with
                     | S.List [S.Atom "par"; S.List parameters; _] ->
                       string_of_int (List.length parameters)
                     | _ -> "0") ] ];
           S.List [body] ])
  | S.List [S.Atom "assert"; expression] ->
    let _, name = assertion_name expression in
    let term = parse_term t expression in
    require t T.Bool [term];
    (match name with
    | None -> E.assert_ ?resource:t.resource t.solver term
    | Some name ->
      let symbol =
        T.symbol t.terms
          { name = Printf.sprintf "@named%d" t.next_guard;
            arguments = [||];
            result = T.Bool;
            kind = T.Uninterpreted
          }
      in
      t.next_guard <- t.next_guard + 1;
      let guard = T.apply t.terms symbol [||] in
      let implication = T.or_ t.terms [| T.not_ t.terms guard; term |] in
      t.named <- { name; guard } :: t.named;
      E.assert_ ?resource:t.resource t.solver implication);
    t.assertions <- expression :: t.assertions;
    invalidate ();
    success ()
  | S.List [S.Atom "push"; S.Atom count] ->
    let count = natural count in
    let scope =
      { arena_terms = T.count t.terms;
        definitions = t.definitions; sorts = t.sorts;
        datatypes = t.datatypes; assertions = t.assertions; named = t.named;
        annotations = t.annotations }
    in
    let rec prepare count scopes =
      if count = 0 then scopes else begin
        t.poll ();
        prepare (count - 1) (scope :: scopes)
      end in
    let scopes = prepare count t.scopes in
    E.push ?resource:t.resource ~count t.solver;
    t.scopes <- scopes;
    invalidate ();
    success ()
  | S.List [S.Atom (("push" | "pop") as op)] ->
    execute t (S.List [S.Atom op; S.Atom "1"])
  | S.List [S.Atom "pop"; S.Atom count] ->
    let count = natural count in
    let rec prepare count scopes last =
      if count = 0 then scopes, last else begin
        t.poll ();
        match scopes with
        | scope :: rest -> prepare (count - 1) rest (Some scope)
        | [] -> error "Pop beyond scope stack"
      end in
    let scopes, scope = prepare count t.scopes None in
    E.pop ?resource:t.resource ~count t.solver;
    if count > 0 then t.reclaim_pending <- true;
    Option.iter (fun (scope : scope) ->
      t.reclaim_at <- min t.reclaim_at
        (max 8192 (2 * scope.arena_terms));
      t.definitions <- scope.definitions;
      t.sorts <- scope.sorts;
      t.datatypes <- scope.datatypes;
      t.assertions <- scope.assertions;
      t.named <- scope.named;
      t.annotations <- scope.annotations) scope;
    t.scopes <- scopes;
    invalidate ();
    success ()
  | S.List [S.Atom "check-sat"] | S.List [S.Atom "check-sat-using"; _] ->
    solve []
  | S.List [S.Atom "check-sat-assuming"; S.List assumptions] ->
    solve assumptions
  | S.List (S.Atom "check-sat" :: assumptions) -> solve assumptions
  | S.List [S.Atom "get-unsat-core"] -> (
    require_option ":produce-unsat-cores";
    match t.result with
    | Some (E.Unsat _) -> Some (render_core t.core)
    | _ -> error "No unsat core available")
  | S.List [S.Atom "get-unsat-assumptions"] -> (
    require_option ":produce-unsat-assumptions";
    match t.result with
    | Some (E.Unsat _) -> Some (render_core t.assumption_core)
    | _ -> error "No unsat assumptions available")
  | S.List [S.Atom "get-value"; S.List expressions] ->
    require_option ":produce-models";
    let model = model t in
    let values =
      List.map
        (fun expression ->
          t.poll ();
          let value =
            render t model
              (M.value ?resource:t.resource model (parse_term t expression))
          in
          "(" ^ S.to_string ?resource:t.resource expression ^ " " ^ value ^ ")")
        expressions
    in
    Some ("(" ^ String.concat " " values ^ ")")
  | S.List [S.Atom "get-assignment"] ->
    require_option ":produce-assignments";
    let model = model t in
    let assignments = List.filter_map (fun annotation ->
      t.poll ();
      if T.sort t.terms annotation.value <> T.Bool then None else
      Some ("(" ^ annotation.label ^ " " ^ render t model
        (M.value ?resource:t.resource model annotation.value) ^ ")"))
      (List.rev t.annotations.ordered) in
    Some ("(" ^ String.concat " " assignments ^ ")")
  | S.List [S.Atom "get-model"] ->
    require_option ":produce-models";
    Some (model_string t)
  | S.List [S.Atom "get-assertions"] ->
    require_option ":produce-assertions";
    Some (S.to_string ?resource:t.resource (S.List (List.rev t.assertions)))
  | S.List [S.Atom "get-option"; S.Atom name] -> (
    let name = canonical_option name in
    match SM.find_opt name t.options with
    | Some value -> Some (S.to_string value)
    | None -> (
      match name with
      | ":print-success" | ":global-declarations" | ":produce-models"
      | ":produce-assertions" | ":produce-unsat-cores"
      | ":produce-unsat-assumptions" | ":produce-assignments"
      | ":produce-proofs" | ":interactive-mode" ->
        Some "false"
      | ":random-seed" | ":verbosity" | ":timeout" | ":rlimit"
      | ":reproducible-resource-limit" -> Some "0"
      | ":diagnostic-output-channel" -> Some "\"stderr\""
      | ":regular-output-channel" -> Some "\"stdout\""
      | _ -> Some "unsupported"))
  | S.List [S.Atom "get-info"; S.Atom ":name"] ->
    Some "(:name \"Vox SMT\")"
  | S.List [S.Atom "get-info"; S.Atom ":version"] ->
    Some "(:version \"0.1\")"
  | S.List [S.Atom "get-info"; S.Atom ":authors"] ->
    Some "(:authors \"Vox contributors\")"
  | S.List [S.Atom "get-info"; S.Atom ":error-behavior"] ->
    Some "(:error-behavior continued-execution)"
  | S.List [S.Atom "get-info"; S.Atom ":assertion-stack-levels"] ->
    Some (Printf.sprintf "(:assertion-stack-levels %d)"
      (List.length t.scopes))
  | S.List [S.Atom "get-info"; S.Atom ":status"] ->
    Some ("(:status " ^ (match t.result with
      | Some E.Sat -> "sat"
      | Some (E.Unsat _) -> "unsat"
      | None | Some (E.Unknown _) -> "unknown") ^ ")")
  | S.List [S.Atom "get-info"; S.Atom ":reason-unknown"] -> (
    match t.result with
    | Some (E.Unknown _) ->
      Some ("(:reason-unknown " ^ S.to_string (S.String t.reason) ^ ")")
    | _ -> error "Reason unavailable without an unknown result")
  | S.List [S.Atom "get-info"; S.Atom ":all-statistics"] ->
    let stats = E.statistics t.solver in
    Some
      (Printf.sprintf
         "(:rlimit-count %d :decisions %d :propagations %d :conflicts %d)"
         t.resources
         stats.decisions stats.propagations stats.conflicts)
  | S.List [S.Atom "get-info"; S.Atom ":rlimit"] ->
    Some (Printf.sprintf "(:rlimit %d)" t.resources)
  | S.List [S.Atom "get-info"; S.Atom _] -> Some "unsupported"
  | S.List [S.Atom "echo"; (S.String _ as message)] ->
    Some (S.to_string message)
  | S.List [S.Atom "reset-assertions"] ->
    if not t.global_declarations
    then begin
      t.definitions <- t.global_definitions;
      t.sorts <- t.global_sorts;
      t.datatypes <- t.global_datatypes
    end;
    t.reclaim_pending <- true;
    t.reclaim_at <- 8192;
    t.solver <- E.create t.terms;
    t.scopes <- [];
    t.named <- [];
    if not t.global_declarations then
      t.annotations <- t.global_annotations;
    t.assertions <- [];
    SM.iter
      (fun _ definition ->
        match definition with
        | Function symbol ->
          if (T.Unsafe.signature t.terms symbol).arguments = [||]
          then E.register ?resource:t.resource t.solver
            (T.apply t.terms symbol [||])
        | Overloaded choices -> List.iter (function
          | Declared symbol ->
            if (T.Unsafe.signature t.terms symbol).arguments = [||]
            then E.register ?resource:t.resource t.solver
              (T.apply t.terms symbol [||])
          | Datatype _ -> ()) choices
        | Macro _ | Polymorphic _ -> ())
      t.definitions;
    invalidate ();
    success ()
  | S.List [S.Atom "reset"] ->
    let new_t = create () in
    Elaborations.reset t.elaborations;
    t.reclaim_pending <- false;
    t.reclaim_at <- 8192;
    t.terms <- new_t.terms;
    t.solver <- new_t.solver;
    t.definitions <- SM.empty;
    t.sorts <- SM.empty;
    t.global_definitions <- SM.empty;
    t.global_sorts <- SM.empty;
    t.datatypes <- SM.empty;
    t.global_datatypes <- SM.empty;
    Hashtbl.clear t.instances;
    Hashtbl.clear t.instantiated;
    Hashtbl.clear t.sort_applications;
    Hashtbl.clear t.applied_sorts;
    Hashtbl.clear t.sort_names;
    t.next_datatype <- 0;
    t.scopes <- [];
    t.named <- [];
    t.annotations <- no_annotations;
    t.global_annotations <- no_annotations;
    t.assertions <- [];
    t.options <- SM.empty;
    t.next_sort <- 0;
    t.next_guard <- 0;
    t.print_success <- false;
    t.global_declarations <- false;
    t.budget <- max_int;
    t.timeout_ms <- 0;
    t.resources <- 0;
    invalidate ();
    success ()
  | S.List [S.Atom "exit"] -> success ()
  | _ -> error ("Unsupported command: " ^ S.to_string command)

let compact t =
  if t.reclaim_pending && T.count t.terms >= t.reclaim_at then begin
    let eligibility = Definition_maps.create 16 in
    let simple definitions =
      t.poll ();
      match Definition_maps.find_opt eligibility definitions with
      | Some result -> result
      | None ->
        let result = SM.for_all (fun _ definition ->
          t.poll ();
          match definition with
          | Function symbol ->
            (T.signature t.terms symbol).kind = T.Uninterpreted
          | _ -> false) definitions in
        Definition_maps.add eligibility definitions result;
        result in
    let eligible = t.named = [] && t.annotations.ordered = []
      && t.global_annotations.ordered = []
      && Hashtbl.length t.instances = 0
      && simple t.definitions && simple t.global_definitions
      && List.for_all (fun (scope : scope) ->
        t.poll (); scope.named = [] && scope.annotations.ordered = []
        && simple scope.definitions) t.scopes in
    if not eligible then t.reclaim_at <- max 8192 (2 * T.count t.terms)
    else begin
      let terms = T.create () in
      let solver = E.create terms in
      let scratch = { t with terms; solver; scopes = []; assertions = [];
        named = []; result = None; model = None; core = [];
        assumption_core = []; elaborations = Elaborations.create 16 } in
      let symbols = Hashtbl.create 128 in
      let definition value =
        t.poll ();
        match value with
        | Function symbol ->
          let fresh = match Hashtbl.find_opt symbols symbol with
            | Some fresh -> fresh
            | None ->
              t.poll ();
              let signature = T.signature t.terms symbol in
              let fresh = T.symbol terms signature in
              Hashtbl.add symbols symbol fresh;
              if signature.arguments = [||] then
                E.register ?resource:t.resource solver
                  (T.apply terms fresh [||]);
              fresh in
          Function fresh
        | _ -> invalid_arg "SMT compaction eligibility" in
      let environments = Definition_maps.create 16 in
      let remap definitions =
        t.poll ();
        match Definition_maps.find_opt environments definitions with
        | Some remapped -> remapped
        | None ->
          let remapped = SM.map definition definitions in
          Definition_maps.add environments definitions remapped;
          remapped in
      T.with_poll terms t.poll (fun () ->
        let definitions = remap t.definitions in
        let globals = remap t.global_definitions in
        let scopes = List.map (fun (scope : scope) ->
          t.poll ();
          { scope with definitions = remap scope.definitions }) t.scopes in
        let current = { arena_terms = 0; definitions; sorts = t.sorts;
          datatypes = t.datatypes; assertions = t.assertions; named = [];
          annotations = t.annotations } in
        let rec difference previous assertions result =
          t.poll ();
          if assertions == previous then result else
          match assertions with
          | assertion :: rest -> difference previous rest
              (assertion :: result)
          | [] -> invalid_arg "SMT compaction assertion history" in
        let previous = ref [] in
        let rebuilt = ref [] in
        List.iteri (fun depth (scope : scope) ->
          t.poll ();
          scratch.definitions <- scope.definitions;
          scratch.sorts <- scope.sorts;
          scratch.datatypes <- scope.datatypes;
          if depth > 0 then E.push ?resource:t.resource solver;
          List.iter (fun expression ->
            ignore (execute scratch (S.List [S.Atom "assert"; expression])))
            (difference !previous scope.assertions []);
          previous := scope.assertions;
          rebuilt := { scope with arena_terms = T.count terms } :: !rebuilt)
          (List.fold_left (fun acc scope -> t.poll (); scope :: acc)
             [] (current :: scopes));
        let scopes = match !rebuilt with
          | _current :: scopes -> scopes
          | [] -> invalid_arg "SMT compaction scope history" in
        Option.iter Resource.check t.resource;
        t.terms <- terms;
        t.solver <- solver;
        t.definitions <- definitions;
        t.global_definitions <- globals;
        t.scopes <- scopes;
        Elaborations.reset t.elaborations;
        t.reclaim_at <- max 8192 (2 * T.count terms);
        t.reclaim_pending <- false)
    end
  end

let restore t saved =
  t.reclaim_pending <- saved.reclaim_pending;
  t.reclaim_at <- saved.reclaim_at;
  t.terms <- saved.terms;
  t.solver <- saved.solver;
  t.definitions <- saved.definitions;
  t.sorts <- saved.sorts;
  t.datatypes <- saved.datatypes;
  t.global_definitions <- saved.global_definitions;
  t.global_sorts <- saved.global_sorts;
  t.global_datatypes <- saved.global_datatypes;
  t.scopes <- saved.scopes;
  t.named <- saved.named;
  t.annotations <- saved.annotations;
  t.global_annotations <- saved.global_annotations;
  t.assertions <- saved.assertions;
  t.options <- saved.options;
  t.result <- saved.result;
  t.model <- saved.model;
  t.core <- saved.core;
  t.assumption_core <- saved.assumption_core;
  t.reason <- saved.reason;
  t.print_success <- saved.print_success;
  t.global_declarations <- saved.global_declarations;
  t.budget <- saved.budget;
  t.timeout_ms <- saved.timeout_ms

let execute ?resource ?cancelled t command =
  if t.busy then invalid_arg "SMT frontend operation is already active";
  let checking = match command with
    | S.List (S.Atom ("check-sat" | "check-sat-assuming"
                    | "check-sat-using") :: _) -> true
    | _ -> false in
  let timeout =
    if checking && t.timeout_ms > 0
    then Some (float_of_int t.timeout_ms /. 1000.) else None in
  let budget = if checking then Some t.budget else None in
  let cancelled = if checking then cancelled else None in
  t.annotation_epoch <- t.annotation_epoch + 1;
  let saved = { t with busy = false } in
  let allocated = ref [] in
  t.busy <- true;
  t.allocations <- Some allocated;
  let rollback () =
    restore t saved;
    List.iter (remove_allocation t) !allocated in
  Fun.protect ~finally:(fun () ->
    Elaborations.reset t.elaborations;
    t.busy <- false;
    t.resource <- saved.resource;
    t.poll <- saved.poll;
    t.allocations <- saved.allocations)
    (fun () ->
      match (let resource =
        if resource <> None || cancelled <> None
           || checking && (t.budget <> max_int || timeout <> None)
        then Some (Resource.create ?parent:resource ?cancelled ?budget
                     ~clock:t.clock ?timeout ())
        else None in
        t.resource <- resource;
        t.poll <- (fun () -> Option.iter Resource.poll resource);
        if checking then compact t;
        T.with_poll t.terms t.poll (fun () ->
        t.poll ();
        let result = execute t command in
        (match command with
        | S.List (S.Atom ("get-unsat-core" | "get-unsat-assumptions"
                         | "get-model" | "get-value" | "get-assignment"
                         | "get-assertions" | "get-info" | "get-option") :: _) ->
          Option.iter Resource.check resource
        | _ -> ());
        (match command with
        | S.List [S.Atom "set-option"; S.Atom name; value]
          when result <> Some "unsupported" ->
          t.options <- SM.add (canonical_option name) value t.options
        | _ -> ());
        result)) with
      | result -> result
      | exception Resource.Exhausted reason when checking ->
        rollback ();
        t.result <- Some (E.Unknown reason);
        t.model <- None;
        t.core <- [];
        t.assumption_core <- [];
        t.reason <- reason;
        Some "unknown"
      | exception exn ->
        let discard = checking && (match exn with
          | Error _ | Ambiguous _ -> t.result = None
          | _ -> true) in
        rollback ();
        if discard || t.result = Some E.Sat
                      && E.model_generation t.solver = None then begin
          t.result <- None;
          t.model <- None;
          t.core <- [];
          t.assumption_core <- [];
          t.reason <- ""
        end;
        (match exn with
        | Ambiguous name -> error ("Ambiguous datatype constructor: " ^ name)
        | M.Unavailable message | Vox_smt_bitvector.Unsupported message ->
          error ("Model value unavailable: " ^ message)
        | exn -> raise exn))

let run ?resource ?cancelled t source =
  if t.busy then invalid_arg "SMT frontend operation is already active";
  t.busy <- true;
  let commands = Fun.protect ~finally:(fun () -> t.busy <- false)
      (fun () -> S.parse ?resource
        ~stop:(function S.List [S.Atom "exit"] -> true | _ -> false) source) in
  List.filter_map (execute ?resource ?cancelled t) commands
