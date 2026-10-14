module Term = Vox_smt_term
module Engine = Vox_smt_engine
module Z = Vox_smt_integer.Z
module BV = Vox_smt_bitvector
module Resource = Vox_smt_resource

type value =
  | Boolean of bool
  | Integer of Z.t
  | Bits of int * Z.t
  | Abstract of int * int
  | Constructor of Term.symbol * value array

module Values = Hashtbl.Make (struct
  type t = value
  let equal = ( == )
  let hash = Hashtbl.hash
end)

module Value_pairs = Hashtbl.Make (struct
  type t = value * value
  let equal (left, right) (other_left, other_right) =
    left == other_left && right == other_right
  let hash = Hashtbl.hash
end)

type t =
  { terms : Term.t;
    solver : Engine.t;
    generation : Engine.model_generation;
    mutable resource : Resource.t option;
    mutable poll : unit -> unit;
    mutable busy : bool;
    observed : Term.term list;
    raw_cache : (Term.term, Engine.value option) Hashtbl.t;
    applications : (Term.symbol, Term.term list) Hashtbl.t;
    cache : (Term.term, value) Hashtbl.t;
    active : (Term.term, unit) Hashtbl.t;
    constructors : (int, Term.term) Hashtbl.t;
    completions : (int, value) Hashtbl.t;
    defaults : (Term.sort, value) Hashtbl.t;
    heights : int Values.t;
    hashes : int Values.t;
    functions : (Term.symbol, (value array * value) list * value) Hashtbl.t
  }

exception Unavailable of string

let unavailable message = raise (Unavailable message)

let raw_value t term =
  t.poll ();
  match Hashtbl.find_opt t.raw_cache term with
  | Some value -> value
  | None ->
    let value = Engine.raw_value ?resource:t.resource t.solver term in
    Hashtbl.add t.raw_cache term value;
    value

let integer = function
  | Integer z -> z
  | _ -> unavailable "Expected an integer model value"

let boolean = function
  | Boolean b -> b
  | _ -> unavailable "Expected a Boolean model value"

let rec default t visiting sort =
  t.poll ();
  match Hashtbl.find_opt t.defaults sort with
  | Some value -> value
  | None ->
    let result =
      match sort with
      | Term.Bool -> Boolean false
      | Term.Int -> Integer Z.zero
      | Term.Bitvec width -> Bits (width, Z.zero)
      | Term.Named id ->
        let constructors = Term.constructors t.terms id in
        if constructors = []
        then Abstract (id, -1)
        else if List.mem id visiting
        then unavailable "Recursive datatype model"
        else
          let rec choose = function
            | [] -> unavailable "No finite datatype model value"
            | constructor :: rest -> (
              let signature = Term.Unsafe.signature t.terms constructor in
              match
                Array.map (default t (id :: visiting)) signature.arguments
              with
              | fields -> Constructor (constructor, fields)
              | exception Unavailable _ -> choose rest)
          in
          choose (List.sort Int.compare constructors)
    in
    Hashtbl.replace t.defaults sort result;
    result

let constant_bv t name result values =
  t.poll ();
  let terms = Term.create () in
  Term.with_poll terms t.poll (fun () ->
  let arguments =
    Array.map
      (function
        | Boolean b -> Term.boolean terms b
        | Integer z -> Term.integer terms z
        | Bits (width, z) -> Term.bits terms width z
        | Abstract _ | Constructor _ ->
          unavailable "Non-scalar bitvector operand")
      values
  in
  let next = ref 0 in
  let truth = ref None in
  let fresh () =
    incr next;
    !next
  in
  let clause = function
    | [| literal |] -> truth := Some literal
    | _ -> unavailable "Nonconstant bitvector evaluation"
  in
  let truth () =
    match !truth with
    | Some truth -> truth
    | None -> unavailable "Missing bitvector truth value"
  in
  let as_boolean term =
    match Term.Unsafe.node terms term with
    | Term.Boolean b -> if b then truth () else -truth ()
    | _ -> unavailable "Nonconstant bitvector condition"
  in
  let encoder =
    BV.create ~terms ~fresh ~clause ~boolean:as_boolean ~poll:t.poll () in
  ignore (BV.bits encoder (Term.bits terms 1 Z.zero));
  let bit literal =
    if literal = truth ()
    then true
    else if literal = -truth ()
    then false
    else unavailable "Nonconstant bitvector result"
  in
  match result with
  | Term.Bitvec width ->
    let term = Term.builtin terms name result arguments in
    let bits = BV.bits encoder term in
    let value = ref Z.zero in
    for i = Array.length bits - 1 downto 0 do
      value := Z.mul ~poll:t.poll !value (Z.of_int 2);
      if bit bits.(i) then value := Z.add ~poll:t.poll !value Z.one
    done;
    Bits (width, !value)
  | Term.Bool -> (
    match BV.predicate encoder name arguments with
    | Some literal -> Boolean (bit literal)
    | None -> unavailable ("Unsupported model operator: " ^ name))
  | Term.Int | Term.Named _ ->
    ignore t;
    unavailable "Invalid bitvector model sort")

(* Branching values need memoization to preserve sharing. Unary chains avoid
   physical-key buckets whose bounded structural hashes coincide. *)
let rec hash_value t = function
  | Constructor (symbol, fields) as value ->
    t.poll ();
    let compute () = Array.fold_left (fun hash value ->
      (hash lxor hash_value t value) * 65599) (17 + symbol) fields in
    if Array.length fields <= 1 then compute () else
    (match Values.find_opt t.hashes value with
    | Some hash -> hash
    | None ->
      let hash = compute () in
      Values.add t.hashes value hash;
      hash)
  | value -> t.poll (); Hashtbl.hash value

let equal_value t left right =
  t.poll ();
  if left == right then true else
    match left, right with
    | Constructor _, Constructor _ ->
      let compared = ref None in
      let pending = Stack.create () and result = ref true in
      Stack.push (left, right) pending;
      while !result && not (Stack.is_empty pending) do
        t.poll ();
        let left, right = Stack.pop pending in
        if left != right then
          match left, right with
          | Constructor (symbol, fields), Constructor (other, other_fields) ->
            if symbol <> other
               || Array.length fields <> Array.length other_fields
            then result := false
            else begin
              let visited =
                if Array.length fields <= 1 then false else
                  let table = match !compared with
                    | Some table -> table
                    | None ->
                      let table = Value_pairs.create 16 in
                      compared := Some table;
                      table in
                  let pair = left, right in
                  if Value_pairs.mem table pair then true else begin
                    Value_pairs.add table pair ();
                    false
                  end in
              if not visited then
                for index = Array.length fields - 1 downto 0 do
                  t.poll ();
                  Stack.push (fields.(index), other_fields.(index)) pending
                done
            end
          | Constructor _, _ | _, Constructor _ -> result := false
          | _ -> result := left = right
      done;
      !result
    | Constructor _, _ | _, Constructor _ -> false
    | _ -> left = right

let acyclic_value t value =
  let seen = Values.create 16 and pending = Stack.create () in
  let unary = function
    | Constructor (_, fields) when Array.length fields = 1 -> Some fields.(0)
    | _ -> None in
  Stack.push (`Enter value) pending;
  while not (Stack.is_empty pending) do
    t.poll ();
    match Stack.pop pending with
    | `Leave value -> Values.replace seen value true
    | `Enter (Constructor (_, fields) as value) ->
      (match Values.find_opt seen value with
      | Some false -> unavailable "Cyclic model value"
      | Some true -> ()
      | None ->
        Values.add seen value false;
        Stack.push (`Leave value) pending;
        if Array.length fields = 1 then begin
          let cursor = ref value in
          let slow = ref (Some value) and fast = ref (Some value) in
          while Option.is_some (unary !cursor) do
            t.poll ();
            (match unary !cursor with
            | Some child -> cursor := child
            | None -> unavailable "Unary model value invariant");
            slow := Option.bind !slow unary;
            fast := Option.bind (Option.bind !fast unary) unary;
            (match !slow, !fast with
            | Some left, Some right when left == right ->
              unavailable "Cyclic model value"
            | _ -> ())
          done;
          Stack.push (`Enter !cursor) pending
        end else
          Array.iter (fun child ->
            t.poll (); Stack.push (`Enter child) pending) fields)
    | `Enter _ -> ()
  done

let rec value t term =
  t.poll ();
  let term = Engine.model_term ?resource:t.resource t.solver term in
  match Hashtbl.find_opt t.cache term with
  | Some result -> result
  | None ->
    if Hashtbl.mem t.active term then unavailable "Cyclic model value";
    Hashtbl.add t.active term ();
    let result =
      Fun.protect
        ~finally:(fun () -> Hashtbl.remove t.active term)
        (fun () -> evaluate t term)
    in
    Hashtbl.add t.cache term result;
    result

and abstract t term id representative =
  if Term.constructors t.terms id = []
  then Abstract (id, representative)
  else
    match Hashtbl.find_opt t.constructors representative with
    | Some constructor -> (
      match Term.Unsafe.node t.terms constructor with
      | Term.Apply (symbol, arguments) ->
        Constructor (symbol, Array.map (value t) arguments)
      | _ -> unavailable "Datatype constructor model invariant")
    | None -> (
      ignore term;
      match Hashtbl.find_opt t.completions representative with
      | Some value -> value
      | None -> default t [] (Term.Named id))

and evaluate t term =
  let raw = raw_value t term in
  match raw with
  | Some (Engine.Boolean b) -> Boolean b
  | Some (Engine.Integer z) -> Integer z
  | Some (Engine.Bits (width, z)) -> Bits (width, z)
  | Some (Engine.Abstract representative) -> (
    match Term.sort t.terms term with
    | Term.Named id -> abstract t term id representative
    | _ -> unavailable "Abstract scalar model value")
  | None -> (
    match Term.Unsafe.node t.terms term with
    | Term.Boolean b -> Boolean b
    | Term.Integer z -> Integer z
    | Term.Bits (width, z) -> Bits (width, z)
    | Term.Apply (symbol, arguments) -> (
      let signature = Term.Unsafe.signature t.terms symbol in
      match signature.kind with
      | Term.Builtin "ite" ->
        let condition = boolean (value t arguments.(0)) in
        value t arguments.(if condition then 1 else 2)
      | _ -> (
        let values = Array.map (value t) arguments in
        match signature.kind with
        | Term.Constructor _ -> Constructor (symbol, values)
        | Term.Tester constructor -> (
          match values.(0) with
          | Constructor (actual, _) -> Boolean (constructor = actual)
          | _ -> unavailable "Datatype tester model unavailable")
        | Term.Selector (constructor, index) -> (
          match values.(0) with
          | Constructor (actual, fields) when constructor = actual ->
            fields.(index)
          | _ -> apply t symbol values)
        | Term.Uninterpreted -> apply t symbol values
        | Term.Builtin ("div" | "mod")
          when Z.is_zero (integer values.(1)) -> apply t symbol values
        | Term.Builtin name -> builtin t name signature.result values)))

and builtin t name result values =
  let ints operation = Integer (operation (Array.map integer values)) in
  match name with
  | "not" -> Boolean (not (boolean values.(0)))
  | "and" -> Boolean (Array.for_all boolean values)
  | "or" -> Boolean (Array.exists boolean values)
  | "xor" -> Boolean (boolean values.(0) <> boolean values.(1))
  | "=" -> Boolean (equal_value t values.(0) values.(1))
  | "distinct" ->
    let seen = Hashtbl.create (Array.length values) in
    Boolean (Array.for_all (fun value ->
      t.poll ();
      let hash = hash_value t value in
      let bucket = Option.value (Hashtbl.find_opt seen hash) ~default:[] in
      if List.exists (equal_value t value) bucket then false
      else begin Hashtbl.replace seen hash (value :: bucket); true end) values)
  | "+" -> ints (Array.fold_left (Z.add ~poll:t.poll) Z.zero)
  | "*" -> ints (Array.fold_left (Z.mul ~poll:t.poll) Z.one)
  | "-" ->
    ints (fun xs ->
        if Array.length xs = 1
        then Z.neg xs.(0)
        else
          Array.fold_left (Z.sub ~poll:t.poll) xs.(0)
            (Array.sub xs 1 (Array.length xs - 1)))
  | "int.log2" -> Integer (Z.log2 ~poll:t.poll (integer values.(0)))
  | "int.pow2" | "piand" ->
    let value = if name = "int.pow2"
      then Z.pow2 ~poll:t.poll (integer values.(0))
      else Z.piand ~poll:t.poll (integer values.(0))
          (integer values.(1)) (integer values.(2)) in
    (match value with
    | Some value -> Integer value
    | None ->
      unavailable "Integer extension model exceeds materialization limit")
  | "div" | "mod" ->
    let a = integer values.(0) and b = integer values.(1) in
    if Z.is_zero b then unavailable "Division-by-zero model unavailable";
    let q, r = Z.div_rem ~poll:t.poll a b in
    Integer (if name = "div" then q else r)
  | "<" | "<=" | ">" | ">=" ->
    let c = Z.compare (integer values.(0)) (integer values.(1)) in
    Boolean
      (match name with
      | "<" -> c < 0
      | "<=" -> c <= 0
      | ">" -> c > 0
      | _ -> c >= 0)
  | "bv2int" | "ubv_to_int" | "sbv_to_int" -> (
    match values.(0) with
    | Bits (width, z) ->
      if
        name = "sbv_to_int"
        && Z.compare z (Z.pow ~poll:t.poll (Z.of_int 2) (width - 1)) >= 0
      then Integer
        (Z.sub ~poll:t.poll z (Z.pow ~poll:t.poll (Z.of_int 2) width))
      else Integer z
    | _ -> unavailable "Expected a bitvector model value")
  | _ -> constant_bv t name result values

and apply t symbol values =
  let entries, fallback = interpretation t symbol in
  match List.find_opt (fun (arguments, _) ->
      Array.for_all2 (equal_value t) arguments values) entries with
  | Some (_, result) -> result
  | None -> fallback

and interpretation t symbol =
  t.poll ();
  match Hashtbl.find_opt t.functions symbol with
  | Some function_ -> function_
  | None ->
    let signature = Term.Unsafe.signature t.terms symbol in
    let buckets = Hashtbl.create 4 in
    let entries =
      List.fold_left
        (fun entries term ->
          t.poll ();
          match Term.Unsafe.node t.terms term with
          | Term.Apply (candidate, arguments)
            when candidate = symbol
                 && Option.is_some
                   (raw_value t term) -> (
            let arguments = Array.map (value t) arguments in
            let result = value t term in
            let hash = Array.fold_left (fun hash value ->
              (hash lxor hash_value t value) * 65599) 17 arguments in
            let bucket = Option.value (Hashtbl.find_opt buckets hash)
                ~default:[] in
            match List.find_opt (fun (other, _) ->
              Array.for_all2 (equal_value t) arguments other) bucket with
            | None ->
              Hashtbl.replace buckets hash ((arguments, result) :: bucket);
              (arguments, result) :: entries
            | Some (_, other) when equal_value t result other -> entries
            | Some _ -> unavailable "Inconsistent function model")
          | _ -> entries)
        []
        (Option.value (Hashtbl.find_opt t.applications symbol) ~default:[])
    in
    let fallback =
      match entries with
      | (_, result) :: _ -> result
      | [] -> default t [] signature.result
    in
    let function_ = entries, fallback in
    Hashtbl.add t.functions symbol function_;
    function_

type layer =
  { constructor : Term.symbol;
    field : int;
    fields : value array;
    height : int
  }

type family =
  | Scalar of Term.sort * layer list
  | Recursive of layer list * layer list * value ref * int ref

let rec height t = function
  | Boolean _ | Integer _ | Bits _ | Abstract _ -> t.poll (); 0
  | Constructor (_, fields) as value ->
    t.poll ();
    let compute () =
      1 + Array.fold_left (fun n field -> max n (height t field)) 0 fields in
    if Array.length fields <= 1 then compute () else
    (match Values.find_opt t.heights value with
    | Some height -> height
    | None ->
      let height = compute () in
      Values.add t.heights value height;
      height)

let layer t constructor field =
  let signature = Term.Unsafe.signature t.terms constructor in
  let fields = Array.map (default t []) signature.arguments in
  let maximum = ref 0 in
  Array.iteri (fun index value ->
    if index <> field then maximum := max !maximum (height t value)) fields;
  { constructor; field; fields; height = !maximum }

let wrap t layers (value, height) =
  List.fold_right (fun layer (value, height) ->
    t.poll ();
    let fields = Array.copy layer.fields in
    fields.(layer.field) <- value;
    Constructor (layer.constructor, fields),
    1 + max layer.height height) layers (value, height)

let family t sort =
  let children visit datatype =
    let rec constructors = function
      | [] -> None
      | constructor :: rest ->
        let signature = Term.Unsafe.signature t.terms constructor in
        let rec fields index =
          if index = Array.length signature.arguments
          then constructors rest
          else match visit signature.arguments.(index) with
          | None -> fields (index + 1)
          | Some result -> Some (constructor, index, result)
        in
        fields 0
    in
    constructors (List.sort Int.compare (Term.constructors t.terms datatype))
  in
  let discovered = Hashtbl.create 16 in
  let rec scalar sort =
    t.poll ();
    match sort with
    | Term.Int -> Some (sort, [])
    | Term.Named id when Term.constructors t.terms id = [] ->
      Some (sort, [])
    | Term.Named id when not (Hashtbl.mem discovered id) ->
      Hashtbl.add discovered id ();
      Option.map (fun (constructor, index, (source, layers)) ->
        source, layer t constructor index :: layers)
        (children scalar id)
    | Term.Bool | Term.Bitvec _ | Term.Named _ -> None
  in
  match scalar sort with
  | Some (source, layers) -> Scalar (source, layers)
  | None ->
    Hashtbl.clear discovered;
    let rec cycle visiting sort =
      t.poll ();
      match sort with
      | Term.Named id when List.mem id visiting -> Some (sort, [])
      | Term.Named id when not (Hashtbl.mem discovered id) ->
        Hashtbl.add discovered id ();
        Option.map (fun (constructor, index, (target, layers)) ->
          target, (sort, layer t constructor index) :: layers)
          (children (cycle (id :: visiting)) id)
      | Term.Bool | Term.Int | Term.Bitvec _ | Term.Named _ -> None
    in
    match cycle [] sort with
    | None -> unavailable "Unexpanded finite datatype model class"
    | Some (target, layers) ->
      let rec split prefix = function
        | [] -> unavailable "Recursive datatype model family invariant"
        | (sort, _) :: _ as layers when sort = target ->
          List.rev prefix, List.map snd layers
        | (_, layer) :: layers -> split (layer :: prefix) layers
      in
      let prefix, cycle = split [] layers in
      let value = default t [] target in
      Recursive (prefix, cycle, ref value, ref (height t value))

let complete t free =
  (* Finite classes already have constructors. Fresh scalar leaves and
     recursive heights separated by the constructor graph depth preserve
     every distinction between the remaining classes and constructor terms. *)
  let depths = Hashtbl.create 32 in
  let active = Hashtbl.create 32 in
  let rec depth representative =
    t.poll ();
    match Hashtbl.find_opt depths representative with
    | Some depth -> depth
    | None ->
      if Hashtbl.mem active representative
      then unavailable "Cyclic datatype model graph";
      Hashtbl.add active representative ();
      let result = match Hashtbl.find_opt t.constructors representative with
        | None -> 0
        | Some term ->
          match Term.Unsafe.node t.terms term with
          | Term.Apply (_, fields) ->
            1 + Array.fold_left (fun n field ->
              match raw_value t field with
              | Some (Engine.Abstract representative) ->
                max n (depth representative)
              | _ -> n) 0 fields
          | _ -> unavailable "Datatype constructor model invariant"
      in
      Hashtbl.remove active representative;
      Hashtbl.add depths representative result;
      result
  in
  let gap = Hashtbl.fold (fun representative _ n ->
    max n (depth representative)) t.constructors 0 in
  let families = Hashtbl.create 16 in
  let lower = ref gap in
  List.iter (fun (_, sort) ->
    if not (Hashtbl.mem families sort) then begin
      let family = family t sort in
      let layers = match family with
        | Scalar (_, layers) -> layers
        | Recursive (prefix, cycle, _, _) -> prefix @ cycle in
      List.iter (fun layer -> lower := max !lower (gap + layer.height))
        layers;
      Hashtbl.add families sort family
    end) free;
  let integer = ref Z.zero in
  let abstracts = Hashtbl.create 16 in
  List.iter (fun term ->
    t.poll ();
    match raw_value t term with
    | Some (Engine.Integer value) ->
      if Z.compare value !integer > 0 then integer := value
    | Some (Engine.Abstract representative) ->
      (match Term.sort t.terms term with
      | Term.Named sort when Term.constructors t.terms sort = [] ->
        let lowest = Option.value (Hashtbl.find_opt abstracts sort)
            ~default:(-1) in
        Hashtbl.replace abstracts sort (min lowest representative)
      | _ -> ())
    | _ -> ()) t.observed;
  List.iter (fun (representative, sort) ->
    t.poll ();
    let value = match Hashtbl.find families sort with
      | Scalar (source, layers) ->
        let value = match source with
          | Term.Int ->
            integer := Z.add ~poll:t.poll !integer Z.one;
            Integer !integer
          | Term.Named sort ->
            let next = Option.value (Hashtbl.find_opt abstracts sort)
                ~default:(-1) - 1 in
            Hashtbl.replace abstracts sort next;
            Abstract (sort, next)
          | Term.Bool | Term.Bitvec _ ->
            unavailable "Infinite datatype scalar invariant"
        in
        fst (wrap t layers (value, 0))
      | Recursive (prefix, cycle, current, current_height) ->
        let value = ref (wrap t prefix (!current, !current_height)) in
        while snd !value <= !lower + gap do
          let next, next_height = wrap t cycle (!current, !current_height) in
          current := next;
          current_height := next_height;
          value := wrap t prefix (next, next_height)
        done;
        lower := snd !value;
        fst !value
    in
    Hashtbl.replace t.completions representative value) free

let validate t =
  List.iter
    (fun term ->
      t.poll ();
      match Term.Unsafe.node t.terms term,
          raw_value t term with
      | Term.Apply (symbol, arguments), Some raw -> (
        match (Term.Unsafe.signature t.terms symbol).kind, raw with
        | Term.Builtin "=", Engine.Boolean expected ->
          if equal_value t (value t arguments.(0)) (value t arguments.(1))
              <> expected
          then unavailable "Datatype model completion unavailable"
        | Term.Tester constructor, Engine.Boolean expected -> (
          match value t arguments.(0) with
          | Constructor (actual, _) when constructor = actual = expected -> ()
          | _ -> unavailable "Datatype tester model unavailable")
        | Term.Selector (constructor, index), _ -> (
          match value t arguments.(0) with
          | Constructor (actual, fields) when constructor = actual ->
            if not (equal_value t (value t term) fields.(index))
            then unavailable "Datatype selector model unavailable"
          | _ -> ignore (interpretation t symbol))
        | Term.Uninterpreted, _ -> ignore (interpretation t symbol)
        | _ -> ())
      | _ -> ())
    t.observed

let build ?resource ~terms ~solver () =
  let poll () = Option.iter Resource.poll resource in
  Option.iter Resource.check resource;
  poll ();
  let generation = match Engine.model_generation solver with
    | Some generation -> generation
    | None -> unavailable "No satisfying model available" in
  let raw_cache = Hashtbl.create 128 in
  let raw_value term =
    match Hashtbl.find_opt raw_cache term with
    | Some value -> value
    | None ->
      let value = Engine.raw_value ?resource solver term in
      Hashtbl.add raw_cache term value;
      value in
  let observed = Engine.model_terms ?resource solver
    |> List.filter (fun term -> Option.is_some (raw_value term)) in
  let applications = Hashtbl.create 32 in
  List.iter
    (fun term ->
      poll ();
      match Term.Unsafe.node terms term with
      | Term.Apply (symbol, _) ->
        let old =
          Option.value (Hashtbl.find_opt applications symbol) ~default:[]
        in
        Hashtbl.replace applications symbol (term :: old)
      | _ -> ())
    (List.rev observed);
  let t =
    { terms;
      solver;
      generation;
      resource;
      poll;
      busy = false;
      observed;
      raw_cache;
      applications;
      cache = Hashtbl.create 128;
      active = Hashtbl.create 16;
      constructors = Hashtbl.create 32;
      completions = Hashtbl.create 32;
      defaults = Hashtbl.create 16;
      heights = Values.create 16;
      hashes = Values.create 16;
      functions = Hashtbl.create 32
    }
  in
  List.iter
    (fun term ->
      poll ();
      match Term.Unsafe.node terms term,
          raw_value term with
      | Term.Apply (symbol, _), Some (Engine.Abstract representative) -> (
        match (Term.Unsafe.signature terms symbol).kind with
        | Term.Constructor _ ->
          Hashtbl.replace t.constructors representative term
        | _ -> ())
      | _ -> ())
    observed;
  let free = Hashtbl.create 32 in
  List.iter
    (fun term ->
      poll ();
      match Term.sort terms term, raw_value term with
      | (Term.Named id as sort), Some (Engine.Abstract representative)
        when Term.constructors terms id <> []
             && not (Hashtbl.mem t.constructors representative) ->
        Hashtbl.replace free representative sort
      | _ -> ())
    observed;
  let free =
    Hashtbl.fold (fun id sort xs -> (id, sort) :: xs) free []
    |> List.sort compare
  in
  if free <> [] then complete t free;
  validate t;
  let result = t in
  Option.iter Resource.check resource;
  if not (Engine.model_is_current solver generation)
  then unavailable "Model is no longer current";
  result.resource <- None;
  result.poll <- (fun () -> ());
  result

let create ~terms ~solver = build ~terms ~solver ()

let create_with_resource resource ~terms ~solver =
  build ~resource ~terms ~solver ()

type rendered =
  { tag : int;
    head : string;
    fields : int array;
    expanded : int;
    mutable references : int;
    mutable alias : string option
  }

type render_visit =
  | Visit of value
  | Finish of value * Term.symbol * int * bool

let render_dag ?(qualify = fun _ -> false) t ~sort_name value =
  let names = Hashtbl.create 16 in
  let reserve name =
    let length = String.length name in
    let name =
      if length >= 2 && name.[0] = '|' && name.[length - 1] = '|'
      then String.sub name 1 (length - 2) else name in
    Hashtbl.replace names name () in
  let head symbol =
    let signature = Term.Unsafe.signature t.terms symbol in
    reserve signature.name;
    match signature.result with
    | Term.Named id
      when qualify symbol || String.starts_with ~prefix:"(" (sort_name id) ->
      "(as " ^ signature.name ^ " " ^ sort_name id ^ ")"
    | _ -> signature.name in
  let empty =
    { tag = 0; head = ""; fields = [||]; expanded = 0;
      references = 0; alias = None } in
  let nodes = ref (Array.make 16 empty) and count = ref 0 in
  let buckets = Hashtbl.create 16 and seen = Values.create 16 in
  let bounded_add a b = if a > max_int - b then max_int else a + b in
  let intern tag head fields =
    t.poll ();
    let hash = Array.fold_left (fun hash field ->
      t.poll (); hash * 65599 lxor field) (Hashtbl.hash (tag, head)) fields in
    let bucket = Option.value (Hashtbl.find_opt buckets hash) ~default:[] in
    let matching index =
      t.poll ();
      let other = !nodes.(index) in
      tag = other.tag && head = other.head
      && Array.length fields = Array.length other.fields
      && Array.for_all2 (fun a b -> t.poll (); a = b) fields other.fields in
    match List.find_opt matching bucket with
    | Some index -> index
    | None ->
      let index = !count in
      if index = Array.length !nodes then begin
        let larger = Array.make (2 * index) empty in
        Array.blit !nodes 0 larger 0 index;
        nodes := larger
      end;
      let expanded =
        if Array.length fields = 0 then String.length head else
        Array.fold_left (fun size field ->
          t.poll ();
          let child = !nodes.(field) in
          child.references <- child.references + 1;
          bounded_add (bounded_add size 1) child.expanded)
          (bounded_add 2 (String.length head)) fields in
      !nodes.(index) <-
        { tag; head; fields; expanded; references = 0; alias = None };
      incr count;
      Hashtbl.replace buckets hash (index :: bucket);
      index in
  let pending = Stack.create () and results = Stack.create () in
  let unary = function
    | Constructor (_, fields) when Array.length fields = 1 ->
      Some fields.(0)
    | _ -> None in
  Stack.push (Visit value) pending;
  while not (Stack.is_empty pending) do
    t.poll ();
    match Stack.pop pending with
    | Visit (Constructor (symbol, fields) as value) ->
      (match Values.find_opt seen value with
      | Some (-1) -> unavailable "Cyclic model value"
      | Some index -> Stack.push index results
      | None ->
        Values.add seen value (-1);
        if Array.length fields = 1 then begin
          let cursor = ref value in
          let slow = ref (Some value) and fast = ref (Some value) in
          let first = ref true in
          while Option.is_some (unary !cursor) do
            t.poll ();
            (match !cursor with
            | Constructor (symbol, fields) ->
              Stack.push (Finish (!cursor, symbol, 1, !first)) pending;
              first := false;
              cursor := fields.(0)
            | _ -> unavailable "Unary model value invariant");
            slow := Option.bind !slow unary;
            fast := Option.bind (Option.bind !fast unary) unary;
            (match !slow, !fast with
            | Some left, Some right when left == right ->
              unavailable "Cyclic model value"
            | _ -> ())
          done;
          Stack.push (Visit !cursor) pending
        end else begin
          Stack.push (Finish (value, symbol, Array.length fields, true)) pending;
          for index = Array.length fields - 1 downto 0 do
            t.poll (); Stack.push (Visit fields.(index)) pending
          done
        end)
    | Visit value ->
      let text = match value with
        | Boolean b -> string_of_bool b
        | Integer z ->
          if Z.sign z < 0
          then "(- " ^ Z.to_string ~poll:t.poll (Z.abs z) ^ ")"
          else Z.to_string ~poll:t.poll z
        | Bits (width, z) ->
          Printf.sprintf "(_ bv%s %d)" (Z.to_string ~poll:t.poll z) width
        | Abstract (sort, id) ->
          let name = Printf.sprintf "@value%d_%d" sort (id + 1) in
          reserve name;
          Printf.sprintf "(as %s %s)" name (sort_name sort)
        | Constructor _ -> unavailable "Scalar model value invariant" in
      Stack.push (intern (-1) text [||]) results
    | Finish (value, symbol, length, remember) ->
      let fields = Array.make length 0 in
      for index = length - 1 downto 0 do
        t.poll (); fields.(index) <- Stack.pop results
      done;
      let index = intern symbol (head symbol) fields in
      if remember then Values.replace seen value index;
      Stack.push index results
  done;
  let root = Stack.pop results in
  let next_name = ref 0 in
  let fresh () =
    let rec next () =
      t.poll ();
      let name = "@model" ^ string_of_int !next_name in
      incr next_name;
      if Hashtbl.mem names name then next () else name in
    next () in
  let aliases = ref [] in
  if !nodes.(root).expanded > 256 then
    for index = 0 to !count - 1 do
      t.poll ();
      let node = !nodes.(index) in
      if node.references > 1 && node.expanded >= 24 then begin
        node.alias <- Some (fresh ());
        aliases := index :: !aliases
      end
    done;
  let output = Buffer.create 256 in
  let emit root expand =
    let pending = Stack.create () in
    Stack.push (`Node (root, expand)) pending;
    while not (Stack.is_empty pending) do
      t.poll ();
      match Stack.pop pending with
      | `Text text -> Buffer.add_string output text
      | `Node (index, expand) ->
        let node = !nodes.(index) in
        (match node.alias with
        | Some name when not expand -> Buffer.add_string output name
        | _ ->
          if Array.length node.fields = 0 then
            Buffer.add_string output node.head
          else begin
            Buffer.add_char output '(';
            Buffer.add_string output node.head;
            Stack.push (`Text ")") pending;
            for index = Array.length node.fields - 1 downto 0 do
              t.poll ();
              Stack.push (`Node (node.fields.(index), false)) pending;
              Stack.push (`Text " ") pending
            done
          end)
    done in
  List.iter (fun index ->
    t.poll ();
    match !nodes.(index).alias with
    | None -> unavailable "Model alias invariant"
    | Some name ->
      Buffer.add_string output "(let ((";
      Buffer.add_string output name;
      Buffer.add_char output ' ';
      emit index true;
      Buffer.add_string output ")) ") (List.rev !aliases);
  emit root false;
  List.iter (fun _ -> t.poll (); Buffer.add_char output ')') !aliases;
  Buffer.contents output

let to_string ?qualify t ~sort_name value =
  t.poll ();
  match value with
  | Constructor _ -> render_dag ?qualify t ~sort_name value
  | Boolean b -> string_of_bool b
  | Integer z ->
    if Z.sign z < 0 then "(- " ^ Z.to_string ~poll:t.poll (Z.abs z) ^ ")"
    else Z.to_string ~poll:t.poll z
  | Bits (width, z) ->
    Printf.sprintf "(_ bv%s %d)" (Z.to_string ~poll:t.poll z) width
  | Abstract (sort, id) ->
    Printf.sprintf "(as @value%d_%d %s)" sort (id + 1) (sort_name sort)

let check_current t =
  if not (Engine.model_is_current t.solver t.generation)
  then unavailable "Model is no longer current"

let with_resource ?resource t f =
  if t.busy then invalid_arg "SMT model operation is already active";
  t.busy <- true;
  t.resource <- resource;
  t.poll <- (fun () -> Option.iter Resource.poll resource);
  Fun.protect
    ~finally:(fun () ->
      t.busy <- false;
      t.resource <- None;
      t.poll <- (fun () -> ()))
    (fun () ->
      check_current t;
      Option.iter Resource.check resource;
      t.poll ();
      let result = f () in
      Option.iter Resource.check resource;
      check_current t;
      result)

let value ?resource t term =
  with_resource ?resource t (fun () -> value t term)

let interpretation ?resource t symbol =
  with_resource ?resource t (fun () -> interpretation t symbol)

let to_string ?resource ?qualify t ~sort_name value =
  with_resource ?resource t (fun () -> to_string ?qualify t ~sort_name value)

let equal ?resource t left right =
  with_resource ?resource t (fun () ->
    (match left with Constructor _ -> acyclic_value t left | _ -> ());
    if left != right then
      (match right with Constructor _ -> acyclic_value t right | _ -> ());
    equal_value t left right)
