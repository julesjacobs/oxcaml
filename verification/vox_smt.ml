type datatype =
  { datatype_id : int;
    datatype_label : string
  }

and sort =
  | Bool
  | Int63
  | Int
  | Opaque of int
  | Datatype of datatype

and constructor =
  { constructor_id : int;
    constructor_datatype : datatype;
    constructor_label : string;
    constructor_fields : (string * sort) list
  }

module Datatype = struct
  type t = datatype

  let next = ref 0

  let create ~label =
    if !next = max_int then invalid_arg "Vox_smt.Datatype.create: exhausted";
    let id = !next in
    incr next;
    { datatype_id = id; datatype_label = label }

  let label t = t.datatype_label
end

module Constructor = struct
  type t = constructor

  let next = ref 0

  let create ~datatype ~label fields =
    if !next = max_int then invalid_arg "Vox_smt.Constructor.create: exhausted";
    let id = !next in
    incr next;
    { constructor_id = id;
      constructor_datatype = datatype;
      constructor_label = label;
      constructor_fields = fields
    }

  let label t = t.constructor_label

  let datatype t = t.constructor_datatype

  let fields t = t.constructor_fields
end

type datatype_declaration =
  { datatype : Datatype.t;
    constructors : Constructor.t list
  }

let datatypes_well_founded declarations =
  let declared = Hashtbl.create (List.length declarations) in
  List.iter
    (fun declaration ->
      Hashtbl.replace declared declaration.datatype.datatype_id declaration)
    declarations;
  let inhabited = Hashtbl.create (List.length declarations) in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun declaration ->
        let id = declaration.datatype.datatype_id in
        if
          (not (Hashtbl.mem inhabited id))
          && List.exists
               (fun constructor ->
                 List.for_all
                   (fun (_, sort) ->
                     match sort with
                     | Datatype datatype ->
                       Hashtbl.mem declared datatype.datatype_id
                       && Hashtbl.mem inhabited datatype.datatype_id
                     | Bool | Int63 | Int | Opaque _ -> true)
                   constructor.constructor_fields)
               declaration.constructors
        then begin
          Hashtbl.add inhabited id ();
          changed := true
        end)
      declarations
  done;
  Hashtbl.length inhabited = Hashtbl.length declared

module Symbol = struct
  type t =
    { id : int;
      label : string;
      sort : sort
    }

  let next = ref 0

  let create ~label sort =
    if !next = max_int then invalid_arg "Vox_smt.Symbol.create: exhausted";
    let id = !next in
    incr next;
    { id; label; sort }

  let label t = t.label

  let sort t = t.sort
end

module Function = struct
  type t =
    { id : int;
      label : string;
      arguments : sort list;
      result : sort
    }

  let next = ref 0

  let create ~label ~arguments ~result =
    if !next = max_int then invalid_arg "Vox_smt.Function.create: exhausted";
    let id = !next in
    incr next;
    { id; label; arguments; result }

  let label t = t.label

  let arguments t = t.arguments

  let result t = t.result
end

type op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Bit_and
  | Bit_or
  | Bit_xor
  | Shift_right_logical
  | Shift_left
  | Shift_right_arithmetic
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Not
  | And
  | Or
  | Implies
  | Ite
  | Int_add
  | Int_sub
  | Int_mul
  | Int_div
  | Int_mod
  | Int_neg
  | Int_lt
  | Int_le
  | Int_gt
  | Int_ge
  | Int_of_int63
  | Int63_of_int

type term =
  | Boolean of bool
  | Integer of int64
  | Big_integer of string
  | Var of Symbol.t
  | App of op * term list
  | Call of Function.t * term list
  | Construct of Constructor.t * term list
  | Is of Constructor.t * term
  | Select of Constructor.t * int * term

type labelled_term =
  { label : string;
    term : term
  }

type query =
  { datatypes : datatype_declaration list;
    symbols : Symbol.t list;
    functions : Function.t list;
    facts : labelled_term list;
    goal : labelled_term
  }

exception Sort_error of string

exception Unsupported_target of int

let operator = function
  | Add -> "int63_add"
  | Sub -> "int63_sub"
  | Mul -> "int63_mul"
  | Div -> "int63_div"
  | Rem -> "int63_rem"
  | Neg -> "int63_neg"
  | Bit_and -> "int63_and"
  | Bit_or -> "int63_or"
  | Bit_xor -> "int63_xor"
  | Shift_right_logical -> "int63_lsr"
  | Shift_left -> "int63_lsl"
  | Shift_right_arithmetic -> "int63_asr"
  | Eq -> "="
  | Ne -> "distinct"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Ite -> "ite"
  | Int_add -> "+"
  | Int_sub | Int_neg -> "-"
  | Int_mul -> "*"
  | Int_div -> "div"
  | Int_mod -> "mod"
  | Int_lt -> "<"
  | Int_le -> "<="
  | Int_gt -> ">"
  | Int_ge -> ">="
  | Int_of_int63 -> "int_of_int63"
  | Int63_of_int -> "int63_of_int"

let sort_name = function
  | Bool -> "Bool"
  | Int63 -> "Int"
  | Int -> "Int"
  | Opaque id -> Printf.sprintf "opaque(%d)" id
  | Datatype datatype -> Printf.sprintf "datatype(%s)" datatype.datatype_label

let decimal_integer text =
  let len = String.length text in
  let start = if len > 0 && text.[0] = '-' then 1 else 0 in
  len > start
  && (text.[start] <> '0' || len = 1)
  && String.for_all
       (fun c -> c >= '0' && c <= '9')
       (String.sub text start (len - start))

let error fmt = Printf.ksprintf (fun s -> raise (Sort_error s)) fmt

type operator_signature =
  | Fixed of sort list * sort
  | Equality
  | Conditional

let operator_signature = function
  | Add | Sub | Mul | Div | Rem -> Fixed ([Int63; Int63], Int63)
  | Bit_and | Bit_or | Bit_xor | Shift_right_logical | Shift_left
  | Shift_right_arithmetic ->
    Fixed ([Int63; Int63], Int63)
  | Neg -> Fixed ([Int63], Int63)
  | Lt | Le | Gt | Ge -> Fixed ([Int63; Int63], Bool)
  | Not -> Fixed ([Bool], Bool)
  | And | Or | Implies -> Fixed ([Bool; Bool], Bool)
  | Eq | Ne -> Equality
  | Ite -> Conditional
  | Int_add | Int_sub | Int_mul | Int_div | Int_mod -> Fixed ([Int; Int], Int)
  | Int_neg -> Fixed ([Int], Int)
  | Int_lt | Int_le | Int_gt | Int_ge -> Fixed ([Int; Int], Bool)
  | Int_of_int63 -> Fixed ([Int63], Int)
  | Int63_of_int -> Fixed ([Int], Int63)

let rec term_sort = function
  | Boolean _ -> Bool
  | Integer _ -> Int63
  | Big_integer _ -> Int
  | Var s -> Symbol.sort s
  | Call (f, _) -> Function.result f
  | Construct (constructor, _) -> Datatype constructor.constructor_datatype
  | Is _ -> Bool
  | Select (constructor, index, _) ->
    begin match
      if index < 0
      then None
      else List.nth_opt constructor.constructor_fields index
    with
    | Some (_, sort) -> sort
    | None ->
      error "Invalid selector %d for %s" index constructor.constructor_label
    end
  | App (op, args) -> (
    match operator_signature op, args with
    | Fixed (_, result), _ -> result
    | Equality, _ -> Bool
    | Conditional, [_; t; _] -> term_sort t
    | Conditional, _ -> error "ite expects 3 operands")

let check ?(poll = fun () -> ()) ~int_width q =
  poll ();
  if int_width <> 63 then raise (Unsupported_target int_width);
  let datatypes = Hashtbl.create 16 in
  List.iter
    (fun declaration ->
      let datatype = declaration.datatype in
      if Hashtbl.mem datatypes datatype.datatype_id
      then error "Duplicate SMT datatype %S" datatype.datatype_label;
      if declaration.constructors = []
      then error "SMT datatype %S has no constructors" datatype.datatype_label;
      Hashtbl.add datatypes datatype.datatype_id declaration)
    q.datatypes;
  let constructors = Hashtbl.create 32 in
  List.iter
    (fun declaration ->
      List.iter
        (fun constructor ->
          if
            constructor.constructor_datatype.datatype_id
            <> declaration.datatype.datatype_id
          then
            error "Constructor %S belongs to the wrong datatype"
              constructor.constructor_label;
          if Hashtbl.mem constructors constructor.constructor_id
          then
            error "Duplicate SMT constructor %S" constructor.constructor_label;
          Hashtbl.add constructors constructor.constructor_id constructor)
        declaration.constructors)
    q.datatypes;
  let require_declared_sort = function
    | Datatype datatype when not (Hashtbl.mem datatypes datatype.datatype_id) ->
      error "Undeclared SMT datatype %S" datatype.datatype_label
    | Bool | Int63 | Int | Opaque _ | Datatype _ -> ()
  in
  List.iter
    (fun declaration ->
      List.iter
        (fun constructor ->
          List.iter
            (fun (_, sort) -> require_declared_sort sort)
            constructor.constructor_fields)
        declaration.constructors)
    q.datatypes;
  if not (datatypes_well_founded q.datatypes)
  then error "SMT datatype declarations are not well-founded";
  let declared = Hashtbl.create 16 in
  List.iter
    (fun s ->
      if Hashtbl.mem declared s.Symbol.id
      then error "Duplicate SMT symbol %S" (Symbol.label s);
      require_declared_sort (Symbol.sort s);
      Hashtbl.add declared s.Symbol.id ())
    q.symbols;
  let functions = Hashtbl.create 16 in
  List.iter
    (fun f ->
      if Hashtbl.mem functions f.Function.id
      then error "Duplicate SMT function %S" (Function.label f);
      List.iter require_declared_sort (Function.arguments f);
      require_declared_sort (Function.result f);
      Hashtbl.add functions f.Function.id ())
    q.functions;
  let require expected actual =
    if actual <> expected
    then error "Expected %s, got %s" (sort_name expected) (sort_name actual)
  in
  let rec infer term =
    poll ();
    match term with
    | Boolean _ -> Bool
    | Integer n ->
      if n < -4611686018427387904L || n > 4611686018427387903L
      then error "Integer %Ld is outside the signed 63-bit range" n;
      Int63
    | Big_integer text ->
      if not (decimal_integer text)
      then error "Invalid unbounded integer constant %S" text;
      Int
    | Var s ->
      if not (Hashtbl.mem declared s.Symbol.id)
      then error "Undeclared SMT symbol %S" (Symbol.label s);
      Symbol.sort s
    | Call (f, args) ->
      if not (Hashtbl.mem functions f.Function.id)
      then error "Undeclared SMT function %S" (Function.label f);
      if List.length args <> List.length (Function.arguments f)
      then error "Wrong arity for SMT function %S" (Function.label f);
      List.iter2 require (Function.arguments f) (List.map infer args);
      Function.result f
    | Construct (constructor, args) ->
      if not (Hashtbl.mem constructors constructor.constructor_id)
      then error "Undeclared SMT constructor %S" constructor.constructor_label;
      if List.length args <> List.length constructor.constructor_fields
      then
        error "Wrong arity for SMT constructor %S" constructor.constructor_label;
      List.iter2 require
        (List.map snd constructor.constructor_fields)
        (List.map infer args);
      Datatype constructor.constructor_datatype
    | Is (constructor, value) ->
      if not (Hashtbl.mem constructors constructor.constructor_id)
      then error "Undeclared SMT constructor %S" constructor.constructor_label;
      require (Datatype constructor.constructor_datatype) (infer value);
      Bool
    | Select (constructor, index, value) ->
      if not (Hashtbl.mem constructors constructor.constructor_id)
      then error "Undeclared SMT constructor %S" constructor.constructor_label;
      require (Datatype constructor.constructor_datatype) (infer value);
      begin match
        if index < 0
        then None
        else List.nth_opt constructor.constructor_fields index
      with
      | Some (_, sort) -> sort
      | None ->
        error "Invalid selector %d for %S" index constructor.constructor_label
      end
    | App (op, args) -> (
      let signature = operator_signature op in
      let arity =
        match signature with
        | Fixed (arguments, _) -> List.length arguments
        | Equality -> 2
        | Conditional -> 3
      in
      if List.length args <> arity
      then error "%s expects %d operands" (operator op) arity;
      let sorts = List.map infer args in
      match signature, sorts with
      | Fixed (arguments, result), _ ->
        List.iter2 require arguments sorts;
        result
      | Equality, [a; b] ->
        require a b;
        Bool
      | Conditional, [c; a; b] ->
        require Bool c;
        require a b;
        a
      | _ -> error "Invalid operands for %s" (operator op))
  in
  let fact { label; term } =
    try require Bool (infer term)
    with Sort_error message -> error "%s: %s" label message
  in
  List.iter fact q.facts;
  fact q.goal

(* Each distinct bitwise application becomes a call to an uninterpreted
   function, constrained by facts that hold for the 63-bit operation. *)
let abstract_bitwise q =
  let functions = ref [] in
  let function_for op =
    match List.assq_opt op !functions with
    | Some f -> f
    | None ->
      let f =
        Function.create ~label:(operator op) ~arguments:[Int63; Int63]
          ~result:Int63
      in
      functions := (op, f) :: !functions;
      f
  in
  let applications = ref [] and seen = Hashtbl.create 16 in
  let rec rewrite term =
    match term with
    | Boolean _ | Integer _ | Big_integer _ | Var _ -> term
    | App (((Bit_and | Bit_or | Bit_xor | Shift_right_logical) as op), [a; b])
      ->
      let a = rewrite a and b = rewrite b in
      let call = Call (function_for op, [a; b]) in
      if not (Hashtbl.mem seen call)
      then begin
        Hashtbl.add seen call ();
        applications := (op, a, b, call) :: !applications
      end;
      call
    | App (op, args) -> App (op, List.map rewrite args)
    | Call (f, args) -> Call (f, List.map rewrite args)
    | Construct (c, args) -> Construct (c, List.map rewrite args)
    | Is (c, value) -> Is (c, rewrite value)
    | Select (c, index, value) -> Select (c, index, rewrite value)
  in
  let goal = { q.goal with term = rewrite q.goal.term } in
  let facts = List.map (fun f -> { f with term = rewrite f.term }) q.facts in
  if !functions = []
  then None
  else begin
    let nonnegative = function
      | Integer n -> Boolean (n >= 0L)
      | x -> App (Ge, [x; Integer 0L])
    in
    let negative = function
      | Integer n -> Boolean (n < 0L)
      | x -> App (Lt, [x; Integer 0L])
    in
    let both op a b =
      match op, a, b with
      | And, Boolean true, x | And, x, Boolean true -> x
      | Or, Boolean false, x | Or, x, Boolean false -> x
      | And, Boolean false, _ | And, _, Boolean false -> Boolean false
      | Or, Boolean true, _ | Or, _, Boolean true -> Boolean true
      | _ -> App (op, [a; b])
    in
    let implies guard fact =
      match guard with
      | Boolean true -> [fact]
      | Boolean false -> []
      | guard -> [App (Implies, [guard; fact])]
    in
    let eq a b = App (Eq, [a; b]) and le a b = App (Le, [a; b]) in
    (* 2^k - 1 for 1 <= k <= 61 gives Some 2^k. *)
    let low_mask = function
      | Integer m when m > 0L && m < 0x2000_0000_0000_0000L ->
        let p = Int64.succ m in
        if Int64.logand p m = 0L then Some p else None
      | _ -> None
    in
    let euclidean_mod a p =
      let r = App (Rem, [a; Integer p]) in
      App (Ite, [App (Lt, [r; Integer 0L]); App (Add, [r; Integer p]); r])
    in
    let facts_for (op, a, b, r) =
      match op with
      | Bit_and ->
        let mask =
          match low_mask b, low_mask a with
          | Some p, _ -> [eq r (euclidean_mod a p)]
          | None, Some p -> [eq r (euclidean_mod b p)]
          | None, None -> (
            match a, b with
            | _, Integer -1L -> [eq r a]
            | Integer -1L, _ -> [eq r b]
            | _ -> [])
        in
        implies (both Or (nonnegative a) (nonnegative b)) (nonnegative r)
        @ implies (both And (negative a) (negative b)) (negative r)
        @ implies (nonnegative a) (le r a)
        @ implies (nonnegative b) (le r b)
        @ mask
      | Bit_or -> (
        let sign = both Or (negative a) (negative b) in
        (match sign with
          | Boolean true -> [negative r]
          | Boolean false -> [nonnegative r]
          | sign -> [eq (negative r) sign])
        @ implies (both Or (nonnegative b) (negative a)) (le a r)
        @ implies (both Or (nonnegative a) (negative b)) (le b r)
        @
        match a, b with
        | _, Integer 0L -> [eq r a]
        | Integer 0L, _ -> [eq r b]
        | _ -> [])
      | Bit_xor -> (
        [eq (negative r) (App (Ne, [negative a; negative b]))]
        @
        match a, b with
        | _, Integer 0L -> [eq r a]
        | Integer 0L, _ -> [eq r b]
        | _ -> [])
      | Shift_right_logical -> (
        match b with
        | Integer 0L -> [eq r a]
        | Integer k when k >= 1L && k <= 61L ->
          let p = Int64.shift_left 1L (Int64.to_int k) in
          [ eq r
              (App
                 ( Ite,
                   [ nonnegative a;
                     App (Div, [a; Integer p]);
                     App
                       ( Add,
                         [ App
                             ( Div,
                               [ App
                                   (Sub, [a; Integer (-0x4000_0000_0000_0000L)]);
                                 Integer p ] );
                           Integer (Int64.shift_left 1L (62 - Int64.to_int k))
                         ] ) ] )) ]
        | Integer 62L ->
          [eq r (App (Ite, [nonnegative a; Integer 0L; Integer 1L]))]
        | Integer 63L -> [eq r (Integer 0L)]
        | Integer _ -> []
        | count ->
          let in_range low =
            both And (le (Integer low) count) (le count (Integer 63L))
          in
          implies
            (both And (in_range 0L) (nonnegative a))
            (both And (nonnegative r) (le r a))
          @ implies (in_range 1L) (nonnegative r))
      | _ -> []
    in
    let bitwise_facts =
      List.concat_map
        (fun application ->
          List.map
            (fun term -> { label = "bitwise abstraction"; term })
            (facts_for application))
        (List.rev !applications)
    in
    Some
      { q with
        functions = q.functions @ List.rev_map snd !functions;
        facts = facts @ bitwise_facts;
        goal
      }
  end

let to_smtlib ?(poll = fun () -> ()) ?resource_limit ?assumptions ~int_width
    ~timeout_ms q =
  check ~poll ~int_width q;
  if timeout_ms <= 0 then invalid_arg "Vox_smt.to_smtlib: timeout_ms";
  Option.iter
    (List.iter (fun s ->
         if Symbol.sort s <> Bool || not (List.memq s q.symbols)
         then invalid_arg "Vox_smt.to_smtlib: assumption"))
    assumptions;
  (match resource_limit with
  | Some limit when limit <= 0 ->
    invalid_arg "Vox_smt.to_smtlib: resource_limit"
  | _ -> ());
  let names = Hashtbl.create 16 in
  List.iteri
    (fun i s -> Hashtbl.add names s.Symbol.id ("v" ^ string_of_int i))
    q.symbols;
  let b = Buffer.create 256 in
  let functions = Hashtbl.create 16 in
  List.iteri
    (fun i f -> Hashtbl.add functions f.Function.id ("f" ^ string_of_int i))
    q.functions;
  let datatype_names = Hashtbl.create 16 in
  let constructor_names = Hashtbl.create 32 in
  let selector_names = Hashtbl.create 64 in
  let next_constructor = ref 0 and next_selector = ref 0 in
  List.iteri
    (fun i declaration ->
      Hashtbl.add datatype_names declaration.datatype.datatype_id
        ("d" ^ string_of_int i);
      List.iter
        (fun constructor ->
          Hashtbl.add constructor_names constructor.constructor_id
            ("c" ^ string_of_int !next_constructor);
          incr next_constructor;
          List.iteri
            (fun index _ ->
              Hashtbl.add selector_names
                (constructor.constructor_id, index)
                ("p" ^ string_of_int !next_selector);
              incr next_selector)
            constructor.constructor_fields)
        declaration.constructors)
    q.datatypes;
  let add s = Buffer.add_string b s in
  let minimum = "(- 4611686018427387904)" in
  let maximum = "4611686018427387903" in
  let modulus = "9223372036854775808" in
  let rec iter f term =
    poll ();
    f term;
    match term with
    | Boolean _ | Integer _ | Big_integer _ | Var _ -> ()
    | App (_, arguments) | Call (_, arguments) | Construct (_, arguments) ->
      List.iter (iter f) arguments
    | Is (_, value) | Select (_, _, value) -> iter f value
  in
  let roots = q.goal.term :: List.map (fun fact -> fact.term) q.facts in
  let uses operator =
    List.exists
      (fun root ->
        let found = ref false in
        iter
          (function App (op, _) when op = operator -> found := true | _ -> ())
          root;
        !found)
      roots
  in
  let uses_general operator =
    List.exists
      (fun root ->
        let found = ref false in
        iter
          (function
            | App (op, [_; Integer divisor]) when op = operator && divisor <> 0L
              ->
              ()
            | App (op, _) when op = operator -> found := true
            | _ -> ())
          root;
        !found)
      roots
  in
  let multiplications =
    let found = ref [] in
    List.iter
      (iter (function
        | App (Mul, _) as multiplication -> found := multiplication :: !found
        | _ -> ()))
      roots;
    List.sort_uniq Stdlib.compare !found
  in
  let int63_results =
    let found = ref [] in
    List.iter
      (iter (function
        | Call (function_, _) as term when Function.result function_ = Int63 ->
          found := term :: !found
        | Select _ as term when term_sort term = Int63 ->
          found := term :: !found
        | _ -> ()))
      roots;
    List.sort_uniq Stdlib.compare !found
  in
  let integer value =
    if value < 0L
    then add (Printf.sprintf "(- %Ld)" (Int64.neg value))
    else add (Int64.to_string value)
  in
  let opaque_ids =
    let collect ids = function
      | Opaque id -> if List.mem id ids then ids else id :: ids
      | Bool | Int63 | Int | Datatype _ -> ids
    in
    let ids =
      List.fold_left (fun ids s -> collect ids (Symbol.sort s)) [] q.symbols
    in
    let ids =
      List.fold_left
        (fun ids f ->
          List.fold_left collect
            (collect ids (Function.result f))
            (Function.arguments f))
        ids q.functions
    in
    let ids =
      List.fold_left
        (fun ids declaration ->
          List.fold_left
            (fun ids constructor ->
              List.fold_left
                (fun ids (_, sort) -> collect ids sort)
                ids constructor.constructor_fields)
            ids declaration.constructors)
        ids q.datatypes
    in
    List.sort Int.compare ids
  in
  let opaque_names = Hashtbl.create 8 in
  List.iteri
    (fun index id -> Hashtbl.add opaque_names id ("s" ^ string_of_int index))
    opaque_ids;
  let has_bitwise =
    List.exists uses
      [ Bit_and;
        Bit_or;
        Bit_xor;
        Shift_right_logical;
        Shift_left;
        Shift_right_arithmetic ]
  in
  let smt_sort = function
    | Bool -> "Bool"
    | Int63 -> if has_bitwise then "(_ BitVec 63)" else "Int"
    | Int -> "Int"
    | Opaque id -> Hashtbl.find opaque_names id
    | Datatype datatype -> Hashtbl.find datatype_names datatype.datatype_id
  in
  let rec term value =
    poll ();
    match value with
    | Boolean v -> add (string_of_bool v)
    | Integer value ->
      if has_bitwise
      then
        add (Printf.sprintf "(_ bv%Ld 63)" (Int64.logand value Int64.max_int))
      else integer value
    | Big_integer text ->
      if text.[0] = '-'
      then add ("(- " ^ String.sub text 1 (String.length text - 1) ^ ")")
      else add text
    | Var s -> add (Hashtbl.find names s.Symbol.id)
    | App (Int_of_int63, [argument]) ->
      if has_bitwise then add "(int63_of_bits ";
      term argument;
      if has_bitwise then add ")"
    | App (Int63_of_int, [argument]) ->
      (* Wraps modulo 2^63, like [int2bv]. *)
      if has_bitwise
      then begin
        add "((_ int2bv 63) ";
        term argument;
        add ")"
      end
      else begin
        add "(- (mod (+ ";
        term argument;
        add " 4611686018427387904) ";
        add modulus;
        add ") 4611686018427387904)"
      end
    | App (Div, [dividend; Integer divisor])
      when divisor <> 0L && not has_bitwise ->
      add "(let ((x ";
      term dividend;
      add ")) (let ((q ";
      if divisor < 0L then add "(- ";
      add "(ite (< x 0) (- (div (- x) ";
      integer (Int64.abs divisor);
      add ")) (div x ";
      integer (Int64.abs divisor);
      add "))";
      if divisor < 0L then add ")";
      add ")) (ite (> q ";
      add maximum;
      add ") (- q ";
      add modulus;
      add ") q)))"
    | App (Rem, [dividend; Integer divisor])
      when divisor <> 0L && not has_bitwise ->
      add "(let ((x ";
      term dividend;
      add ")) (let ((r (mod (ite (< x 0) (- x) x) ";
      integer (Int64.abs divisor);
      add "))) (ite (< x 0) (- r) r)))"
    | Call (f, args) ->
      let name = Hashtbl.find functions f.Function.id in
      if args = []
      then add name
      else begin
        add "(";
        add name;
        List.iter
          (fun arg ->
            add " ";
            term arg)
          args;
        add ")"
      end
    | Construct (constructor, args) ->
      let name = Hashtbl.find constructor_names constructor.constructor_id in
      if args = []
      then add name
      else begin
        add "(";
        add name;
        List.iter
          (fun arg ->
            add " ";
            term arg)
          args;
        add ")"
      end
    | Is (constructor, value) ->
      add "((_ is ";
      add (Hashtbl.find constructor_names constructor.constructor_id);
      add ") ";
      term value;
      add ")"
    | Select (constructor, index, value) ->
      add "(";
      add (Hashtbl.find selector_names (constructor.constructor_id, index));
      add " ";
      term value;
      add ")"
    | App
        ( ((Shift_right_logical | Shift_left | Shift_right_arithmetic) as op),
          [value; count] ) ->
      let shift, unspecified =
        match op with
        | Shift_left -> "bvshl", "int63_lsl_unspecified"
        | Shift_right_arithmetic -> "bvashr", "int63_asr_unspecified"
        | _ -> "bvlshr", "int63_lsr_unspecified"
      in
      add "(ite (and (bvsge ";
      term count;
      add " (_ bv0 63)) (bvsle ";
      term count;
      add " (_ bv63 63))) (";
      add shift;
      add " ";
      term value;
      add " ";
      term count;
      add ") (";
      add unspecified;
      add " ";
      term value;
      add " ";
      term count;
      add "))"
    | App (op, args) ->
      add "(";
      add
        (if has_bitwise
         then
           match op with
           | Mul -> "bvmul"
           | Add -> "bvadd"
           | Sub -> "bvsub"
           | Neg -> "bvneg"
           | Lt -> "bvslt"
           | Le -> "bvsle"
           | Gt -> "bvsgt"
           | Ge -> "bvsge"
           | Bit_and -> "bvand"
           | Bit_or -> "bvor"
           | Bit_xor -> "bvxor"
           | _ -> operator op
         else operator op);
      List.iter
        (fun arg ->
          add " ";
          term arg)
        args;
      add ")"
  in
  add "(set-option :print-success false)\n";
  add "(set-option :produce-models true)\n";
  if Option.is_some assumptions
  then add "(set-option :produce-unsat-cores true)\n";
  add (Printf.sprintf "(set-option :timeout %d)\n" timeout_ms);
  (* Z3 options persist across queries in a session, so every query sets its own
     limit (0: unlimited). *)
  add
    (Printf.sprintf "(set-option :rlimit %d)\n"
       (Option.value resource_limit ~default:0));
  let rec uses_int t =
    term_sort t = Int
    ||
    match t with
    | App (_, args) | Call (_, args) | Construct (_, args) ->
      List.exists uses_int args
    | Is (_, value) | Select (_, _, value) -> uses_int value
    | Boolean _ | Integer _ | Big_integer _ | Var _ -> false
  in
  let has_int =
    List.exists (fun s -> Symbol.sort s = Int) q.symbols
    || List.exists
         (fun f ->
           Function.result f = Int || List.mem Int (Function.arguments f))
         q.functions
    || List.exists (fun f -> uses_int f.term) (q.goal :: q.facts)
  in
  add
    (if
       has_int || opaque_ids <> [] || q.datatypes <> [] || uses_general Div
       || uses_general Rem || has_bitwise
     then "(set-logic ALL)\n"
     else if multiplications = [] && q.functions = []
     then "(set-logic QF_LIA)\n"
     else "(set-logic QF_UFLIA)\n");
  if has_bitwise
  then begin
    add
      "(define-fun int63_of_bits ((x (_ BitVec 63))) Int\n\
      \  (ite (= ((_ extract 62 62) x) #b0) (bv2int x)\n\
      \    (- (bv2int x) 9223372036854775808)))\n"
  end;
  List.iter
    (fun (op, name) ->
      if uses op
      then
        add
          (Printf.sprintf
             "(declare-fun %s ((_ BitVec 63) (_ BitVec 63)) (_ BitVec 63))\n"
             name))
    [ Shift_right_logical, "int63_lsr_unspecified";
      Shift_left, "int63_lsl_unspecified";
      Shift_right_arithmetic, "int63_asr_unspecified" ];
  if uses Add && not has_bitwise
  then
    add
      (Printf.sprintf
         "(define-fun int63_add ((x Int) (y Int)) Int\n\
         \  (ite (> (+ x y) %s) (- (+ x y) %s)\n\
         \    (ite (< (+ x y) %s) (+ (+ x y) %s) (+ x y))))\n"
         maximum modulus minimum modulus);
  if uses Sub && not has_bitwise
  then
    add
      (Printf.sprintf
         "(define-fun int63_sub ((x Int) (y Int)) Int\n\
         \  (ite (> (- x y) %s) (- (- x y) %s)\n\
         \    (ite (< (- x y) %s) (+ (- x y) %s) (- x y))))\n"
         maximum modulus minimum modulus);
  if uses Neg && not has_bitwise
  then
    add
      (Printf.sprintf
         "(define-fun int63_neg ((x Int)) Int\n  (ite (= x %s) %s (- x)))\n"
         minimum minimum);
  if (not has_bitwise) && (uses_general Div || uses_general Rem)
  then add "(define-fun int63_abs ((x Int)) Int (ite (< x 0) (- x) x))\n";
  if has_bitwise && uses Div
  then
    add
      "(define-fun int63_div ((x (_ BitVec 63)) (y (_ BitVec 63))) (_ BitVec \
       63) (ite (= y (_ bv0 63)) (_ bv0 63) (bvsdiv x y)))\n";
  if uses_general Div && not has_bitwise
  then
    add
      (Printf.sprintf
         "(define-fun int63_div ((x Int) (y Int)) Int\n\
         \  (ite (= y 0) 0\n\
         \ (let ((q (ite (= (< x 0) (< y 0))\n\
         \                   (div (int63_abs x) (int63_abs y))\n\
         \                   (- (div (int63_abs x) (int63_abs y))))))\n\
         \ (ite (> q %s) (- q %s) q))))\n"
         maximum modulus);
  if has_bitwise && uses Rem
  then
    add
      "(define-fun int63_rem ((x (_ BitVec 63)) (y (_ BitVec 63))) (_ BitVec \
       63) (ite (= y (_ bv0 63)) (_ bv0 63) (bvsrem x y)))\n";
  if uses_general Rem && not has_bitwise
  then
    add
      "(define-fun int63_rem ((x Int) (y Int)) Int\n\
      \  (ite (= y 0) 0\n\
      \    (let ((r (mod (int63_abs x) (int63_abs y))))\n\
      \      (ite (< x 0) (- r) r))))\n";
  if multiplications <> [] && not has_bitwise
  then add "(declare-fun int63_mul (Int Int) Int)\n";
  List.iter
    (fun id ->
      add
        (Printf.sprintf "(declare-sort %s 0)\n" (Hashtbl.find opaque_names id)))
    opaque_ids;
  if q.datatypes <> []
  then begin
    add "(declare-datatypes (";
    List.iteri
      (fun index declaration ->
        if index > 0 then add " ";
        add "(";
        add (Hashtbl.find datatype_names declaration.datatype.datatype_id);
        add " 0)")
      q.datatypes;
    add ") (";
    List.iteri
      (fun datatype_index declaration ->
        if datatype_index > 0 then add " ";
        add "(";
        List.iteri
          (fun constructor_index constructor ->
            if constructor_index > 0 then add " ";
            add "(";
            add (Hashtbl.find constructor_names constructor.constructor_id);
            List.iteri
              (fun index (_, sort) ->
                add " (";
                add
                  (Hashtbl.find selector_names
                     (constructor.constructor_id, index));
                add " ";
                add (smt_sort sort);
                add ")")
              constructor.constructor_fields;
            add ")")
          declaration.constructors;
        add ")")
      q.datatypes;
    add "))\n"
  end;
  List.iter
    (fun s ->
      add
        (Printf.sprintf "(declare-fun %s () %s)\n"
           (Hashtbl.find names s.Symbol.id)
           (smt_sort (Symbol.sort s))))
    q.symbols;
  List.iter
    (fun f ->
      add
        (Printf.sprintf "(declare-fun %s (%s) %s)\n"
           (Hashtbl.find functions f.Function.id)
           (String.concat " " (List.map smt_sort (Function.arguments f)))
           (smt_sort (Function.result f))))
    q.functions;
  let bounded value =
    if not has_bitwise
    then begin
      add "(assert (and (<= ";
      add minimum;
      add " ";
      term value;
      add ") (<= ";
      term value;
      add " ";
      add maximum;
      add ")))\n"
    end
  in
  List.iter
    (fun symbol ->
      match Symbol.sort symbol with
      | Bool | Int | Opaque _ | Datatype _ -> ()
      | Int63 -> bounded (Var symbol))
    q.symbols;
  List.iter bounded multiplications;
  if not has_bitwise
  then
    List.iter
      (function
        | App (Mul, ([Integer factor; value] | [value; Integer factor])) as
          multiplication ->
          add "(assert (= ";
          term multiplication;
          add " (- (mod (+ (* ";
          integer factor;
          add " ";
          term value;
          add ") 4611686018427387904) 9223372036854775808) ";
          add "4611686018427387904)))\n"
        | _ -> ())
      multiplications;
  List.iter bounded int63_results;
  List.iter
    (fun f ->
      add "(assert ";
      term f.term;
      add ")\n")
    q.facts;
  add "(assert (not ";
  term q.goal.term;
  add "))\n";
  (match assumptions with
  | Some assumptions ->
    (* Z3's tactics do not report cores, so a query with assumptions always uses
       its default solver. *)
    add "(check-sat-assuming (";
    List.iteri
      (fun i s ->
        if i > 0 then add " ";
        term (Var s))
      assumptions;
    add "))\n"
  | None ->
    add
      (if has_bitwise
       then "(check-sat-using (then simplify solve-eqs smt))\n"
       else "(check-sat)\n"));
  Buffer.contents b

type value =
  | Bool_value of bool
  | Int_value of int64
  | Bigint_value of string

type validity =
  | Valid
  | Invalid of (Symbol.t * value) list option
  | Unknown of string option
  | Timeout
  | Failure of string

let explain_invalid query model =
  let names label prefix items =
    let counts = Hashtbl.create (List.length items) in
    List.iter
      (fun item ->
        let name = label item in
        let count = Option.value (Hashtbl.find_opt counts name) ~default:0 in
        Hashtbl.replace counts name (count + 1))
      items;
    let table = Hashtbl.create (List.length items) in
    List.iteri
      (fun index item ->
        let name = label item in
        let name =
          if Hashtbl.find counts name = 1
          then name
          else name ^ "[" ^ prefix ^ string_of_int index ^ "]"
        in
        Hashtbl.add table item name)
      items;
    fun item -> Option.value (Hashtbl.find_opt table item) ~default:(label item)
  in
  let symbol_name = names Symbol.label "v" query.symbols in
  let function_name = names Function.label "f" query.functions in
  let rec term depth = function
    | _ when depth = 0 -> "..."
    | Boolean b -> string_of_bool b
    | Integer n -> Int64.to_string n
    | Big_integer n -> n ^ "Z"
    | Var s -> symbol_name s
    | Call (f, args) -> application depth (function_name f) args
    | Construct (c, args) -> application depth (Constructor.label c) args
    | Is (c, t) -> application depth ("is " ^ Constructor.label c) [t]
    | Select (c, index, t) ->
      application depth (Constructor.label c ^ "." ^ string_of_int index) [t]
    | App (op, args) -> (
      let name =
        match op with
        | Add | Int_add -> "+"
        | Sub | Int_sub -> "-"
        | Mul | Int_mul -> "*"
        | Div | Int_div -> "/"
        | Rem | Int_mod -> "mod"
        | Neg | Int_neg -> "negate"
        | Eq -> "="
        | Ne -> "<>"
        | Lt | Int_lt -> "<"
        | Le | Int_le -> "<="
        | Gt | Int_gt -> ">"
        | Ge | Int_ge -> ">="
        | Not -> "not"
        | And -> "&&"
        | Or -> "||"
        | Implies -> "implies"
        | Ite -> "if"
        | Bit_and -> "land"
        | Bit_or -> "lor"
        | Bit_xor -> "lxor"
        | Shift_right_logical -> "lsr"
        | Shift_left -> "lsl"
        | Shift_right_arithmetic -> "asr"
        | Int_of_int63 -> "Bigint.of_int"
        | Int63_of_int -> "int_of_bigint"
      in
      match args with
      | [left; right] ->
        "("
        ^ term (depth - 1) left
        ^ " " ^ name ^ " "
        ^ term (depth - 1) right
        ^ ")"
      | _ -> application depth name args)
  and application depth name args =
    name ^ "(" ^ String.concat ", " (List.map (term (depth - 1)) args) ^ ")"
  in
  let lines =
    ref ["Goal (" ^ query.goal.label ^ "): " ^ term 8 query.goal.term]
  in
  let add s = lines := s :: !lines in
  List.iter
    (fun fact -> add ("Assumption (" ^ fact.label ^ "): " ^ term 8 fact.term))
    query.facts;
  (match model with
  | None -> add "The solver did not return printable model values."
  | Some values ->
    List.iter
      (fun (symbol, value) ->
        let value =
          match value with
          | Bool_value b -> string_of_bool b
          | Int_value n -> Int64.to_string n
          | Bigint_value n -> n ^ "Z"
        in
        add ("Model: " ^ symbol_name symbol ^ " = " ^ value))
      values);
  if query.functions <> []
  then begin
    add
      ("Opaque functions: "
      ^ String.concat ", " (List.map function_name query.functions));
    add
      "A model of opaque calls may indicate missing facts, not a runtime \
       counterexample."
  end;
  add
    "The solver found a model satisfying the assumptions and falsifying the \
     goal.";
  String.concat "\n" (List.rev !lines)
