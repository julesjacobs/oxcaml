module Source = Vox_smt
module Term = Vox_smt_term
module Engine = Vox_smt_engine
module Model = Vox_smt_model
module Resource = Vox_smt_resource
module Z = Vox_smt_integer.Z

module Symbols = Hashtbl.Make (struct
  type t = Source.Symbol.t
  let equal a b = a == b
  let hash = Source.Symbol.hash
end)

module Functions = Hashtbl.Make (struct
  type t = Source.Function.t
  let equal a b = a == b
  let hash = Source.Function.hash
end)

module Datatypes = Hashtbl.Make (struct
  type t = Source.Datatype.t
  let equal a b = a == b
  let hash = Source.Datatype.hash
end)

module Constructors = Hashtbl.Make (struct
  type t = Source.Constructor.t
  let equal a b = a == b
  let hash = Source.Constructor.hash
end)

let equal_terms ~poll left right =
  if left == right then true else
  let open Source in
  let seen = lazy (Hashtbl.create 16) in
  let leaf = function
    | Boolean _ | Integer _ | Big_integer _ | Var _ -> true
    | App _ | Call _ | Construct _ | Is _ | Select _ -> false in
  let rec equal left right =
    left == right || begin
      poll ();
      match left, right with
      | Boolean a, Boolean b -> a = b
      | Integer a, Integer b -> a = b
      | Big_integer a, Big_integer b -> a = b
      | Var a, Var b -> a == b
      | App (a, xs), App (b, ys) when a = b ->
        arguments left right xs ys
      | Call (a, xs), Call (b, ys) when a == b ->
        arguments left right xs ys
      | Construct (a, xs), Construct (b, ys) when a == b ->
        arguments left right xs ys
      | Is (a, x), Is (b, y) when a == b -> equal x y
      | Select (a, i, x), Select (b, j, y) when a == b && i = j ->
        equal x y
      | _ -> false
    end
  and arguments left right xs ys =
    if xs == ys then true else
    match xs with
    | [] -> ys = []
    | [x] when leaf x -> children xs ys
    | [x; y] when leaf x && leaf y -> children xs ys
    | _ ->
      let seen = Lazy.force seen in
      let hash = Hashtbl.hash_param 2 8 left in
      let previous = Option.value (Hashtbl.find_opt seen hash) ~default:[] in
      if List.exists (fun (a, b) -> left == a && right == b) previous
      then true
      else if children xs ys then begin
        let previous = match previous with
          | a :: b :: c :: _ -> [a; b; c]
          | previous -> previous in
        Hashtbl.replace seen hash ((left, right) :: previous);
        true
      end else false
  and children xs ys =
    match xs, ys with
    | [], [] -> true
    | x :: xs, y :: ys -> poll (); equal x y && children xs ys
    | _ -> false in
  equal left right

module Ids = Set.Make (Int)

type result =
  { validity : Source.validity;
    resources : int option;
    core : Source.Symbol.t list option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled
exception Callback_error of exn * Printexc.raw_backtrace

let callback f =
  try f () with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    raise (Callback_error (exn, backtrace))

let sample clock =
  let value = callback clock in
  (match classify_float value with
  | FP_infinite | FP_nan -> invalid_arg "Vox_smt_embedded: non-finite clock"
  | FP_normal | FP_subnormal | FP_zero -> ());
  value

let encode ~poll ~has_bitwise ~machine_multiplication terms solver resource
    (query : Source.query) assumptions =
  let open Source in
  let module Terms = struct
    let create = Hashtbl.create
    let find_opt cache term =
      let rec find = function
        | [] -> None
        | (other, value) :: rest ->
          if equal_terms ~poll term other then Some value else find rest in
      match Hashtbl.find_opt cache (Hashtbl.hash term) with
      | None -> None | Some bucket -> find bucket
    let add cache term value =
      let hash = Hashtbl.hash term in
      let previous = Option.value (Hashtbl.find_opt cache hash) ~default:[] in
      let rec keep n = function
        | [] -> [] | _ when n = 0 -> []
        | x :: xs -> x :: keep (n - 1) xs in
      Hashtbl.replace cache hash ((term, value) :: keep 7 previous)
    let replace = add
  end in
  let cache = Terms.create 1024 in
  let machine_sort = if has_bitwise then Term.Bitvec 63 else Term.Int in
  let datatypes = Datatypes.create 16 and opaque = Hashtbl.create 8 in
  List.iteri (fun index declaration ->
    poll ();
    Datatypes.add datatypes declaration.datatype index) query.datatypes;
  let next_sort = ref (List.length query.datatypes) in
  let sort = function
    | Bool -> Term.Bool
    | Int -> Term.Int
    | Int63 -> machine_sort
    | Datatype datatype -> Term.Named (Datatypes.find datatypes datatype)
    | Opaque id ->
      let index = match Hashtbl.find_opt opaque id with
        | Some index -> index
        | None ->
          let index = !next_sort in
          incr next_sort;
          Hashtbl.add opaque id index;
          index in
      Term.Named index in
  let constructors = Constructors.create 32 in
  let constructor_count = ref 0 and selector_count = ref 0 in
  List.iter (fun declaration ->
    let datatype = Datatypes.find datatypes declaration.datatype in
    List.iter (fun constructor ->
      poll ();
      let fields = Constructor.fields constructor in
      let symbol = Term.symbol terms
          { name = "c" ^ string_of_int !constructor_count;
            arguments = Array.of_list
              (List.map (fun (_, field) -> poll (); sort field) fields);
            result = Term.Named datatype; kind = Term.Constructor datatype } in
      incr constructor_count;
      Constructors.add constructors constructor symbol;
      List.iteri (fun index (_, field) ->
        poll ();
        ignore (Term.symbol terms
          { name = "p" ^ string_of_int !selector_count;
            arguments = [|Term.Named datatype|]; result = sort field;
            kind = Term.Selector (symbol, index) });
        incr selector_count) fields) declaration.constructors)
    query.datatypes;
  let symbols = Symbols.create 128 in
  List.iteri (fun index symbol ->
    poll ();
    let id = Term.symbol terms
        { name = "v" ^ string_of_int index; arguments = [||];
          result = sort (Symbol.sort symbol); kind = Term.Uninterpreted } in
    let term = Term.apply terms id [||] in
    Symbols.add symbols symbol term) query.symbols;
  let functions = Functions.create 16 in
  List.iteri (fun index function_ ->
    poll ();
    let symbol = Term.symbol terms
        { name = "f" ^ string_of_int index;
          arguments = Array.of_list
            (List.map (fun arg -> poll (); sort arg)
              (Function.arguments function_));
          result = sort (Function.result function_);
          kind = Term.Uninterpreted } in
    Functions.add functions function_ symbol) query.functions;
  let integer n = Term.integer terms (Z.of_string ~poll n) in
  let minimum = integer "-4611686018427387904" in
  let maximum = integer "4611686018427387903" in
  let half = integer "4611686018427387904" in
  let modulus = integer "9223372036854775808" in
  let zero = integer "0" in
  let int op args = Term.builtin terms op Term.Int args in
  let bool op args = Term.builtin terms op Term.Bool args in
  let neg x = int "-" [|x|] in
  let abs x = Term.ite terms (bool "<" [|x; zero|]) (neg x) x in
  let wrap x = int "-" [|int "mod" [|int "+" [|x; half|]; modulus|]; half|] in
  let wrap_once x =
    Term.ite terms (bool ">" [|x; maximum|]) (int "-" [|x; modulus|])
      (Term.ite terms (bool "<" [|x; minimum|])
        (int "+" [|x; modulus|]) x) in
  let signed_quotient x y =
    let quotient = int "div" [|abs x; abs y|] in
    let same_sign = Term.equal terms
        (bool "<" [|x; zero|]) (bool "<" [|y; zero|]) in
    let quotient = Term.ite terms same_sign quotient (neg quotient) in
    Term.ite terms (bool ">" [|quotient; maximum|])
      (int "-" [|quotient; modulus|]) quotient in
  let helpers = Hashtbl.create 4 in
  let helper name argument_sort =
    match Hashtbl.find_opt helpers name with
    | Some symbol -> symbol
    | None ->
      let symbol = Term.symbol terms
          { name; arguments = [|argument_sort; argument_sort|];
            result = argument_sort; kind = Term.Uninterpreted } in
      Hashtbl.add helpers name symbol;
      symbol in
  let multiplications = ref [] and bounded_results = ref Ids.empty in
  let constant_products = Hashtbl.create 16 in
  let rec term source =
    poll ();
    match source with
    | Boolean value -> Term.boolean terms value
    | Var symbol -> Symbols.find symbols symbol
    | _ -> cached_term source
  and cached_term source =
    match Terms.find_opt cache source with
    | Some value when value >= 0 -> value
    | _ ->
      let value = match source with
        | Boolean value -> Term.boolean terms value
        | Integer value ->
          let value = if Sys.int_size >= 63 then
              Z.of_int (Int64.to_int value)
            else Z.of_string ~poll (Int64.to_string value) in
          if has_bitwise then Term.bits terms 63 value
          else Term.integer terms value
        | Big_integer value -> integer value
        | Var symbol -> Symbols.find symbols symbol
        | Call (function_, args) ->
          let result = Term.apply terms (Functions.find functions function_)
              (arguments args) in
          if Function.result function_ = Int63 then
            bounded_results := Ids.add result !bounded_results;
          result
        | Construct (constructor, args) ->
          Term.apply terms (Constructors.find constructors constructor)
            (arguments args)
        | Is (constructor, arg) ->
          Term.apply terms
            (Term.tester terms (Constructors.find constructors constructor))
            [|term arg|]
        | Select (constructor, index, arg) ->
          let selector = Term.selector terms
              (Constructors.find constructors constructor) index in
          let result = Term.apply terms selector [|term arg|] in
          if Source.term_sort source = Int63 then
            bounded_results := Ids.add result !bounded_results;
          result
        | App ((And | Or as op), args) ->
          let seen = Hashtbl.create 16 in
          let rec keep n = function
            | [] -> [] | _ when n = 0 -> []
            | x :: xs -> x :: keep (n - 1) xs in
          let rec collect values source =
            poll ();
            match source with
            | App (other, args) when other = op ->
              let hash = Hashtbl.hash source in
              let previous = Option.value (Hashtbl.find_opt seen hash)
                  ~default:[] in
              if List.exists (fun other -> source == other) previous
              then values else begin
                let values = List.fold_left collect values args in
                let previous = Option.value (Hashtbl.find_opt seen hash)
                    ~default:[] in
                Hashtbl.replace seen hash (source :: keep 7 previous);
                values
              end
            | _ -> term source :: values in
          let args = Array.of_list (List.rev (List.fold_left collect [] args)) in
          if op = And then Term.and_ terms args else Term.or_ terms args
        | App (op, args) -> application op args (arguments args) in
      Terms.replace cache source value;
      value
  and arguments args = Array.of_list (List.map term args)
  and application op source args =
    match op, args with
    | Eq, [|x; y|] -> Term.equal terms x y
    | Ne, [|x; y|] -> Term.not_ terms (Term.equal terms x y)
    | Not, [|x|] -> Term.not_ terms x
    | And, _ -> Term.and_ terms args
    | Or, _ -> Term.or_ terms args
    | Implies, [|x; y|] -> Term.or_ terms [|Term.not_ terms x; y|]
    | Ite, [|condition; left; right|] -> Term.ite terms condition left right
    | Int_of_int63, [|x|] ->
      if has_bitwise then int "sbv_to_int" [|x|] else x
    | Int63_of_int, [|x|] ->
      if has_bitwise then Term.builtin terms "int2bv:63" machine_sort [|x|]
      else wrap x
    | (Shift_left | Shift_right_logical | Shift_right_arithmetic), [|_x; y|] ->
      let operator, name = match op with
        | Shift_left -> "bvshl", "int63_lsl_unspecified"
        | Shift_right_arithmetic -> "bvashr", "int63_asr_unspecified"
        | _ -> "bvlshr", "int63_lsr_unspecified" in
      let lower = Term.bits terms 63 Z.zero in
      let upper = Term.bits terms 63 (Z.of_int 63) in
      let valid = Term.and_ terms
          [|bool "bvsge" [|y; lower|]; bool "bvsle" [|y; upper|]|] in
      Term.ite terms valid (Term.builtin terms operator machine_sort args)
        (Term.apply terms (helper name machine_sort) args)
    | (Add | Sub), _ when not has_bitwise ->
      wrap_once (int (if op = Add then "+" else "-") args)
    | Neg, [|x|] when not has_bitwise ->
      Term.ite terms (Term.equal terms x minimum) minimum (neg x)
    | Mul, [|left; right|] when not has_bitwise ->
      let result = Term.apply terms (helper "int63_mul" Term.Int) args in
      multiplications := (source, result) :: !multiplications;
      (match source with
      | Integer factor :: _ ->
        Hashtbl.replace constant_products result
          (wrap (int "*" [|integer (Int64.to_string factor); right|]))
      | [_; Integer factor] ->
        Hashtbl.replace constant_products result
          (wrap (int "*" [|integer (Int64.to_string factor); left|]))
      | _ -> ());
      result
    | Div, [|x; y|] when not has_bitwise ->
      let quotient = signed_quotient x y in
      (match source with
      | [_; Integer divisor] when divisor <> 0L -> quotient
      | _ -> Term.ite terms (Term.equal terms y zero) zero quotient)
    | Rem, [|x; y|] when not has_bitwise ->
      let remainder = int "mod" [|abs x; abs y|] in
      let remainder = Term.ite terms (bool "<" [|x; zero|])
          (neg remainder) remainder in
      (match source with
      | [_; Integer divisor] when divisor <> 0L -> remainder
      | _ -> Term.ite terms (Term.equal terms y zero) zero remainder)
    | (Div | Rem), [|_x; y|] ->
      let zero = Term.bits terms 63 Z.zero in
      Term.ite terms (Term.equal terms y zero) zero
        (Term.builtin terms (if op = Div then "bvsdiv" else "bvsrem")
          machine_sort args)
    | (Add | Sub | Mul | Neg | Bit_and | Bit_or | Bit_xor), _ ->
      let operator = match op with
        | Add -> "bvadd" | Sub -> "bvsub" | Mul -> "bvmul"
        | Neg -> "bvneg" | Bit_and -> "bvand" | Bit_or -> "bvor"
        | _ -> "bvxor" in
      Term.builtin terms operator machine_sort args
    | (Lt | Le | Gt | Ge), _ ->
      let operator = match op, has_bitwise with
        | Lt, false -> "<" | Le, false -> "<="
        | Gt, false -> ">" | Ge, false -> ">="
        | Lt, true -> "bvslt" | Le, true -> "bvsle"
        | Gt, true -> "bvsgt" | _ -> "bvsge" in
      bool operator args
    | (Int_add | Int_sub | Int_neg | Int_mul | Int_div | Int_mod), _ ->
      let operator = match op with
        | Int_add -> "+" | Int_sub | Int_neg -> "-" | Int_mul -> "*"
        | Int_div -> "div" | _ -> "mod" in
      int operator args
    | (Int_lt | Int_le | Int_gt | Int_ge), _ ->
      let operator = match op with
        | Int_lt -> "<" | Int_le -> "<=" | Int_gt -> ">" | _ -> ">=" in
      bool operator args
    | _ -> invalid_arg "Vox_smt_embedded: validated operator arity" in
  let assertions = ref [] and count = ref 0 in
  let flush () =
    let pending = List.fold_left (fun acc assertion ->
        poll (); assertion :: acc) [] !assertions in
    List.iter (fun assertion ->
      poll (); Engine.assert_ ~resource solver assertion) pending;
    assertions := []; count := 0 in
  let assert_ assertion =
    poll ();
    assertions := assertion :: !assertions;
    incr count;
    if !count = 512 then flush () in
  let bounded term =
    if not has_bitwise then
      assert_ (Term.and_ terms
        [|bool "<=" [|minimum; term|]; bool "<=" [|term; maximum|]|]) in
  List.iter (fun symbol ->
    poll ();
    if Symbol.sort symbol = Int63 then bounded (Symbols.find symbols symbol))
    query.symbols;
  let facts, goal =
    if machine_multiplication && not has_bitwise then
      let facts = List.map (fun fact -> term fact.term) query.facts in
      facts, term query.goal.term
    else begin
      List.iter (fun fact -> assert_ (term fact.term)) query.facts;
      [], term query.goal.term
    end in
  let multiplications = List.sort_uniq (fun (left, _) (right, _) ->
    Source.compare_operands ~poll left right) !multiplications in
  let products = ref Ids.empty in
  let multiplications = List.filter (fun (_, result) ->
    poll ();
    if Ids.mem result !products then false else begin
      products := Ids.add result !products;
      true
    end) multiplications in
  List.iter (fun (_, term) -> poll (); bounded term) multiplications;
  List.iter (fun (_, term) ->
    poll ();
    Option.iter (fun product -> assert_ (Term.equal terms term product))
      (Hashtbl.find_opt constant_products term)) multiplications;
  List.iter assert_ facts;
  Ids.iter (fun term -> poll (); bounded term) !bounded_results;
  assert_ (Term.not_ terms goal);
  let assumptions = Option.map
      (List.map (fun symbol -> poll (); Symbols.find symbols symbol))
      assumptions in
  flush ();
  symbols, assumptions

let check ?(request_model = true) ?(cancelled = fun () -> false)
    ?resource_limit ?assumptions
    ~clock ~timeout_ms ~int_width query =
  if timeout_ms <= 0 then invalid_arg "Vox_smt_embedded.check: timeout_ms";
  Option.iter (fun limit ->
    if limit <= 0 then invalid_arg "Vox_smt_embedded.check: resource_limit")
    resource_limit;
  try
    let started = sample clock in
    let encoded = ref None in
    let first = ref true in
    let resource_clock () =
      if !first then begin first := false; started end else sample clock in
    let resource = Resource.create ?budget:resource_limit
        ~clock:resource_clock ~timeout:(float_of_int timeout_ms /. 1000.)
        ~cancelled:(fun () -> callback cancelled) () in
    let poll () = Resource.poll resource in
    let core = ref None in
    let validity =
      try
        Resource.check resource;
        let features = Source.check_features ~poll ~int_width query in
        Option.iter (fun assumptions ->
          let declared = Symbols.create 128 in
          List.iter (fun symbol -> poll (); Symbols.add declared symbol ())
            query.Source.symbols;
          List.iter (fun symbol ->
            poll ();
            if Source.Symbol.sort symbol <> Source.Bool
               || not (Symbols.mem declared symbol)
            then invalid_arg "Vox_smt_embedded.check: assumption") assumptions)
          assumptions;
        let terms = Term.create () in
        let solver = Engine.create terms in
        Term.with_poll terms poll (fun () ->
          let symbols, encoded_assumptions =
            encode ~poll ~has_bitwise:features.bitvector_encoding
              ~machine_multiplication:features.machine_multiplication
              terms solver resource query assumptions in
          Resource.check resource;
          encoded := Some (sample clock);
          let result = Engine.check ~resource ?assumptions:encoded_assumptions
              solver in
          Resource.check resource;
          let validity = match result with
            | Engine.Unknown reason -> Source.Unknown (Some reason)
            | Engine.Unsat returned ->
              core := Option.map (fun allowed ->
                let originals = Hashtbl.create 16 in
                List.iter (fun symbol ->
                  poll ();
                  Hashtbl.replace originals
                    (Symbols.find symbols symbol) symbol) allowed;
                List.map (fun term ->
                  poll ();
                  match Hashtbl.find_opt originals term with
                  | Some symbol -> symbol
                  | None -> invalid_arg "Vox_smt_embedded: assumption core")
                  returned) assumptions;
              Source.Valid
            | Engine.Sat when not request_model -> Source.Invalid None
            | Engine.Sat ->
              List.iter (fun symbol ->
                poll ();
                Engine.register ~resource solver
                  (Symbols.find symbols symbol)) query.Source.symbols;
              let reconstructed = lazy
                (Model.create_with_resource resource ~terms ~solver) in
              let model = List.filter_map (fun symbol ->
                poll ();
                let term = Symbols.find symbols symbol in
                let value = match Source.Symbol.sort symbol with
                  | Source.Opaque _ | Source.Datatype _ -> None
                  | sort ->
                    let value = Model.value ~resource
                        (Lazy.force reconstructed) term in
                    let decoded = match sort, value with
                      | Source.Bool, Model.Boolean value ->
                        Source.Bool_value value
                      | Source.Int, Model.Integer value ->
                        Source.Bigint_value (Z.to_string ~poll value)
                      | Source.Int63, Model.Integer value ->
                        Source.Int_value
                          (Int64.of_string (Z.to_string ~poll value))
                      | Source.Int63, Model.Bits (63, value) ->
                        let value = Int64.of_string (Z.to_string ~poll value) in
                        Source.Int_value
                          (Int64.shift_right (Int64.shift_left value 1) 1)
                      | _ -> invalid_arg "Vox_smt_embedded: scalar model" in
                    Some decoded in
                Option.map (fun value -> symbol, value) value)
                  query.Source.symbols in
              Source.Invalid (Some model) in
          Resource.check resource;
          validity)
      with
      | Model.Unavailable reason ->
        core := None;
        Source.Unknown (Some ("model unavailable: " ^ reason))
      | Resource.Exhausted reason ->
        core := None;
        match reason with
        | "cancelled" -> raise Cancelled
        | "timeout" -> Source.Timeout
        | reason -> Source.Unknown (Some reason) in
    let finished = sample clock in
    let validity =
      if finished -. started >= float_of_int timeout_ms /. 1000.
      then begin core := None; Source.Timeout end
      else validity in
    let encoded = Option.value !encoded ~default:finished in
    { validity; resources = Some (Resource.used resource); core = !core;
      encoding_seconds = encoded -. started;
      solving_seconds = finished -. encoded }
  with Callback_error (exn, backtrace) ->
    Printexc.raise_with_backtrace exn backtrace
