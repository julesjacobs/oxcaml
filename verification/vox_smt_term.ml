module Z = Vox_smt_integer.Z

type sort =
  | Bool
  | Int
  | Bitvec of int
  | Named of int

type symbol = int

type term = int

type node =
  | Boolean of bool
  | Integer of Z.t
  | Bits of int * Z.t
  | Apply of symbol * term array

type symbol_kind =
  | Uninterpreted
  | Builtin of string
  | Constructor of int
  | Selector of symbol * int
  | Tester of symbol

type signature =
  { name : string;
    arguments : sort array;
    result : sort;
    kind : symbol_kind
  }

module Nodes = Hashtbl.Make (struct
  type t = node

  let equal left right = match left, right with
    | Boolean a, Boolean b -> Bool.equal a b
    | Integer a, Integer b -> Z.equal a b
    | Bits (n, a), Bits (m, b) -> n = m && Z.equal a b
    | Apply (f, xs), Apply (g, ys) ->
      f = g && Array.length xs = Array.length ys
      && Array.for_all2 Int.equal xs ys
    | _ -> false

  let hash = function
    | Boolean b -> if b then 0 else 1
    | Integer z -> Hashtbl.hash z * 4 + 2
    | Bits (width, z) -> (Hashtbl.hash z * 65599) lxor (width * 4 + 3)
    | Apply (symbol, arguments) ->
      Array.fold_left (fun h arg -> h * 65599 lxor arg) symbol arguments
      land max_int
end)

type t =
  { nodes : term Nodes.t;
    mutable terms : (node * sort) array;
    mutable length : int;
    mutable signatures : signature array;
    builtins : (string * sort array * sort, symbol) Hashtbl.t;
    constructors : (int, symbol list) Hashtbl.t;
    selectors : (symbol * int, symbol) Hashtbl.t;
    testers : (symbol, symbol) Hashtbl.t;
    connectives : (term, unit) Hashtbl.t;
    mutable symbols : int;
    mutable poll : unit -> unit;
    mutable rollback : (unit -> unit) list option
  }

let create () =
  { nodes = Nodes.create 1024;
    terms = Array.make 128 (Boolean false, Bool);
    length = 0;
    signatures = [||];
    builtins = Hashtbl.create 64;
    constructors = Hashtbl.create 16;
    selectors = Hashtbl.create 32;
    testers = Hashtbl.create 32;
    connectives = Hashtbl.create 128;
    symbols = 0;
    poll = (fun () -> ());
    rollback = None
  }

let with_poll t poll f =
  let previous = t.poll in
  t.poll <- poll;
  Fun.protect f ~finally:(fun () -> t.poll <- previous)

let replace t table key value =
  (match t.rollback with
  | None -> ()
  | Some undo ->
    let previous = Hashtbl.find_opt table key in
    t.rollback <- Some ((fun () ->
      match previous with
      | None -> Hashtbl.remove table key
      | Some previous -> Hashtbl.replace table key previous) :: undo));
  Hashtbl.replace table key value

let with_temporary t f =
  let length = t.length and symbols = t.symbols in
  let terms = t.terms and signatures = t.signatures in
  let rollback = t.rollback in
  t.rollback <- Some [];
  Fun.protect f ~finally:(fun () ->
    for i = length to t.length - 1 do
      Nodes.remove t.nodes (fst t.terms.(i));
      if i < Array.length terms then terms.(i) <- Boolean false, Bool
    done;
    if Array.length signatures > 0 then
      for i = symbols to min t.symbols (Array.length signatures) - 1 do
        signatures.(i) <- signatures.(0)
      done;
    Option.iter (List.iter (fun undo -> undo ())) t.rollback;
    t.length <- length;
    t.symbols <- symbols;
    t.terms <- terms;
    t.signatures <- signatures;
    t.rollback <- rollback)

let symbol t signature =
  t.poll ();
  let signature = { signature with
    arguments = Array.copy signature.arguments } in
  let id = t.symbols in
  t.symbols <- id + 1;
  if id = Array.length t.signatures then begin
    let signatures = Array.make (max 16 (2 * id)) signature in
    Array.blit t.signatures 0 signatures 0 id;
    t.signatures <- signatures
  end;
  t.signatures.(id) <- signature;
  (match signature.kind with
  | Constructor datatype ->
    let old =
      Option.value (Hashtbl.find_opt t.constructors datatype) ~default:[]
    in
    replace t t.constructors datatype (id :: old)
  | Selector (constructor, index) ->
    replace t t.selectors (constructor, index) id
  | Tester constructor -> replace t t.testers constructor id
  | Uninterpreted | Builtin _ -> ());
  id

let signature t s =
  if s < 0 || s >= t.symbols then raise Not_found;
  t.signatures.(s)

let has_datatypes t = Hashtbl.length t.constructors <> 0

let constructors t datatype =
  Option.value (Hashtbl.find_opt t.constructors datatype) ~default:[]

let selector t constructor index = Hashtbl.find t.selectors (constructor, index)

let tester t constructor =
  match Hashtbl.find_opt t.testers constructor with
  | Some symbol -> symbol
  | None ->
    let sign = signature t constructor in
    symbol t
      { name = "(_ is " ^ sign.name ^ ")";
        arguments = [| sign.result |];
        result = Bool;
        kind = Tester constructor
      }

let valid_term t n =
  if n < 0 || n >= t.length then invalid_arg "SMT term identifier"

let node t n = fst t.terms.(n)

let sort t n = valid_term t n; snd t.terms.(n)

let count t = t.length

let intern t sort node =
  t.poll ();
  match Nodes.find_opt t.nodes node with
  | Some n -> n
  | None ->
    let n = t.length in
    if n = Array.length t.terms then t.terms <- Array.append t.terms t.terms;
    t.terms.(n) <- node, sort;
    t.length <- n + 1;
    Nodes.add t.nodes node n;
    n

let boolean t b = intern t Bool (Boolean b)

let integer t z = intern t Int (Integer z)

let bits t width value =
  if width <= 0 then invalid_arg "SMT bitvector width";
  let _, value =
    Z.div_rem ~poll:t.poll value (Z.pow ~poll:t.poll (Z.of_int 2) width) in
  intern t (Bitvec width) (Bits (width, value))

let apply t symbol args =
  t.poll ();
  let sign = signature t symbol in
  if
    Array.length args <> Array.length sign.arguments
    || not (Array.for_all2 (fun x s -> sort t x = s) args sign.arguments)
  then invalid_arg ("SMT argument sorts: " ^ sign.name);
  match sign.kind, args with
  | Tester constructor, [|x|] -> (
    match node t x with
    | Apply (other, _) -> (
      match (signature t other).kind with
      | Constructor _ -> boolean t (constructor = other)
      | _ -> intern t sign.result (Apply (symbol, Array.copy args)))
    | _ -> intern t sign.result (Apply (symbol, Array.copy args)))
  | Selector (constructor, index), [|x|] -> (
    match node t x with
    | Apply (other, fields) when constructor = other -> fields.(index)
    | _ -> intern t sign.result (Apply (symbol, Array.copy args)))
  | _ -> intern t sign.result (Apply (symbol, Array.copy args))

let raw_builtin t name result args =
  let arguments = Array.map (sort t) args in
  let key = name, arguments, result in
  let s =
    match Hashtbl.find_opt t.builtins key with
    | Some s -> s
    | None ->
      let s = symbol t { name; arguments; result; kind = Builtin name } in
      replace t t.builtins key s;
      s
  in
  apply t s args

module Bitvector_order = struct
  let operation t width term = match node t term with
    | Apply (symbol, args) -> (
      let sign = signature t symbol in
      match sign.kind with
      | Builtin name when sign.result = Bitvec width
          && Array.length args <= 2
          && Array.length sign.arguments = Array.length args
          && Array.for_all ((=) (Bitvec width)) sign.arguments
          && Array.for_all (fun arg -> sort t arg = Bitvec width) args ->
        Some (name, args)
      | _ -> None)
    | _ -> None

  let less_equal t width ~poll left right =
    let remaining = ref 32 in
    let rec prove left right =
      if !remaining = 0 then false else begin
        decr remaining;
        poll ();
        if left = right then true else
        match node t left, node t right with
        | Bits (_, a), Bits (_, b) -> Z.compare a b <= 0
        | Bits (_, a), _ when Z.is_zero a -> true
        | _ ->
          let upper = match operation t width left with
            | Some ("bvand", [|a; b|]) -> prove a right || prove b right
            | Some ("bvlshr", [|value; amount|]) ->
              prove value right ||
              (match operation t width value with
              | Some ("bvmul", [|a; b|]) ->
                (a = amount && prove b right)
                || (b = amount && prove a right)
              | _ -> false)
            | Some ("bvurem", [|a; _|]) -> prove a right
            | _ -> false in
          upper || match operation t width right with
          | Some ("bvor", [|a; b|]) -> prove left a || prove left b
          | _ -> false
      end in
    width > 0 && sort t left = Bitvec width && sort t right = Bitvec width
    && prove left right

  let disjoint t width ~poll left right =
    let shifted value other = match operation t width value with
      | Some ("bvshl", [|_; amount|]) ->
        less_equal t width ~poll other amount
      | _ -> false in
    shifted left right || shifted right left
end

module Bitvector_polynomial = struct
  type atom = Value of term | Shift_power of term
  module Monomials = Map.Make (struct
    type t = atom list
    let compare = compare
  end)

  exception Limit

  let equality ?poll ?(atom = Fun.id) t width left right =
    let external_poll = Option.value poll ~default:t.poll in
    let operation = Bitvector_order.operation t width in
    let relevant term = match operation term with
      | Some (("bvadd" | "bvsub" | "bvmul" | "bvneg" | "bvnot"
              | "bvshl" | "bvor" | "bvxor" | "bvand"), _) -> true
      | _ -> false in
    if width <= 0 || width > 512
        || sort t left <> Bitvec width || sort t right <> Bitvec width
        || not (relevant left || relevant right) then None else
    let work = ref 0 in
    let poll () =
      external_poll ();
      incr work;
      if !work > 256 then raise Limit in
    try
      let modulus = Z.pow ~poll (Z.of_int 2) width in
      let normalize value = snd (Z.div_rem ~poll value modulus) in
      let single monomial coefficient =
        let coefficient = normalize coefficient in
        if Z.is_zero coefficient then Monomials.empty
        else Monomials.singleton monomial coefficient in
      let add left right =
        let result = Monomials.fold (fun monomial value result ->
          poll ();
          let value = match Monomials.find_opt monomial result with
            | None -> value
            | Some old -> normalize (Z.add ~poll old value) in
          if Z.is_zero value then Monomials.remove monomial result
          else Monomials.add monomial value result) right left in
        if Monomials.cardinal result > 32 then raise Limit;
        result in
      let negate value = Monomials.map
          (fun coefficient -> poll (); normalize (Z.neg coefficient)) value in
      let multiply left right =
        Monomials.fold (fun first a result ->
          Monomials.fold (fun second b result ->
            poll ();
            if List.length first + List.length second > 8 then raise Limit;
            let monomial = List.sort (fun a b -> poll (); compare a b)
                (first @ second) in
            add result (single monomial (Z.mul ~poll a b))) right result)
          left Monomials.empty in
      let rec polynomial term =
        poll ();
        match node t term, operation term with
        | Bits (_, value), _ -> single [] value
        | _, Some ("bvadd", [|x; y|]) ->
          add (polynomial x) (polynomial y)
        | _, Some ("bvsub", [|x; y|]) ->
          add (polynomial x) (negate (polynomial y))
        | _, Some ("bvmul", [|x; y|]) ->
          multiply (polynomial x) (polynomial y)
        | _, Some ("bvneg", [|x|]) -> negate (polynomial x)
        | _, Some ("bvnot", [|x|]) ->
          add (single [] Z.minus_one) (negate (polynomial x))
        | _, Some (("bvor" | "bvxor"), [|x; y|])
            when Bitvector_order.disjoint t width ~poll x y ->
          add (polynomial x) (polynomial y)
        | _, Some ("bvand", [|x; y|])
            when Bitvector_order.disjoint t width ~poll x y ->
          Monomials.empty
        | _, Some ("bvshl", [|value; amount|]) ->
          let factor = match node t amount with
            | Bits (_, amount) ->
              if Z.compare amount (Z.of_int width) >= 0
              then Monomials.empty
              else (match Z.to_int_opt amount with
                | Some amount -> single [] (Z.pow ~poll (Z.of_int 2) amount)
                | None -> raise Limit)
            | _ -> single [Shift_power (atom amount)] Z.one in
          multiply (polynomial value) factor
        | _ -> single [Value (atom term)] Z.one in
      let difference = add (polynomial left) (negate (polynomial right)) in
      if Monomials.is_empty difference then Some true
      else if Monomials.cardinal difference = 1
              && Monomials.mem [] difference then Some false
      else None
    with Limit -> None
end

module Bitvector_boolean = struct
  module Monomials = Set.Make (struct
    type t = term list
    let compare = compare
  end)

  exception Limit

  let equality t width ~poll ~atom left right =
    let operation = Bitvector_order.operation t width in
    let relevant term = match operation term with
      | Some (("bvand" | "bvor" | "bvxor" | "bvnot" | "bvnand"
              | "bvnor" | "bvxnor"), _) -> true
      | _ -> false in
    if not (relevant left || relevant right) then None else
    let remaining = ref 256 in
    let poll () =
      if !remaining = 0 then raise Limit;
      decr remaining;
      poll () in
    try
      let ones = Z.sub ~poll (Z.pow ~poll (Z.of_int 2) width) Z.one in
      let single monomial = Monomials.singleton monomial in
      let add left right =
        let result = Monomials.fold (fun monomial result ->
          poll ();
          if Monomials.mem monomial result
          then Monomials.remove monomial result
          else Monomials.add monomial result) right left in
        if Monomials.cardinal result > 32 then raise Limit;
        result in
      let multiply left right =
        Monomials.fold (fun first result ->
          Monomials.fold (fun second result ->
            poll ();
            let monomial = List.sort_uniq (fun a b -> poll (); compare a b)
                (first @ second) in
            if List.length monomial > 8 then raise Limit;
            add result (single monomial)) right result)
          left Monomials.empty in
      let invert value = add (single []) value in
      let rec polynomial term =
        poll ();
        match node t term, operation term with
        | Bits (_, value), _ when Z.is_zero value -> Monomials.empty
        | Bits (_, value), _ when Z.equal value ones -> single []
        | _, Some ("bvnot", [|x|]) -> invert (polynomial x)
        | _, Some (("bvand" | "bvnand" as name), [|x; y|]) ->
          let product =
            if Bitvector_order.disjoint t width ~poll x y
            then Monomials.empty
            else multiply (polynomial x) (polynomial y) in
          if name = "bvnand" then invert product else product
        | _, Some (("bvxor" | "bvxnor" as name), [|x; y|]) ->
          let sum = add (polynomial x) (polynomial y) in
          if name = "bvxnor" then invert sum else sum
        | _, Some (("bvor" | "bvnor" as name), [|x; y|]) ->
          let left = polynomial x and right = polynomial y in
          let union = add (add left right) (multiply left right) in
          if name = "bvnor" then invert union else union
        | _ -> single [atom term] in
      let difference = add (polynomial left) (polynomial right) in
      if Monomials.is_empty difference then Some true
      else if Monomials.equal difference (single []) then Some false
      else None
    with Limit -> None
end

module Bitvector_equality = struct
  exception Limit

  let equality t width left right =
    let relevant term = match node t term with
      | Apply (_, args) -> Array.length args > 0
      | _ -> false in
    if width <= 0 || width > 512 || not (relevant left || relevant right)
    then None else
    let remaining = ref 1024 in
    let poll () =
      if !remaining = 0 then raise Limit;
      decr remaining;
      t.poll () in
    let memo = Hashtbl.create 16 in
    let representatives = Hashtbl.create 16 in
    let rec compare left right =
      poll ();
      if left = right then Some true else
      let key = if left < right then left, right else right, left in
      match Hashtbl.find_opt memo key with
      | Some value -> value
      | None ->
        Hashtbl.add memo key None;
        let value = match sort t left, sort t right with
          | Bitvec width, Bitvec other
              when width = other && width > 0 && width <= 512 ->
            let simplified =
              match Bitvector_polynomial.equality ~poll ~atom
                  t width left right with
              | Some _ as answer -> answer
              | None ->
                Bitvector_boolean.equality t width ~poll ~atom left right in
            (match simplified with
            | Some _ -> simplified
            | None -> congruent left right)
          | _ -> None in
        Hashtbl.replace memo key value;
        value
    and congruent left right =
      let shift_chain name width term =
        let rec collect remaining term amounts =
          poll ();
          if remaining = 0 then term, amounts else
          match Bitvector_order.operation t width term with
          | Some (other, [|value; amount|]) when name = other ->
            let parts = match Bitvector_order.operation t width amount with
              | Some ("bvadd", [|a; b|]) ->
                let shr_by value amount =
                  match Bitvector_order.operation t width value with
                  | Some ("bvlshr", [|_; shift|]) -> shift = amount
                  | _ -> false in
                if shr_by a b || shr_by b a then [a; b] else [amount]
              | _ -> [amount] in
            collect (remaining - 1) value (List.rev_append parts amounts)
          | _ -> term, amounts in
        let base, amounts = collect 8 term [] in
        base, List.sort (fun a b -> poll (); Int.compare a b) amounts in
      let same_shifts = match sort t left with
        | Bitvec width ->
          (match Bitvector_order.operation t width left,
                 Bitvector_order.operation t width right with
          | Some (("bvshl" | "bvlshr" | "bvashr" as first), _),
            Some (second, _) when first = second ->
            let a, xs = shift_chain first width left in
            let b, ys = shift_chain second width right in
            List.length xs = List.length ys && compare a b = Some true
            && List.for_all2 (fun x y -> compare x y = Some true) xs ys
          | _ -> false)
        | _ -> false in
      if same_shifts then Some true else
      match node t left, node t right with
      | Apply (first, xs), Apply (second, ys)
          when first = second && Array.length xs <= 8
               && Array.length xs = Array.length ys ->
        let equal x y = compare x y = Some true in
        if Array.for_all2 equal xs ys then Some true else
        (match sort t left with
        | Bitvec width ->
          (match Bitvector_order.operation t width left,
                 Bitvector_order.operation t width right with
          | Some (("bvadd" | "bvmul" | "bvand" | "bvor" | "bvxor"),
                  [|a; b|]), Some (_, [|c; d|])
              when equal a d && equal b c -> Some true
          | _ -> None)
        | _ -> None)
      | _ -> None
    and atom term =
      match node t term with
      | Apply (symbol, args) when Array.length args > 0 ->
        let previous = Option.value
            (Hashtbl.find_opt representatives symbol) ~default:[] in
        (match List.find_opt (fun other -> compare term other = Some true)
            previous with
        | Some representative -> representative
        | None ->
          if List.length previous < 32 then
            Hashtbl.replace representatives symbol (term :: previous);
          term)
      | _ -> term in
    try compare left right with Limit -> None
end

module Bitvector_cases = struct
  exception Limit

  let equality ~rebuild t width left right =
    let rec shifted depth term =
      if depth = 0 then false else
      match Bitvector_order.operation t width term with
      | Some (("bvshl" | "bvlshr"), _) -> true
      | Some (_, arguments) -> Array.exists (shifted (depth - 1)) arguments
      | None -> false in
    if width <= 0 || width > 64 || sort t left <> Bitvec width
       || sort t right <> Bitvec width
       || not (shifted 3 left || shifted 3 right) then None else
    let external_poll = t.poll in
    let remaining = ref 20000 in
    let charge () =
      if !remaining = 0 then raise Limit;
      decr remaining;
      external_poll () in
    let operation = Bitvector_order.operation t width in
    let pivots left right =
      let seen = Hashtbl.create 32 and counts = Hashtbl.create 32
      and pivots = ref [] in
      let rec collect term =
        charge ();
        let count = Option.value (Hashtbl.find_opt counts term) ~default:0 in
        Hashtbl.replace counts term (count + 1);
        if not (Hashtbl.mem seen term) then begin
          if Hashtbl.length seen = 64 then raise Limit;
          Hashtbl.add seen term ();
          match node t term with
          | Bits (other, _) when other = width -> ()
          | Apply (symbol, [||])
              when (signature t symbol).kind = Uninterpreted
                   && sort t term = Bitvec width -> ()
          | _ ->
            match operation term with
            | Some (("bvneg" | "bvnot" | "bvadd" | "bvsub" | "bvmul"
                    | "bvand" | "bvor" | "bvxor" | "bvudiv" | "bvurem"
                    | "bvsdiv" | "bvsrem" | "bvsmod"
                    | "bvshl" | "bvlshr" | "bvashr" as name), arguments) ->
              Array.iter collect arguments;
              (match name, arguments with
              | ("bvshl" | "bvlshr"), [|_; amount|] ->
                (match node t amount with
                | Bits _ -> ()
                | _ ->
                  if not (List.mem amount !pivots) then
                    pivots := amount :: !pivots)
              | _ -> ())
            | _ -> raise Limit
        end in
      collect left;
      collect right;
      List.sort (fun a b ->
        charge ();
        let order = Int.compare (Hashtbl.find counts b)
            (Hashtbl.find counts a) in
        if order = 0 then Int.compare a b else order) !pivots in
    let rec zero_bits depth term =
      charge ();
      if depth = 0 then 0 else match node t term, operation term with
      | Bits (_, value), _ ->
        let rec count remaining value =
          charge ();
          if remaining = 0 then width else
          let quotient, remainder = Z.div_rem ~poll:charge value (Z.of_int 2) in
          if not (Z.is_zero remainder) then width - remaining else
            count (remaining - 1) quotient in
        count width value
      | _, Some ("bvshl", [|value; amount|]) ->
        (match node t amount with
        | Bits (_, shift) ->
          if Z.compare shift (Z.of_int width) >= 0 then width else
          (match Z.to_int_opt shift with
          | None -> 0
          | Some shift -> min width (shift + zero_bits (depth - 1) value))
        | _ -> 0)
      | _ -> 0 in
    let rec upper depth term =
      charge ();
      if depth = 0 then None else match node t term, operation term with
      | Bits (_, value), _ -> Some value
      | _, Some ("bvlshr", [|value; _|]) -> upper (depth - 1) value
      | _, Some (("bvadd" | "bvmul" as name), [|a; b|]) ->
        (match upper (depth - 1) a, upper (depth - 1) b with
        | Some a, Some b ->
          Some (if name = "bvadd" then Z.add ~poll:charge a b
                else Z.mul ~poll:charge a b)
        | _ -> None)
      | _, Some ("bvand", [|a; b|]) ->
        (match upper (depth - 1) a, upper (depth - 1) b with
        | Some a, Some b -> Some (if Z.compare a b <= 0 then a else b)
        | (Some _ as bound), None | None, (Some _ as bound) -> bound
        | None, None -> None)
      | _ -> None in
    let rec prove depth left right =
      charge ();
      left = right
      || Bitvector_equality.equality t width left right = Some true
      || (depth > 0 && List.exists (split depth left right) (pivots left right))
    and split depth left right pivot =
      let test value =
        let cache = Hashtbl.create 64 in
        let rec replace term =
          charge ();
          match value with
          | Some value when term = pivot -> value
          | _ ->
            match Hashtbl.find_opt cache term with
            | Some result -> result
            | None ->
              let result = match operation term with
                | Some (("bvshl" | "bvlshr"), [|_; amount|])
                    when value = None && amount = pivot -> bits t width Z.zero
                | Some (name, arguments) ->
                  let arguments = Array.map replace arguments in
                  (match name, arguments with
                  | ("bvshl" | "bvlshr"), [|_; amount|]
                      when value = None && amount = pivot ->
                    bits t width Z.zero
                  | "bvlshr", [|input; amount|] ->
                    let zero = match node t amount, upper 8 input with
                      | Bits (_, amount), Some bound ->
                        Z.is_zero bound ||
                        Z.compare amount (Z.log2 ~poll:charge bound) > 0
                      | _ -> false in
                    if zero then bits t width Z.zero
                    else rebuild name (Bitvec width) arguments
                  | _ -> rebuild name (Bitvec width) arguments)
                | None -> term in
              Hashtbl.add cache term result;
              result in
        prove (depth - 1) (replace left) (replace right) in
      if not (test None) then false else
      let zeros = zero_bits 8 pivot in
      let step = if zeros >= 7 then width else min width (1 lsl zeros) in
      let rec small value =
        if value >= width then true else
        let constant = bits t width (Z.of_int value) in
        test (Some constant) && small (value + step) in
      small 0 in
    try with_poll t charge (fun () ->
      if prove 2 left right then Some true else None)
    with Limit -> None
end

let known_integer_multiple t modulus term =
  let remaining = ref 128 in
  let cache = Hashtbl.create 16 in
  let gcd = Z.gcd ~poll:t.poll in
  let rec visit term =
    t.poll ();
    if !remaining = 0 then Z.one else begin
      decr remaining;
      match Hashtbl.find_opt cache term with
      | Some divisor -> divisor
      | None ->
        let divisor = match node t term with
          | Integer value -> gcd modulus value
          | Apply (symbol, arguments) ->
            (match (signature t symbol).kind with
            | Builtin ("+" | "-") ->
              let divisor = ref modulus and index = ref 0 in
              while !index < Array.length arguments && !remaining > 0
                  && not (Z.equal !divisor Z.one) do
                divisor := gcd !divisor (visit arguments.(!index));
                incr index
              done;
              if !index < Array.length arguments then Z.one else !divisor
            | Builtin "*" ->
              let divisor = ref Z.one and index = ref 0 in
              while !index < Array.length arguments && !remaining > 0
                  && not (Z.equal !divisor modulus) do
                divisor := gcd modulus
                  (Z.mul ~poll:t.poll !divisor (visit arguments.(!index)));
                incr index
              done;
              !divisor
            | Builtin "ite" when Array.length arguments = 3 ->
              gcd (visit arguments.(1)) (visit arguments.(2))
            | _ -> Z.one)
          | Boolean _ | Bits _ -> Z.one in
        Hashtbl.add cache term divisor;
        divisor
    end in
  Z.equal (visit term) modulus

let integer_range t term =
  match node t term with
  | Apply (symbol, [|argument|]) ->
    (match (signature t symbol).kind, sort t argument with
    | Builtin ("bv2int" | "ubv_to_int"), Bitvec width when width > 0 ->
      let modulus = Z.pow ~poll:t.poll (Z.of_int 2) width in
      Some (Z.zero, Z.sub ~poll:t.poll modulus Z.one)
    | Builtin "sbv_to_int", Bitvec width when width > 0 ->
      let half = Z.pow ~poll:t.poll (Z.of_int 2) (width - 1) in
      Some (Z.neg half, Z.sub ~poll:t.poll half Z.one)
    | _ -> None)
  | Apply (symbol, [|_; divisor|])
    when (signature t symbol).kind = Builtin "mod" ->
    (match node t divisor with
    | Integer value when not (Z.is_zero value) ->
      Some (Z.zero, Z.sub ~poll:t.poll (Z.abs value) Z.one)
    | _ -> None)
  | _ -> None

let integer_comparison t name left right =
  let compare left right =
    let ordering = Z.compare left right in
    match name with
    | "<" -> ordering < 0 | "<=" -> ordering <= 0
    | ">" -> ordering > 0 | _ -> ordering >= 0 in
  let endpoints term compare =
    Option.bind (integer_range t term) (fun (lower, upper) ->
      let lo = compare lower and hi = compare upper in
      if lo = hi then Some lo else None) in
  match node t left, node t right with
  | _, Integer value -> endpoints left (fun endpoint -> compare endpoint value)
  | Integer value, _ -> endpoints right (compare value)
  | _ -> None

let integer_conversion t width term =
  match node t term with
  | Apply (symbol, [|argument|]) when sort t argument = Int ->
    (match (signature t symbol).kind with
    | Builtin op ->
      op = "int2bv" || op = "int_to_bv"
      || op = "int2bv:" ^ string_of_int width
      || op = "int_to_bv:" ^ string_of_int width
    | _ -> false)
  | _ -> false

let rec builtin t name result args =
  t.poll ();
  let bool b = boolean t b in
  let int z = integer t z in
  match name, Array.to_list args with
  | op, [x] when (match result with
      | Bitvec width when width > 0 ->
        op = "int2bv" || op = "int_to_bv"
        || op = "int2bv:" ^ string_of_int width
        || op = "int_to_bv:" ^ string_of_int width
      | _ -> false) -> (
    let width = match result with
      | Bitvec width -> width
      | _ -> invalid_arg "SMT bitvector conversion sort" in
    match node t x with
    | Integer value -> bits t width value
    | Apply (symbol, [|argument|]) ->
      (match (signature t symbol).kind, sort t argument with
      | Builtin ("bv2int" | "ubv_to_int" | "sbv_to_int" as source),
        Bitvec previous when previous > 0 ->
        if width = previous then argument
        else if width < previous then
          builtin t (Printf.sprintf "extract:%d:0" (width - 1))
            result [|argument|]
        else
          let op = if source = "sbv_to_int" then "sign_extend"
            else "zero_extend" in
          builtin t (op ^ ":" ^ string_of_int (width - previous))
            result [|argument|]
      | _ -> raw_builtin t name result args)
    | _ -> raw_builtin t name result args)
  | ("int.pow2" | "int.log2"), [x] -> (
    match node t x with
    | Integer value ->
      let folded = if name = "int.log2" then Some (Z.log2 ~poll:t.poll value)
        else Z.pow2 ~poll:t.poll value in
      (match folded with Some value -> int value
      | None -> raw_builtin t name result args)
    | _ -> raw_builtin t name result args)
  | "piand", [k; x; y] -> (
    match node t k, node t x, node t y with
    | Integer width, _, _ when Z.sign width <= 0 -> int Z.zero
    | _, Integer value, _ when Z.is_zero value -> int Z.zero
    | _, _, Integer value when Z.is_zero value -> int Z.zero
    | Integer width, Integer left, Integer right ->
      (match Z.piand ~poll:t.poll width left right with
      | Some value -> int value
      | None -> raw_builtin t name result args)
    | _ -> raw_builtin t name result
      (if x <= y then args else [|k; y; x|]))
  | "not", [x] -> (
    match node t x with
    | Boolean b -> bool (not b)
    | Apply (s, a) when (signature t s).kind = Builtin "not" -> a.(0)
    | _ -> raw_builtin t name result args)
  | ("and" | "or"), xs -> (
    let identity = name = "and" in
    let standard_negation argument =
      t.poll ();
      match node t argument with
      | Apply (symbol, _) when
          (signature t symbol).kind = Builtin "not" ->
        Hashtbl.find_opt t.builtins ("not", [|Bool|], Bool) = Some symbol
      | _ -> true in
    let canonical ?(checked = false) args =
      let term = raw_builtin t name Bool args in
      if not (Hashtbl.mem t.connectives term)
          && (checked || Array.for_all standard_negation args) then
        replace t t.connectives term ();
      term in
    let extend child argument =
      match node t child, node t argument with
      | Apply (symbol, members), _
        when (signature t symbol).kind = Builtin name
          && Hashtbl.mem t.connectives child ->
        let simple = match node t argument with
          | Boolean _ -> false
          | Apply (symbol, _) ->
            (signature t symbol).kind <> Builtin name
          | _ -> true in
        if not simple then None else
        let locate value =
          let first = ref 0 and limit = ref (Array.length members) in
          while !first < !limit do
            let middle = !first + ((!limit - !first) / 2) in
            if members.(middle) < value then first := middle + 1
            else limit := middle
          done;
          !first in
        let contains value =
          let index = locate value in
          index < Array.length members && members.(index) = value in
        if contains argument then Some child else
        let opposite = match node t argument with
          | Apply (symbol, [|value|])
            when (signature t symbol).kind = Builtin "not" -> Some value
          | _ ->
            Option.bind
              (Hashtbl.find_opt t.builtins ("not", [|Bool|], Bool))
              (fun symbol -> Nodes.find_opt t.nodes
                (Apply (symbol, [|argument|]))) in
        if Option.fold ~none:false ~some:contains opposite
        then Some (bool (not identity)) else
        let index = locate argument in
        let arguments = Array.init (Array.length members + 1) (fun i ->
          t.poll ();
          if i = index then argument
          else members.(if i < index then i else i - 1)) in
        Some (canonical ~checked:(standard_negation argument) arguments)
      | _ -> None in
    let fast = match xs with
      | [left; right] ->
        (match extend left right with
        | Some _ as result -> result | None -> extend right left)
      | _ -> None in
    match fast with
    | Some term -> term
    | None ->
    let rec flatten acc values =
      t.poll ();
      match values with
      | [] -> (
        let xs = Array.of_list
          (List.sort_uniq (fun a b -> t.poll (); compare a b) acc) in
        let contains value =
          t.poll ();
          let rec search low high =
            if low > high then false else
            let middle = low + (high - low) / 2 in
            let found = xs.(middle) in
            if found = value then true
            else if found < value then search (middle + 1) high
            else search low (middle - 1) in
          search 0 (Array.length xs - 1) in
        if
          Array.exists
            (fun x ->
              match node t x with
              | Apply (symbol, [| argument |])
                when (signature t symbol).kind = Builtin "not" ->
                contains argument
              | _ -> false)
            xs
        then bool (not identity)
        else
          match Array.length xs with
          | 0 -> bool identity
          | 1 -> xs.(0)
          | _ -> canonical xs)
      | x :: xs -> (
        match node t x with
        | Boolean b -> if b = identity then flatten acc xs else bool b
        | Apply (s, a) when (signature t s).kind = Builtin name ->
          flatten acc (Array.to_list a @ xs)
        | _ -> flatten (x :: acc) xs)
    in
    flatten [] xs)
  | "xor", [x; y] -> (
    if x = y
    then bool false
    else
      match node t x, node t y with
      | Boolean false, _ -> y
      | _, Boolean false -> x
      | Boolean true, _ -> builtin t "not" Bool [| y |]
      | _, Boolean true -> builtin t "not" Bool [| x |]
      | _ -> raw_builtin t name result (if x < y then args else [| y; x |]))
  | "=", [x; y] when x = y -> bool true
  | "=", [x; y] -> (
    match node t x, node t y with
    | Boolean x, Boolean y -> bool (x = y)
    | Integer x, Integer y -> bool (Z.equal x y)
    | Bits (_, x), Bits (_, y) -> bool (Z.equal x y)
    | Boolean true, _ when sort t y = Bool -> y
    | _, Boolean true when sort t x = Bool -> x
    | Boolean false, _ when sort t y = Bool -> builtin t "not" Bool [| y |]
    | _, Boolean false when sort t x = Bool -> builtin t "not" Bool [| x |]
    | Apply (first, fields), Apply (second, other_fields)
      when match (signature t first).kind, (signature t second).kind with
           | Constructor _, Constructor _ -> true
           | _ -> false ->
      if first <> second
      then bool false
      else
        builtin t "and" Bool
          (Array.map2
             (fun a b -> builtin t "=" Bool [| a; b |])
             fields other_fields)
    | _ ->
      let simplified = match sort t x with
        | Bitvec width ->
          (match Bitvector_equality.equality t width x y with
          | Some _ as answer -> answer
          | None -> Bitvector_cases.equality ~rebuild:(builtin t)
              t width x y)
        | Bool | Int | Named _ -> None in
      match simplified with
      | Some value -> bool value
      | None -> raw_builtin t name result
          (if x < y then args else [| y; x |]))
  | "ite", [c; x; y] -> (
    match node t c with
    | Boolean b -> if b then x else y
    | _ when x = y -> x
    | _ when result = Bool -> (
      match node t x, node t y with
      | Boolean true, Boolean false -> c
      | Boolean false, Boolean true -> builtin t "not" Bool [| c |]
      | _ -> raw_builtin t name result args)
    | _ -> raw_builtin t name result args)
  | ("bv2int" | "ubv_to_int" | "sbv_to_int"), [x] when result = Int -> (
    let signed = name = "sbv_to_int" in
    let convert width value =
      let modulus = Z.pow ~poll:t.poll (Z.of_int 2) width in
      if signed then
        let half = int (Z.pow ~poll:t.poll (Z.of_int 2) (width - 1)) in
        let shifted = builtin t "+" Int [|value; half|] in
        let remainder = builtin t "mod" Int [|shifted; int modulus|] in
        builtin t "-" Int [|remainder; half|]
      else builtin t "mod" Int [|value; int modulus|] in
    match sort t x, node t x with
    | Bitvec width, Bits (_, value) when width > 0 ->
      if signed then convert width (int value) else int value
    | Bitvec width, Apply (symbol, [|value|])
      when width > 0 && sort t value = Int ->
      let conversion = match (signature t symbol).kind with
        | Builtin op ->
          op = "int2bv" || op = "int_to_bv"
          || op = "int2bv:" ^ string_of_int width
          || op = "int_to_bv:" ^ string_of_int width
        | _ -> false in
      if conversion then convert width value
      else raw_builtin t name result args
    | _ -> raw_builtin t name result args)
  | ("bvult" | "bvule" | "bvugt" | "bvuge"), [x; y] -> (
    match sort t x with
    | Bitvec width when width > 0 ->
      let numeric = match node t x, node t y with
        | Bits _, _ -> integer_conversion t width y
        | _, Bits _ -> integer_conversion t width x
        | _ -> false in
      if numeric then
        let op = match name with
          | "bvult" -> "<" | "bvule" -> "<="
          | "bvugt" -> ">" | _ -> ">=" in
        builtin t op Bool
          [|builtin t "ubv_to_int" Int [|x|];
            builtin t "ubv_to_int" Int [|y|]|]
      else raw_builtin t name result args
    | _ -> raw_builtin t name result args)
  | ("bvneg" | "bvnot"), [x] -> (
    match node t x with
    | Bits (width, value) ->
      bits t width
        (if name = "bvneg"
         then Z.neg value
         else Z.sub ~poll:t.poll
           (Z.sub ~poll:t.poll (Z.pow ~poll:t.poll (Z.of_int 2) width) Z.one)
           value)
    | Apply (symbol, [| argument |])
      when (signature t symbol).kind = Builtin name ->
      argument
    | _ -> raw_builtin t name result args)
  | ( ( "bvadd" | "bvsub" | "bvmul" | "bvand" | "bvor" | "bvxor" | "bvudiv"
      | "bvurem" | "bvsdiv" | "bvsrem" | "bvsmod" | "bvshl" | "bvlshr"
      | "bvashr" ),
      [x; y] ) -> (
    let width =
      match result with
      | Bitvec width -> width
      | _ -> invalid_arg "SMT bitvector result sort"
    in
    let zero () = bits t width Z.zero in
    let ones () = bits t width Z.minus_one in
    let unary op x = builtin t op result [| x |] in
    let is_constant n x =
      match node t x with Bits (_, value) -> Z.equal value n | _ -> false
    in
    let is_zero = is_constant Z.zero in
    let is_one = is_constant Z.one in
    let is_ones x =
      match node t x with
      | Bits (_, value) ->
        Z.equal (Z.add ~poll:t.poll value Z.one)
          (Z.pow ~poll:t.poll (Z.of_int 2) width)
      | _ -> false
    in
    let negates x y =
      match node t x with
      | Apply (symbol, [| argument |]) ->
        (signature t symbol).kind = Builtin "bvnot" && argument = y
      | _ -> false
    in
    let complement = negates x y || negates y x in
    let commutative =
      match name with
      | "bvadd" | "bvmul" | "bvand" | "bvor" | "bvxor" -> true
      | _ -> false
    in
    let raw () =
      raw_builtin t name result
        (if commutative && x > y then [| y; x |] else args)
    in
    match name, node t x, node t y with
    | ("bvadd" | "bvsub" | "bvmul"), Bits (_, a), Bits (_, b) ->
      bits t width
        ((match name with
          | "bvadd" -> Z.add ~poll:t.poll
          | "bvsub" -> Z.sub ~poll:t.poll
          | _ -> Z.mul ~poll:t.poll)
           a b)
    | ("bvshl" | "bvlshr" | "bvashr"), Bits (_, a), Bits (_, b) ->
      let negative = name = "bvashr" && Z.compare a
          (Z.pow ~poll:t.poll (Z.of_int 2) (width - 1)) >= 0 in
      if Z.compare b (Z.of_int width) >= 0 then
        (if negative then ones () else zero ())
      else
        (match Z.to_int_opt b with
        | None -> raw ()
        | Some amount ->
          let divisor = Z.pow ~poll:t.poll (Z.of_int 2) amount in
          let value = if name = "bvshl" then Z.mul ~poll:t.poll a divisor
            else if negative then
              let signed = Z.sub ~poll:t.poll a
                  (Z.pow ~poll:t.poll (Z.of_int 2) width) in
              Z.floor_div ~poll:t.poll signed divisor
            else Z.floor_div ~poll:t.poll a divisor in
          bits t width value)
    | "bvadd", _, _ when is_zero x -> y
    | ("bvadd" | "bvsub"), _, _ when is_zero y -> x
    | "bvsub", _, _ when x = y -> zero ()
    | "bvsub", _, _ when is_zero x -> unary "bvneg" y
    | "bvmul", _, _ when is_zero x || is_zero y -> zero ()
    | "bvmul", _, _ when is_one x -> y
    | "bvmul", _, _ when is_one y -> x
    | "bvmul", _, _ when is_ones x -> unary "bvneg" y
    | "bvmul", _, _ when is_ones y -> unary "bvneg" x
    | "bvmul", _, _ when is_constant (Z.of_int 2) x ->
      builtin t "bvadd" result [| y; y |]
    | "bvmul", _, _ when is_constant (Z.of_int 2) y ->
      builtin t "bvadd" result [| x; x |]
    | ("bvand" | "bvor"), _, _ when x = y -> x
    | "bvxor", _, _ when x = y -> zero ()
    | "bvand", _, _ when is_zero x || is_zero y || complement -> zero ()
    | "bvand", _, _ when is_ones x -> y
    | "bvand", _, _ when is_ones y -> x
    | "bvor", _, _ when is_ones x || is_ones y || complement -> ones ()
    | ("bvor" | "bvxor"), _, _ when is_zero x -> y
    | ("bvor" | "bvxor"), _, _ when is_zero y -> x
    | "bvxor", _, _ when complement -> ones ()
    | "bvxor", _, _ when is_ones x -> unary "bvnot" y
    | "bvxor", _, _ when is_ones y -> unary "bvnot" x
    | "bvudiv", _, _ when is_zero y -> ones ()
    | ("bvudiv" | "bvsdiv"), _, _ when is_one y -> x
    | "bvsdiv", _, _ when is_ones y -> unary "bvneg" x
    | ("bvurem" | "bvsrem" | "bvsmod"), _, _ when is_zero y -> x
    | ("bvurem" | "bvsrem" | "bvsmod"), _, _ when is_one y -> zero ()
    | ("bvsrem" | "bvsmod"), _, _ when is_ones y -> zero ()
    | ("bvshl" | "bvlshr" | "bvashr"), _, _ when is_zero y -> x
    | ("bvshl" | "bvlshr" | "bvashr"), _, _ when is_zero x -> x
    | "bvlshr", _, _
        when Bitvector_order.less_equal t width ~poll:t.poll x y -> zero ()
    | "bvashr", _, _ when is_ones x -> x
    | _ -> raw ())
  | ("+" | "*"), xs when result = Int -> (
    let op, init =
      if name = "+" then Z.add ~poll:t.poll, Z.zero
      else Z.mul ~poll:t.poll, Z.one in
    let constants, rest =
      List.fold_left
        (fun (c, rest) x ->
          match node t x with Integer z -> op c z, rest | _ -> c, x :: rest)
        (init, []) xs
    in
    if name = "*" && Z.is_zero constants
    then int Z.zero
    else
      let rest = List.sort (fun a b -> t.poll (); compare a b) rest in
      let rest =
        if Z.equal constants init then rest else int constants :: rest
      in
      match rest with
      | [] -> int constants
      | [x] -> x
      | _ -> raw_builtin t name result (Array.of_list rest))
  | "-", [x] when result = Int -> (
    match node t x with
    | Integer z -> int (Z.neg z)
    | _ -> raw_builtin t name result args)
  | "-", [x; y] when result = Int -> (
    if x = y
    then int Z.zero
    else
      match node t x, node t y with
      | Integer x, Integer y -> int (Z.sub ~poll:t.poll x y)
      | _, Integer y when Z.is_zero y -> x
      | _ -> raw_builtin t name result args)
  | ("<=" | "<" | ">=" | ">"), [x; y] -> (
    match node t x, node t y with
    | Integer x, Integer y ->
      let c = Z.compare x y in
      bool
        (match name with
        | "<=" -> c <= 0
        | "<" -> c < 0
        | ">=" -> c >= 0
        | _ -> c > 0)
    | _ when x = y -> bool (name = "<=" || name = ">=")
    | _ ->
      match integer_comparison t name x y with
      | Some value -> bool value
      | None -> raw_builtin t name result args)
  | ("div" | "mod"), [x; y] when result = Int -> (
    match node t x, node t y with
    | Integer x, Integer y when not (Z.is_zero y) ->
      let q, r = Z.div_rem ~poll:t.poll x y in
      int (if name = "div" then q else r)
    | _, Integer y when Z.equal (Z.abs y) Z.one ->
      if name = "mod"
      then int Z.zero
      else if Z.equal y Z.one
      then x
      else builtin t "-" Int [| x |]
    | _, Integer divisor when name = "mod" && Z.sign divisor < 0 ->
      builtin t "mod" Int [| x; int (Z.abs divisor) |]
    | Apply (symbol, _), Integer divisor
      when name = "mod" && not (Z.is_zero divisor)
        && (match (signature t symbol).kind with
            | Builtin ("+" | "-" | "*" | "ite") -> true
            | _ -> false)
        && known_integer_multiple t divisor x -> int Z.zero
    | _ -> raw_builtin t name result args)
  | _ -> raw_builtin t name result args

let equal t x y =
  if sort t x <> sort t y then invalid_arg "SMT equality sorts";
  builtin t "=" Bool [| x; y |]

let equal_atom t x y =
  if sort t x <> sort t y then invalid_arg "SMT equality sorts";
  if x = y
  then boolean t true
  else raw_builtin t "=" Bool (if x < y then [| x; y |] else [| y; x |])

let not_ t x = builtin t "not" Bool [| x |]

let and_ t xs = builtin t "and" Bool xs

let or_ t xs = builtin t "or" Bool xs

let ite t c x y =
  if sort t c <> Bool || sort t x <> sort t y
  then invalid_arg "SMT conditional sorts";
  builtin t "ite" (sort t x) [| c; x; y |]

let rec to_string t x =
  match node t x with
  | Boolean b -> string_of_bool b
  | Integer z -> Z.to_string z
  | Bits (w, z) -> Printf.sprintf "(_ bv%s %d)" (Z.to_string z) w
  | Apply (s, a) ->
    let name = (signature t s).name in
    if Array.length a = 0
    then name
    else
      Printf.sprintf "(%s %s)" name
        (String.concat " " (Array.to_list (Array.map (to_string t) a)))

module Unsafe = struct
  let node = node
  let signature = signature
end

let node t term =
  valid_term t term;
  match node t term with
  | Apply (symbol, arguments) -> Apply (symbol, Array.copy arguments)
  | Boolean _ | Integer _ | Bits _ as node -> node

let signature t symbol =
  let signature = signature t symbol in
  { signature with arguments = Array.copy signature.arguments }

let builtin t name result arguments =
  Array.iter (valid_term t) arguments;
  builtin t name result arguments

let not_ t term =
  valid_term t term;
  not_ t term

let and_ t arguments =
  Array.iter (valid_term t) arguments;
  and_ t arguments

let or_ t arguments =
  Array.iter (valid_term t) arguments;
  or_ t arguments

let to_string t term =
  valid_term t term;
  to_string t term
