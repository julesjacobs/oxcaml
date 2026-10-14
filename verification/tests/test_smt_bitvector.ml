module Sat = Vox_sat_solver
module Term = Vox_smt_term
module Bv = Vox_smt_bitvector
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message

let variable terms name sort =
  Term.apply terms
    (Term.symbol terms
       { name; arguments = [||]; result = sort; kind = Term.Uninterpreted })
    [||]

let read model bits =
  Array.fold_right
    (fun lit value ->
      (2 * value) + if model.(abs lit) = (lit > 0) then 1 else 0)
    bits 0

let assumptions bits value =
  Array.mapi
    (fun i lit -> if value land (1 lsl i) <> 0 then lit else -lit)
    bits

let fixture ?completion () =
  let terms = Term.create () in
  let sat = Sat.create () in
  let booleans = Hashtbl.create 4 in
  let boolean term =
    match Hashtbl.find_opt booleans term with
    | Some lit -> lit
    | None ->
      let lit = Sat.new_var sat in
      Hashtbl.add booleans term lit;
      (match Term.node terms term with
      | Term.Boolean b -> Sat.add_clause sat [| (if b then lit else -lit) |]
      | _ -> ());
      lit
  in
  let bv =
    Bv.create ~terms
      ~fresh:(fun () -> Sat.new_var ?completion sat)
      ~clause:(Sat.add_clause_global sat)
      ~boolean ()
  in
  terms, sat, bv, boolean

let exhaustive ?completion width =
  let terms, sat, bv, boolean = fixture ?completion () in
  let sort = Term.Bitvec width in
  let x = variable terms "x" sort and y = variable terms "y" sort in
  let c = variable terms "c" Term.Bool in
  let xb = Bv.bits bv x and yb = Bv.bits bv y in
  let cb = boolean c in
  let mask = (1 lsl width) - 1 in
  let signed n = if n land (1 lsl (width - 1)) = 0 then n else n - mask - 1 in
  let unsigned n = n land mask in
  let quotient a b =
    let a = signed a and b = signed b in
    if b = 0 then if a < 0 then 1 else mask else unsigned (a / b)
  in
  let remainder a b =
    let a = signed a and b = signed b in
    if b = 0 then unsigned a else unsigned (a mod b)
  in
  let modulo a b =
    let a = signed a and b = signed b in
    if b = 0
    then unsigned a
    else
      let r = a mod b in
      unsigned (if r <> 0 && r < 0 <> (b < 0) then r + b else r)
  in
  let operations =
    [ "bvand", ( land );
      "bvor", ( lor );
      "bvxor", ( lxor );
      ("bvnand", fun a b -> unsigned (lnot (a land b)));
      ("bvnor", fun a b -> unsigned (lnot (a lor b)));
      ("bvxnor", fun a b -> unsigned (lnot (a lxor b)));
      ("bvadd", fun a b -> unsigned (a + b));
      ("bvsub", fun a b -> unsigned (a - b));
      ("bvmul", fun a b -> unsigned (a * b));
      ("bvshl", fun a b -> if b >= width then 0 else unsigned (a lsl b));
      ("bvlshr", fun a b -> if b >= width then 0 else a lsr b);
      ( "bvashr",
        fun a b ->
          if b >= width
          then if signed a < 0 then mask else 0
          else unsigned (signed a asr b) );
      ("bvudiv", fun a b -> if b = 0 then mask else a / b);
      ("bvurem", fun a b -> if b = 0 then a else a mod b);
      "bvsdiv", quotient;
      "bvsrem", remainder;
      "bvsmod", modulo ]
    |> List.map (fun (name, expected) ->
        name, Bv.bits bv (Term.builtin terms name sort [| x; y |]), expected)
  in
  let unary =
    [("bvnot", fun a -> unsigned (lnot a)); ("bvneg", fun a -> unsigned (-a))]
    |> List.map (fun (name, expected) ->
        ( name,
          Bv.bits bv (Term.builtin terms name sort [| x |]),
          fun a _ -> expected a ))
  in
  let signed_overflow n = n < -(1 lsl (width - 1)) || n >= 1 lsl (width - 1) in
  let predicates =
    [ "=", ( = );
      "distinct", ( <> );
      "bvult", ( < );
      "bvule", ( <= );
      "bvugt", ( > );
      "bvuge", ( >= );
      ("bvslt", fun a b -> signed a < signed b);
      ("bvsle", fun a b -> signed a <= signed b);
      ("bvsgt", fun a b -> signed a > signed b);
      ("bvsge", fun a b -> signed a >= signed b);
      ("bvuaddo", fun a b -> a + b > mask);
      ("bvsaddo", fun a b -> signed_overflow (signed a + signed b));
      ("bvusubo", fun a b -> a < b);
      ("bvssubo", fun a b -> signed_overflow (signed a - signed b));
      ("bvumulo", fun a b -> a * b > mask);
      ("bvsmulo", fun a b -> signed_overflow (signed a * signed b));
      ("bvsdivo", fun a b -> signed a = -(1 lsl (width - 1)) && signed b = -1)
    ]
    |> List.map (fun (name, expected) ->
        match Bv.predicate bv name [| x; y |] with
        | Some lit -> name, [| lit |], fun a b -> if expected a b then 1 else 0
        | None -> failwith ("Missing predicate " ^ name))
  in
  let result name output args =
    Bv.bits bv (Term.builtin terms name (Term.Bitvec output) args)
  in
  let indexed =
    [ ( "concat",
        result "concat" (2 * width) [| x; y |],
        fun a b -> (a lsl width) lor b );
      ("zero_extend", result "zero_extend:3" (width + 3) [| x |], fun a _ -> a);
      ( "sign_extend",
        result "sign_extend:3" (width + 3) [| x |],
        fun a _ -> signed a land ((1 lsl (width + 3)) - 1) );
      ( "repeat",
        result "repeat:3" (3 * width) [| x |],
        fun a _ -> a lor (a lsl width) lor (a lsl (2 * width)) );
      ( "rotate_left",
        result "rotate_left:7" width [| x |],
        fun a _ ->
          let n = 7 mod width in
          unsigned ((a lsl n) lor (a lsr (width - n))) );
      ( "rotate_right",
        result "rotate_right:7" width [| x |],
        fun a _ ->
          let n = 7 mod width in
          unsigned ((a lsr n) lor (a lsl (width - n))) );
      ( "extract",
        result
          (Printf.sprintf "extract:%d:%d" (width - 1) (width / 2))
          (width - (width / 2))
          [| x |],
        fun a _ -> a lsr (width / 2) );
      ( "bvcomp",
        result "bvcomp" 1 [| x; y |],
        fun a b -> if a = b then 1 else 0 );
      ( "bvredand",
        result "bvredand" 1 [| x |],
        fun a _ -> if a = mask then 1 else 0 );
      ( "bvredor",
        result "bvredor" 1 [| x |],
        fun a _ -> if a <> 0 then 1 else 0 ) ]
  in
  let shifted_divisors =
    List.concat_map (fun base ->
      let denominator = Term.builtin terms "bvshl" sort
          [|Term.bits terms width (Z.of_int base); y|] in
      List.map (fun (name, expected) ->
        name ^ " shifted", Bv.bits bv
          (Term.builtin terms name sort [|x; denominator|]),
        fun a b -> expected a (if b >= width then 0
          else unsigned (base lsl b)))
        ["bvudiv", (fun a b -> if b = 0 then mask else a / b);
         "bvurem", (fun a b -> if b = 0 then a else a mod b)])
      [1; 3]
  in
  let conditional = Bv.bits bv (Term.ite terms c x y) in
  let constant_divisors =
    List.sort_uniq compare [0; 1; 2 land mask; 3 land mask; 1 lsl (width - 1)]
    |> List.concat_map (fun divisor ->
        [ ("bvudiv", fun a b -> if b = 0 then mask else a / b);
          ("bvurem", fun a b -> if b = 0 then a else a mod b);
          "bvsdiv", quotient;
          "bvsrem", remainder;
          "bvsmod", modulo ]
        |> List.map (fun (name, expected) ->
            ( name ^ " constant",
              Bv.bits bv
                (Term.builtin terms name sort
                   [| x; Term.bits terms width (Z.of_int divisor) |]),
              fun a _ -> expected a divisor )))
  in
  let negation_overflow =
    match Bv.predicate bv "bvnego" [| x |] with
    | Some lit ->
      [ ( "bvnego",
          [| lit |],
          fun a _ -> if signed_overflow (-signed a) then 1 else 0 ) ]
    | None -> failwith "Missing negation overflow predicate"
  in
  let rewrites =
    List.concat_map
      (fun constant ->
        List.map
          (fun (name, expected) ->
            ( name ^ " constant",
              Bv.bits bv
                (Term.builtin terms name sort
                   [| x; Term.bits terms width (Z.of_int constant) |]),
              fun a _ -> expected a constant ))
          [ ("bvadd", fun a b -> unsigned (a + b));
            ("bvsub", fun a b -> unsigned (a - b));
            ("bvmul", fun a b -> unsigned (a * b));
            "bvand", ( land );
            "bvor", ( lor );
            "bvxor", ( lxor ) ])
      (List.sort_uniq compare [0; 1; 2 land mask; mask])
  in
  let one_bit_predicates =
    List.concat_map (fun (argument, value) ->
      List.concat_map (fun decrement ->
        let smaller = Term.builtin terms "bvsub" sort
            [|argument; Term.bits terms width (Z.of_int decrement)|] in
        List.concat_map (fun reverse_and ->
          let masked = Term.builtin terms "bvand" sort
              (if reverse_and then [|smaller; argument|]
               else [|argument; smaller|]) in
          List.concat_map (fun reverse_equal ->
            List.map (fun name ->
              let zero = Term.bits terms width Z.zero in
              let args = if reverse_equal then [|zero; masked|]
                         else [|masked; zero|] in
              match Bv.predicate bv name args with
              | None -> failwith "Missing one-bit predicate"
              | Some lit ->
                name ^ " one-bit", [|lit|], fun a b ->
                  let v = value a b in
                  let matches = v land unsigned (v - decrement) = 0 in
                  if matches = (name = "=") then 1 else 0)
              ["="; "distinct"]) [false; true]) [false; true]) [1; 3])
      [x, (fun a _ -> a);
       Term.builtin terms "bvadd" sort [|x; y|],
         (fun a b -> unsigned (a + b))]
  in
  let narrowed_dividends =
    List.concat_map (fun used ->
      let mask = (1 lsl used) - 1 in
      let numerator = if used = 0 then Term.bits terms width Z.zero else
        let low = Term.builtin terms
            (Printf.sprintf "extract:%d:0" (used - 1))
            (Term.Bitvec used) [|x|] in
        Term.builtin terms
          ("zero_extend:" ^ string_of_int (width - used)) sort [|low|] in
      List.map (fun (name, expected) ->
        name ^ " narrowed dividend", Bv.bits bv
          (Term.builtin terms name sort [|numerator; y|]),
        fun a b -> expected (a land mask) b)
        ["bvudiv", (fun a b -> if b = 0 then (1 lsl width) - 1 else a / b);
         "bvurem", (fun a b -> if b = 0 then a else a mod b);
         "bvsdiv", quotient; "bvsrem", remainder; "bvsmod", modulo])
      (List.init width Fun.id)
  in
  let outputs =
    narrowed_dividends @ one_bit_predicates @ operations @ unary
    @ predicates @ indexed
    @ constant_divisors
    @ negation_overflow @ rewrites @ shifted_divisors
  in
  for a = 0 to mask do
    for b = 0 to mask do
      let selected = (a + b) land 1 = 0 in
      let assumptions =
        Array.concat
          [ assumptions xb a;
            assumptions yb b;
            [| (if selected then cb else -cb) |] ]
      in
      let model =
        match Sat.solve ~assumptions sat with
        | Sat.Sat model -> model
        | _ -> failwith "Bitvector circuit rejected inputs"
      in
      let outputs =
        ("ite", conditional, fun a b -> if selected then a else b) :: outputs
      in
      List.iter
        (fun (name, bits, expected) ->
          let actual = read model bits in
          let expected = expected a b in
          check (actual = expected)
            (Printf.sprintf "%s width=%d a=%d b=%d: %d <> %d" name width a b
               actual expected);
          if width <= 3
          then
            Array.iteri
              (fun i bit ->
                let wrong =
                  if expected land (1 lsl i) = 0 then bit else -bit
                in
                match
                  Sat.solve
                    ~assumptions:(Array.append assumptions [| wrong |])
                    sat
                with
                | Sat.Unsat _ -> ()
                | _ -> failwith ("Underdetermined bitvector operator " ^ name))
              bits)
        outputs
    done
  done

let constants_and_errors () =
  let terms, sat, bv, _ = fixture () in
  let value = Z.of_string "18446744073709551621" in
  let term = Term.bits terms 65 value in
  let bits = Bv.bits bv term in
  let negative =
    Bv.bits bv
      (Term.builtin terms "int2bv:8" (Term.Bitvec 8)
         [| Term.integer terms (Z.of_int (-129)) |])
  in
  (match Sat.solve sat with
  | Sat.Sat model ->
    check (Array.length bits = 65) "Large bitvector width";
    Array.iteri
      (fun i bit ->
        let expected = i = 0 || i = 2 || i = 64 in
        check
          (model.(abs bit) = if bit > 0 then expected else not expected)
          "Large bitvector constant")
      bits;
    check (read model negative = 127) "Negative int2bv constant"
  | _ -> failwith "Constants were inconsistent");
  let x = variable terms "x" (Term.Bitvec 8) in
  let bad = Term.builtin terms "unknown_bv" (Term.Bitvec 8) [| x |] in
  (match Bv.bits bv bad with
  | _ -> failwith "Unsupported bitvector operation accepted"
  | exception Bv.Unsupported _ -> ());
  let x = variable terms "integer" Term.Int in
  let conversion = Term.builtin terms "int2bv:8" (Term.Bitvec 8) [| x |] in
  match Bv.bits bv conversion with
  | _ -> failwith "Missing integer bridge accepted"
  | exception Bv.Unsupported _ -> ()

let wide_identities () =
  List.iter
    (fun width ->
      let terms, sat, bv, _ = fixture () in
      let sort = Term.Bitvec width in
      let x = variable terms "x" sort in
      let op name args = Term.builtin terms name sort args in
      let constant n = Term.bits terms width (Z.of_int n) in
      let identities =
        [ op "bvmul" [| x; constant (-1) |], op "bvneg" [| x |];
          op "bvmul" [| x; constant 2 |], op "bvadd" [| x; x |];
          op "bvadd" [| x; op "bvnot" [| x |] |], constant (-1);
          op "bvsub" [| x; op "bvneg" [| x |] |], op "bvadd" [| x; x |] ]
      in
      List.iter
        (fun (a, b) ->
          let different = Option.get (Bv.predicate bv "distinct" [| a; b |]) in
          match Sat.solve ~assumptions:[| different |] ~max_conflicts:10000 sat with
          | Sat.Unsat _ -> ()
          | _ -> failwith (Printf.sprintf "Wide BV identity at %d bits" width))
        identities)
    [63; 128; 257]

let polynomial_identities () =
  List.iter (fun width ->
    let terms = Term.create () in
    let sort = Term.Bitvec width in
    let x = variable terms "x" sort and y = variable terms "y" sort in
    let z = variable terms "z" sort in
    let op name args = Term.builtin terms name sort args in
    let add x y = op "bvadd" [|x; y|] in
    let mul x y = op "bvmul" [|x; y|] in
    let neg x = op "bvneg" [|x|] in
    let shift x y = op "bvshl" [|x; y|] in
    let constant n = Term.bits terms width (Z.of_int n) in
    let identities =
      [add x (add y z), add (add x y) z;
       mul x (add y z), add (mul x y) (mul x z);
       mul x (mul (neg y) y), neg (mul x (mul y y));
       mul x (add (op "bvnot" [|y|]) y), neg x;
       shift (mul x y) z, mul x (shift y z);
       shift x (constant width), constant 0;
       shift x (Term.bits terms width (Z.minus_one)), constant 0]
    in
    let compare expected left right =
      match Term.node terms (Term.equal terms left right) with
      | Term.Boolean value -> value = expected
      | Term.Apply (symbol, args) when width = 512 ->
        (Term.signature terms symbol).kind = Term.Builtin "="
        && (args = [|left; right|] || args = [|right; left|])
      | _ -> false in
    List.iteri (fun index (left, right) ->
      check (compare true left right)
        (Printf.sprintf "BV polynomial identity %d at width %d" index width);
      check (compare false (add left (constant 1)) right)
        (Printf.sprintf "BV polynomial residue %d at width %d" index width))
      identities)
    [1; 2; 3; 31; 32; 63; 64; 127; 512]

let polynomial_boundaries () =
  let terms = Term.create () in
  let sort = Term.Bitvec 3 in
  let x = variable terms "x" sort and y = variable terms "y" sort in
  let z = variable terms "z" sort in
  let op name args = Term.builtin terms name sort args in
  let shifted = op "bvshl" [|op "bvshl" [|x; y|]; z|] in
  let combined = op "bvshl" [|x; op "bvadd" [|y; z|]|] in
  check (Term.node terms (Term.equal terms shifted combined)
    <> Term.Boolean true) "Shift amount wraparound";
  let uninterpreted name args = Term.apply terms
      (Term.symbol terms
        { name; arguments = Array.make (Array.length args) sort;
          result = sort; kind = Term.Uninterpreted }) args in
  let false_name = uninterpreted "bvsub" [|x; x|] in
  let zero = Term.bits terms 3 Z.zero in
  check (Term.node terms
      (Term.equal terms (op "bvmul" [|false_name; y|]) zero)
    <> Term.Boolean true) "Uninterpreted builtin name";
  let other_sort = Term.Bitvec 4 in
  let other = variable terms "other" other_sort in
  let malformed = Term.apply terms
      (Term.symbol terms
        { name = "bvsub"; arguments = [|other_sort; other_sort|];
          result = sort; kind = Term.Builtin "bvsub" }) [|other; other|] in
  check (Term.node terms
      (Term.equal terms (op "bvmul" [|malformed; y|]) zero)
    <> Term.Boolean true) "Malformed builtin signature";
  let terms = Term.create () in
  let sort = Term.Bitvec 513 in
  let x = variable terms "x" sort and y = variable terms "y" sort in
  let z = variable terms "z" sort in
  let add a b = Term.builtin terms "bvadd" sort [|a; b|] in
  check (Term.node terms (Term.equal terms (add x (add y z))
    (add (add x y) z)) <> Term.Boolean true) "Width bound fallback"

let polynomial_polling () =
  let make () =
    let terms = Term.create () in
    let sort = Term.Bitvec 32 in
    let x = variable terms "x" sort and y = variable terms "y" sort in
    let z = variable terms "z" sort in
    let add a b = Term.builtin terms "bvadd" sort [|a; b|] in
    let mul a b = Term.builtin terms "bvmul" sort [|a; b|] in
    terms, mul x (add y z), add (mul x y) (mul x z)
  in
  let terms, left, right = make () in
  let calls = ref 0 in
  ignore (Term.with_poll terms (fun () -> incr calls)
    (fun () -> Term.equal terms left right));
  for stop = 1 to !calls do
    let terms, left, right = make () in
    let count = Term.count terms and calls = ref 0 in
    (match Term.with_poll terms (fun () ->
        incr calls; if !calls = stop then raise Exit)
        (fun () -> Term.equal terms left right) with
    | exception Exit -> ()
    | _ -> failwith "Polynomial callback did not interrupt");
    check (Term.count terms = count) "Polynomial cancellation atomicity";
    check (Term.node terms (Term.equal terms left right) = Term.Boolean true)
      "Polynomial cancellation recovery"
  done;
  Printf.printf "BV polynomial: %d callback positions recovered\n%!" !calls

let bitvector_rewrite_identities () =
  List.iter (fun width ->
    let terms = Term.create () in
    let sort = Term.Bitvec width in
    let x = variable terms "x" sort and y = variable terms "y" sort in
    let z = variable terms "z" sort in
    let op name args = Term.builtin terms name sort args in
    let binary name a b = op name [|a; b|] in
    let add = binary "bvadd" and mul = binary "bvmul" in
    let and_ = binary "bvand" and or_ = binary "bvor" in
    let xor = binary "bvxor" and shr = binary "bvlshr" in
    let shl = binary "bvshl" and neg a = op "bvneg" [|a|] in
    let not_ a = op "bvnot" [|a|] in
    let zero = Term.bits terms width Z.zero in
    let identities =
      [shr x x, zero;
       shr (and_ x y) (or_ x z), zero;
       shr (shr x y) x, zero;
       shr (binary "bvurem" x y) x, zero;
       or_ x (and_ y x), x;
       and_ x (or_ y x), x;
       xor (xor x y) z, xor x (xor y z);
       and_ x (xor y z), xor (and_ x y) (and_ x z);
       not_ (or_ x y), and_ (not_ x) (not_ y);
       or_ x (mul y (neg y)), or_ x (neg (mul y y));
       shl x (mul y (neg y)), shl x (neg (mul y y));
       or_ x (shl y x), add x (shl y x);
       and_ x (shl y x), zero;
       mul z (or_ x (shl y x)), mul z (add x (shl y x));
       shr (shr x y) z, shr (shr x z) y;
       binary "bvashr" (binary "bvashr" x y) z,
         binary "bvashr" (binary "bvashr" x z) y]
    in
    List.iteri (fun index (left, right) ->
      check (Term.node terms (Term.equal terms left right) = Term.Boolean true)
        (Printf.sprintf "BV rewrite identity %d width %d" index width))
      identities;
    let function_symbol = Term.symbol terms
        { name = "f"; arguments = [|sort|]; result = sort;
          kind = Term.Uninterpreted } in
    let f argument = Term.apply terms function_symbol [|argument|] in
    let left = f (mul x (neg y)) in
    let right = add (f (neg (mul x y))) (Term.bits terms width Z.one) in
    check (Term.node terms (Term.equal terms left right) = Term.Boolean false)
      "Congruent function atoms in arithmetic";
    let named = Term.apply terms
        (Term.symbol terms
          { name = "bvlshr"; arguments = [|sort; sort|]; result = sort;
            kind = Term.Uninterpreted }) [|x; x|] in
    check (Term.node terms (Term.equal terms named zero) <> Term.Boolean true)
      "Uninterpreted shift name";
    let other_sort = Term.Bitvec (width + 1) in
    let a = variable terms "a" other_sort in
    let b = variable terms "b" other_sort in
    let malformed = Term.symbol terms
        { name = "bvadd"; arguments = [|other_sort; other_sort|];
          result = sort; kind = Term.Builtin "bvadd" } in
    let left = Term.apply terms malformed [|a; b|] in
    let right = Term.apply terms malformed [|b; a|] in
    check (Term.node terms (Term.equal terms left right) <> Term.Boolean true)
      "Malformed commutative builtin signature") [1; 2; 3; 8; 32; 63]

let bitvector_rewrite_polling () =
  let make () =
    let terms = Term.create () in
    let sort = Term.Bitvec 32 in
    let x = variable terms "x" sort and y = variable terms "y" sort in
    let op name args = Term.builtin terms name sort args in
    let left = op "bvor" [|x; op "bvmul" [|y; op "bvneg" [|y|]|]|] in
    let right = op "bvor" [|x; op "bvneg" [|op "bvmul" [|y; y|]|]|] in
    terms, left, right in
  let terms, left, right = make () in
  let calls = ref 0 in
  ignore (Term.with_poll terms (fun () -> incr calls)
    (fun () -> Term.equal terms left right));
  for stop = 1 to !calls do
    let terms, left, right = make () in
    let count = Term.count terms and calls = ref 0 in
    (match Term.with_poll terms (fun () ->
        incr calls; if !calls = stop then raise Exit)
        (fun () -> Term.equal terms left right) with
    | exception Exit -> ()
    | _ -> failwith "Bitvector rewrite callback did not interrupt");
    check (Term.count terms = count)
      "Bitvector rewrite cancellation atomicity";
    check (Term.node terms (Term.equal terms left right) = Term.Boolean true)
      "Bitvector rewrite cancellation recovery"
  done;
  Printf.printf "BV rewrite: %d callback positions recovered\n%!" !calls

let bitvector_rewrite_bound () =
  let terms = Term.create () in
  let sort = Term.Bitvec 32 in
  let symbol = Term.symbol terms
      { name = "bvor"; arguments = [|sort; sort|]; result = sort;
        kind = Term.Builtin "bvor" } in
  let variables = List.init 1000 (fun index ->
    variable terms ("v" ^ string_of_int index) sort) in
  let chain = List.fold_left (fun result term ->
    Term.apply terms symbol [|result; term|]) in
  let zero = Term.bits terms 32 Z.zero in
  let left = chain zero variables in
  let right = chain zero (List.rev variables) in
  let calls = ref 0 in
  let equality = Term.with_poll terms (fun () -> incr calls)
      (fun () -> Term.equal terms left right) in
  check (!calls <= 1040) "Bitvector rewrite work bound";
  check (Term.node terms equality <> Term.Boolean false)
    "Bitvector rewrite limit fallback";
  Printf.printf "BV rewrite limit: %d callbacks\n%!" !calls

let malformed_at_most_one () =
  let raw terms name sort args =
    Term.apply terms (Term.symbol terms
      { name; arguments = Array.map (Term.sort terms) args;
        result = sort; kind = Term.Builtin name }) args in
  List.iter (fun name -> List.iter (fun reverse ->
    List.iter (fun (zero_width, sub_sort, one_width, mask_sort) ->
      let terms, _, bv, _ = fixture () in
      let x = variable terms "x" (Term.Bitvec 8) in
      let one = Term.bits terms one_width Z.one in
      let sub = raw terms "bvsub" sub_sort [|x; one|] in
      let masked = raw terms "bvand" mask_sort [|x; sub|] in
      let zero = Term.bits terms zero_width Z.zero in
      let args = if reverse then [|zero; masked|] else [|masked; zero|] in
      match Bv.predicate bv name args with
      | exception Invalid_argument _ -> ()
      | _ -> failwith "Malformed at-most-one predicate accepted")
      [4, Term.Bitvec 8, 8, Term.Bitvec 8;
       8, Term.Bitvec 4, 8, Term.Bitvec 8;
       8, Term.Bitvec 8, 4, Term.Bitvec 8;
       4, Term.Bitvec 8, 8, Term.Bitvec 4;
       8, Term.Bitvec 8, 8, Term.Int]) [false; true]) ["="; "distinct"]

let constant_shifts () =
  for width = 1 to 6 do
    let terms = Term.create () in
    let mask = (1 lsl width) - 1 in
    for a = 0 to mask do
      for b = 0 to mask do
        let signed = if a land (1 lsl (width - 1)) = 0 then a
          else a - mask - 1 in
        let expected =
          ["bvshl", (if b >= width then 0 else (a lsl b) land mask);
           "bvlshr", (if b >= width then 0 else a lsr b);
           "bvashr", (if b >= width then (if signed < 0 then mask else 0)
                      else (signed asr b) land mask)] in
        List.iter (fun (name, expected) ->
          let actual = Term.builtin terms name (Term.Bitvec width)
              [|Term.bits terms width (Z.of_int a);
                Term.bits terms width (Z.of_int b)|] in
          match Term.node terms actual with
          | Term.Bits (_, value) ->
            check (Z.equal value (Z.of_int expected)) "constant shift value"
          | _ -> failwith "constant shift not folded") expected
      done
    done
  done


module Shift_cases = struct
module T = Vox_smt_term
module Z = Vox_smt_integer.Z
module R = Vox_smt_resource

let check value message = if not value then failwith message

type expression =
  | X | Y | Constant of int | Binary of string * expression * expression
let rec evaluate width x y = function
  | X -> x | Y -> y | Constant value -> value land ((1 lsl width) - 1)
  | Binary (name, a, b) ->
    let a = evaluate width x y a and b = evaluate width x y b in
    let mask = (1 lsl width) - 1 in
    (match name with
    | "bvadd" -> a + b | "bvsub" -> a - b | "bvmul" -> a * b
    | "bvand" -> a land b | "bvor" -> a lor b | "bvxor" -> a lxor b
    | "bvshl" -> if b >= width then 0 else a lsl b
    | "bvlshr" -> if b >= width then 0 else a lsr b
    | "bvashr" ->
      let signed = if a land (1 lsl (width - 1)) = 0 then a else a - mask - 1 in
      if b >= width then (if signed < 0 then mask else 0) else signed asr b
    | _ -> failwith "operator") land mask

let variable terms width name = T.apply terms
  (T.symbol terms { name; arguments = [||]; result = T.Bitvec width;
    kind = T.Uninterpreted }) [||]

let fixture width =
  let terms = T.create () in
  let x = variable terms width "x" and y = variable terms width "y" in
  let rec build = function
    | X -> x | Y -> y | Constant value -> T.bits terms width (Z.of_int value)
    | Binary (name, a, b) ->
      T.builtin terms name (T.Bitvec width) [|build a; build b|] in
  terms, build

let op name a b = Binary (name, a, b)
let times = op "bvmul"
let shift = op "bvshl"
let shr = op "bvlshr"
let plus = op "bvadd"
let pairs =
  [shr (times X X) (shift X X), times X (shr X (shift X X));
   shr (times X X) (shift Y X), times X (shr X (shift Y X));
   shift (shift X X) (shr Y X), shift X (plus X (shr Y X));
   shr (shr Y X) (shr X X), shr Y (plus X (shr X X));
   op "bvashr" (op "bvashr" X Y) (shr X Y),
     op "bvashr" X (plus Y (shr X Y));
   shift (shift X Y) X, shift X (plus X Y);
   shr (shr (times X X) X) Y, shr (times X (shr X Y)) X;
   shr (shr (times X X) Y) X, shr (times X (shr X Y)) X;
   shr (shr (times X Y) X) Y, Constant 0;
   shr (shr (times X Y) Y) X, Constant 0]

let semantics () =
  let random = Random.State.make [|735791|] in
  let names = [|"bvadd"; "bvsub"; "bvmul"; "bvand"; "bvor"; "bvxor";
                "bvshl"; "bvlshr"; "bvashr"|] in
  let rec random_term depth =
    if depth = 0 then match Random.State.int random 3 with
      | 0 -> X | 1 -> Y | _ -> Constant (Random.State.int random 32)
    else op names.(Random.State.int random (Array.length names))
        (random_term (depth - 1)) (random_term (depth - 1)) in
  let tested = ref 0 and simplified = ref 0 in
  for width = 1 to 5 do
    let terms, build = fixture width in
    let pairs = pairs @ List.init 150 (fun index ->
      let a = random_term 3 and b = random_term 2 in
      if index mod 3 = 0 then
        shift (shift a b) (shr X b), shift a (plus b (shr X b))
      else if index mod 3 = 1 then
        shr (times a a) (shift b a), times a (shr a (shift b a))
      else a, b) in
    List.iter (fun (left, right) ->
      List.iter (fun right ->
        incr tested;
        match T.node terms (T.equal terms (build left) (build right)) with
        | T.Boolean expected ->
          incr simplified;
          for x = 0 to (1 lsl width) - 1 do
            for y = 0 to (1 lsl width) - 1 do
              check ((evaluate width x y left = evaluate width x y right)
                = expected) "unsound equality simplification"
            done
          done
        | _ -> ()) [right; plus right (Constant 1)]) pairs
  done;
  Printf.printf "Semantics: %d pairs, %d simplified, exhaustive inputs\n%!"
    !tested !simplified

let recovery () =
  List.iter (fun width ->
    List.iteri (fun index (left, right) ->
      if index <> 5 then begin
        let setup () =
          let terms, build = fixture width in terms, build left, build right in
        let terms, left, right = setup () in
        let polls = ref 0 in
        let result = T.with_poll terms (fun () -> incr polls)
            (fun () -> T.equal terms left right) in
        let proved = T.node terms result = T.Boolean true in
        check (T.node terms result <> T.Boolean false) "false theorem";
        let total = !polls in
        let cutoffs = List.init (min 32 total) Fun.id
            @ List.init 64 (fun i -> total * i / 64) @ [total - 1] in
        List.iter (fun cutoff ->
          let terms, left, right = setup () in
          let resource = R.create ~budget:cutoff () in
          (match T.with_poll terms (fun () -> R.poll resource)
              (fun () -> T.equal terms left right) with
          | exception R.Exhausted "resource limit" -> ()
          | _ -> failwith "expected resource interruption");
          let result = T.node terms (T.equal terms left right) in
          check (result <> T.Boolean false &&
            (not proved || result = T.Boolean true)) "resource recovery";
          let terms, left, right = setup () and polls = ref 0 in
          (match T.with_poll terms (fun () ->
              if !polls = cutoff then raise Exit;
              incr polls) (fun () -> T.equal terms left right) with
          | exception Exit -> ()
          | _ -> failwith "expected callback interruption");
          let result = T.node terms (T.equal terms left right) in
          check (result <> T.Boolean false &&
            (not proved || result = T.Boolean true))
            "callback recovery") cutoffs;
        Printf.printf "Recovery width%d case%d: %d polls, %d cutoffs\n%!"
          width index total (List.length cutoffs)
      end) pairs) [8; 32; 64]

let run () = semantics (); recovery ()
end

let () =
  Shift_cases.run ();
  constant_shifts ();
  malformed_at_most_one ();
  for width = 1 to 5 do
    exhaustive width;
    exhaustive ~completion:true width
  done;
  polynomial_identities ();
  polynomial_boundaries ();
  polynomial_polling ();
  bitvector_rewrite_identities ();
  bitvector_rewrite_polling ();
  bitvector_rewrite_bound ();
  constants_and_errors ();
  wide_identities ();
  print_endline "Bitvector tests passed"
