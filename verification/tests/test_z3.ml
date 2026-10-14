open Vox_smt

let config = { Vox_smt_solver.default_config with executable = Sys.argv.(1) }

let x = Symbol.create ~label:"x" Int63

let b = Symbol.create ~label:"x" Bool

let i n = Integer n

let app op args = App (op, args)

let eq a b = app Eq [a; b]

let force_bits = ref false

let last_query = Buffer.create 1024

let check ?(datatypes = []) ?(symbols = []) ?(functions = []) ?(facts = []) term
    =
  let facts =
    if !force_bits then eq (app Bit_and [i 0L; i 0L]) (i 0L) :: facts else facts
  in
  Buffer.clear last_query;
  Vox_smt_solver.check ~config ~int_width:63
    ~dump:(fun query -> Buffer.add_string last_query query)
    { datatypes;
      symbols;
      functions;
      facts = List.map (fun term -> { label = "fact"; term }) facts;
      goal = { label = "goal"; term }
    }

let valid ?datatypes ?symbols ?functions ?facts term =
  match (check ?datatypes ?symbols ?functions ?facts term).validity with
  | Valid -> ()
  | Failure message -> failwith message
  | Unknown reason ->
    failwith ("Expected valid; unknown: "
      ^ Option.value reason ~default:"unspecified" ^ "\n"
      ^ Buffer.contents last_query)
  | Invalid _ ->
    failwith ("Expected valid; invalid\n" ^ Buffer.contents last_query)
  | Timeout ->
    failwith ("Expected valid; timeout\n" ^ Buffer.contents last_query)

let invalid ?datatypes ?symbols ?functions ?facts term =
  match (check ?datatypes ?symbols ?functions ?facts term).validity with
  | Invalid model -> model
  | Failure message -> failwith message
  | _ -> failwith "Expected invalid"

let arithmetic () =
  let f = Function.create ~label:"same" ~arguments:[Int63] ~result:Int63 in
  let g = Function.create ~label:"same" ~arguments:[Int63] ~result:Int63 in
  valid ~symbols:[x] ~functions:[f]
    ~facts:[eq (Var x) (i 0L)]
    (eq (Call (f, [Var x])) (Call (f, [i 0L])));
  ignore (invalid ~functions:[f; g] (eq (Call (f, [i 0L])) (Call (g, [i 0L]))));
  valid ~functions:[f]
    ~facts:[eq (Call (f, [i 3L])) (i 5L)]
    (eq (Call (f, [i 3L])) (i 5L));
  valid (Boolean true);
  assert (invalid (Boolean false) = Some []);
  valid
    (eq (app Add [i 4611686018427387903L; i 1L]) (i (-4611686018427387904L)));
  valid (eq (app Neg [i (-4611686018427387904L)]) (i (-4611686018427387904L)));
  valid
    (eq (app Sub [i (-4611686018427387904L); i 1L]) (i 4611686018427387903L));
  let product = app Mul [i 4611686018427387903L; i 2L] in
  valid (eq product (i (-2L)));
  valid ~symbols:[x] (eq (app Mul [Var x; i 0L]) (i 0L));
  valid ~symbols:[x] (eq (app Mul [i 1L; Var x]) (Var x));
  valid ~symbols:[x] (eq (app Mul [Var x; i (-1L)]) (app Neg [Var x]));
  valid ~symbols:[x] (eq (app Mul [Var x; i 2L]) (app Add [Var x; Var x]));
  let y = Symbol.create ~label:"y" Int63 in
  valid ~symbols:[x; y]
    ~facts:[eq (Var y) (i 2L)]
    (eq (app Mul [Var x; Var y]) (app Mul [Var x; i 2L]));
  valid (eq product product);
  valid (app Ge [product; i (-4611686018427387904L)]);
  valid (app Le [product; i 4611686018427387903L]);
  valid (eq (app Div [i (-7L); i 2L]) (i (-3L)));
  valid (eq (app Rem [i (-7L); i 2L]) (i (-1L)));
  valid (eq (app Div [i 7L; i (-2L)]) (i (-3L)));
  valid (eq (app Rem [i 7L; i (-2L)]) (i 1L));
  valid (eq (app Div [i (-7L); i (-2L)]) (i 3L));
  valid (eq (app Rem [i (-7L); i (-2L)]) (i (-1L)));
  valid
    (eq
       (app Div [i (-4611686018427387904L); i (-1L)])
       (i (-4611686018427387904L)));
  valid (eq (app Rem [i (-4611686018427387904L); i (-1L)]) (i 0L));
  List.iter (fun op -> valid (app op [i (-1L); i 0L])) [Lt; Le; Ne];
  List.iter (fun op -> valid (app op [i 0L; i (-1L)])) [Gt; Ge; Ne];
  valid ~symbols:[x] (app Ge [Var x; i (-4611686018427387904L)]);
  valid ~symbols:[x] (app Le [Var x; i 4611686018427387903L]);
  assert (
    invalid ~symbols:[x] (app Gt [app Add [Var x; i 1L]; Var x])
    = Some [x, Int_value 4611686018427387903L]);
  List.iter
    (fun n ->
      assert (
        invalid ~symbols:[x] ~facts:[eq (Var x) (i n)] (app Ne [Var x; i n])
        = Some [x, Int_value n]))
    [-4611686018427387904L; -1L; 0L; 4611686018427387903L];
  valid ~symbols:[b] (app Or [Var b; app Not [Var b]]);
  valid ~symbols:[b] (eq (app And [Var b; app Not [Var b]]) (Boolean false));
  valid ~symbols:[b] (app Implies [Var b; Var b]);
  valid ~symbols:[b] (eq (app Ite [Var b; Boolean true; Boolean false]) (Var b));
  valid ~symbols:[x; b] (eq (app Ite [Var b; Var x; Var x]) (Var x));
  assert (invalid ~symbols:[b] (Var b) = Some [b, Bool_value false]);
  valid ~symbols:[x] ~facts:[eq (Var x) (i 0L)] (eq (Var x) (i 0L));
  ignore (invalid ~symbols:[x] (eq (Var x) (i 0L)));
  valid ~symbols:[x] ~facts:[Boolean false] (Boolean false);
  ignore (invalid ~symbols:[x] (Boolean false));
  let z text = Big_integer text in
  let y = Symbol.create ~label:"bigint" Int in
  let large = "123456789012345678901234567890" in
  valid (eq (app Int_add [z large; z "1"]) (z "123456789012345678901234567891"));
  valid
    (eq
       (app Int_mul [z "100000000000000000000"; z "100000000000000000000"])
       (z "10000000000000000000000000000000000000000"));
  List.iter
    (fun (a, b, q, r) ->
      valid (eq (app Int_div [z a; z b]) (z q));
      valid (eq (app Int_mod [z a; z b]) (z r)))
    [ "7", "2", "3", "1";
      "-7", "2", "-4", "1";
      "7", "-2", "-3", "1";
      "-7", "-2", "4", "1" ];
  List.iter
    (fun n -> valid (eq (app Int_of_int63 [i n]) (z (Int64.to_string n))))
    [-4611686018427387904L; -1L; 0L; 4611686018427387903L];
  valid ~symbols:[x]
    (app Int_ge [app Int_of_int63 [Var x]; z "-4611686018427387904"]);
  List.iter
    (fun value ->
      assert (
        invalid ~symbols:[y]
          ~facts:[eq (Var y) (z value)]
          (app Ne [Var y; z value])
        = Some [y, Bigint_value value]))
    [large; "-" ^ large; "0"];
  let mixed =
    Function.create ~label:"mixed" ~arguments:[Int63; Int; Bool] ~result:Int
  in
  valid ~symbols:[x; y; b] ~functions:[mixed]
    ~facts:[eq (Var y) (app Int_of_int63 [Var x])]
    (eq
       (Call (mixed, [Var x; Var y; Var b]))
       (Call (mixed, [Var x; app Int_of_int63 [Var x]; Var b])));
  let tree = Datatype.create ~label:"tree" in
  let leaf = Constructor.create ~datatype:tree ~label:"Leaf" ["value", Int63] in
  let node =
    Constructor.create ~datatype:tree ~label:"Node"
      ["left", Datatype tree; "right", Datatype tree]
  in
  let datatypes = [{ datatype = tree; constructors = [leaf; node] }] in
  let a = Symbol.create ~label:"a" Int63 in
  let b = Symbol.create ~label:"b" Int63 in
  let leaf_a = Construct (leaf, [Var a]) in
  let leaf_b = Construct (leaf, [Var b]) in
  valid ~datatypes ~symbols:[a; b]
    ~facts:[eq leaf_a leaf_b]
    (eq (Var a) (Var b));
  valid ~datatypes ~symbols:[a]
    (app Ne [Construct (node, [leaf_a; leaf_a]); leaf_a]);
  valid ~datatypes ~symbols:[a]
    (eq (Select (node, 0, Construct (node, [leaf_a; leaf_a]))) leaf_a);
  let tree_value = Symbol.create ~label:"tree" (Datatype tree) in
  valid ~datatypes ~symbols:[tree_value]
    (app Ge [Select (leaf, 0, Var tree_value); i (-4611686018427387904L)]);
  valid ~datatypes ~symbols:[tree_value]
    (app Le [Select (leaf, 0, Var tree_value); i 4611686018427387903L]);
  print_endline "Solver integration tests passed"

let () =
  arithmetic ();
  force_bits := true;
  arithmetic ();
  force_bits := false

let () =
  Vox_smt_solver.with_session ~config ~int_width:63 (fun check ->
      let query symbols facts term =
        { datatypes = [];
          functions = [];
          symbols;
          facts = List.map (fun term -> { label = "fact"; term }) facts;
          goal = { label = "goal"; term }
        }
      in
      for _ = 1 to 3 do
        assert (
          (check (query [x] [Boolean false] (Boolean false))).validity = Valid);
        assert (
          (check (query [b] [] (Var b))).validity
          = Invalid (Some [b, Bool_value false]));
        assert (
          (check (query [x] [eq (Var x) (i 1L)] (eq (Var x) (i 1L)))).validity
          = Valid);
        match (check (query [x] [] (eq (Var x) (i 1L)))).validity with
        | Invalid _ -> ()
        | _ -> failwith "A previous query's assumptions escaped its scope"
      done);
  print_endline "Solver session tests passed"

let () =
  let signed n = Int64.shift_right (Int64.shift_left n 1) 1 in
  let cases =
    [0L; 1L; -1L; 65535L; -4611686018427387904L; 4611686018427387903L]
  in
  List.iter
    (fun (op, compute) ->
      List.iter
        (fun a ->
          List.iter
            (fun b -> valid (eq (app op [i a; i b]) (i (signed (compute a b)))))
            cases)
        cases)
    [Bit_and, Int64.logand; Bit_or, Int64.logor; Bit_xor, Int64.logxor];
  valid ~symbols:[x]
    (eq (app Bit_and [Var x; app Bit_xor [Var x; i (-1L)]]) (i 0L));
  ignore (invalid ~symbols:[x] (eq (app Bit_or [Var x; i 1L]) (i 0L)))

let () =
  let values = [0L; 1L; -1L; -4611686018427387904L; 4611686018427387903L] in
  List.iter
    (fun value ->
      List.iter
        (fun count ->
          let bits = Int64.logand value Int64.max_int in
          let result =
            if count = 0 then value else Int64.shift_right_logical bits count
          in
          valid
            (eq
               (app Shift_right_logical [i value; i (Int64.of_int count)])
               (i result)))
        [0; 1; 7; 16; 62; 63])
    values;
  ignore (invalid (eq (app Shift_right_logical [i (-1L); i 64L]) (i 0L)))

let () =
  let query ?(symbols = [x]) ?(functions = []) ?(datatypes = []) facts goal =
    { symbols; functions; datatypes;
      facts = List.map (fun term -> { label = "fact"; term }) facts;
      goal = { label = "goal"; term = goal } }
  in
  let y = Symbol.create ~label:"y" Int63 in
  let n = Symbol.create ~label:"n" Int in
  let old = Symbol.create ~label:"old" Bool in
  let current = Symbol.create ~label:"current" Bool in
  let dump = Buffer.create 1024 in
  let queries =
    [ query [eq (Var x) (i 1L)] (eq (Var x) (i 1L));
      query [eq (Var x) (i 1L); app Lt [Var x; i 2L]]
        (app Lt [Var x; i 2L]);
      query [eq (Var x) (i 1L)] (app Ne [Var x; i 1L]);
      query [] (eq (Var x) (i 1L));
      query ~symbols:[x; y] [eq (Var y) (i 2L)] (eq (Var y) (i 2L));
      query [] (eq (Var x) (i 1L));
      query [] (eq (app Bit_or [Var x; i 1L]) (i 0L));
      query ~symbols:[n]
        [app Int_gt [Var n; Big_integer "4611686018427387904"]]
        (Boolean false);
      query [eq (Var x) (i 1L)] (eq (Var x) (i 1L)) ]
  in
  let queries = queries @ List.map (fun count ->
    let facts = List.init count (fun value ->
      app Ge [Var x; i (Int64.of_int value)]) in
    query facts (app Ge [Var x; i 64L])) [65; 64; 33; 32; 31; 0] in
  let expected =
    List.map (fun query -> (Vox_smt_solver.check ~config ~int_width:63 query)
      .validity) queries
  in
  let same_answer actual expected =
    match actual, expected with
    | Invalid _, Invalid _ | Valid, Valid -> ()
    | _ -> failwith "Incremental result differs from isolated result"
  in
  Vox_smt_solver.with_incremental_session ~config ~int_width:63
    ~dump:(fun text -> Buffer.add_string dump text)
    (fun check ->
      for _ = 1 to 3 do
        List.iter2 (fun query expected ->
          same_answer (check query).validity expected) queries expected;
        List.iter (fun symbol ->
          let query = query ~symbols:[symbol] [] (Var symbol) in
          let proved = check ~assumptions:[symbol] query in
          assert (proved.validity = Valid);
          assert (proved.core = Some [symbol]);
          let unproved = check ~assumptions:[] query in
          assert (unproved.validity
            = Invalid (Some [symbol, Bool_value false]));
          assert (unproved.core = None)) [old; current];
        let query = query [app Ge [Var x; i 0L]]
          (app Ge [app Add [Var x; i 1L]; i 0L]) in
        (match (check ~resource_limit:1 query).validity with
        | Unknown _ -> ()
        | _ -> failwith "Incremental resource limit did not stop");
        (match (check query).validity with
        | Invalid _ -> ()
        | _ -> failwith "Unlimited incremental check did not recover")
      done);
  let commands = String.split_on_char '\n' (Buffer.contents dump) in
  let resets = List.length (List.filter (( = ) "(reset)") commands) in
  assert (resets < 3 * List.length queries);
  if config.executable <> "ocaml" then assert (List.mem "(pop 1)" commands);
  Vox_smt_solver.with_incremental_session ~config ~int_width:63
    (fun check -> List.iter (fun fields ->
      let datatype = Datatype.create ~label:"variant" in
      let a = Constructor.create ~datatype ~label:"A" fields in
      let b = Constructor.create ~datatype ~label:"B" [] in
      let value = Symbol.create ~label:"value" (Datatype datatype) in
      let query = query ~symbols:[value]
        ~datatypes:[{ datatype; constructors = [a; b] }]
        [] (Is (a, Var value)) in
      assert ((check { query with facts =
        [{ label = "is-a"; term = Is (a, Var value) }] }).validity = Valid);
      match (check query).validity with
      | Invalid _ -> ()
      | _ -> failwith "Datatype assumption escaped its scope")
      [["value", Int63]; ["value", Bool]]);
  print_endline "Incremental solver session tests passed"

let () =
  let variable = Symbol.create ~label:"value" Int in
  let boolean = Symbol.create ~label:"guard" Bool in
  let make ?(symbols = [variable]) count goal =
    { symbols; functions = []; datatypes = [];
      facts = List.init count (fun index ->
        { label = "bound";
          term = app Int_ge
            [Var variable; Big_integer (string_of_int index)] });
      goal = { label = "goal"; term = goal } }
  in
  let bounds = make 1024 (app Int_ge [Var variable; Big_integer "1023"]) in
  let insufficient = make 1023 bounds.goal.term in
  let dump = Buffer.create 1024 in
  let commands () = String.split_on_char '\n' (Buffer.contents dump) in
  let protocol command expected =
    if config.executable <> "ocaml" then
      assert (List.mem command (commands ()) = expected) in
  let cleared f = Buffer.clear dump; f () in
  Vox_smt_solver.with_automatic_session ~config ~int_width:63
    ~dump:(fun text -> Buffer.add_string dump text) (fun check ->
      for turn = 1 to 4 do
        cleared (fun () -> assert ((check bounds).validity = Valid));
        protocol "(reset)" (turn = 1);
        protocol "(push 1)" false
      done;
      for turn = 1 to 3 do
        let goal = app Int_ge
          [Var variable; Big_integer (string_of_int (1000 + turn))] in
        cleared (fun () -> assert ((check (make 1024 goal)).validity = Valid));
        protocol "(reset)" (turn = 1);
        protocol "(push 1)" true
      done;
      cleared (fun () ->
        match (check insufficient).validity with
        | Invalid (Some [symbol, Bigint_value "1022"]) ->
          assert (symbol = variable)
        | _ -> failwith "Automatic session retained a removed bound");
      protocol "(reset)" true;
      assert ((check bounds).validity = Valid);
      cleared (fun () ->
        match (check insufficient).validity with
        | Invalid _ -> ()
        | _ -> failwith "Automatic session retained a scoped bound");
      protocol "(reset)" false;
      let small = make 1 (app Int_ge [Var variable; Big_integer "1"]) in
      cleared (fun () ->
        match (check small).validity with
        | Invalid _ -> ()
        | _ -> failwith "Automatic session retained a larger context");
      protocol "(reset)" true;
      protocol "(push 1)" false;
      let guarded = make ~symbols:[variable; boolean] 1024 (Var boolean) in
      for _ = 1 to 4 do
        let result = check ~assumptions:[boolean] guarded in
        assert (result.validity = Valid && result.core = Some [boolean])
      done;
      (match (check ~assumptions:[] guarded).validity with
      | Invalid _ -> ()
      | _ -> failwith "Automatic session retained a goal assumption");
      let limited =
        { symbols = [x]; functions = []; datatypes = [];
          facts = [{ label = "bound"; term = app Ge [Var x; i 0L] }];
          goal = { label = "goal";
            term = app Ge [app Add [Var x; i 1L]; i 0L] } }
      in
      (match (check ~resource_limit:1 limited).validity with
      | Unknown _ -> ()
      | _ -> failwith "Automatic session ignored a resource limit");
      assert ((check bounds).validity = Valid));
  Vox_smt_solver.with_automatic_session ~config ~int_width:63
    ~dump:(fun text -> Buffer.add_string dump text) (fun check ->
      for turn = 1 to 4 do
        let query = make 1024 (app Int_ge
          [Var variable; Big_integer (string_of_int (1000 + turn))]) in
        cleared (fun () -> assert ((check query).validity = Valid));
        protocol "(reset)" (turn <= 3);
        protocol "(push 1)" (turn >= 3)
      done);
  let other = Symbol.create ~label:"other" Bool in
  let number = Symbol.create ~label:"number" Int in
  let guarded = make ~symbols:[boolean] 0 (Var boolean) in
  Vox_smt_solver.with_automatic_session ~config ~int_width:63
    ~dump:(fun text -> Buffer.add_string dump text) (fun check ->
      let first = check ~assumptions:[boolean] guarded in
      assert (first.validity = Valid && first.core = Some [boolean]);
      cleared (fun () ->
        assert ((check ~assumptions:[] guarded).validity =
          Invalid (Some [boolean, Bool_value false])));
      protocol "(reset)" false;
      cleared (fun () ->
        let query = { guarded with symbols = [boolean; other] } in
        match (check ~assumptions:[] query).validity with
        | Invalid (Some values) ->
          assert (List.assoc boolean values = Bool_value false);
          assert (List.mem_assoc other values)
        | _ -> failwith "Exact-context recheck lost its model");
      protocol "(reset)" false;
      assert (List.mem "(declare-fun v1 () Bool)" (commands ()));
      cleared (fun () ->
        let query = { guarded with symbols = [boolean; number] } in
        let result = check ~assumptions:[boolean] query in
        assert (result.validity = Valid && result.core = Some [boolean]));
      protocol "(reset)" true;
      assert (List.mem "(declare-fun v1 () Int)" (commands ())));
  print_endline "Automatic solver session tests passed"
