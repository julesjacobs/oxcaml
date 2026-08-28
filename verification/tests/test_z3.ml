open Vox_smt

let config = { Vox_smt_solver.default_config with executable = Sys.argv.(1) }

let x = Symbol.create ~label:"x" Int63

let b = Symbol.create ~label:"x" Bool

let i n = Integer n

let app op args = App (op, args)

let eq a b = app Eq [a; b]

let check ?(symbols = []) ?(functions = []) ?(facts = []) term =
  Vox_smt_solver.check ~config ~int_width:63
    { symbols;
      functions;
      facts = List.map (fun term -> { label = "fact"; term }) facts;
      goal = { label = "goal"; term }
    }

let valid ?symbols ?functions ?facts term =
  match (check ?symbols ?functions ?facts term).validity with
  | Valid -> ()
  | Failure message -> failwith message
  | _ -> failwith "Expected valid"

let invalid ?symbols ?functions ?facts term =
  match (check ?symbols ?functions ?facts term).validity with
  | Invalid model -> model
  | Failure message -> failwith message
  | _ -> failwith "Expected invalid"

let () =
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
  ignore (invalid (eq product (i (-2L))));
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
  print_endline "Z3 integration tests passed"

let () =
  Vox_smt_solver.with_session ~config ~int_width:63 (fun check ->
      let query symbols facts term =
        { functions = []; symbols;
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
  print_endline "Z3 session tests passed"
