open Vox_smt

let config = { Vox_smt_solver.default_config with executable = Sys.argv.(1) }

let x = Symbol.create ~label:"x" Bv63

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
  let f = Function.create ~label:"same" ~arguments:[Bv63] ~result:Bv63 in
  let g = Function.create ~label:"same" ~arguments:[Bv63] ~result:Bv63 in
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
  valid (eq (app Mul [i 4611686018427387903L; i 2L]) (i (-2L)));
  List.iter (fun op -> valid (app op [i (-1L); i 0L])) [Lt; Le; Ne];
  List.iter (fun op -> valid (app op [i 0L; i (-1L)])) [Gt; Ge; Ne];
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
  print_endline "Z3 integration tests passed"
