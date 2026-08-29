open Vox_smt

let config = { Vox_smt_solver.default_config with executable = Sys.argv.(1) }

let x = Symbol.create ~label:"x" Int63

let b = Symbol.create ~label:"x" Bool

let i n = Integer n

let app op args = App (op, args)

let eq a b = app Eq [a; b]

let check ?(symbols = []) ?(facts = []) term =
  Vox_smt_solver.check ~config ~int_width:63
    { symbols;
      facts = List.map (fun term -> { label = "fact"; term }) facts;
      goal = { label = "goal"; term }
    }

let valid ?symbols ?facts term =
  match (check ?symbols ?facts term).validity with
  | Valid -> ()
  | Failure message -> failwith message
  | _ -> failwith "Expected valid"

let invalid ?symbols ?facts term =
  match (check ?symbols ?facts term).validity with
  | Invalid model -> model
  | Failure message -> failwith message
  | _ -> failwith "Expected invalid"

let () =
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
  print_endline "Z3 integration tests passed"
