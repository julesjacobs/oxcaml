module E = Vox_smt_engine
module T = Vox_smt_term
module Z = Vox_smt_integer.Z
module F = Vox_smt_frontend
module S = Vox_smt_sexp

let check condition message = if not condition then failwith message

let declaration terms name result =
  T.apply terms (T.symbol terms
    { name; arguments = [||]; result; kind = T.Uninterpreted }) [||]

let int terms n = T.integer terms (Z.of_int n)
let bv terms n = T.bits terms 3 (Z.of_int n)
let builtin terms name sort args = T.builtin terms name sort (Array.of_list args)
let equal = T.equal

let fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "x" T.Int in
  let y = declaration terms "y" T.Int in
  let p = declaration terms "p" (T.Bitvec 3) in
  let q = declaration terms "q" (T.Bitvec 3) in
  let guard = declaration terms "guard" T.Bool in
  let box = T.symbol terms
    { name = "Box"; arguments = [|T.Int; T.Bitvec 3|];
      result = T.Named 0; kind = T.Constructor 0 } in
  Array.iteri (fun index result ->
    ignore (T.symbol terms
      { name = "field" ^ string_of_int index; arguments = [|T.Named 0|];
        result; kind = T.Selector (box, index) })) [|T.Int; T.Bitvec 3|];
  let cell = declaration terms "cell" (T.Named 0) in
  List.iter (E.register solver) [x; y; p; q; guard; cell];
  let product = builtin terms "bvmul" (T.Bitvec 3) [p; q] in
  let quotient = builtin terms "bvudiv" (T.Bitvec 3) [product; q] in
  let base =
    [equal terms x (builtin terms "+" T.Int [y; int terms 1]);
     equal terms product (bv terms 3);
     equal terms quotient p;
     equal terms (builtin terms "mod" T.Int [x; int terms 3]) (int terms 1);
     equal terms cell (T.apply terms box [|x; p|])] in
  List.iter (E.assert_ solver) base;
  E.push solver;
  E.push solver;
  let inner = equal terms y (int terms 3) in
  E.assert_ solver inner;
  let value = T.apply terms (T.selector terms box 0) [|cell|] in
  let good = equal terms value (int terms 4) in
  let bad = equal terms value (int terms 5) in
  let assumption = T.or_ terms [|T.not_ terms guard; good|] in
  terms, solver, x, p, product, good, bad, guard, assumption, base

let recover_fixture terms solver x product bad =
  check (E.check solver = E.Sat) "Recovery was not SAT";
  check (E.value solver x = Some (E.Integer (Z.of_int 4))) "Recovered Int model";
  check (E.value solver product = Some (E.Bits (3, Z.of_int 3)))
    "Recovered BV model";
  check (E.check ~assumptions:[bad] solver = E.Unsat [bad])
    "Recovered assumption core";
  E.pop solver;
  check (E.check ~assumptions:[equal terms x (int terms 7)] solver = E.Sat)
    "Replay merged assertion scopes";
  E.pop solver;
  E.assert_ solver (equal terms x (int terms 10));
  check (E.check solver = E.Sat) "Assertion after recovered scopes";
  check (E.value solver x = Some (E.Integer (Z.of_int 10)))
    "Lost assertion after recovery";
  check (try E.pop solver; false with Invalid_argument _ -> true)
    "Empty frame was lost"

let interrupt_every_poll reordered =
  let _, solver, _, _, _, _, _, guard, assumption, _ = fixture () in
  let polls = ref 0 in
  let reorder_after = if reordered then 0 else max_int in
  check (E.For_testing.check ~reorder_after ~assumptions:[guard; assumption]
    ~cancelled:(fun () -> incr polls; false) solver = E.Sat)
    "Uninterrupted fixture";
  let count = !polls in
  let unknowns = ref 0 in
  for stop = 1 to count do
    let terms, solver, x, _, product, _, bad, guard, assumption, _ = fixture () in
    let polls = ref 0 in
    let result = E.For_testing.check
      ~reorder_after ~assumptions:[guard; assumption]
      ~cancelled:(fun () -> incr polls; !polls = stop) solver in
    (match result with
    | E.Unknown "cancelled" ->
      incr unknowns;
      check (E.value solver x = None) "Cancelled model leaked";
      check (E.raw_value solver x = None) "Cancelled raw model leaked"
    | E.Sat -> ()
    | _ -> failwith "Unexpected exhaustive cancellation result");
    recover_fixture terms solver x product bad
  done;
  Printf.printf "every-poll reordered=%b: %d polls, %d cancellations\n%!"
    reordered count !unknowns

let interrupted_assumption () =
  let count = ref 0 in
  let setup () =
    let terms = T.create () in
    let solver = E.create terms in
    let a = declaration terms "a" (T.Bitvec 3) in
    let b = declaration terms "b" (T.Bitvec 3) in
    List.iter (E.register solver) [a; b];
    check (E.check solver = E.Sat) "Assumption setup";
    let product = builtin terms "bvmul" (T.Bitvec 3) [a; b] in
    let assumption = equal terms product (bv terms 3) in
    terms, solver, product, assumption in
  let _, solver, _, assumption = setup () in
  check (E.check ~assumptions:[assumption]
    ~cancelled:(fun () -> incr count; false) solver = E.Sat)
    "Assumption count";
  for stop = 1 to !count do
    let terms, solver, product, assumption = setup () in
    let polls = ref 0 in
    (match E.check ~assumptions:[assumption]
        ~cancelled:(fun () -> incr polls; !polls = stop) solver with
    | E.Sat | E.Unknown "cancelled" -> ()
    | _ -> failwith "Assumption cancellation");
    check (E.check ~assumptions:[equal terms product (bv terms 4)] solver = E.Sat)
      "Cancelled assumption persisted"
  done;
  let work = E.resource_count solver in
  for budget = 1 to work do
    let terms, solver, product, assumption = setup () in
    (match E.check ~budget ~assumptions:[assumption] solver with
    | E.Sat | E.Unknown _ -> ()
    | _ -> failwith "Assumption budget");
    check (E.check ~assumptions:[equal terms product (bv terms 4)] solver = E.Sat)
      "Budgeted assumption persisted"
  done;
  Printf.printf "assumption-only: %d polls, %d work limits\n%!" !count work

let interrupted_pop () =
  for stop = 1 to 200 do
    let terms, solver, x, _, product, _, _, _, _, _ = fixture () in
    let polls = ref 0 in
    ignore (E.check ~cancelled:(fun () -> incr polls; !polls = stop) solver);
    E.pop solver;
    E.assert_ solver (equal terms x (int terms 7));
    check (E.check solver = E.Sat) "Pop after interrupted encoding";
    check (E.value solver x = Some (E.Integer (Z.of_int 7)))
      "Pop retained inner assertion";
    check (E.value solver product = Some (E.Bits (3, Z.of_int 3)))
      "Pop lost base assertion";
    E.pop solver;
    check (E.check ~assumptions:[equal terms x (int terms 10)] solver = E.Sat)
      "Empty scope reconstruction"
  done

let wide_budget () =
  let terms = T.create () in
  let solver = E.create terms in
  let width = 256 in
  let x = declaration terms "wide_x" (T.Bitvec width) in
  let y = declaration terms "wide_y" (T.Bitvec width) in
  let product = builtin terms "bvmul" (T.Bitvec width) [x; y] in
  E.assert_ solver (equal terms product (T.bits terms width Z.one));
  let before = E.statistics solver in
  let start = Sys.time () in
  check (E.check ~budget:1 solver = E.Unknown "resource limit")
    "Wide multiplication ignored budget 1";
  let seconds = Sys.time () -. start in
  check (E.resource_count solver = 1) "Encoding budget did not count work";
  check (E.statistics solver = before) "Wide budget entered SAT";
  check (seconds < 0.02) "Wide budget encoded expensive multiplication";
  Printf.printf "256-bit multiplication budget1: %.6fs\n%!" seconds

let budget_replay () =
  List.iter (fun reorder_after ->
  let _, solver, _, _, _, _, _, _, _, _ = fixture () in
  check (E.For_testing.check ~reorder_after solver = E.Sat) "Budget fixture";
  let count = E.resource_count solver in
  for budget = 1 to count do
    let terms, solver, x, _, product, _, bad, _, _, _ = fixture () in
    (match E.For_testing.check ~reorder_after ~budget solver with
    | E.Unknown _ -> check (E.resource_count solver >= budget)
        "Budget interruption underreported work"
    | E.Sat -> ()
    | E.Unsat _ -> failwith "Budget changed satisfiability");
    recover_fixture terms solver x product bad
  done;
  Printf.printf "every-budget reordered=%b: %d work limits\n%!"
    (reorder_after = 0) count) [max_int; 0]

let synchronous_validation () =
  let terms = T.create () in
  let solver = E.create terms in
  let bad = int terms 0 in
  check (try E.assert_ solver bad; false with Invalid_argument _ -> true)
    "Assertion validation was deferred";
  check (try ignore (E.check ~assumptions:[bad] solver); false
    with Invalid_argument _ -> true) "Invalid assumption accepted";
  check (try E.pop solver; false with Invalid_argument _ -> true)
    "Pop validation was deferred";
  check (E.check solver = E.Sat) "Rejected mutation damaged state"

let preprocessing_recovery () =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "preprocess_x" T.Int in
  let y = declaration terms "preprocess_y" T.Int in
  let z = declaration terms "preprocess_z" T.Int in
  E.assert_ solver
    (equal terms x (builtin terms "+" T.Int [y; int terms 1]));
  let next = equal terms z (builtin terms "+" T.Int [x; int terms 1]) in
  let before = T.count terms in
  E.assert_ solver next;
  check (T.count terms > before) "Assertion preprocessing was delayed";
  E.push solver;
  let next = equal terms z (builtin terms "+" T.Int [x; int terms 2]) in
  let before = T.count terms in
  E.assert_ solver next;
  check (T.count terms = before) "Preprocessing crossed a queued scope";
  E.pop solver;
  check (E.check ~assumptions:[equal terms y (int terms 3)] solver = E.Sat)
    "Queued scope recovery";
  check (E.value solver z = Some (E.Integer (Z.of_int 5)))
    "Preprocessing substitution changed";
  let bad = T.apply terms (T.symbol terms
    { name = "and"; arguments = [|T.Bool; T.Int|]; result = T.Bool;
      kind = T.Builtin "and" })
    [|equal terms y (int terms 4); int terms 0|] in
  check (try E.assert_ solver bad; false with Invalid_argument _ -> true)
    "Malformed conjunction accepted";
  check (E.value solver z = None) "Failed preprocessing retained a model";
  check (E.check ~assumptions:[equal terms y (int terms 6)] solver = E.Sat)
    "Failed preprocessing committed part of an assertion";
  check (E.value solver z = Some (E.Integer (Z.of_int 8)))
    "Failed preprocessing lost preceding assertions"

let named_frontend () =
  let setup = {|(declare-const x (_ BitVec 3))(declare-const y (_ BitVec 3))
    (assert (! (= (bvmul x y) #b011) :named product))
    (push 1)(push 1)(assert (! (= x #b011) :named lhs))|} in
  for stop = 1 to 250 do
    let session = F.create () in
    ignore (F.run session setup);
    let polls = ref 0 in
    ignore (F.run ~cancelled:(fun () -> incr polls; !polls = stop) session
      "(check-sat)");
    (match F.run session
      {|(assert (! (= y #b010) :named rhs))(check-sat)(get-unsat-core)|} with
    | ["unsat"; core] ->
      (match S.parse core with
      | [S.List names] ->
        check (List.mem (S.Atom "product") names
          && List.mem (S.Atom "rhs") names
          && List.for_all (fun name ->
            List.mem name [S.Atom "product"; S.Atom "lhs"; S.Atom "rhs"])
            names) "Named assertion core"
      | _ -> failwith "Named assertion core syntax")
    | _ -> failwith "Named assertion replay");
    check (F.run session "(pop 1)(check-sat)(pop 1)(check-sat)"
      = ["sat"; "sat"]) "Named scoped replay"
  done

let exception_recovery () =
  for stop = 2 to 150 do
    let terms, solver, x, _, product, _, bad, _, _, _ = fixture () in
    let polls = ref 0 in
    (try ignore (E.check ~cancelled:(fun () ->
      incr polls; if !polls = stop then failwith "callback"; false) solver)
    with Failure message when message = "callback" -> ());
    recover_fixture terms solver x product bad
  done

let reentrancy () =
  let terms = T.create () in
  let solver = E.create terms in
  List.iter (fun mutate ->
    check (try ignore (E.check ~cancelled:(fun () -> mutate (); false) solver);
        false with Invalid_argument _ -> true) "Reentrant operation accepted";
    check (E.check solver = E.Sat) "Reentrant operation corrupted journal")
    [(fun () -> E.push solver); (fun () -> E.pop solver);
     (fun () -> E.assert_ solver (T.boolean terms false));
     (fun () -> E.register solver (T.boolean terms false));
     (fun () -> ignore (E.check solver))]

let retired_statistics () =
  let terms = T.create () in
  let solver = E.create terms in
  check (E.check solver = E.Sat) "Statistics initial model";
  let before = E.statistics solver in
  let x = declaration terms "stat_x" (T.Bitvec 32) in
  let y = declaration terms "stat_y" (T.Bitvec 32) in
  let product = builtin terms "bvmul" (T.Bitvec 32) [x; y] in
  E.assert_ solver (equal terms product (T.bits terms 32 Z.one));
  for _ = 1 to 4 do
    check (E.check ~budget:40 solver = E.Unknown "resource limit")
      "Statistics encoding budget";
    check (E.statistics solver = before) "Retired statistics counted twice"
  done;
  E.assert_ solver (equal terms x (T.bits terms 32 Z.one));
  E.assert_ solver (equal terms y (T.bits terms 32 Z.one));
  check (E.check solver = E.Sat) "Statistics recovery";
  check ((E.statistics solver).solves = before.solves + 1)
    "Statistics lost or duplicated a solve";
  let cold = E.resource_count solver in
  check (E.check ~budget:1 solver = E.Unknown "resource limit")
    "Warm search interruption";
  check (E.check solver = E.Sat) "Warm search recovery";
  check (E.resource_count solver < cold) "Search interruption lost warm core"

let recursive_datatype () =
  let setup = {|(declare-datatype L ((nil) (cons (head Int) (tail L))))
    (declare-const d L)(assert ((_ is cons) d))
    (assert (= (head d) 4))(assert ((_ is nil) (tail d)))|} in
  let create () =
    let session = F.create () in
    ignore (F.run session setup); session in
  let polls = ref 0 in
  check (F.run ~cancelled:(fun () -> incr polls; false) (create ())
    "(check-sat)" = ["sat"]) "Recursive datatype fixture";
  let count = !polls in
  for stop = 1 to count do
    let session = create () in
    let polls = ref 0 in
    ignore (F.run ~cancelled:(fun () -> incr polls; !polls = stop) session
      "(check-sat)");
    check (F.run session "(check-sat)(get-value ((head d)))"
      = ["sat"; "(((head d) 4))"]) "Recursive datatype recovery";
    check (F.run session "(push)(assert ((_ is nil) d))(check-sat)(pop)(check-sat)"
      = ["unsat"; "sat"]) "Recursive datatype scoped recovery"
  done;
  Printf.printf "recursive datatype: %d polls\n%!" count

let append_only_arena () =
  List.iter (fun budget ->
  let terms, solver, x, _, _, _, _, _, _, _ = fixture () in
  let initial = T.count terms in
  for _ = 1 to 40 do ignore (E.check ~budget solver) done;
  let after = T.count terms in
  let late = declaration terms "late" T.Int in
  E.assert_ solver
    (equal terms late (builtin terms "+" T.Int [x; int terms 1]));
  check (E.check solver = E.Sat) "Term allocated after repeated discard";
  check (E.value solver late = Some (E.Integer (Z.of_int 5)))
    "Term IDs changed across core discard";
  Printf.printf "append-only terms after 40 budget-%d calls: %d -> %d\n%!"
    budget initial after) [100; 300; 500; 700]

let constant_polling () =
  let terms = T.create () in
  let value = T.bits terms 4096 Z.zero in
  let polls = ref 0 and variables = ref 0 in
  let bitvectors = Vox_smt_bitvector.create ~terms
      ~poll:(fun () -> incr polls; if !polls = 16 then raise Exit)
      ~fresh:(fun () -> incr variables; !variables)
      ~clause:ignore ~boolean:(fun _ -> failwith "Unexpected Boolean term")
      () in
  match Vox_smt_bitvector.bits bitvectors value with
  | _ -> failwith "Bitvector constant did not poll"
  | exception Exit -> check (!polls = 16) "Bitvector constant poll count"

let comparison_fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "x" T.Int in
  let y = declaration terms "y" T.Int in
  let z = declaration terms "z" T.Int in
  let guard = declaration terms "guard" T.Bool in
  let same = equal terms x y in
  E.assert_ solver (T.or_ terms [|guard; same|]);
  E.assert_ solver (T.or_ terms [|T.not_ terms guard; same|]);
  let comparisons z = List.map (fun op ->
    builtin terms op T.Bool [x; z], builtin terms op T.Bool [y; z])
    ["<"; "<="; ">"; ">="] in
  List.iter (fun (a, b) ->
    E.assert_ solver (T.or_ terms [|a; T.not_ terms a|]);
    E.assert_ solver (T.or_ terms [|b; T.not_ terms b|])) (comparisons z);
  E.push solver;
  let recover () =
    check (E.For_testing.check ~comparison_after:0 solver = E.Sat)
      "Comparison activation recovery";
    List.iter (fun (a, b) ->
      let bad = T.not_ terms b in
      (match E.For_testing.check
        ~comparison_after:0 ~assumptions:[a; bad] solver with
      | E.Unsat _ -> ()
      | _ -> failwith "Comparison congruence assumptions");
      check (E.For_testing.check ~comparison_after:0 solver = E.Sat)
        "Comparison assumption leaked";
      E.push solver;
      E.assert_ solver a;
      E.assert_ solver bad;
      check (E.For_testing.check ~comparison_after:0 solver = E.Unsat [])
        "Comparison congruence scope";
      E.pop solver)
      (comparisons (builtin terms "+" T.Int [z; int terms 1]));
    E.pop solver;
    check (E.For_testing.check ~comparison_after:0 solver = E.Sat)
      "Comparison congruence pop"
  in
  solver, recover

let forced_comparison_activation () =
  let fixture () =
    let terms = T.create () in
    let solver = E.create terms in
    let guard = declaration terms "guard" T.Bool in
    let pairs = Array.init 32 (fun i ->
      let x = declaration terms (Printf.sprintf "x%d" i) T.Int in
      let y = declaration terms (Printf.sprintf "y%d" i) T.Int in
      let same = equal terms x y in
      E.assert_ solver (T.or_ terms [|guard; same|]);
      E.assert_ solver (T.or_ terms [|T.not_ terms guard; same|]);
      x, y) in
    let sum f = T.builtin terms "+" T.Int (Array.map f pairs) in
    let compare x = builtin terms "<=" T.Bool [x; int terms 0] in
    E.assert_ solver (compare (sum fst));
    E.assert_ solver (T.not_ terms (compare (sum snd)));
    solver
  in
  let check_within_budget = E.For_testing.check
    ~budget:5000 ~reorder_after:max_int in
  let solver = fixture () in
  check (check_within_budget ~comparison_after:0 solver = E.Unsat [])
    "Zero comparison threshold did not force activation";
  let solver = fixture () in
  (match check_within_budget ~comparison_after:max_int solver with
  | E.Unknown _ -> ()
  | _ -> failwith "Disabled comparison threshold");
  check (check_within_budget ~comparison_after:0 solver = E.Unsat [])
    "Forced comparison activation after budget exhaustion";
  match check_within_budget (fixture ()) with
  | E.Unknown _ -> ()
  | _ -> failwith "Default comparison threshold"

let comparison_activation () =
  let solver, _ = comparison_fixture () in
  check (try ignore (E.For_testing.check ~comparison_after:(-1) solver); false
    with Invalid_argument _ -> true) "Negative comparison threshold";
  let polls = ref 0 in
  check (E.For_testing.check ~comparison_after:0
    ~cancelled:(fun () -> incr polls; false) solver = E.Sat)
    "Comparison activation fixture";
  let count = !polls and work = E.resource_count solver in
  for stop = 1 to count do
    let solver, recover = comparison_fixture () in
    let polls = ref 0 in
    check (E.For_testing.check ~comparison_after:0
      ~cancelled:(fun () -> incr polls; !polls = stop) solver
      = E.Unknown "cancelled") "Comparison activation cancellation";
    recover ()
  done;
  for budget = 1 to work do
    let solver, recover = comparison_fixture () in
    (match E.For_testing.check ~comparison_after:0 ~budget solver with
    | E.Sat -> ()
    | E.Unknown _ -> check (E.resource_count solver >= budget)
        "Comparison activation work accounting"
    | _ -> failwith "Comparison activation budget");
    recover ()
  done

let semantic_congruence_fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let variable = declaration terms in
  let p = variable "p" T.Bool and q = variable "q" T.Bool in
  let x = variable "x" T.Int and y = variable "y" T.Int in
  let a = variable "a" (T.Named 0) and b = variable "b" (T.Named 0) in
  let guard = variable "guard" T.Bool in
  let v = variable "v" (T.Bitvec 3) and w = variable "w" (T.Bitvec 3) in
  let box = T.symbol terms
    {name = "Box"; arguments = [|T.Bool; T.Int|]; result = T.Named 1;
     kind = T.Constructor 1} in
  Array.iteri (fun index result ->
    ignore (T.symbol terms
      {name = "field" ^ string_of_int index; arguments = [|T.Named 1|];
       result; kind = T.Selector (box, index)})) [|T.Bool; T.Int|];
  let left = T.and_ terms [|p; q|] in
  let right = T.ite terms p q (T.boolean terms false) in
  List.iter (fun same ->
    E.assert_ solver (T.or_ terms [|guard; same|]);
    E.assert_ solver (T.or_ terms [|T.not_ terms guard; same|]))
    [T.equal terms p q; T.equal terms x y; T.equal terms v w];
  let pairs =
    [T.Bool, left, right;
     T.Bool, T.not_ terms p, T.not_ terms q;
     T.Bool, T.builtin terms "xor" T.Bool [|p; q|], T.boolean terms false;
     T.Bool, T.equal terms p q, T.boolean terms true;
     T.Bool, T.or_ terms [|p; q|], T.ite terms p (T.boolean terms true) q;
     T.Int, T.ite terms p x y, x;
     T.Bitvec 3, T.ite terms p v w, v;
     T.Named 0, T.ite terms p a b, T.ite terms q a b;
     T.Named 1, T.apply terms box [|left; x|],
       T.apply terms box [|right; y|]] in
  let equalities = List.mapi (fun i (sort, left, right) ->
    let symbol = T.symbol terms
      {name = Printf.sprintf "f%d" i; arguments = [|sort|]; result = T.Int;
       kind = T.Uninterpreted} in
    T.equal terms (T.apply terms symbol [|left|])
      (T.apply terms symbol [|right|])) pairs in
  List.iter (fun equal ->
    E.assert_ solver (T.or_ terms [|guard; equal|])) equalities;
  E.push solver;
  let recover () =
    check (E.check solver = E.Sat) "Semantic congruence recovery";
    List.iteri (fun i equal ->
      let opposite = T.not_ terms equal in
      (match E.check ~assumptions:[opposite] solver with
       | E.Unsat _ -> ()
       | result -> failwith (Printf.sprintf "Semantic assumption %d: %s" i
         (match result with E.Sat -> "sat" | E.Unknown s -> s
          | E.Unsat _ -> "unsat")));
      check (E.check solver = E.Sat) "Semantic assumption rollback") equalities;
    E.pop solver;
    check (E.check solver = E.Sat) "Semantic scope rollback"
  in
  solver, recover

let semantic_congruence_recovery () =
  let solver, recover = semantic_congruence_fixture () in
  let count = ref 0 in
  check (E.check ~cancelled:(fun () -> incr count; false) solver = E.Sat)
    "Semantic congruence fixture";
  let work = E.resource_count solver in
  recover ();
  for stop = 1 to !count do
    let solver, recover = semantic_congruence_fixture () in
    let polls = ref 0 in
    check (E.check ~cancelled:(fun () -> incr polls; !polls = stop) solver
      = E.Unknown "cancelled") "Semantic congruence cancellation";
    recover ();
    let solver, recover = semantic_congruence_fixture () in
    let polls = ref 0 in
    check (try ignore (E.check ~cancelled:(fun () ->
      incr polls; if !polls = stop then raise Exit; false) solver);
      false with Exit -> true) "Semantic congruence exception";
    recover ()
  done;
  for budget = 0 to work do
    let solver, recover = semantic_congruence_fixture () in
    (match E.check ~budget solver with
    | E.Unknown _ | E.Sat -> () | _ -> failwith "Semantic congruence budget");
    recover ()
  done;
  Printf.printf
    "semantic congruence: %d cancellations, %d exceptions, %d budgets\n"
    !count !count (work + 1)

let occurrence_fixture queued =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "x" T.Int in
  let y = declaration terms "y" T.Int in
  let f = T.symbol terms
    { name = "f"; arguments = [|T.Int|]; result = T.Int;
      kind = T.Uninterpreted } in
  let right = T.apply terms f [|y|] in
  let cyclic = T.equal terms y right in
  let opposite = T.not_ terms cyclic in
  if queued then E.push solver;
  E.assert_ solver (T.equal terms x right);
  E.assert_ solver cyclic;
  let recover () =
    check (E.check solver = E.Sat) "Occurs-check replay";
    check (E.check ~assumptions:[opposite] solver
      = E.Unsat [opposite]) "Occurs-check shared DAG";
    if queued then begin
      E.pop solver;
      check (E.check ~assumptions:[opposite] solver = E.Sat)
        "Occurs-check scope recovery"
    end in
  solver, recover

let occurrence_recovery () =
  List.iter (fun queued ->
    let solver, recover = occurrence_fixture queued in
    let polls = ref 0 in
    check (E.check ~cancelled:(fun () -> incr polls; false) solver = E.Sat)
      "Occurs-check fixture";
    recover ();
    for stop = 1 to !polls do
      let solver, recover = occurrence_fixture queued in
      let polls = ref 0 in
      (match E.check ~cancelled:(fun () -> incr polls; !polls = stop) solver with
      | E.Unknown "cancelled" | E.Sat -> ()
      | _ -> failwith "Occurs-check cancellation");
      recover ()
    done;
    Printf.printf "shared DAG queued=%b: %d polls passed\n%!" queued !polls)
    [false; true]

let reclaimed_scopes () =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "persistent" T.Int in
  E.register solver x;
  E.assert_ solver (equal terms x (int terms 4));
  let exception Abort in
  for cycle = 0 to 47 do
    E.push solver;
    let atoms = Array.init 96 (fun index ->
      declaration terms (Printf.sprintf "scoped%d_%d" cycle index) T.Bool) in
    Array.iter (E.assert_ solver) atoms;
    check (E.check solver = E.Sat) "Repeated scoped check";
    let generation = Option.get (E.model_generation solver) in
    E.push solver;
    E.assert_ solver (T.not_ terms atoms.(0));
    if cycle >= 32 then begin
      (match E.check ~budget:1 solver with
      | E.Unknown _ -> ()
      | _ -> failwith "Bounded scoped check")
    end;
    check (E.check solver = E.Unsat []) "Scoped contradiction after replay";
    check (not (E.model_is_current solver generation)) "Stale scoped model";
    E.pop solver;
    check (E.check solver = E.Sat) "Inner scope after replay";
    E.pop solver;
    if cycle >= 32 && cycle mod 7 = 0 then begin
      check (try ignore (E.check ~cancelled:(fun () -> raise Abort) solver);
        false with Abort -> true) "Reclamation callback exception";
      check (E.check ~budget:0 solver = E.Unknown "resource limit")
        "Reclamation zero budget"
    end;
    let negative = T.not_ terms atoms.(0) in
    check (E.check ~assumptions:[negative] solver = E.Sat)
      "Discarded scoped assertion survived";
    check (E.value solver x = Some (E.Integer (Z.of_int 4)))
      "Reclamation lost root model"
  done

let bitvector_ite_fixture ~deferred width =
  let terms = T.create () in
  let solver = E.create terms in
  let sort = T.Bitvec width in
  let x = declaration terms "ite_x" sort in
  let y = declaration terms "ite_y" sort in
  let z = declaration terms "ite_z" sort in
  let guard =
    if deferred then declaration terms "ite_guard" T.Bool
    else
      let predicate = T.symbol terms
        { name = "ite_guard"; arguments = [|T.Int|]; result = T.Bool;
          kind = T.Uninterpreted } in
      T.apply terms predicate [|int terms 1|] in
  let selected = T.ite terms guard x y in
  if deferred then begin
    E.assert_ solver (builtin terms "bvult" T.Bool
      [selected; T.bits terms width Z.one]);
    check (E.check solver = E.Sat) "Pure BV ITE warmup"
  end;
  let add value = builtin terms "bvadd" sort [value; z] in
  let predicate = T.symbol terms
    { name = "ite_observe"; arguments = [|sort|]; result = T.Bool;
      kind = T.Uninterpreted } in
  let observe value = T.apply terms predicate [|add value|] in
  let left = T.equal terms (observe selected) (observe x) in
  let right = T.equal terms (observe selected) (observe y) in
  E.push solver;
  E.assert_ solver guard;
  E.assert_ solver (T.not_ terms left);
  let recover () =
    check (E.check solver = E.Unsat []) "BV ITE congruence recovery";
    E.pop solver;
    check (E.check ~assumptions:[T.not_ terms guard; T.not_ terms left]
      solver = E.Sat) "BV ITE branch equality escaped guard";
    (match E.check ~assumptions:[T.not_ terms guard; T.not_ terms right]
        solver with
    | E.Unsat _ -> () | _ -> failwith "BV ITE false branch congruence");
    check (E.check solver = E.Sat) "BV ITE assumptions persisted"
  in
  solver, recover

let bitvector_ite_congruence deferred =
  let fixture = bitvector_ite_fixture ~deferred in
  List.iter (fun width ->
    let solver, recover = fixture width in
    check (E.check ~budget:(1000 + 100 * width) solver = E.Unsat [])
      "BV ITE congruence search budget";
    recover ()) [1; 3; 32; 129];
  let solver, recover = fixture 3 in
  let polls = ref 0 in
  check (E.check ~cancelled:(fun () -> incr polls; false) solver = E.Unsat [])
    "BV ITE fixture";
  let work = E.resource_count solver in
  recover ();
  for stop = 1 to !polls do
    let solver, recover = fixture 3 in
    let calls = ref 0 in
    (match E.check ~cancelled:(fun () -> incr calls; !calls = stop) solver with
    | E.Unknown "cancelled" | E.Unsat [] -> ()
    | _ -> failwith "BV ITE cancellation");
    recover ();
    let solver, recover = fixture 3 in
    let calls = ref 0 in
    (match E.check ~cancelled:(fun () ->
       incr calls; if !calls = stop then raise Exit; false) solver with
    | exception Exit -> () | _ -> failwith "BV ITE callback exception");
    recover ()
  done;
  for budget = 0 to work do
    let solver, recover = fixture 3 in
    (match E.check ~budget solver with
    | E.Unknown _ | E.Unsat [] -> ()
    | _ -> failwith "BV ITE interrupted encoding");
    recover ()
  done;
  Printf.printf "BV ITE deferred=%b: %d polls and %d budgets recovered\n%!"
    deferred !polls (work + 1)


let boolean_sharing_fixture kind =
  let terms = T.create () in
  let solver = E.create terms in
  let p = declaration terms "p" T.Bool
  and q = declaration terms "q" T.Bool in
  List.iter (E.register solver) [p;q];
  List.iter (E.assert_ solver) [p;q];
  check (E.check solver = E.Sat) "Propositional Boolean warmup";
  let old = Vox_smt_model.create ~terms ~solver in
  let later = declaration terms "later" T.Bool in
  E.register solver later;
  (match Vox_smt_model.value old p with
  | exception Vox_smt_model.Unavailable _ -> ()
  | _ -> failwith "Late registration model generation");
  let left,right = match kind with
    | 0 ->
      let f = T.symbol terms {name="f";arguments=[|T.Bool|];result=T.Int;
        kind=T.Uninterpreted} in
      T.apply terms f [|p|],T.apply terms f [|q|]
    | 1 ->
      let box = T.symbol terms
        {name="box";arguments=[|T.Bool|];result=T.Named 0;
         kind=T.Constructor 0} in
      ignore (T.symbol terms
        {name="field";arguments=[|T.Named 0|];result=T.Bool;
         kind=T.Selector (box,0)});
      T.apply terms box [|p|],T.apply terms box [|q|]
    | _ ->
      let f = T.symbol terms {name="f";arguments=[|T.Int|];result=T.Int;
        kind=T.Uninterpreted} in
      let branch b = T.ite terms b (int terms 7) (int terms 9) in
      T.apply terms f [|branch p|],T.apply terms f [|branch q|] in
  let bad = T.not_ terms (T.equal terms left right) in
  let recover () =
    check (E.check ~assumptions:[bad] solver = E.Unsat [bad])
      "Late Boolean sharing core";
    check (E.check solver = E.Sat) "Late Boolean assumption rollback";
    let model = Vox_smt_model.create ~terms ~solver in
    List.iter (fun term ->
      check (Vox_smt_model.value model term = Vox_smt_model.Boolean true)
        "Propositional Boolean model") [p;q];
    check (Vox_smt_model.equal model
      (Vox_smt_model.value model left) (Vox_smt_model.value model right))
      "Shared Boolean function or constructor model";
    E.push solver;
    E.assert_ solver bad;
    check (E.check solver = E.Unsat []) "Late Boolean sharing scope";
    E.pop solver;
    check (E.check solver = E.Sat) "Late Boolean sharing pop" in
  solver,bad,recover

let boolean_sharing_recovery () =
  List.iter (fun kind ->
    let solver,bad,recover = boolean_sharing_fixture kind in
    let polls = ref 0 in
    check (E.check ~assumptions:[bad]
      ~cancelled:(fun () -> incr polls;false) solver = E.Unsat [bad])
      "Late Boolean sharing fixture";
    let work = E.resource_count solver and poll_count = !polls in
    recover ();
    for budget=0 to work do
      let solver,bad,recover = boolean_sharing_fixture kind in
      (match E.check ~budget ~assumptions:[bad] solver with
      | E.Unknown _ | E.Unsat [_] -> ()
      | _ -> failwith "Late Boolean sharing budget");
      recover ()
    done;
    for stop=1 to poll_count do
      List.iter (fun raises ->
        let solver,bad,recover = boolean_sharing_fixture kind in
        let calls=ref 0 in
        (match E.check ~assumptions:[bad] ~cancelled:(fun () ->
          incr calls;
          if !calls=stop then (if raises then raise Exit else true)
          else false) solver with
        | E.Unknown "cancelled" when not raises -> ()
        | exception Exit when raises -> ()
        | E.Unsat [result] when not raises && result=bad -> ()
        | E.Sat -> failwith "Late Boolean sharing returned SAT"
        | E.Unsat _ -> failwith (Printf.sprintf
            "Late Boolean sharing kind%d stop%d raises%b calls%d early UNSAT"
            kind stop raises !calls)
        | E.Unknown reason -> failwith (Printf.sprintf
            "Late Boolean sharing kind%d stop%d raises%b calls%d: %s"
            kind stop raises !calls reason));
        recover ()) [false;true]
    done;
    Printf.printf "Boolean sharing %d: %d budgets, %d callbacks recovered\n%!"
      kind (work+1) (2 * poll_count)) [0;1;2]


let finite_domain_boundaries () =
  let check_sat source =
    check (F.run (F.create ()) (source ^ "(check-sat)") = ["sat"])
      "Finite-domain satisfiable boundary" in
  check_sat
    "(declare-const x Int)(declare-const y Int)(declare-const z Int)\
     (assert (or (= x 0) (= x 1)))\
     (assert (or (= y 1) (= y 2)))\
     (assert (or (= z 0) (= z 2)))(assert (distinct x y z))";
  let impossible =
    "(and (or (= x 0) (= x 1))(or (= y 0) (= y 1))\
      (or (= z 0) (= z 1))(distinct x y z))" in
  List.iter (fun formula -> check_sat
    ("(declare-const x Int)(declare-const y Int)(declare-const z Int)\
      (declare-const p Bool)(assert " ^ formula ^ ")"))
    ["(or p " ^ impossible ^ ")"; "(not " ^ impossible ^ ")"];
  check_sat
    "(declare-const a Int)(declare-const b Int)\
     (declare-const c Int)(declare-const d Int)(declare-const x Int)\
     (assert (or (= x a) (= x b)))(assert (or (= x c) (= x d)))";
  check_sat
    "(declare-sort U 0)(declare-const x U)(declare-const y U)\
     (declare-const z U)(assert (distinct x y z))";
  check_sat
    "(declare-datatype Nat ((zero)(succ (pred Nat))))\
     (declare-const x Nat)(declare-const y Nat)(declare-const z Nat)\
     (assert (distinct x y z))";
  check_sat
    "(declare-const x Int)(assert (or (= x 0) (= x 1)))\
     (assert (or (= x (- 0)) (= x 2)))"

let finite_domain_fixture kind =
  let terms=T.create () in
  let solver=E.create terms in
  check (E.check solver=E.Sat) "Finite-domain warmup";
  let sort,values = match kind with
    | 1 -> T.Bitvec 2,Array.init 4 (fun i -> T.bits terms 2 (Z.of_int i))
    | 2 -> T.Named 0,Array.init 2 (fun i ->
      T.apply terms (T.symbol terms
        {name="value"^string_of_int i;arguments=[||];result=T.Named 0;
         kind=T.Constructor 0}) [||])
    | _ -> T.Int,Array.init 4 (int terms) in
  let variables=Array.init (if kind=4 then 5 else 3) (fun i ->
    declaration terms ("x"^string_of_int i) sort) in
  Array.iter (E.register solver) variables;
  let domain subject indices = T.or_ terms (Array.of_list
    (List.map (fun i -> T.equal terms subject values.(i)) indices)) in
  let indices i = if kind=4 && i>0 then [0;1;2] else [0;1] in
  let domains = if kind=2 then [] else
    Array.to_list
      (Array.mapi (fun i variable -> domain variable (indices i)) variables) in
  let distinct i j=T.not_ terms (T.equal terms variables.(i) variables.(j)) in
  let contradictions = if kind=3 then
    [domain variables.(0) [2;3]]
    else if kind=4 then
      [distinct 0 1;distinct 1 2;distinct 1 3;distinct 1 4;
       distinct 2 3;distinct 2 4;distinct 3 4]
    else [distinct 0 1;distinct 0 2;distinct 1 2] in
  E.push solver;
  List.iter (E.assert_ solver) (domains@contradictions);
  let recover () =
    check (E.check solver=E.Unsat []) "Finite-domain recovery";
    E.pop solver;
    check (E.check solver=E.Sat) "Finite-domain scope rollback";
    E.push solver;
    List.iter (E.assert_ solver) domains;
    check (E.check solver=E.Sat) "Finite-domain satisfiable scope";
    let model=Vox_smt_model.create ~terms ~solver in
    Array.iteri (fun index variable ->
      let value=Vox_smt_model.value model variable in
      check (List.exists (fun i -> Vox_smt_model.equal model value
        (Vox_smt_model.value model values.(i))) (indices index))
        "Finite-domain recovered model") variables;
    E.pop solver;
    check (E.check ~assumptions:(domains@contradictions) solver
      |> function E.Unsat core ->
        let replay=E.create terms in
        List.iter (E.assert_ replay) core;
        E.check replay=E.Unsat []
      | _ -> false) "Finite-domain assumption core";
    check (E.check solver=E.Sat) "Finite-domain assumptions leaked" in
  solver,recover

let finite_domain_recovery () =
  let module R=Vox_smt_resource in
  List.iter (fun kind ->
    let solver,recover=finite_domain_fixture kind in
    let polls=ref 0 in
    let resource=R.create ~cancelled:(fun () -> incr polls;false) () in
    check (E.check ~resource solver=E.Unsat []) "Finite-domain fixture";
    let work=R.used resource and callbacks= !polls in
    recover ();
    for budget=0 to work-1 do
      let solver,recover=finite_domain_fixture kind in
      let resource=R.create ~budget () in
      (match E.check ~resource solver with
      | E.Unknown _ -> () | _ -> failwith "Finite-domain resource limit");
      check (R.used resource<=budget) "Finite-domain budget exceeded";
      recover ()
    done;
    List.iter (fun raises -> for stop=1 to callbacks do
      let solver,recover=finite_domain_fixture kind in
      let calls=ref 0 in
      let resource=R.create ~cancelled:(fun () -> incr calls;
        if !calls=stop then (if raises then raise Exit else true)
        else false) () in
      (match E.check ~resource solver with
      | E.Unknown _ when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Finite-domain callback interruption");
      recover ()
    done) [false;true];
    Printf.printf "Finite domain %d: %d budgets, %d callbacks recovered\n%!"
      kind work (2*callbacks)) [0;1;2;3;4]

let fixed_linear_forms_recovery () =
  let module R = Vox_smt_resource in
  let command text = match S.parse text with
    | [term] -> term | _ -> failwith "Expected one command" in
  let fixture () =
    let solver = F.create () in
    ignore (F.run solver {|(set-logic QF_NIA)
      (declare-const i Int)(declare-const n Int)(declare-const s Int)
      (assert (= i (+ (* (- 2) s) (* i i))))
      (push 1)
      (assert (>= (+ i (* (- 1) n)) 1))
      (assert (not (>= (+ i (* (- 1) n)) 2)))
      (assert (not (= n (+ (* 2 s) (* (- 1) (* n n))))))|});
    solver in

  let recover solver =
    check (F.run solver "(check-sat)" = ["unsat"]) "Fixed linear form recheck";
    check (F.run solver "(pop 1)(assert (= i 0))(assert (= n 2))\
      (check-sat)(get-value (i n s))" =
      ["sat"; "((i 0) (n 2) (s 0))"]) "Fixed linear form scope and model" in

  let near = F.create () in
  check (F.run near {|(declare-const x Int)(declare-const y Int)
    (assert (<= (- x y) 1))(assert (>= (- x y) 0))
    (assert (not (= x y)))(check-sat)|} = ["sat"])
    "Different linear bounds collapsed";
  let cycle = F.create () in
  check (F.run cycle {|(declare-const x Int)(declare-const y Int)
    (declare-const z Int)
    (assert (<= (- x y) 1))(assert (>= (- x y) 1))
    (assert (<= (- y z) 1))(assert (>= (- y z) 1))
    (assert (<= (- z x) (- 1)))(assert (>= (- z x) (- 1)))
    (check-sat)|} = ["unsat"]) "Cyclic linear bounds";
  let solver = fixture () in
  let polls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr polls; false) () in
  check (F.execute ~resource solver (command "(check-sat)") = Some "unsat")
    "Fixed linear form fixture";
  let work = R.used resource and callbacks = !polls in
  recover solver;
  for budget = 0 to work - 1 do
    let solver = fixture () in
    let resource = R.create ~budget () in
    check (F.execute ~resource solver (command "(check-sat)") = Some "unknown")
      "Fixed linear form interruption";
    check (R.used resource <= budget) "Fixed linear form budget exceeded";
    recover solver
  done;
  List.iter (fun raises ->
    for stop = 1 to callbacks do
      let solver = fixture () and polls = ref 0 in
      let resource = R.create ~cancelled:(fun () -> incr polls;
        if !polls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource solver (command "(check-sat)") with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Fixed linear form callback");
      recover solver
    done) [false; true];
  Printf.printf "%d interrupted budgets, %d callbacks passed\n%!"
    work (2 * callbacks)

let exposed_equalities_recovery () =
  let module R = Vox_smt_resource in
  let command text = match S.parse text with
    | [term] -> term | _ -> failwith "Expected one command" in
  let fixture () =
    let solver = F.create () in
    ignore (F.run solver {|(set-logic QF_BV)
      (declare-const c (_ BitVec 3))
      (declare-const x (_ BitVec 3))
      (declare-const y (_ BitVec 3))
      (declare-const z (_ BitVec 3))
      (assert (= c #b001))
      (assert (=> (= c #b001) (= x y)))
      (assert (=> (= x y) (= z (bvadd x #b001))))
      (push 1)
      (assert (distinct (bvsub z #b001) y))|});
    solver in
  let recover solver =
    check (F.run solver "(check-sat)" = ["unsat"])
      "Exposed equality recheck";
    check (F.run solver {|(pop 1)(push 1)(assert (= y #b011))
      (check-sat)(get-value (x y z))|} =
      ["sat"; "((x (_ bv3 3)) (y (_ bv3 3)) (z (_ bv4 3)))"])
      "Exposed equality scope and model";
    check (F.run solver {|(pop 1)(push 1)(assert (= y #b111))
      (check-sat)(get-value (x y z))|} =
      ["sat"; "((x (_ bv7 3)) (y (_ bv7 3)) (z (_ bv0 3)))"])
      "Exposed equality alternate scope" in
  let solver = fixture () in
  let callbacks = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
  check (F.execute ~resource solver (command "(check-sat)") = Some "unsat")
    "Exposed equality fixture";
  let work = R.used resource and calls = !callbacks in
  recover solver;
  for budget = 0 to work - 1 do
    let solver = fixture () in
    let resource = R.create ~budget () in
    check (F.execute ~resource solver (command "(check-sat)") = Some "unknown")
      "Exposed equality interruption";
    check (R.used resource <= budget) "Exposed equality budget exceeded";
    recover solver
  done;
  List.iter (fun raises ->
    for stop = 1 to calls do
      let solver = fixture () and count = ref 0 in
      let resource = R.create ~cancelled:(fun () -> incr count;
        if !count = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource solver (command "(check-sat)") with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Exposed equality callback");
      recover solver
    done) [false; true];
  Printf.printf "Exposed equalities: %d budgets, %d callbacks recovered\n%!"
    work (2 * calls)

let prepared_substitution_recovery () =
  let module R = Vox_smt_resource in
  let command text = match S.parse text with
    | [term] -> term | _ -> failwith "Expected one command" in
  let fixture () =
    let solver = F.create () in
    check (F.run solver {|(set-option :global-declarations true)
      (set-option :produce-models true)
      (declare-const x Bool)(declare-const y Bool)
      (declare-const wide (_ BitVec 4096))
      (push 1)(assert (or x y))(check-sat)(pop 1)
      (assert (= x y))(push 1)(assert (not (= x y)))
      (push 1)(assert (bvule wide (_ bv0 4096)))
      (push 1)(pop 2)(check-sat)|} = ["sat"; "unsat"])
      "Prepared substitution fixture";
    solver in
  let recover solver =
    check (F.run solver "(check-sat)" = ["unsat"])
      "Prepared substitution was discarded";
    check (F.run solver {|(pop 1)(check-sat-assuming (x (not y)))
      (assert x)(check-sat)(get-value (x y))|}
      = ["unsat"; "sat"; "((x true) (y true))"])
      "Prepared substitution scope and model" in
  let integers = F.create () in
  check (F.run integers {|(set-option :global-declarations true)
    (set-option :produce-models true)
    (declare-const x Int)(declare-const y Int)
    (declare-const wide (_ BitVec 4096))
    (push 1)(assert (or (> x 0) (> y 0)))(check-sat)(pop 1)
    (assert (<= x y))(assert (>= x y))
    (push 1)(assert (not (= x y)))
    (push 1)(assert (bvule wide (_ bv0 4096)))(push 1)(pop 2)
    (check-sat)(check-sat)(pop 1)(assert (= x 7))
    (check-sat)(get-value (x y))|}
    = ["sat"; "unsat"; "unsat"; "sat"; "((x 7) (y 7))"])
    "Prepared linear substitution was discarded";
  let solver = fixture () and callbacks = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
  check (F.execute ~resource solver (command "(check-sat)") = Some "unsat")
    "Prepared substitution replay";
  let work = R.used resource and calls = !callbacks in
  recover solver;
  for budget = 0 to work - 1 do
    let solver = fixture () in
    let resource = R.create ~budget () in
    check (F.execute ~resource solver (command "(check-sat)") = Some "unknown")
      "Prepared substitution budget";
    check (R.used resource <= budget) "Prepared substitution budget exceeded";
    recover solver
  done;
  List.iter (fun raises ->
    for stop = 1 to calls do
      let solver = fixture () and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () -> incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource solver (command "(check-sat)") with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Prepared substitution callback");
      recover solver
    done) [false; true];
  Printf.printf "Prepared substitutions: %d budgets, %d callbacks recovered\n%!"
    work (2 * calls)

module Finite_domain_symmetry_tests = struct
  module T = Vox_smt_term
  module E = Vox_smt_engine
  module R = Vox_smt_resource

  let ensure condition message = if not condition then failwith message
  let declare terms name =
    T.apply terms (T.symbol terms
      { name; arguments = [||]; result = T.Named 0;
        kind = T.Uninterpreted }) [||]

  let distinct terms values =
    let pairs = ref [] in
    Array.iteri (fun i left -> Array.iteri (fun j right ->
      if i < j then pairs := T.not_ terms (T.equal terms left right) :: !pairs)
        values) values;
    T.and_ terms (Array.of_list !pairs)

  let fixture colors count =
    let terms = T.create () in
    let solver = E.create terms in
    let representatives = Array.init colors (fun i ->
      declare terms ("color" ^ string_of_int i)) in
    let witnesses = Array.init count (fun i ->
      declare terms ("node" ^ string_of_int i)) in
    Array.iter (E.register solver) representatives;
    Array.iter (E.register solver) witnesses;
    let memberships = Array.map (fun witness -> T.or_ terms
        (Array.map (T.equal terms witness) representatives)) witnesses in
    E.assert_ solver (distinct terms representatives);
    Array.iter (E.assert_ solver) memberships;
    terms, solver, representatives, witnesses

  let different terms left right = T.not_ terms (T.equal terms left right)

  let validate_model terms solver representatives witnesses edges excluded =
    let value term = match E.value solver term with
      | Some (E.Abstract value) -> value
      | _ -> failwith "Missing UF model value" in
    let colors = Array.map value representatives in
    let nodes = Array.map value witnesses in
    Array.iter (fun node -> ensure (Array.mem node colors)
        "Model violates finite membership") nodes;
    List.iter (fun (a, b) -> ensure (nodes.(a) <> nodes.(b))
        "Model violates graph edge") edges;
    List.iter (fun (a, b) -> ensure (nodes.(a) <> colors.(b))
        "Model violates asymmetric exclusion") excluded;
    ignore terms

  let colorable colors count edges excluded =
    let assignment = Array.make count 0 in
    let rec loop index =
      if index = count then
        List.for_all (fun (a, b) -> assignment.(a) <> assignment.(b)) edges
        && List.for_all (fun (a, b) -> assignment.(a) <> b) excluded
      else
        let rec choose color =
          color < colors &&
          (assignment.(index) <- color;
           loop (index + 1) || choose (color + 1)) in
        choose 0 in
    loop 0

  let check_graph colors count edges excluded =
    let terms, solver, representatives, witnesses = fixture colors count in
    let edge_terms = List.map (fun (a, b) ->
        different terms witnesses.(a) witnesses.(b)) edges in
    List.iter (E.assert_ solver) edge_terms;
    let expected = colorable colors count edges [] in
    let test excluded assumptions =
      let expected = colorable colors count edges excluded in
      match E.check ~assumptions solver with
      | E.Sat ->
        ensure expected "Graph oracle expected UNSAT";
        validate_model terms solver representatives witnesses edges excluded
      | E.Unsat core ->
        ensure (not expected) "Graph oracle expected SAT";
        ensure (List.for_all (fun term -> List.mem term assumptions) core)
          "Private symmetry constraint escaped in core";
        let replay = E.create terms in
        E.assert_ replay (distinct terms representatives);
        Array.iter (fun witness -> E.assert_ replay (T.or_ terms
            (Array.map (T.equal terms witness) representatives))) witnesses;
        List.iter (E.assert_ replay) edge_terms;
        List.iter (E.assert_ replay) core;
        (match E.check replay with E.Unsat _ -> ()
         | _ -> failwith "Returned assumption core is invalid")
      | E.Unknown reason -> failwith ("Graph UNKNOWN: " ^ reason) in
    test [] [];
    let assumptions = List.map (fun (a, b) ->
        different terms witnesses.(a) representatives.(b)) excluded in
    test excluded assumptions;
    test [] [];
    E.push solver;
    List.iter (E.assert_ solver) assumptions;
    (match E.check solver with
    | E.Sat -> ensure (colorable colors count edges excluded)
        "Scoped asymmetric SAT is wrong"
    | E.Unsat _ -> ensure (not (colorable colors count edges excluded))
        "Scoped asymmetric UNSAT is wrong"
    | E.Unknown reason -> failwith ("Scoped UNKNOWN: " ^ reason));
    E.pop solver;
    (match E.check solver with
    | E.Sat -> ensure expected "Pop changed original UNSAT"
    | E.Unsat _ -> ensure (not expected) "Pop retained symmetry restriction"
    | E.Unknown reason -> failwith ("Pop UNKNOWN: " ^ reason))

  let graph_oracle () =
    let random = Random.State.make [|80917; 661|] in
    for instance = 0 to 159 do
      let colors = 2 + instance mod 2 and count = 5 in
      let edges = ref [] and excluded = ref [] in
      for a = 0 to count - 1 do
        for b = a + 1 to count - 1 do
          if Random.State.bool random then edges := (a, b) :: !edges
        done;
        if Random.State.bool random then
          excluded := (a, Random.State.int random colors) :: !excluded
      done;
      check_graph colors count !edges !excluded
    done;
    Printf.printf "160 symmetry graph/model/core/scoping oracles passed\n%!"

  let asymmetric_followup () =
    for color = 0 to 2 do
      let terms, solver, representatives, witnesses = fixture 3 2 in
      ensure (E.check solver = E.Sat) "Initial symmetric problem";
      let chosen = T.equal terms witnesses.(0) representatives.(color) in
      ensure (E.check ~assumptions:[chosen] solver = E.Sat)
        "Old symmetry survived asymmetric assumption";
      ensure (E.check solver = E.Sat) "Assumption polluted symmetric context";
      E.push solver;
      E.assert_ solver chosen;
      ensure (E.check solver = E.Sat)
        "Old symmetry survived scoped asymmetric assertion";
      E.pop solver;
      ensure (E.check solver = E.Sat) "Pop polluted symmetric context";
      E.assert_ solver chosen;
      ensure (E.check solver = E.Sat)
        "Old symmetry survived permanent asymmetric assertion"
    done

  let interrupted () =
    let budgets = ref 0 and callbacks = ref 0 in
    for limit = 0 to 200 do
      let terms, solver, representatives, witnesses = fixture 3 4 in
      let asymmetric = T.equal terms witnesses.(0) representatives.(2) in
      let resource = R.create ~budget:(limit * 11) () in
      (match E.check ~resource solver with
      | E.Unknown _ -> incr budgets
      | E.Sat -> () | E.Unsat _ -> failwith "Interrupted SAT became UNSAT");
      ensure (E.check ~assumptions:[asymmetric] solver = E.Sat)
        "Budget recovery retained temporary symmetry";
      validate_model terms solver representatives witnesses [] []
    done;
    for limit = 1 to 30 do
      let terms, solver, representatives, witnesses = fixture 3 4 in
      let polls = ref 0 in
      (match E.check ~cancelled:(fun () -> incr polls; !polls >= limit)
          solver with
      | E.Unknown _ -> incr callbacks
      | E.Sat -> () | E.Unsat _ -> failwith "Cancelled SAT became UNSAT");
      ensure (E.check solver = E.Sat) "Callback recovery lost SAT";
      let asymmetric = T.equal terms witnesses.(0) representatives.(2) in
      ensure (E.check ~assumptions:[asymmetric] solver = E.Sat)
        "Callback recovery retained temporary symmetry"
    done;
    ensure (!budgets > 0 && !callbacks > 0) "No interruptions exercised";
    Printf.printf "%d interrupted budgets, %d callbacks recovered\n%!"
      !budgets !callbacks

  let algebra_oracle () =
    let size = 3 in
    let properties table =
      let at a b = table.(a * size + b) in
      let all f =
        let result = ref true in
        for a = 0 to size - 1 do
          for b = 0 to size - 1 do
            for c = 0 to size - 1 do
              if not (f a b c) then result := false
            done
          done
        done;
        !result in
      [|all (fun a b _ -> at a b = at b a);
        all (fun a b c -> at (at a b) c = at a (at b c));
        all (fun a _ _ -> at a a = a);
        all (fun a _ _ -> at a a <> a);
        all (fun a b _ -> at a a = at b b);
        all (fun a b _ -> at a (at a b) = b);
        all (fun a b c -> b = c || at a b <> at a c);
        all (fun a b c -> a = b || at a c <> at b c)|] in
    for mask = 0 to 63 do
      let terms = T.create () in
      let solver = E.create terms in
      let representatives = Array.init size (fun i ->
        declare terms ("algebra" ^ string_of_int i)) in
      let symbol = T.symbol terms
        { name = "operation"; arguments = [|T.Named 0; T.Named 0|];
          result = T.Named 0; kind = T.Uninterpreted } in
      let app a b = T.apply terms symbol [|a; b|] in
      let cells = Array.init (size * size) (fun i ->
        app representatives.(i / size) representatives.(i mod size)) in
      Array.iter (E.register solver) representatives;
      Array.iter (E.register solver) cells;
      E.assert_ solver (distinct terms representatives);
      Array.iter (fun cell -> E.assert_ solver (T.or_ terms
        (Array.map (T.equal terms cell) representatives))) cells;
      let all f =
        let clauses = ref [] in
        Array.iter (fun a -> Array.iter (fun b -> Array.iter (fun c ->
          clauses := f a b c :: !clauses) representatives) representatives)
          representatives;
        T.and_ terms (Array.of_list !clauses) in
      let axioms =
        [|all (fun a b _ -> T.equal terms (app a b) (app b a));
          all (fun a b c -> T.equal terms (app (app a b) c)
              (app a (app b c)));
          all (fun a _ _ -> T.equal terms (app a a) a);
          all (fun a _ _ -> different terms (app a a) a);
          all (fun a b _ -> T.equal terms (app a a) (app b b));
          all (fun a b _ -> T.equal terms (app a (app a b)) b);
          all (fun a b c -> if b = c then T.boolean terms true else
              different terms (app a b) (app a c));
          all (fun a b c -> if a = b then T.boolean terms true else
              different terms (app a c) (app b c))|] in
      let selected index = mask land (1 lsl (index mod 6)) <> 0 in
      Array.iteri (fun index axiom ->
        if selected index then E.assert_ solver axiom) axioms;
      let table = Array.make (size * size) 0 in
      let accepts table =
        Array.for_all Fun.id (Array.mapi (fun index value ->
          not (selected index) || value) (properties table)) in
      let rec enumerate index =
        if index = Array.length table then accepts table else
          let rec choose value =
            value < size && (table.(index) <- value;
              enumerate (index + 1) || choose (value + 1)) in
          choose 0 in
      let expected = enumerate 0 in
      match E.check solver with
      | E.Unsat _ -> ensure (not expected) "Finite algebra false UNSAT"
      | E.Sat ->
        ensure expected "Finite algebra false SAT";
        let colors = Array.map (E.value solver) representatives in
        let table = Array.map (fun cell ->
          let value = E.value solver cell in
          ensure (Option.is_some value) "Missing finite algebra value";
          let rec find i =
            if i = size then failwith "Algebra closure model" else
            if colors.(i) = value then i else find (i + 1) in
          find 0) cells in
        ensure (accepts table) "Finite algebra model violates axiom"
      | E.Unknown reason -> failwith ("Finite algebra UNKNOWN: " ^ reason)
    done;
    print_endline "64 exhaustive finite algebra/model oracles passed"

  let aliased_representatives () =
    for all_equal = 0 to 1 do
      let terms = T.create () in
      let solver = E.create terms in
      let colors = Array.init 3 (fun i ->
        declare terms ("alias" ^ string_of_int i)) in
      let x = declare terms "alias_x" in
      let symbol = T.symbol terms
        { name = "alias_f"; arguments = [|T.Named 0|];
          result = T.Named 0; kind = T.Uninterpreted } in
      let cells = Array.map (fun term -> T.apply terms symbol [|term|])
          colors in
      Array.iter (E.register solver) colors;
      E.register solver x;
      Array.iter (E.register solver) cells;
      let pairs =
        [|T.equal terms colors.(0) colors.(1);
          T.equal terms colors.(0) colors.(2);
          T.equal terms colors.(1) colors.(2)|] in
      E.assert_ solver (T.or_ terms pairs);
      E.assert_ solver (if all_equal = 1 then T.and_ terms pairs
          else T.not_ terms (T.and_ terms pairs));
      Array.iter (fun subject -> E.assert_ solver (T.or_ terms
        (Array.map (T.equal terms subject) colors))) (Array.append [|x|] cells);
      ensure (E.check solver = E.Sat) "Aliased representative model";
      let values = Array.map (E.value solver) colors in
      Array.iter (fun value -> ensure (Option.is_some value)
        "Missing aliased representative value") values;
      let count = List.length (List.sort_uniq compare (Array.to_list values)) in
      ensure (count = if all_equal = 1 then 1 else 2)
        "Aliased representative cardinality";
      let chosen = T.equal terms x colors.(2) in
      ensure (E.check ~assumptions:[chosen] solver = E.Sat)
        "Aliased asymmetric assumption";
      ensure (E.check solver = E.Sat) "Aliased assumption recovery"
    done

  let run () =
    aliased_representatives ();
    asymmetric_followup ();
    graph_oracle ();
    algebra_oracle ();
    interrupted ();
    print_endline "Finite-domain symmetry tests passed"
end

let () =
  Finite_domain_symmetry_tests.run ();
  prepared_substitution_recovery ();
  exposed_equalities_recovery ();
  fixed_linear_forms_recovery ();
  finite_domain_boundaries ();
  finite_domain_recovery ();
  boolean_sharing_recovery ();
  List.iter bitvector_ite_congruence [false; true];
  reclaimed_scopes ();
  synchronous_validation ();
  preprocessing_recovery ();
  reentrancy ();
  retired_statistics ();
  recursive_datatype ();
  append_only_arena ();
  constant_polling ();
  forced_comparison_activation ();
  comparison_activation ();
  semantic_congruence_recovery ();
  occurrence_recovery ();
  wide_budget ();
  interrupt_every_poll false;
  interrupt_every_poll true;
  interrupted_assumption ();
  interrupted_pop ();
  budget_replay ();
  named_frontend ();
  exception_recovery ();
  print_endline "Disposable encoding tests passed"
