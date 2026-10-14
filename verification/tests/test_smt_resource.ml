module R = Vox_smt_resource
module E = Vox_smt_engine
module T = Vox_smt_term
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message
let exhausted reason f =
  match f () with
  | exception R.Exhausted actual -> check (actual = reason) "Exhaustion reason"
  | _ -> failwith "Expected exhaustion"
let invalid f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "Expected Invalid_argument"
let declaration terms name result =
  T.apply terms (T.symbol terms
    {name; arguments = [||]; result; kind = T.Uninterpreted}) [||]
let int terms n = T.integer terms (Z.of_int n)
let builtin terms name sort arguments =
  T.builtin terms name sort (Array.of_list arguments)

let primitive () =
  let resource = R.create ~budget:3 () in
  R.poll resource; R.charge resource 2;
  check (R.used resource = 3 && R.remaining resource = 0) "Exact budget";
  R.check resource;
  exhausted "resource limit" (fun () -> R.poll resource);
  exhausted "resource limit" (fun () -> R.charge resource 0);
  check (R.used resource = 3) "Budget exceeded";
  invalid (fun () -> R.create ~budget:(-1) ());
  invalid (fun () -> R.charge (R.create ()) (-1));
  invalid (fun () -> R.create ~timeout:1. ());
  List.iter (fun timeout ->
    invalid (fun () -> R.create ~clock:(fun () -> 0.) ~timeout ()))
    [0.; -1.; infinity; nan];
  invalid (fun () -> R.create ~clock:(fun () -> nan) ~timeout:1. ());
  let samples = ref 0 in
  let resource = R.create ~clock:(fun () -> incr samples; 0.) () in
  R.charge resource 1000; R.check resource;
  check (!samples = 0) "Unused clock sampled";
  let time = ref 2. in
  let resource = R.create ~clock:(fun () -> !time) ~timeout:3. () in
  R.poll resource; time := 5.;
  exhausted "timeout" (fun () -> R.check resource);
  time := 0.; exhausted "timeout" (fun () -> R.check resource);
  let time = ref 0. in
  let resource = R.create ~budget:0 ~cancelled:(fun () -> true)
      ~clock:(fun () -> !time) ~timeout:1. () in
  time := 2.; exhausted "cancelled" (fun () -> R.poll resource)

let hierarchy () =
  let parent = R.create ~budget:10 () in
  let child = R.create ~parent ~budget:3 () in
  R.charge child 2;
  check (R.used parent = 2 && R.used child = 2) "Parent work";
  exhausted "resource limit" (fun () -> R.charge child 4);
  check (R.used parent = 3 && R.used child = 3) "Child saturation";
  let sibling = R.create ~parent ~budget:20 () in
  R.charge sibling 5;
  exhausted "resource limit" (fun () -> R.charge sibling 5);
  check (R.used parent = 10 && R.used sibling = 7) "Parent saturation";
  exhausted "resource limit" (fun () -> R.check parent);
  exhausted "resource limit" (fun () -> R.check sibling);
  let calls = ref [] in
  let parent = R.create ~cancelled:(fun () -> calls := 1 :: !calls; false) () in
  let child = R.create ~parent ~cancelled:(fun () ->
    calls := 2 :: !calls; true) () in
  exhausted "cancelled" (fun () -> R.poll child);
  check (List.rev !calls = [1; 2]) "Ancestor check ordering";
  let stop = ref false in
  let parent = R.create ~cancelled:(fun () -> !stop) () in
  let child = R.create ~parent ~budget:0 () in
  exhausted "resource limit" (fun () -> R.poll child);
  stop := true;
  exhausted "cancelled" (fun () -> R.check parent);
  exhausted "resource limit" (fun () -> R.check child);
  let action = ref (fun () -> ()) in
  let parent = R.create ~cancelled:(fun () -> !action (); false) () in
  let child = R.create ~parent () in
  action := (fun () -> R.poll child);
  invalid (fun () -> R.check parent);
  check (R.used parent = 0 && R.used child = 0) "Reentrant resource charge";
  action := (fun () -> ());
  R.poll child;
  let child = R.create ~parent ~cancelled:(fun () -> R.poll parent; false) () in
  invalid (fun () -> R.poll child);
  check (R.used parent = 1) "Reentrant ancestor charge";
  let parent = R.create () in
  invalid (fun () -> R.create ~parent ~timeout:1.
    ~clock:(fun () -> R.poll parent; 0.) ());
  check (R.used parent = 0) "Reentrant deadline creation";
  R.poll parent;
  let action = ref (fun () -> ()) in
  let parent = R.create ~cancelled:(fun () -> !action (); false) () in
  action := (fun () -> ignore (R.create ~parent ~timeout:1.
    ~clock:(fun () -> 0.) ()));
  invalid (fun () -> R.check parent);
  action := (fun () -> ());
  R.poll parent

let assertion_fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let x = declaration terms "x" T.Int in
  E.assert_ solver (T.equal terms x (int terms 7));
  let ys = Array.init 64 (fun i -> declaration terms
      ("y" ^ string_of_int i) T.Int) in
  let candidate = T.and_ terms (Array.append
      (Array.mapi (fun i y -> T.equal terms y (int terms (i + 1))) ys)
      [|T.equal terms x (int terms 8)|]) in
  terms, solver, x, ys.(0), candidate

let atomic_assertions () =
  let _, solver, _, _, candidate = assertion_fixture () in
  let resource = R.create () in
  E.assert_ ~resource solver candidate;
  let work = R.used resource in
  check (E.check solver = E.Unsat []) "Candidate fixture";
  for budget = 0 to work - 1 do
    let terms, solver, x, y, candidate = assertion_fixture () in
    let resource = R.create ~budget () in
    exhausted "resource limit" (fun () -> E.assert_ ~resource solver candidate);
    check (R.used resource = budget) "Assertion accounting";
    check (E.check ~assumptions:[T.equal terms y (int terms 0)] solver = E.Sat)
      "Failed assertion committed substitutions";
    check (E.value solver x = Some (E.Integer (Z.of_int 7)))
      "Failed assertion lost prior state";
    E.assert_ solver (T.equal terms y (int terms 9));
    check (E.check solver = E.Sat) "Failed assertion did not recover"
  done;
  Printf.printf "atomic assertions: %d work limits\n%!" work;
  let _, solver, _, _, candidate = assertion_fixture () in
  let resource = R.create ~cancelled:(fun () -> E.push solver; false) () in
  invalid (fun () -> E.assert_ ~resource solver candidate);
  check (E.check solver = E.Sat) "Reentrant assertion recovery";
  invalid (fun () -> E.pop solver);
  let polls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr polls; false) () in
  E.assert_ ~resource solver candidate;
  for stop = 1 to !polls do
    let terms, solver, _, y, candidate = assertion_fixture () in
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls; if !calls = stop then raise Exit; false) () in
    (match E.assert_ ~resource solver candidate with
    | exception Exit -> ()
    | () -> failwith "Expected callback exception");
    check (E.check ~assumptions:[T.equal terms y (int terms 0)] solver = E.Sat)
      "Callback exception committed assertion"
  done

let search_fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let a = declaration terms "a" (T.Bitvec 4) in
  let b = declaration terms "b" (T.Bitvec 4) in
  let x = declaration terms "x" T.Int in
  let y = declaration terms "y" T.Int in
  List.iter (E.register solver) [a; b; x; y];
  E.assert_ solver (T.equal terms
    (builtin terms "bvmul" (T.Bitvec 4) [a; b])
    (T.bits terms 4 (Z.of_int 3)));
  E.assert_ solver (builtin terms "<=" T.Bool
    [builtin terms "+" T.Int [x; y]; int terms 8]);
  E.assert_ solver (builtin terms ">=" T.Bool [x; int terms 2]);
  let assumption = builtin terms ">=" T.Bool [y; int terms 5] in
  terms, solver, x, y, assumption

let checks ?(scoped = false) reordered =
  let fixture () =
    let terms, solver, x, y, assumption = search_fixture () in
    if scoped then begin
      E.push solver;
      E.assert_ solver (builtin terms "<=" T.Bool [x; int terms 2])
    end;
    terms, solver, x, y, assumption in
  let reorder_after = if reordered then 0 else max_int in
  let _, solver, _, _, assumption = fixture () in
  let resource = R.create () in
  check (E.For_testing.check
    ~resource ~reorder_after ~assumptions:[assumption] solver
    = E.Sat) "Search fixture";
  let work = R.used resource in
  for budget = 0 to work do
    let terms, solver, x, y, assumption = fixture () in
    let resource = R.create ~budget () in
    (match E.For_testing.check ~resource ~reorder_after
        ~assumptions:[assumption] solver with
    | E.Unknown "resource limit" ->
      check (E.value solver x = None) "Interrupted check leaked model"
    | E.Sat -> ()
    | _ -> failwith "Interrupted check result");
    check (R.used resource <= budget) "Check accounting";
    check (E.check ~assumptions:[T.equal terms y (int terms 0)] solver = E.Sat)
      "Interrupted assumptions persisted";
    E.push solver;
    E.assert_ solver (builtin terms "<" T.Bool [x; int terms 2]);
    check (E.check solver = E.Unsat []) "Interrupted check lost assertion";
    E.pop solver;
    check (E.check solver = E.Sat) "Interrupted check lost scope";
    if scoped then begin
      check (E.value solver x = Some (E.Integer (Z.of_int 2)))
        "Interrupted retry lost scoped assertion";
      E.pop solver;
      check (E.check ~assumptions:[T.equal terms x (int terms 3);
        T.equal terms y (int terms 0)] solver = E.Sat)
        "Interrupted retry retained popped assertion"
    end
  done;
  Printf.printf "shared checks reordered=%b scoped=%b: %d work limits\n%!"
    reordered scoped work;
  let _, solver, _, _, assumption = fixture () in
  let resource = R.create () in
  check (E.check ~resource solver = E.Sat) "Shared resource first check";
  let first = R.used resource in
  check (E.check ~resource ~assumptions:[assumption] solver = E.Sat)
    "Shared resource second check";
  check (R.used resource > first) "Shared resource was reset";
  let time = ref 0. in
  let resource = R.create ~clock:(fun () -> let old = !time in
    time := old +. 1.; old) ~timeout:2. () in
  check (E.check ~resource solver = E.Unknown "timeout") "Injected deadline";
  check (E.check solver = E.Sat) "Deadline recovery";
  let resource = R.create ~budget:0 ~cancelled:(fun () -> true) () in
  check (E.check ~resource ~budget:0 solver = E.Unknown "cancelled")
    "Check cancellation precedence"

let scoped_retry_callbacks () =
  let fixture () =
    let terms, solver, x, y, assumption = search_fixture () in
    E.push solver;
    E.assert_ solver (builtin terms "<=" T.Bool [x; int terms 2]);
    terms, solver, x, y, assumption in
  let _, solver, _, _, assumption = fixture () in
  let polls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr polls; false) () in
  check (E.For_testing.check
    ~resource ~reorder_after:0 ~assumptions:[assumption]
    solver = E.Sat) "Scoped retry callback fixture";
  check ((E.statistics solver).solves >= 2) "Scoped retry was not used";
  for stop = 1 to !polls do
    List.iter (fun raise_exception ->
      let terms, solver, x, y, assumption = fixture () in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then begin
          if raise_exception then raise Exit;
          true
        end else false) () in
      (match E.For_testing.check ~resource ~reorder_after:0
          ~assumptions:[assumption] solver with
      | E.Unknown "cancelled" when not raise_exception -> ()
      | exception Exit when raise_exception -> ()
      | _ -> failwith "Scoped retry callback result");
      check (E.value solver x = None) "Scoped retry leaked interrupted model";
      check (E.check ~assumptions:[T.equal terms y (int terms 0)] solver
        = E.Sat) "Scoped retry retained interrupted assumption";
      check (E.value solver x = Some (E.Integer (Z.of_int 2)))
        "Scoped retry lost assertion after callback";
      E.pop solver;
      check (E.check ~assumptions:[T.equal terms x (int terms 3);
        T.equal terms y (int terms 0)] solver = E.Sat)
        "Scoped retry retained popped assertion after callback")
      [false; true]
  done;
  Printf.printf "scoped retry callbacks: %d cancellation and exceptions\n%!"
    !polls

let models () =
  let terms = T.create () in
  let solver = E.create terms in
  let xs = Array.init 80 (fun i ->
    let x = declaration terms ("m" ^ string_of_int i) T.Int in
    E.register solver x;
    E.assert_ solver (T.equal terms x (int terms i)); x) in
  check (E.check solver = E.Sat) "Model fixture";
  let queries =
    [ (fun resource -> ignore (E.model_terms ~resource solver));
      (fun resource -> ignore (E.value ~resource solver
         (builtin terms "+" T.Int (Array.to_list xs))));
      (fun resource -> ignore (E.raw_value ~resource solver xs.(0)));
      (fun resource -> ignore (E.model_term ~resource solver xs.(1))) ] in
  List.iter (fun query ->
    let resource = R.create () in
    query resource;
    let work = R.used resource in
    for budget = 0 to work - 1 do
      let resource = R.create ~budget () in
      (match query resource with
      | exception R.Exhausted "resource limit" -> ()
      | () -> ()
      | exception exn -> raise exn);
      check (E.value solver xs.(12) = Some (E.Integer (Z.of_int 12)))
        "Model changed after interrupted access";
      query (R.create ())
    done) queries;
  let resource = R.create ~cancelled:(fun () -> E.push solver; false) () in
  invalid (fun () -> E.model_terms ~resource solver);
  check (E.value solver xs.(12) = Some (E.Integer (Z.of_int 12)))
    "Reentrant model callback changed model";
  print_endline "shared model operations passed"

let arithmetic_registration () =
  let module A = Vox_smt_arithmetic in
  let coefficients = List.init 30 (fun i -> i, Z.of_int (2 * i + 1)) in
  let resource = R.create () in
  A.register ~poll:(fun () -> R.poll resource) (A.create ()) ~literal:1
    coefficients Z.zero;
  let work = R.used resource in
  for budget = 0 to work - 1 do
    let solver = A.create () in
    let resource = R.create ~budget () in
    exhausted "resource limit" (fun () ->
      A.register ~poll:(fun () -> R.poll resource) solver ~literal:1
        coefficients Z.zero);
    A.register solver ~literal:1 coefficients Z.zero;
    A.assert_literal solver 1;
    match A.check solver with
    | A.Consistent _ -> ()
    | A.Conflict _ | A.Unknown -> failwith "Interrupted registration recovery"
  done;
  Printf.printf "arithmetic registration: %d work limits\n%!" work

let scopes () =
  let terms = T.create () in
  let solver = E.create terms in
  E.assert_ solver (T.boolean terms true);
  check (E.check solver = E.Sat) "Scope fixture";
  let generation = Option.get (E.model_generation solver) in
  E.push ~count:0 solver; E.pop ~count:0 solver;
  check (E.model_is_current solver generation) "Zero scope invalidated model";
  invalid (fun () -> E.push ~count:(-1) solver);
  invalid (fun () -> E.pop ~count:(-1) solver);
  for budget = 0 to 20 do
    let resource = R.create ~budget () in
    exhausted "resource limit" (fun () -> E.push ~resource ~count:20 solver);
    invalid (fun () -> E.pop solver);
    check (E.check solver = E.Sat) "Atomic push recovery"
  done;
  E.push ~count:20 solver;
  for budget = 0 to 20 do
    let resource = R.create ~budget () in
    exhausted "resource limit" (fun () -> E.pop ~resource ~count:20 solver);
    check (E.check solver = E.Sat) "Atomic pop recovery"
  done;
  invalid (fun () -> E.pop ~count:21 solver);
  E.pop ~count:20 solver;
  invalid (fun () -> E.pop solver);
  check (E.check solver = E.Sat) "Atomic scopes final recovery"

let heuristic_counters () =
  List.iter (fun comparison_after ->
    List.iter (fun reorder_after ->
      let run resource =
        let _, solver, _, _, assumption = search_fixture () in
        let result = E.For_testing.check
          ?resource ~comparison_after ~reorder_after
            ~assumptions:[assumption] solver in
        check (result = E.Sat) "Heuristic counter fixture";
        E.statistics solver, E.resource_count solver in
      let baseline = run None in
      List.iter (fun resource ->
        check (run (Some resource) = baseline)
          "Resource changed search counters")
        [R.create (); R.create ~clock:Sys.time ~timeout:60. ()])
      [0; max_int]) [0; 32; 200000]

let comparison_activation () =
  let fixture () =
    let terms = T.create () in
    let solver = E.create terms in
    let variables = Array.init 24 (fun i ->
      declaration terms ("x" ^ string_of_int i) T.Int) in
    Array.iter (fun x ->
      E.assert_ solver (builtin terms ">=" T.Bool [x; int terms 0]);
      E.assert_ solver (builtin terms "<=" T.Bool [x; int terms 3]))
      variables;
    check (E.For_testing.check
      ~comparison_after:max_int ~reorder_after:max_int solver
      = E.Sat) "Comparison activation fixture";
    terms, solver, variables
  in
  let limit shared budget =
    if shared then Some (R.create ~budget ()), max_int else None, budget
  in
  List.iter (fun shared ->
    let entered_search budget =
      let _, solver, _ = fixture () in
      let solves = (E.statistics solver).solves in
      let resource, budget = limit shared budget in
      ignore (E.For_testing.check ?resource ~budget ~comparison_after:0
        ~reorder_after:max_int solver);
      (E.statistics solver).solves > solves
    in
    let rec upper bound =
      if entered_search bound then bound else begin
        check (bound < 1048576) "Comparison search never entered";
        upper (2 * bound)
      end
    in
    let rec first lower upper =
      if upper - lower <= 1 then upper else
      let middle = lower + (upper - lower) / 2 in
      if entered_search middle then first lower middle
      else first middle upper
    in
    let first = first 0 (upper 1) in
    List.iter (fun budget ->
      let terms, solver, variables = fixture () in
      let solves = (E.statistics solver).solves in
      let resource, budget = limit shared budget in
      check (E.For_testing.check ?resource ~budget ~comparison_after:0
        ~reorder_after:max_int solver = E.Unknown "resource limit")
        "Comparison activation did not exhaust";
      check ((E.statistics solver).solves > solves)
        "Comparison activation did not enter search";
      let probe = declaration terms "retention_probe" T.Int in
      let resource = R.create () in
      E.register ~resource solver probe;
      (* A discarded core defers registration until the next check. *)
      check (R.used resource > 1) "Comparison activation discarded the core";
      check (E.For_testing.check
        ~comparison_after:0 ~reorder_after:max_int solver = E.Sat)
        "Comparison activation retry";
      Array.iter (fun x -> match E.value solver x with
        | Some (E.Integer value) ->
          check (Z.compare value Z.zero >= 0
            && Z.compare value (Z.of_int 3) <= 0) "Comparison retry model"
        | _ -> failwith "Comparison retry model unavailable") variables;
      let impossible = builtin terms ">=" T.Bool [variables.(0); int terms 4] in
      (match E.check ~assumptions:[impossible] solver with
      | E.Unsat [core] -> check (core = impossible) "Comparison retry core"
      | _ -> failwith "Comparison retry assumptions");
      E.push solver;
      E.assert_ solver impossible;
      check (E.check solver = E.Unsat []) "Comparison retry scope";
      E.pop solver;
      check (E.check solver = E.Sat) "Comparison retry pop")
      [first + 128; first + 256])
    [false; true]

let () =
  comparison_activation ();
  heuristic_counters ();
  primitive ();
  hierarchy ();
  atomic_assertions ();
  checks false;
  checks true;
  checks ~scoped:true true;
  scoped_retry_callbacks ();
  models ();
  scopes ();
  arithmetic_registration ();
  print_endline "Operation resource tests passed"
