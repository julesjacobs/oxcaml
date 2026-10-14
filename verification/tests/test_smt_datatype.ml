module T = Vox_smt_term
module E = Vox_smt_engine
module Z = Vox_smt_integer.Z

let variable terms name sort =
  let symbol = T.symbol terms
      { name; arguments = [||]; result = sort; kind = T.Uninterpreted } in
  T.apply terms symbol [||]

let constructor terms datatype name fields =
  let symbol = T.symbol terms
      { name;
        arguments = Array.of_list fields;
        result = T.Named datatype;
        kind = T.Constructor datatype
      } in
  List.iteri
    (fun index result ->
      ignore (T.symbol terms
                { name = name ^ "_" ^ string_of_int index;
                  arguments = [|T.Named datatype|];
                  result;
                  kind = T.Selector (symbol, index)
                }))
    fields;
  symbol

let select terms constructor index term =
  T.apply terms (T.selector terms constructor index) [|term|]

let is terms constructor term =
  T.apply terms (T.tester terms constructor) [|term|]

let expect solver expected =
  match E.check ~budget:1_000_000 solver, expected with
  | E.Sat, true | E.Unsat _, false -> ()
  | E.Unknown why, _ -> failwith ("Unexpected unknown: " ^ why)
  | E.Sat, false -> failwith "Expected unsat"
  | E.Unsat _, true -> failwith "Expected sat"

let distinct terms solver values =
  List.iteri
    (fun i value ->
      List.iteri
        (fun j other ->
          if i < j
          then E.assert_ solver (T.not_ terms (T.equal terms value other)))
        values)
    values

let constructors_and_selectors () =
  let terms = T.create () in
  let a = constructor terms 0 "A" [T.Bool] in
  let b = constructor terms 0 "B" [T.Bool] in
  let truth = T.boolean terms true and falsehood = T.boolean terms false in
  let at = T.apply terms a [|truth|] in
  let af = T.apply terms a [|falsehood|] in
  let bt = T.apply terms b [|truth|] in
  let solver = E.create terms in
  E.push solver;
  E.assert_ solver (T.equal terms at af);
  expect solver false;
  E.pop solver;
  E.push solver;
  E.assert_ solver (T.equal terms at bt);
  expect solver false;
  E.pop solver;
  E.push solver;
  E.assert_ solver (T.not_ terms (select terms a 0 at));
  expect solver false;
  E.pop solver;
  E.push solver;
  E.assert_ solver (T.not_ terms (select terms b 0 at));
  expect solver true;
  E.pop solver;
  let x = variable terms "x" (T.Named 0) in
  E.assert_ solver (is terms a x);
  E.assert_ solver (T.not_ terms (select terms a 0 x));
  E.assert_ solver (select terms b 0 x);
  expect solver true;
  E.push solver;
  E.assert_ solver (T.not_ terms (is terms a x));
  expect solver false;
  E.pop solver;
  expect solver true

let finite_product count expected =
  let terms = T.create () in
  ignore (constructor terms 0 "Pair" [T.Bool; T.Bool]);
  let solver = E.create terms in
  let values = List.init count
      (fun i -> variable terms ("p" ^ string_of_int i) (T.Named 0)) in
  distinct terms solver values;
  expect solver expected

let explained_constructors () =
  List.iter (fun field ->
    let terms = T.create () in
    let box = constructor terms 0 "Box" [field] in
    let empty = constructor terms 0 "Empty" [] in
    let a = variable terms "a" field in
    let b = variable terms "b" field in
    let x = variable terms "x" (T.Named 0) in
    let y = variable terms "y" (T.Named 0) in
    let guard = variable terms "guard" T.Bool in
    let solver = E.create terms in
    E.assert_ solver (T.or_ terms [|T.not_ terms guard;
      T.equal terms x (T.apply terms box [|a|])|]);
    E.assert_ solver (T.equal terms y (T.apply terms box [|b|]));
    let check assumptions expected =
      match E.check ~assumptions ~budget:1_000_000 solver, expected with
      | E.Sat, true | E.Unsat _, false -> ()
      | E.Unknown why, _ -> failwith ("Constructor explanation: " ^ why)
      | _ -> failwith "Invalid constructor explanation"
    in
    let same = T.equal terms x y in
    let different = T.not_ terms (T.equal terms a b) in
    let unequal_field = T.not_ terms
        (T.equal terms (select terms box 0 x) a) in
    let other_constructor = T.equal terms x (T.apply terms empty [||]) in
    for _ = 1 to 3 do
      check [guard; same; different] false;
      check [T.not_ terms guard; same; different] true;
      check [guard; T.not_ terms same; different] true;
      check [guard; same; T.not_ terms different] true;
      List.iter (fun contradicted ->
        check [guard; contradicted] false;
        check [T.not_ terms guard; contradicted] true)
        [unequal_field; T.not_ terms (is terms box x); other_constructor];
      E.push solver;
      E.assert_ solver guard;
      check [same; different] false;
      E.pop solver;
      check [T.not_ terms guard; same; different] true
    done)
    [T.Bool; T.Int; T.Bitvec 4; T.Named 1]

let finite_enum () =
  let terms = T.create () in
  let constructors = List.map (fun name -> constructor terms 0 name [])
      ["Red"; "Green"; "Blue"] in
  let solver = E.create terms in
  let x = variable terms "color" (T.Named 0) in
  List.iter (fun constructor ->
      E.assert_ solver (T.not_ terms (is terms constructor x))) constructors;
  expect solver false

let list_bool count expected =
  let terms = T.create () in
  let nil = constructor terms 0 "Nil" [] in
  let cons = constructor terms 0 "Cons" [T.Bool; T.Named 0] in
  let empty = T.apply terms nil [||] in
  let solver = E.create terms in
  let values = List.init count
      (fun i -> variable terms ("xs" ^ string_of_int i) (T.Named 0)) in
  List.iter
    (fun value ->
      E.assert_ solver (is terms cons value);
      E.assert_ solver (T.equal terms (select terms cons 1 value) empty))
    values;
  distinct terms solver values;
  expect solver expected

let recursive_cycles () =
  let terms = T.create () in
  ignore (constructor terms 0 "Nil" []);
  let cons = constructor terms 0 "Cons" [T.Bool; T.Named 0] in
  let solver = E.create terms in
  let x = variable terms "x" (T.Named 0) in
  let y = variable terms "y" (T.Named 0) in
  let truth = T.boolean terms true in
  E.push solver;
  E.assert_ solver (T.equal terms x (T.apply terms cons [|truth; x|]));
  expect solver false;
  E.pop solver;
  E.push solver;
  E.assert_ solver (T.equal terms x (T.apply terms cons [|truth; y|]));
  E.assert_ solver (T.equal terms y (T.apply terms cons [|truth; x|]));
  expect solver false;
  E.pop solver;
  E.assert_ solver (is terms cons x);
  E.assert_ solver (is terms cons y);
  E.assert_ solver (T.not_ terms (T.equal terms x y));
  expect solver true;
  if T.count terms > 500 then failwith "unbounded recursive expansion"

let mutual_cycle () =
  let terms = T.create () in
  ignore (constructor terms 0 "Zero" []);
  let a = constructor terms 0 "A" [T.Named 1] in
  let b = constructor terms 1 "B" [T.Named 0] in
  let x = variable terms "x" (T.Named 0) in
  let solver = E.create terms in
  let cycle = T.apply terms a [|T.apply terms b [|x|]|] in
  E.assert_ solver (T.equal terms x cycle);
  expect solver false

let integer_fields () =
  let terms = T.create () in
  let box = constructor terms 0 "Box" [T.Int] in
  let x = variable terms "x" (T.Named 0) in
  let solver = E.create terms in
  let integer n = T.integer terms (Z.of_int n) in
  E.assert_ solver (T.equal terms x (T.apply terms box [|integer 12|]));
  E.assert_ solver
    (T.builtin terms "<" T.Bool [|select terms box 0 x; integer 10|]);
  expect solver false

let nested_fixture terms depth fields =
  let record = constructor terms 1 "Record" fields in
  let shells = Array.init depth (fun index ->
      constructor terms (index + 2) ("Shell" ^ string_of_int index)
        [T.Named (index + 1); T.Named 0]) in
  let x = variable terms "x" (T.Named (depth + 1)) in
  let subject = ref x in
  for index = depth - 1 downto 0 do
    subject := select terms shells.(index) 0 !subject
  done;
  let wrap = constructor terms (depth + 2) "Wrap" [T.Named 1] in
  ignore (constructor terms (depth + 2) "Nil" []);
  ignore (constructor terms (depth + 2) "Other" []);
  let f = T.symbol terms
      { name = "f"; arguments = [|T.Named 1|];
        result = T.Named (depth + 2); kind = T.Uninterpreted } in
  let apply argument = T.apply terms f [|argument|] in
  let result = T.apply terms wrap [|variable terms "y" (T.Named 1)|] in
  record, !subject, apply, result

let nested_reconstruction () =
  let random = Random.State.make [|0x713; 0x128|] in
  for trial = 0 to 199 do
    let terms = T.create () in
    let depth = 1 + Random.State.int random 5 in
    let fields = T.Named 0 :: List.init (Random.State.int random 4)
        (fun _ -> match Random.State.int random 3 with
          | 0 -> T.Bool | 1 -> T.Int | _ -> T.Bitvec 3) in
    let record, subject, apply, result = nested_fixture terms depth fields in
    let selections = Array.init (List.length fields)
        (fun index -> select terms record index subject) in
    let reconstructed = T.apply terms record selections in
    let solver = E.create terms in
    let constraints =
      [T.equal terms (apply reconstructed) result;
       T.not_ terms (T.equal terms (apply subject) result)] in
    E.push solver;
    List.iter (E.assert_ solver)
      (if Random.State.bool random then constraints else List.rev constraints);
    (try expect solver false with Failure message ->
      failwith (Printf.sprintf "nested trial %d: %s" trial message));
    E.pop solver;
    expect solver true;
    let changed = Array.copy selections in
    changed.(0) <- variable terms "different" (T.Named 0);
    E.push solver;
    E.assert_ solver (T.not_ terms (T.equal terms changed.(0) selections.(0)));
    E.assert_ solver
      (T.equal terms (apply (T.apply terms record changed)) result);
    E.assert_ solver (T.not_ terms (T.equal terms (apply subject) result));
    expect solver true;
    E.pop solver;
    expect solver true
  done

exception Interrupted

let interruption_fixture fixture =
  let run limit =
    let module D = Vox_smt_datatype in
    let terms = T.create () in
    let root = fixture terms in
    let remaining = ref limit and polls = ref 0 in
    let poll () =
      incr polls;
      match !remaining with
      | None -> ()
      | Some 0 -> raise Interrupted
      | Some count -> remaining := Some (count - 1)
    in
    let clauses = ref [] in
    let datatype = ref None in
    let register term = D.register (Option.get !datatype) term in
    let rec internalize term =
      poll ();
      register term;
      match T.node terms term with
      | T.Apply (_, arguments) -> Array.iter internalize arguments
      | T.Boolean _ | T.Integer _ | T.Bits _ -> ()
    in
    let boolean term = poll (); register term; term + 1 in
    let clause literals =
      poll ();
      clauses := Array.to_list literals :: !clauses
    in
    let solver = D.create ~terms ~boolean ~clause ~internalize ~poll () in
    datatype := Some solver;
    let rec finish attempts =
      if attempts = 0 then failwith "datatype recovery did not terminate";
      register root;
      match D.check solver ~representative:Fun.id
              ~assigned:(fun _ -> Some true) with
      | D.Done -> ()
      | D.Continue -> finish (attempts - 1)
      | D.Incomplete why -> failwith why
    in
    (try finish 20 with Interrupted -> ());
    remaining := None;
    finish 20;
    let clauses = List.map (fun clause ->
        List.map (fun literal ->
            (if literal < 0 then "-" else "+")
            ^ T.to_string terms (abs literal - 1)) clause
        |> List.sort String.compare) !clauses in
    List.sort_uniq compare clauses, !polls
  in
  let expected, polls = run None in
  for limit = 0 to polls do
    if fst (run (Some limit)) <> expected
    then failwith ("datatype recovery at poll " ^ string_of_int limit)
  done

let interruption_recovery () =
  interruption_fixture (fun terms ->
      ignore (constructor terms 0 "Pair" [T.Bool; T.Bool]);
      ignore (constructor terms 0 "Empty" []);
      variable terms "x" (T.Named 0));
  interruption_fixture (fun terms ->
      let record, subject, apply, result =
        nested_fixture terms 2 [T.Named 0; T.Bool] in
      let reconstructed = T.apply terms record
          [|select terms record 0 subject; select terms record 1 subject|] in
      T.equal terms (apply reconstructed) result)

let shared_sort_hierarchy () =
  let terms = T.create () in
  let leaf = constructor terms 0 "Leaf" [] in
  let value = ref (T.apply terms leaf [||]) in
  for depth = 1 to 35 do
    let field = T.Named (depth - 1) in
    let symbol = constructor terms depth ("Pair" ^ string_of_int depth)
        [field; field] in
    value := T.apply terms symbol [|!value; !value|]
  done;
  let polls = ref 0 in
  let datatypes = Vox_smt_datatype.create ~terms
      ~boolean:(fun term -> term + 1) ~clause:(fun _ -> ())
      ~internalize:(fun _ -> ()) ~poll:(fun () ->
        incr polls;
        if !polls > 10000 then failwith "Repeated finite sort traversal") () in
  Vox_smt_datatype.register datatypes !value

let finite_sort_changes () =
  let module D = Vox_smt_datatype in
  let terms = T.create () in
  let wrap = constructor terms 0 "Wrap" [T.Named 1] in
  let branch = constructor terms 1 "Branch" [T.Named 2] in
  let subjects = Array.init 4 (fun index ->
      variable terms ("x" ^ string_of_int index) (T.Named 1)) in
  let roots = Array.map (fun subject ->
      let root = T.apply terms wrap [|subject|] in
      ignore (is terms wrap root);
      ignore (is terms branch subject);
      root) subjects in
  let tested = ref [] and expected = ref false and datatype = ref None in
  let boolean term =
    (match T.node terms term with
    | T.Apply (symbol, [|subject|]) -> (
      match (T.signature terms symbol).kind with
      | T.Tester _ -> tested := subject :: !tested
      | _ -> ())
    | _ -> ());
    term + 1 in
  let internalize subject =
    D.register (Option.get !datatype) subject;
    if List.mem subject !tested <> !expected then
      failwith "Generated datatype demand used stale sort finiteness" in
  let solver = D.create ~terms ~boolean ~internalize
      ~clause:(fun _ -> ()) () in
  datatype := Some solver;
  D.enable_eager solver;
  let register index finite =
    tested := [];
    expected := finite;
    D.register solver roots.(index) in
  register 0 false;
  ignore (constructor terms 2 "Leaf" [T.Bool]);
  register 1 true;
  T.with_temporary terms (fun () ->
    ignore (constructor terms 2 "Recursive" [T.Named 2]);
    register 2 false);
  register 3 true

let sparse_term_ids () =
  let terms = T.create () in
  let a = constructor terms 0 "A" [] in
  let b = constructor terms 0 "B" [] in
  let values = Array.init 32 (fun index ->
      while T.count terms < index * 256 do
        ignore (T.integer terms (Z.of_int (T.count terms)))
      done;
      let value = variable terms ("x" ^ string_of_int index) (T.Named 0) in
      ignore (is terms a value);
      ignore (is terms b value);
      value) in
  let solver = E.create terms in
  Array.iteri (fun index value ->
    E.assert_ solver (is terms (if index mod 2 = 0 then a else b) value))
    values;
  expect solver true;
  List.iter (fun contradiction ->
    E.push solver;
    E.assert_ solver contradiction;
    expect solver false;
    E.pop solver;
    expect solver true)
    [T.equal terms values.(0) values.(1);
     T.not_ terms (T.equal terms values.(0) values.(2))]

let finite_enum_cardinality () =
  let module D = Vox_smt_datatype in
  let require condition message = if not condition then failwith message in
  let propagate datatypes = D.propagate datatypes ~classes:[]
      ~members:(fun _ -> []) ~constructor_members:(fun _ -> [])
      ~uses:(fun _ -> [])
      ~assigned:(fun _ -> None) ~explain:(fun _ _ () -> [||])
      ~imply:(fun _ ~reason:_ -> ()) ~conflict:(fun ~reason:_ -> ())
      ~equate:(fun _ _ ~reason:_ -> ()) in
  for cardinal = 1 to 4 do
    let terms = T.create () in
    for index = 0 to cardinal - 1 do
      ignore (constructor terms 0 ("C" ^ string_of_int index) [])
    done;
    let values = Array.init (cardinal + 1) (fun index ->
      variable terms ("x" ^ string_of_int index) (T.Named 0)) in
    let edges = ref [] and next = ref 1 in
    Array.iteri (fun left _ ->
      for right = left + 1 to cardinal do
        let literal = if !next mod 2 = 0 then - !next else !next in
        edges := (left, right, literal) :: !edges;
        incr next
      done) values;
    let expected = List.sort Int.compare
        (List.map (fun (_, _, literal) -> literal) !edges) in
    let exercise ?limit edges =
      let clauses = ref [] and count = ref 0 and limit = ref limit in
      let poll () =
        let index = !count in incr count;
        if !limit = Some index then raise Exit in
      let datatypes = D.create ~terms ~poll
          ~boolean:(fun _ -> failwith "Cardinality generated a Boolean")
          ~internalize:(fun _ -> failwith "Cardinality internalized a term")
          ~clause:(fun clause ->
            poll (); clauses := Array.to_list clause :: !clauses; poll ())
          () in
      let register () = List.iter (fun (left, right, literal) ->
        D.register_equality datatypes ~literal values.(left) values.(right))
          edges in
      (try register (); propagate datatypes with Exit -> ());
      let polls = !count in
      limit := None;
      register (); propagate datatypes; propagate datatypes;
      List.sort_uniq compare !clauses, polls in
    let clauses, polls = exercise !edges in
    require (clauses = [expected]) "Missing finite enum cardinality clause";
    List.iter (fun omitted ->
      require (fst (exercise (List.filter ((<>) omitted) !edges)) = [])
        "Finite enum clause without a complete graph") !edges;
    for limit = 0 to polls - 1 do
      require (fst (exercise ~limit !edges) = clauses)
        "Interrupted finite enum cardinality clause"
    done;
    let inequalities = List.map (fun (left, right, _) ->
      T.not_ terms (T.equal terms values.(left) values.(right))) !edges in
    let solver = E.create terms in
    Array.iter (E.register solver) values;
    E.push solver;
    List.iter (E.assert_ solver) inequalities;
    expect solver false;
    E.pop solver;
    expect solver true;
    List.iter (fun omitted ->
      E.push solver;
      List.iter (E.assert_ solver)
        (List.filter ((<>) omitted) inequalities);
      expect solver true;
      E.pop solver) inequalities
  done

let () =
  let tests =
    [ "constructors", constructors_and_selectors;
      "pair-four", (fun () -> finite_product 4 true);
      "pair-five", (fun () -> finite_product 5 false);
      "enum", finite_enum;
      "enum-cardinality", finite_enum_cardinality;
      "explanations", explained_constructors;
      "list-two", (fun () -> list_bool 2 true);
      "list-three", (fun () -> list_bool 3 false);
      "cycles", recursive_cycles;
      "mutual", mutual_cycle;
      "integer", integer_fields;
      "nested", nested_reconstruction;
      "shared-sort", shared_sort_hierarchy;
      "finite-sort-changes", finite_sort_changes;
      "sparse-term-ids", sparse_term_ids;
      "interruption", interruption_recovery
    ]
  in
  List.iter
    (fun (name, test) ->
      if Array.length Sys.argv = 1 || Sys.argv.(1) = name
      then
        try test ()
        with Failure message -> failwith (name ^ ": " ^ message))
    tests;
  print_endline "Datatype integration tests passed"
