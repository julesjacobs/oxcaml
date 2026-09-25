(* TEST
 include ocamlcommon;
 native;
*)

let domains = [|2; 3; 5|]
let random = Random.State.make [|0x74; 0x19|]
let choose n = Random.State.int random n

let assignments domains visit =
  let values = Array.make (Array.length domains) 0 in
  let rec loop index =
    if index = Array.length domains then visit values
    else
      for value = 0 to domains.(index) - 1 do
        values.(index) <- value;
        loop (index + 1)
      done
  in
  loop 0

let evaluates manager node values =
  let result = ref node in
  Array.iteri
    (fun var value -> result := Solver_mdd.restrict manager var value !result)
    values;
  not (Solver_mdd.is_false !result)

let () =
  for _case = 1 to 200 do
    let manager = Solver_mdd.create ~bit_order:[|[|2|]; [|0; 4|]; [|3; 1; 5|]|] domains in
    let atom () =
      let table = Array.init 2 (fun _ -> Array.init 3 (fun _ -> choose 2 = 0)) in
      let node =
        Solver_mdd.mk manager 0
          (Array.map
             (fun row -> Solver_mdd.mk manager 1
                 (Array.map (fun b -> if b then Solver_mdd.true_ else Solver_mdd.false_) row))
             table)
      in
      node, (fun values -> table.(values.(0)).(values.(1)))
    in
    let left, left_eval = atom () in
    let right, right_eval = atom () in
    let tail = Array.init 5 (fun _ -> choose 2 = 0) in
    let tail_node = Solver_mdd.mk manager 2
        (Array.map (fun b -> if b then Solver_mdd.true_ else Solver_mdd.false_) tail)
    in
    let node = Solver_mdd.or_ manager
        (Solver_mdd.and_ manager left (Solver_mdd.not_ manager right)) tail_node
    in
    let eval values = (left_eval values && not (right_eval values)) || tail.(values.(2)) in
    let combined = Solver_mdd.and_many manager [left; right; tail_node; left; Solver_mdd.true_] in
    assignments domains (fun values ->
        assert (evaluates manager combined values =
          (left_eval values && right_eval values && tail.(values.(2)))));
    let implication = ref true in
    assignments domains (fun values ->
        implication := !implication && (not (eval values) || left_eval values));
    assert (Solver_mdd.entails manager node left = !implication);
    (match Solver_mdd.counterexample manager node left with
    | None -> assert !implication
    | Some values -> assert (eval values && not (left_eval values)));
    assert (Solver_mdd.entails manager Solver_mdd.false_ node);
    assert (Solver_mdd.entails manager node Solver_mdd.true_);
    assignments domains (fun values ->
        assert (evaluates manager node values = eval values));
    for var = 0 to 2 do
      let ex = Solver_mdd.exists manager var node in
      let all = Solver_mdd.forall manager var node in
      assignments domains (fun values ->
          let input = Array.copy values in
          let some = ref false and every = ref true in
          for value = 0 to domains.(var) - 1 do
            input.(var) <- value;
            some := !some || eval input;
            every := !every && eval input
          done;
          assert (evaluates manager ex values = !some);
          assert (evaluates manager all values = !every))
    done;
    let ex = Solver_mdd.and_exists manager [0; 2] left node in
    let all = Solver_mdd.implies_forall manager [0; 2] left node in
    assignments domains (fun values ->
        let input = Array.copy values in
        let some = ref false and every = ref true in
        for x = 0 to domains.(0) - 1 do
          for z = 0 to domains.(2) - 1 do
            input.(0) <- x;
            input.(2) <- z;
            some := !some || (left_eval input && eval input);
            every := !every && (not (left_eval input) || eval input)
          done
        done;
        assert (evaluates manager ex values = !some);
        assert (evaluates manager all values = !every));
    let guarded_domain = Solver_mdd.and_ manager node left in
    let guarded_witness = Solver_mdd.and_ manager right tail_node in
    List.iter (fun selected ->
        let domain_ex, winning_ex = Solver_mdd.eliminate_guarded manager
            ~universal:false selected ~domain:guarded_domain ~witness:guarded_witness in
        let domain_all, winning_all = Solver_mdd.eliminate_guarded manager
            ~universal:true selected ~domain:guarded_domain ~witness:guarded_witness in
        assignments domains (fun values ->
            let input = Array.copy values in
            let inhabited = ref false and some = ref false and every = ref true in
            let rec quantify = function
              | [] ->
                let domain = eval input && left_eval input in
                let witness = right_eval input && tail.(input.(2)) in
                inhabited := !inhabited || domain;
                some := !some || (domain && witness);
                every := !every && (not domain || witness)
              | var :: rest ->
                for value = 0 to domains.(var) - 1 do
                  input.(var) <- value;
                  quantify rest
                done in
            quantify selected;
            assert (evaluates manager domain_ex values = !inhabited);
            assert (evaluates manager domain_all values = !inhabited);
            assert (evaluates manager winning_ex values = !some);
            assert (evaluates manager winning_all values = (!inhabited && !every))))
      [[]; [0]; [1]; [2]; [0; 2]; [0; 1; 2]];
    let target = Solver_mdd.create ~bit_order:[|[|0; 2; 1|]; [||]; [|4|]; [|3; 1|]|] [|5; 1; 2; 3|] in
    let copied = Solver_mdd.import target ~old_manager:manager ~old_to_new:[|2; 3; 0|] node in
    assignments [|5; 1; 2; 3|] (fun values ->
        assert (evaluates target copied values = eval [|values.(2); values.(3); values.(0)|]));
    let support = Solver_mdd.support manager [node] in
    assignments domains (fun values ->
        Array.iteri (fun var present -> if not present then
            for value = 0 to domains.(var) - 1 do
              let other = Array.copy values in
              other.(var) <- value;
              assert (eval values = eval other)
            done) support);
    for fixed = 0 to 2 do
      let reduced = Solver_mdd.create [|5; 2|] in
      let imported = Solver_mdd.import ~fixed:[|None; Some fixed; None|]
          reduced ~old_manager:manager ~old_to_new:[|1; -1; 0|] node in
      assignments [|5; 2|] (fun values ->
          assert (evaluates reduced imported values =
            eval [|values.(1); fixed; values.(0)|]))
    done;
    let factored = Solver_mdd.factor manager node in
    assignments domains (fun values ->
        assert (evaluates manager factored values = eval values));
    assert (Solver_mdd.entails manager node factored);
    assert (Solver_mdd.entails manager factored node);
    let projected = Solver_mdd.exists manager 0 node in
    let smaller = Solver_mdd.create [|5; 3|] in
    let imported = Solver_mdd.import smaller ~old_manager:manager
        ~old_to_new:[|-1; 1; 0|] projected in
    assignments [|5; 3|] (fun values ->
        assert (evaluates smaller imported values =
          (eval [|0; values.(1); values.(0)|] || eval [|1; values.(1); values.(0)|])));
    (match Solver_mdd.find_sat ~prefer_high:true manager node with
    | Some values -> assert (eval values)
    | None -> assignments domains (fun values -> assert (not (eval values))));
    (match Solver_mdd.find_sat manager node with
    | Some values -> assert (eval values)
    | None -> assignments domains (fun values -> assert (not (eval values))))
  done;
  let manager = Solver_mdd.create [|3; 3|] in
  let relation cmp = Solver_mdd.mk manager 0
      (Array.init 3 (fun r -> Solver_mdd.mk manager 1
          (Array.init 3 (fun a -> if cmp r a then Solver_mdd.true_ else Solver_mdd.false_))))
  in
  let witness = Solver_mdd.and_ manager (relation ( <= )) (relation ( >= )) in
  assert (not (Solver_mdd.is_false (Solver_mdd.forall manager 0 (Solver_mdd.exists manager 1 witness))));
  assert (Solver_mdd.is_false (Solver_mdd.exists manager 1 (Solver_mdd.forall manager 0 witness)));
  Solver_mdd.compact manager [Solver_mdd.true_];
  let recreated = relation ( = ) in
  assert (Solver_mdd.entails manager witness recreated);
  assert (Solver_mdd.entails manager recreated witness);
  assignments [|3; 3|] (fun values ->
      assert (evaluates manager witness values = evaluates manager recreated values));
  assert (Solver_mdd.is_false
      (Solver_mdd.and_ manager witness (Solver_mdd.not_ manager recreated)));
  Solver_mdd.For_testing.with_node_limit 5000 (fun () ->
      let manager = Solver_mdd.create (Array.make 40 4) in
      let relation = ref Solver_mdd.true_ in
      for pair = 0 to 19 do
        let bound = Solver_mdd.mk manager (2 * pair)
            (Array.init 4 (fun x -> Solver_mdd.mk manager (2 * pair + 1)
                (Array.init 4 (fun y -> if x <= y then Solver_mdd.true_ else Solver_mdd.false_)))) in
        relation := Solver_mdd.and_ manager !relation bound
      done;
      assert (Solver_mdd.is_false (Solver_mdd.not_ manager
          (Solver_mdd.exists_many manager (List.init 20 (fun i -> 2 * i + 1)) !relation))));
  Solver_mdd.For_testing.with_node_limit 5000 (fun () ->
      let manager = Solver_mdd.create (Array.make 40 2) in
      let equal i j = Solver_mdd.mk manager i
          (Array.init 2 (fun x -> Solver_mdd.mk manager j
              (Array.init 2 (fun y -> if x = y then Solver_mdd.true_ else Solver_mdd.false_)))) in
      let relation = Solver_mdd.and_many manager
          (List.init 20 (fun i -> equal i (20 + i))) in
      assert (Option.is_some (Solver_mdd.find_sat manager relation));
      assert (Solver_mdd.entails manager relation (equal 0 20));
      let one i = Solver_mdd.mk manager i [|Solver_mdd.false_; Solver_mdd.true_|] in
      let fixed = Solver_mdd.and_many manager
          (relation :: List.init 20 (fun i -> one (20 + i))) in
      (match Solver_mdd.find_sat manager fixed with
      | None -> assert false
      | Some values -> assert (Array.for_all (( = ) 1) values));
      assert (Solver_mdd.entails manager fixed (one 0));
      assert (Solver_mdd.is_false (Solver_mdd.and_ manager fixed
          (Solver_mdd.not_ manager (one 0))));
      let projected = Solver_mdd.exists_many manager (List.init 20 (fun i -> 20 + i)) relation in
      assert (Solver_mdd.entails manager Solver_mdd.true_ projected);
      let fixed = Solver_mdd.forall manager 0 relation in
      assert (Solver_mdd.is_false fixed));
  let manager = Solver_mdd.create [|2; 2|] in
  let zero var = Solver_mdd.mk manager var [|Solver_mdd.true_; Solver_mdd.false_|] in
  let bound = zero 0 in
  let relation = Solver_mdd.and_ manager bound (zero 1) in
  let found = Option.get (Solver_mdd.find_sat manager relation) in
  found.(0) <- 1;
  assert (Solver_mdd.entails manager relation bound);
  let other = Option.get (Solver_mdd.counterexample manager relation Solver_mdd.false_) in
  other.(0) <- 1;
  assert (Solver_mdd.entails manager relation bound);
  Solver_mdd.compact manager [relation];
  assert (Solver_mdd.entails manager relation bound);
  print_endline "symbolic oracle passed"
