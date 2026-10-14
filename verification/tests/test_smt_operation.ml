module F = Vox_smt_frontend
module S = Vox_smt_sexp
module R = Vox_smt_resource
module T = Vox_smt_term
module E = Vox_smt_engine
module M = Vox_smt_model
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message

let command source = match S.parse source with
  | [command] -> command
  | _ -> failwith "Expected one command"

let run frontend source = F.run frontend source
let setup source =
  let frontend = F.create () in
  ignore (run frontend source);
  frontend

let rejected frontend source =
  match run frontend source with
  | exception F.Error _ -> ()
  | _ -> failwith ("Accepted: " ^ source)

let atomic name initial source unchanged after =
  let expression = command source in
  let frontend = setup initial in
  let resource = R.create () in
  ignore (F.execute ~resource frontend expression);
  after frontend;
  let used = R.used resource in
  for budget = 0 to used - 1 do
    let frontend = setup initial in
    let resource = R.create ~budget () in
    (match F.execute ~resource frontend expression with
    | exception R.Exhausted _ -> ()
    | _ -> failwith (Printf.sprintf "%s budget %d did not stop" name budget));
    unchanged frontend;
    ignore (F.execute frontend expression);
    after frontend
  done;
  Printf.printf "%s: %d interrupted budgets recovered\n%!" name used;
  let frontend = setup initial in
  let callbacks = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
  ignore (F.execute ~resource frontend expression);
  let count = !callbacks in
  List.iter (fun raises ->
    for stop = 1 to count do
      let frontend = setup initial in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | exception Exit when raises -> ()
      | exception R.Exhausted "cancelled" when not raises -> ()
      | _ -> failwith (name ^ " callback did not interrupt"));
      unchanged frontend;
      ignore (F.execute frontend expression);
      after frontend
    done) [false; true];
  Printf.printf "%s: %d cancellation/exception positions recovered\n%!"
    name (2 * count)

let uninterpreted_sort_commands () =
  atomic "sort application"
    "(declare-sort S 2)(declare-sort T 1)"
    "(declare-const x (S (T Int) (T Bool)))"
    (fun frontend -> rejected frontend "(assert (= x x))")
    (fun frontend ->
      check (run frontend "(assert (= x x))(check-sat)" = ["sat"])
        "Sort application recovery");
  atomic "sort application in datatype"
    {|(declare-sort S 1)
      (declare-datatype Box (par (X) ((box (val (S X))))))|}
    "(declare-const x (Box Int))"
    (fun frontend -> rejected frontend "(assert (= x x))")
    (fun frontend ->
      check (run frontend "(assert (= x (box (val x))))(check-sat)"
        = ["sat"]) "Datatype sort application recovery");
  atomic "sort application in definition"
    "(declare-sort S 1)"
    "(define-fun f ((x (S Int))) (S Int) x)"
    (fun frontend -> rejected frontend "(assert (= f f))")
    (fun frontend ->
      check (run frontend
        "(declare-const x (S Int))(assert (distinct x (f x)))(check-sat)"
        = ["unsat"]) "Definition sort application recovery");
  let frontend = setup "(declare-sort S 1)" in
  rejected frontend "(declare-const x S)";
  rejected frontend "(declare-const x (S Int Bool))";
  ignore (run frontend "(declare-const x (S Int))");
  rejected frontend "(declare-sort S 2)";
  rejected frontend "(declare-datatype S ((unit)))";
  rejected frontend "(define-sort S () Int)";
  ignore (run frontend "(declare-const y (S Bool))");
  rejected frontend "(assert (= x y))";
  rejected frontend "(assert (= (match x ((unit true))) true))";
  check (run frontend "(check-sat)" = ["sat"]) "Rejected command state"

let atomic_commands () =
  let initial = "(declare-const x Int)(assert (= x 7))" in
  atomic "assert" initial "(assert (! (= (* x x) 50) :named bad))"
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"]) "Assertion persisted";
      check (run frontend "(get-value (x))" = ["((x 7))"])
        "Assertion changed model")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"]) "Missing assertion");
  atomic "push" initial "(push 32)"
    (fun frontend -> rejected frontend "(pop 1)")
    (fun frontend ->
      ignore (run frontend "(pop 32)");
      rejected frontend "(pop 1)");
  atomic "pop" (initial ^ "(push 32)(assert false)") "(pop 32)"
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"]) "Partial pop")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"]) "Pop failed";
      rejected frontend "(pop 1)");
  atomic "datatype" ""
    "(declare-datatype Tree ((leaf (value Int))\
     (node (left Tree) (right Tree))))"
    (fun frontend -> rejected frontend "(declare-const x Tree)")
    (fun frontend ->
      check (run frontend
        "(declare-const x Tree)(assert (= x (leaf 7)))\
         (check-sat)(get-value ((value x)))" = ["sat"; "(((value x) 7))"])
        "Datatype recovery");
  atomic "polymorphic instance"
    "(declare-datatypes ((Tree 1))\
     ((par (A) ((leaf (value A)) (node (left (Tree A)) (right (Tree A)))))))"
    "(declare-const x (Tree Int))"
    (fun frontend -> rejected frontend "(get-value (x))")
    (fun frontend ->
      check (run frontend
        "(assert (= x (leaf 7)))(check-sat)(get-value ((value x)))"
        = ["sat"; "(((value x) 7))"])
        "Polymorphic instance recovery");
  atomic "reset assertions"
    "(set-option :global-declarations true)(declare-const x Int)\
     (declare-const y Int)(assert false)"
    "(reset-assertions)"
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"]) "Partial reset")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"]) "Reset recovery")

let numeric_alias_commands () =
  List.iter (fun count ->
    let names = List.init count (fun index -> "p" ^ string_of_int index) in
    List.iter (fun (sort, predicate) ->
      let initial = "(declare-const a Bool)(declare-const x " ^ sort ^ ")"
        ^ String.concat "" (List.map (fun name ->
            "(declare-const " ^ name ^ " Bool)") names) in
      let source = "(assert (= a (and " ^ String.concat " " names
        ^ " " ^ predicate ^ ")))" in
      let contradict = "(push 1)(assert a)(assert (not " ^ predicate
        ^ "))(check-sat)(pop 1)(check-sat)" in
      let name = Printf.sprintf "Boolean alias %s/%d" sort (count + 1) in
      atomic name initial source
        (fun frontend ->
          check (run frontend contradict = ["sat"; "sat"])
            "Interrupted alias persisted")
        (fun frontend ->
          check (run frontend contradict = ["unsat"; "sat"])
            "Numeric alias scope recovery";
          check (run frontend
            "(check-sat-assuming (a (not p0)))(check-sat)"
            = ["unsat"; "sat"]) "Numeric alias assumption recovery"))
      ["Int", "(< x 3)"; "(_ BitVec 8)", "(bvult x #x03)"])
    [63; 64]

let divisibility_commands () =
  atomic "conditional divisibility"
    "(declare-const x Int)(declare-const y Int)(declare-const p Bool)"
    {|(assert (distinct
      (mod (+ (* y (ite p (* 4 x) 0)) (* x (ite p 0 (* 4 y)))) 4)
      0))|}
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"])
        "Interrupted divisibility assertion persisted")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"])
        "Divisibility assertion recovery")


let connective_commands () =
  atomic "nested connective" ""
    "(assert (and (! true :named first)\
     (and true (and (! false :named second) true))))"
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"])
        "Interrupted connective changed assertions";
      rejected frontend "(assert first)";
      rejected frontend "(assert second)")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"])
        "Nested connective recovery")

let annotation_commands () =
  let initial = "(declare-const x Int)(assert (= x 7))(check-sat)" in
  let unchanged frontend =
    check (run frontend "(check-sat)(get-assignment)(get-value (x))"
      = ["sat"; "()"; "((x 7))"]) "Interrupted annotation changed model";
    rejected frontend "(get-value (n))" in
  atomic "nested annotation" initial
    "(assert (! (= (! (+ x 1) :named n) 8) :named named))"
    unchanged
    (fun frontend ->
      check (run frontend "(check-sat)(get-assignment)(get-value (n))"
        = ["sat"; "((named true))"; "((n 8))"])
        "Nested annotation recovery");
  atomic "named definition" initial
    "(define-fun f ((y Int)) Int (+ y (! x :named n)))"
    unchanged
    (fun frontend ->
      check (run frontend "(check-sat)(get-value (n (f 3)))"
        = ["sat"; "((n 7) ((f 3) 10))"])
        "Named definition recovery");
  atomic "named datatype definition"
    "(declare-datatypes ((Box 1)) ((par (A) ((box (unbox A))))))"
    "(define-fun f ((unused (Box Bool))) (Box Int) (! (box 7) :named n))"
    (fun frontend ->
      check (run frontend "(check-sat)(get-assignment)" = ["sat"; "()"])
        "Interrupted datatype definition changed annotations";
      rejected frontend "(get-value (n))")
    (fun frontend ->
      check (run frontend
        "(check-sat)(get-value ((unbox n) (unbox (f (box true)))))"
        = ["sat"; "(((unbox n) 7) ((unbox (f (box true))) 7))"])
        "Named datatype definition recovery");
  let initial =
    "(declare-datatypes ((L 1))"
    ^ "((par (A) ((nil) (cons (head A) (tail (L A)))))))"
    ^ "(declare-const xs (L Int))(assert (= xs (cons 7 nil)))"
    ^ "(check-sat)" in
  atomic "datatype match" initial
    ("(assert (= (match xs ((nil (! 0 :named n))"
     ^ " ((cons h t) (+ h 1)))) 8))")
    (fun frontend ->
      check (run frontend
        "(check-sat)(get-assignment)(get-value ((head xs)))"
        = ["sat"; "()"; "(((head xs) 7))"]) "Interrupted match changed model";
      rejected frontend "(get-value (n))")
    (fun frontend ->
      check (run frontend "(check-sat)(get-value (n))"
        = ["sat"; "((n 0))"]) "Match recovery");
  atomic "datatype update" initial
    "(assert (= ((_ update head) xs (! 8 :named n)) (cons 8 nil)))"
    (fun frontend ->
      check (run frontend "(check-sat)(get-assignment)(get-value ((head xs)))"
        = ["sat"; "()"; "(((head xs) 7))"])
        "Interrupted update changed model";
      rejected frontend "(get-value (n))")
    (fun frontend ->
      check (run frontend "(check-sat)(get-value (n))"
        = ["sat"; "((n 8))"]) "Update recovery");
  let assignment frontend =
    check (run frontend "(get-assignment)" = ["((named true))"])
      "Assignment read changed model" in
  atomic "assignment" "(assert (! true :named named))(check-sat)"
    "(get-assignment)" assignment assignment

let conversion_commands () =
  List.iter (fun source ->
    atomic "conversion preprocessing"
      "(declare-const b (_ BitVec 8))(declare-const x Int)"
      source
      (fun frontend ->
        check (run frontend "(check-sat)" = ["sat"])
          "Interrupted conversion assertion persisted")
      (fun frontend ->
        check (run frontend "(check-sat)" = ["unsat"])
          "Conversion assertion recovery"))
    ["(assert (distinct ((_ int_to_bv 129) (sbv_to_int b))\
        ((_ sign_extend 121) b)))";
     "(assert (distinct ((_ int_to_bv 4) (ubv_to_int b))\
        ((_ extract 3 0) b)))";
     "(assert (bvugt ((_ int_to_bv 8) x) #xff))";
     "(assert (< (ubv_to_int b) 0))"];
  atomic "integer bitwise and"
    "(declare-const x Int)(declare-const y Int)(assert (= y 3))"
    "(assert (= ((_ iand 5) x y) 2))"
    (fun frontend ->
      check (run frontend "(check-sat-assuming ((= x 0)))" = ["sat"])
        "Interrupted integer bitwise and persisted")
    (fun frontend ->
      check (run frontend "(check-sat-assuming ((= x 0)))" = ["unsat"])
        "Integer bitwise and recovery");
  List.iter (fun converter ->
    atomic converter "(declare-const x Int)(assert (= x (- 1)))"
      (Printf.sprintf
        "(assert (= (%s ((_ int_to_bv 129) x)) %s))"
        converter
        (if converter = "sbv_to_int" then "(- 1)"
         else "680564733841876926926749214863536422911"))
      (fun frontend ->
        check (run frontend "(check-sat)(get-value (x))"
          = ["sat"; "((x (- 1)))"]) "Interrupted conversion changed model")
      (fun frontend ->
        check (run frontend "(check-sat)" = ["sat"])
          "Conversion recovery")) ["ubv_to_int"; "sbv_to_int"]

let check_budgets () =
  let frontend = setup "(declare-const x Int)(assert (= x 7))" in
  let expression = command "(check-sat-assuming ((= (* x x) 49)))" in
  let resource = R.create () in
  check (F.execute ~resource frontend expression = Some "sat") "Check";
  let used = R.used resource in
  for budget = 0 to used - 1 do
    let frontend = setup "(declare-const x Int)(assert (= x 7))" in
    check (F.execute ~resource:(R.create ~budget ()) frontend expression
           = Some "unknown") "Limited check did not stop";
    rejected frontend "(get-value (x))";
    check (run frontend "(check-sat)(get-value (x))"
           = ["sat"; "((x 7))"]) "Check recovery"
  done;
  Printf.printf "check: %d interrupted budgets recovered\n%!" used

let parser_and_constants () =
  let numeral = String.make 2000 '9' in
  let source = "(assert (= (* " ^ numeral ^ " " ^ numeral ^ ") 0))" in
  let resource = R.create ~budget:100 () in
  (match S.parse ~resource source with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "Parser did not stop");
  let expression = command source in
  List.iter (fun budget ->
    let frontend = F.create () in
    let resource = R.create ~budget () in
    (match F.execute ~resource frontend expression with
    | exception R.Exhausted _ -> ()
    | _ -> failwith "Constant elaboration did not stop");
    check (run frontend "(check-sat)" = ["sat"])
      "Interrupted constant assertion persisted") [0; 1; 100; 1000; 10000];
  let terms = T.create () in
  let resource = R.create ~budget:100 () in
  (match T.with_poll terms (fun () -> R.poll resource)
      (fun () -> T.bits terms 1000000 Z.one) with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "Bitvector constant construction did not stop");
  ignore (T.bits terms 8 Z.one);
  let resource = R.create ~budget:100 () in
  (match S.to_string ~resource (S.String (String.make 1000 '"')) with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "String serialization did not stop")

let model_resources () =
  let terms = T.create () in
  let solver = E.create terms in
  let symbol = T.symbol terms
    { name = "x"; arguments = [||]; result = T.Int;
      kind = T.Uninterpreted } in
  let x = T.apply terms symbol [||] in
  E.assert_ solver (T.equal terms x (T.integer terms (Z.of_int 7)));
  check (E.check solver = E.Sat) "Model setup";
  (match M.create_with_resource (R.create ~budget:0 ()) ~terms ~solver with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "Model creation did not stop");
  let resource = R.create () in
  let model = M.create_with_resource resource ~terms ~solver in
  let remaining = R.remaining resource in
  R.charge resource remaining;
  check (M.value model x = M.Integer (Z.of_int 7))
    "Model retained creation resource";
  (match M.value ~resource:(R.create ~budget:0 ()) model x with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "Model access did not stop");
  check (M.value model x = M.Integer (Z.of_int 7)) "Model read recovery"

let model_constructive_completion () =
  let fixture declaration sort tester count extra =
    declaration ^ "(declare-fun f (" ^ sort ^ ") Int)"
    ^ String.concat "" (List.init count (fun index ->
      let name = "n" ^ string_of_int index in
      "(declare-const " ^ name ^ " " ^ sort ^ ")"
      ^ "(assert ((_ is " ^ tester ^ ") " ^ name ^ "))"
      ^ "(assert (= (f " ^ name ^ ") " ^ string_of_int index ^ "))"))
    ^ extra ^ "(check-sat)" in
  let nat = "(declare-datatype Nat ((Z) (S (pred Nat))))" in
  let cases =
    [ nat, "Nat", "S", 513, "";
      nat, "Nat", "S", 4,
      "(assert (= n0 " ^ String.concat "" (List.init 300 (fun _ -> "(S "))
      ^ "Z" ^ String.make 300 ')' ^ "))";
      "(declare-datatypes ((A 0) (B 0))\
       (((AZ) (AS (nextB B))) ((BS (nextA A)))))", "A", "AS", 8, "";
      "(declare-datatype Seq ((Nil) (Cons (head Int) (tail Seq))))",
      "Seq", "Cons", 8,
      "(assert (= (head n0) 1000000000000000000000000000000))";
      "(declare-datatype Pair ((Pair (first Bool) (second Bool))))",
      "Pair", "Pair", 4, "" ] in
  List.iter (fun (declaration, sort, tester, count, extra) ->
    let frontend = F.create () in
    check (run frontend (fixture declaration sort tester count extra)
      = ["sat"]) "Constructive completion setup";
    let last = "n" ^ string_of_int (count - 1) in
    let distinct = "(distinct n0 " ^ last ^ ")" in
    let first_test = "((_ is " ^ tester ^ ") n0)" in
    let last_test = "((_ is " ^ tester ^ ") " ^ last ^ ")" in
    let expected = "((" ^ distinct ^ " true) (" ^ first_test ^ " true) ("
      ^ last_test ^ " true))" in
    check (run frontend ("(get-value (" ^ distinct ^ " " ^ first_test
      ^ " " ^ last_test ^ "))") = [expected])
      "Constructive completion model") cases;
  let hierarchy = "(declare-datatype T0 ((Leaf)))"
    ^ String.concat "" (List.init 30 (fun index ->
      let current = string_of_int (index + 1) in
      let previous = "T" ^ string_of_int index in
      "(declare-datatype T" ^ current ^ " ((C" ^ current
      ^ " (left" ^ current ^ " " ^ previous ^ ") (right" ^ current
      ^ " " ^ previous ^ "))))")) in
  let declaration = hierarchy
    ^ "(declare-datatype Seq ((Deep (heavy T30) (tail Seq))"
    ^ "(Nil) (Next (next Seq))))" in
  let frontend = setup (fixture declaration "Seq" "Next" 2 "") in
  check (run frontend "(get-value ((f n0) (f n1)))"
    = ["(((f n0) 0) ((f n1) 1))"])
    "Shared completion defaults";
  let nodes = String.concat "" (List.init 30 (fun index ->
    let current = "x" ^ string_of_int (index + 1) in
    let previous = "x" ^ string_of_int index in
    "(declare-const " ^ current ^ " Tree)(assert (= " ^ current
    ^ " (Node " ^ previous ^ " " ^ previous ^ ")))")) in
  let frontend = setup
    ("(declare-datatype Tree ((Leaf) (Node (left Tree) (right Tree))))"
    ^ "(declare-fun f (Tree) Int)(declare-const x0 Tree)"
    ^ nodes ^ "(assert (= (f x30) 1))(check-sat)") in
  check (run frontend "(get-value ((f x30)))" = ["(((f x30) 1))"])
    "Shared function argument";
  let initial = fixture nat "Nat" "S" 2 "" in
  let source = "(get-value ((distinct n0 n1) (= n0 (S (pred n0)))\
    (= n1 (S (pred n1))) (f n0) (f n1)))" in
  let expected = ["(((distinct n0 n1) true) ((= n0 (S (pred n0))) true) \
    ((= n1 (S (pred n1))) true) ((f n0) 0) ((f n1) 1))"] in
  let recovered frontend =
    check (run frontend source = expected) "Completion resource recovery";
    check (run frontend "(push 1)(assert (= n0 n1))(check-sat)\
      (pop 1)(check-sat)" = ["unsat"; "sat"])
      "Completion scope recovery";
    check (run frontend source = expected) "Completion model after pop" in
  atomic "constructive model completion" initial source recovered recovered

let dag_source depth =
  "(declare-datatype T0 ((Leaf (bit Bool))))"
  ^ String.concat "" (List.init depth (fun index ->
    let current = string_of_int (index + 1) in
    let previous = "T" ^ string_of_int index in
    "(declare-datatype T" ^ current ^ " ((C" ^ current
    ^ " (left" ^ current ^ " " ^ previous ^ ") (right" ^ current
    ^ " " ^ previous ^ "))))"))
  ^ "(declare-const x T" ^ string_of_int depth ^ ")(check-sat)"

let dag_equality depth leaf =
  let rec surround level body =
    if level < 0 then body else
      let fields = if level = 0 then "(Leaf " ^ leaf ^ ")" else
        let previous = "v" ^ string_of_int (level - 1) in
        "(C" ^ string_of_int level ^ " " ^ previous ^ " " ^ previous
        ^ ")" in
      surround (level - 1)
        ("(let ((v" ^ string_of_int level ^ " " ^ fields ^ ")) "
         ^ body ^ ")") in
  surround depth ("(= x v" ^ string_of_int depth ^ ")")

let model_equal_shared_dags () =
  let exercise depth recovery =
    let initial = dag_source depth in
    let expression = dag_equality depth "false" in
    let source = "(get-value (" ^ expression ^ "))" in
    let expected = ["((" ^ expression ^ " true))"] in
    let recover frontend =
      check (run frontend source = expected) "Equal shared DAG model" in
    let frontend = setup initial in
    recover frontend;
    let different = dag_equality depth "true" in
    check (run frontend ("(get-value (" ^ different ^ "))")
      = ["((" ^ different ^ " false))"]) "Unequal shared DAG model";
    if recovery then atomic "equal shared DAG" initial source recover recover
  in
  exercise 30 false;
  exercise 8 true

let model_recursive_products () =
  let frontend = setup
    "(declare-datatypes ((Record 0) (Items 0))\
     (((record (a Int) (b Int) (c Int) (d Int) (rest Items)))\
      ((nil) (cons (head Record) (tail Items)))))\
     (declare-const x Items)(declare-const y Items)\
     (declare-const r Record)(assert (distinct x (cons r y)))" in
  check (run frontend "(check-sat)" = ["sat"])
    "Recursive product setup";
  let expression = command "(get-value ((distinct x (cons r y))))" in
  let resource = R.create ~budget:5000 () in
  check (F.execute ~resource frontend expression
    = Some "(((distinct x (cons r y)) true))")
    "Recursive product model";
  check (run frontend "(get-value ((distinct x (cons r y))))"
    = ["(((distinct x (cons r y)) true))"])
    "Recursive product model reuse"

let cancellation_and_clock () =
  let frontend = setup "(declare-const x Int)(assert (= x 7))" in
  let expression = command "(push 10000)" in
  let polls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr polls; !polls = 3) () in
  (match F.execute ~resource frontend expression with
  | exception R.Exhausted "cancelled" -> ()
  | _ -> failwith "Cancellation did not stop");
  rejected frontend "(pop 1)";
  let frontend = F.create ~clock:(fun () -> 0.) () in
  ignore (run frontend "(set-option :timeout 1)");
  check (run frontend "(check-sat)" = ["sat"]) "Injected clock";
  let now = ref 0. in
  let frontend = F.create ~clock:(fun () -> now := !now +. 1.; !now) () in
  ignore (run frontend "(set-option :timeout 1)");
  check (run frontend "(check-sat)(get-info :reason-unknown)"
         = ["unknown"; "(:reason-unknown \"timeout\")"])
    "Injected timeout";
  let frontend = F.create () in
  let resource = R.create ~cancelled:(fun () ->
    ignore (run frontend "(reset)"); false) () in
  (match F.execute ~resource frontend (command "(declare-const x Int)") with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "Frontend reentry accepted");
  ignore (run frontend "(declare-const x Int)(check-sat)")

let command_callbacks () =
  let initial =
    "(declare-datatypes ((Tree 1))"
    ^ "((par (A) ((leaf (value A)) (node (left (Tree A))"
    ^ " (right (Tree A)))))))"
    ^ "(declare-const x (Tree Int))(assert (= x (leaf 7)))"
    ^ "(declare-fun f (Int) Int)(assert (= (f 7) 9))(check-sat)" in
  let model_commands =
    ["(get-model)"; "(get-value (x (value x) (f 7)"
     ^ " (leaf true) (node (leaf true) (leaf false))))"] in
  List.iter (fun source ->
    let expression = command source in
    let frontend = setup initial in
    let expected = F.execute frontend expression in
    let calls = ref 0 in
    ignore (F.execute ~resource:(R.create ~cancelled:(fun () ->
      incr calls; false) ()) (setup initial) expression);
    for stop = 1 to !calls do
      List.iter (fun raises ->
        let frontend = setup initial in
        let seen = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr seen;
          if !seen = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | exception Exit when raises -> ()
        | exception R.Exhausted "cancelled" when not raises -> ()
        | _ -> failwith "Model callback did not interrupt");
        check (F.execute frontend expression = expected)
          "Model/datatype cache changed after callback";
        check (run frontend "(check-sat)(get-value ((value x) (f 7)))"
          = ["sat"; "(((value x) 7) ((f 7) 9))"])
          "Model callback changed assertions") [false; true]
    done;
    Printf.printf "model command: %d callback positions recovered\n%!"
      (2 * !calls)) model_commands;
  let query = command "(check-sat-assuming ((= (value x) 7)))" in
  let calls = ref 0 in
  let frontend = setup initial in
  ignore (F.execute ~resource:(R.create ~cancelled:(fun () ->
    incr calls; false) ()) frontend query);
  for stop = 1 to !calls do
    List.iter (fun raises ->
      let frontend = setup initial in
      let seen = ref 0 in
      let resource = R.create ~cancelled:(fun () -> incr seen;
        if !seen = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend query with
      | exception Exit when raises -> ()
      | Some "unknown" when not raises -> ()
      | _ -> failwith "Check callback did not interrupt");
      rejected frontend "(get-model)";
      check (run frontend "(check-sat)(get-value ((value x)))"
        = ["sat"; "(((value x) 7))"]) "Check callback recovery") [false; true]
  done

let frontend_entry_reentry () =
  let frontend = setup "(declare-const x Int)(assert (= x 7))(check-sat)" in
  let resource = R.create ~cancelled:(fun () ->
    ignore (F.run frontend "(reset)"); false) () in
  (match F.run ~resource frontend "(assert false)" with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "Parsing callback reentered frontend");
  check (run frontend "(get-value (x))" = ["((x 7))"])
    "Parsing callback changed model";
  let callback = ref (fun () -> 0.) in
  let frontend = F.create ~clock:(fun () -> !callback ()) () in
  ignore (run frontend "(declare-const x Int)(assert (= x 7))"
    );
  ignore (run frontend "(check-sat)(set-option :timeout 10)");
  callback := (fun () -> ignore (run frontend "(reset)"); 0.);
  (match run frontend "(check-sat)" with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "Initial deadline clock reentered frontend");
  callback := (fun () -> 0.);
  rejected frontend "(get-model)";
  check (run frontend "(check-sat)(get-value (x))" = ["sat"; "((x 7))"])
    "Clock callback changed assertions"

let engine_term_polling () =
  let terms = T.create () in
  let solver = E.create terms in
  let variable name = T.apply terms (T.symbol terms
    {name; arguments = [||]; result = T.Int; kind = T.Uninterpreted}) [||] in
  let x = variable "large" and y = variable "quotient" in
  let number = Z.of_string (String.make 1800 '9') in
  let divisor = Z.of_string ("8" ^ String.make 899 '1') in
  let literal = T.integer terms number in
  E.assert_ solver (T.equal terms x literal);
  let quotient = T.builtin terms "div" T.Int
      [|x; T.integer terms divisor|] in
  let assertion = T.equal terms quotient y in
  let resource = R.create ~budget:1000 () in
  (match E.assert_ ~resource solver assertion with
  | exception R.Exhausted "resource limit" -> ()
  | _ -> failwith "Engine-only constant folding did not stop");
  check (R.used resource = 1000) "Engine folding accounting";
  check (E.check ~assumptions:[T.equal terms y (T.integer terms Z.zero)] solver
    = E.Sat) "Interrupted folding assertion persisted";
  check (E.value solver x = Some (E.Integer number))
    "Interrupted folding lost prior substitution";
  let square = T.builtin terms "*" T.Int [|x; x|] in
  (match E.value ~resource:(R.create ~budget:1000 ()) solver square with
  | exception R.Exhausted "resource limit" -> ()
  | _ -> failwith "Engine-only model folding did not stop");
  check (E.value solver x = Some (E.Integer number))
    "Interrupted model folding changed model";
  check (E.value solver square = Some (E.Integer (Z.mul number number)))
    "Interrupted model folding did not recover"

let parser_resources () =
  let expression = S.List [S.Atom "echo";
      S.String "a quoted \"value\""; S.Atom "|quoted symbol|";
      S.List [S.Atom "left"; S.Atom "right"]] in
  let source = S.to_string expression in
  let check_work operation =
    let resource = R.create () in
    operation resource;
    for budget = 0 to R.used resource - 1 do
      match operation (R.create ~budget ()) with
      | exception R.Exhausted "resource limit" -> ()
      | _ -> failwith "Parser/render budget did not interrupt"
    done in
  check_work (fun resource ->
    check (S.parse ~resource source = [expression]) "Parser changed output");
  check_work (fun resource ->
    check (S.to_string ~resource expression = source)
      "Renderer changed output");
  let path = Filename.temp_file "vox-resource-stream-" ".smt2" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let output = open_out_bin path in
    output_string output source; close_out output;
    let interrupted () =
      let input = open_in_bin path in
      let reference = Weak.create 1 in
      Weak.set reference 0 (Some input);
      Fun.protect ~finally:(fun () -> close_in input) (fun () ->
        match S.read ~resource:(R.create ~budget:0 ()) input with
        | exception R.Exhausted "resource limit" -> ()
        | _ -> failwith "Stream budget did not interrupt");
      reference in
    let reference = interrupted () in
    Gc.full_major ();
    check (Weak.get reference 0 = None) "Interrupted channel retained";
    let input = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      check (S.read input = Some expression) "Fresh stream after interruption";
      check (S.read input = None) "Fresh stream end"))

let check_recovery name initial expression recover =
  let frontend = setup initial in
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  check (F.execute ~resource frontend expression = Some "unsat")
    "Check recovery fixture";
  let used = R.used resource and callbacks = !calls in
  for budget = 0 to used - 1 do
    let frontend = setup initial in
    check (F.execute ~resource:(R.create ~budget ()) frontend expression
      = Some "unknown") "Check recovery budget did not stop";
    recover frontend
  done;
  List.iter (fun raises ->
    for stop = 1 to callbacks do
      let frontend = setup initial in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | exception Exit when raises -> ()
      | Some "unknown" when not raises -> ()
      | _ -> failwith "Check recovery callback did not interrupt");
      recover frontend
    done) [false; true];
  Printf.printf
    "%s: %d budgets, %d callback positions recovered\n%!"
    name used (2 * callbacks)

let pure_uf_recovery () =
  let boolean =
    {|(declare-sort U 0)(declare-const p Bool)(declare-const q Bool)
      (declare-fun f (Bool) U)(assert p)(assert q)(check-sat)
      (push)(assert (distinct (f p) (f q)))|} in
  let datatype =
    {|(declare-sort U 0)(declare-const u U)(declare-const v U)
      (declare-const p Bool)(declare-const q Bool)
      (declare-fun f (Bool) U)(assert p)(assert q)
      (assert (distinct u v))(check-sat)
      (push)(declare-datatype D ((empty) (box (contents U))))
      (declare-const d D)(assert (= d (box u)))
      (push)(assert (= (contents d) v))|} in
  List.iter (fun (name, initial) ->
    check_recovery name initial (command "(check-sat)")
      (fun frontend ->
        check (run frontend "(check-sat)(pop)(check-sat)"
          = ["unsat"; "sat"]) (name ^ " recovery")))
    ["Boolean UF", boolean; "UF to datatype", datatype]

let difference_presolver_recovery () =
  let initial =
    "(declare-const x Int)(declare-const y Int)\
     (declare-const z Int)(declare-const w Int)(push 1)\
     (assert (> x 0))(assert (> z 0))\
     (assert (or (<= x 0) (<= z 0)))\
     (assert (or (<= x 0) (<= w 0)))\
     (assert (or (<= y 0) (<= z 0)))\
     (assert (or (<= y 0) (<= w 0)))" in
  check_recovery "difference presolver" initial (command "(check-sat)")
    (fun frontend ->
      check (run frontend "(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "Difference presolver scope recovery";
      check (run frontend "(reset-assertions)(check-sat)" = ["sat"])
        "Difference presolver reset recovery")

let buffered_scope_recovery () =
  let initial =
    {|(declare-const x Int)(declare-fun f (Int) Int)
      (assert (> (f x) 0))(push 1)(assert (= x 1))
      (push 1)(assert (= (f 1) 0))|} in
  let expression = command "(check-sat)" in
  let recover frontend =
    check (run frontend
      "(check-sat)(pop 1)(check-sat)(get-value (x))"
      = ["unsat"; "sat"; "((x 1))"])
      "Buffered child assertion recovery";
    check (run frontend
      "(pop 1)(push 1)(assert (= x 2))(assert (= (f 1) 0))\
       (check-sat)(check-sat-assuming ((= (f 2) 0)))\
       (pop 1)(check-sat-assuming ((<= (f x) 0)))"
      = ["sat"; "unsat"; "unsat"])
      "Buffered parent assertion recovery" in
  check_recovery "buffered scopes" initial expression recover

let requested_equality_recovery () =
  let initial =
    {|(declare-datatype T
        ((box (contents Int)) (pair (first T) (second Int))))
      (declare-sort U 0)(declare-const k U)(declare-fun p (U T Int) Bool)
      (declare-const x Int)(declare-const y Int)(declare-const q Bool)
      (assert (<= 0 x))(assert (< x 16))
      (assert (<= 0 y))(assert (< y 16))
      (assert (=> q (= y (mod (+ x 32) 16))))
      (assert (p k (pair (box x) 3) 7))
      (assert (not (p k (pair (box y) 3) 7)))|} in
  let expression = command "(check-sat-assuming (q))" in
  let recover frontend =
    check (run frontend
      "(check-sat)(check-sat-assuming (q))(check-sat)"
      = ["sat"; "unsat"; "sat"]) "Requested equality recovery";
    check (run frontend
      "(push 1)(assert q)(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"]) "Requested equality scope recovery" in
  check_recovery "requested equality" initial expression recover

let quotient_bounds_recovery () =
  let initial =
    {|(declare-const x Int)(declare-const k Int)(declare-const p Bool)
      (define-fun delta () Int (- x (* 3 k)))
      (assert (=> p (and (<= 0 delta) (< delta 3))))
      (assert (distinct (mod x 3) delta))|} in
  let expression = command "(check-sat-assuming (p))" in
  let recover frontend =
    check (run frontend
      "(check-sat)(check-sat-assuming (p))(check-sat)"
      = ["sat"; "unsat"; "sat"]) "Quotient bounds assumption recovery";
    check (run frontend
      "(push 1)(assert p)(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"]) "Quotient bounds scope recovery" in
  check_recovery "quotient bounds" initial expression recover

let bitvector_quotient_recovery () =
  let initial =
    {|(declare-const q (_ BitVec 5))(declare-const p Bool)
      (define-fun r () (_ BitVec 5)
        (bvsub (_ bv5 5) (bvmul (_ bv4 5) q)))
      (assert (=> p (and (bvsle (_ bv0 5) q) (bvslt q (_ bv4 5))
        (bvsle (_ bv0 5) r) (bvslt r (_ bv4 5)))))
      (assert (distinct q (_ bv1 5)))|} in
  let expression = command "(check-sat-assuming (p))" in
  let recover frontend =
    check (run frontend
      "(check-sat)(check-sat-assuming (p))(check-sat)"
      = ["sat"; "unsat"; "sat"]) "BV quotient assumption recovery";
    check (run frontend
      "(push 1)(assert p)(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"]) "BV quotient scope recovery" in
  check_recovery "bitvector quotient" initial expression recover

let cumulative_resources () =
  let count frontend =
    match run frontend "(get-info :rlimit)" with
    | [response] -> Scanf.sscanf response "(:rlimit %d)" Fun.id
    | _ -> failwith "Missing resource count"
  in
  let frontend = setup "(declare-const x Int)(assert (> x 0))" in
  check (count frontend = 0) "Resources before first check";
  check (run frontend "(push 1)(assert (< x 1))(check-sat)" = ["unsat"])
    "Resource fixture";
  let first = count frontend in
  check (first > 0) "No resource count";
  ignore (run frontend "(pop 1)(push 1)(assert (< x 2))");
  check (count frontend = first) "Mutation changed resource count";
  check (run frontend "(check-sat)" = ["sat"]) "Scoped resource fixture";
  let second = count frontend in
  check (second > first) "Resource count did not accumulate";
  ignore (run frontend "(get-value (x))(pop 1)(reset-assertions)");
  check (count frontend = second) "Reset assertions changed resource count";
  check (run frontend "(check-sat)" = ["sat"]) "Reset assertions fixture";
  check (count frontend > second) "New solver did not accumulate resources";
  ignore (run frontend "(reset)");
  check (count frontend = 0) "Reset retained resource count";
  let initial =
    "(declare-const x Int)(assert (> x 0))(assert (< x 10))" in
  let expression = command "(check-sat)" in
  let frontend = setup initial in
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  ignore (F.execute ~resource frontend expression);
  let callbacks = !calls in
  List.iter (fun raises ->
    for stop = 1 to callbacks do
      let frontend = setup initial in
      ignore (run frontend "(check-sat)");
      let before = count frontend in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls >= stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | exception Exit when raises -> ()
      | Some "unknown" when not raises -> ()
      | Some "sat" when !calls < stop -> ()
      | _ -> failwith "Resource cancellation result");
      let interrupted = count frontend in
      check (interrupted >= before) "Interrupted check lost resources";
      check (run frontend "(check-sat)" = ["sat"])
        "Resource cancellation recovery";
      check (count frontend > interrupted) "Recovered check lost resources"
    done) [false; true];
  let frontend = setup initial in
  ignore (run frontend "(check-sat)");
  let before = count frontend in
  ignore (run frontend "(set-option :rlimit 8)");
  check (run frontend "(check-sat)" = ["unknown"]) "Resource limit fixture";
  check (count frontend >= before) "Resource limit lost resources";
  ignore (run frontend "(set-option :rlimit 0)");
  let before = count frontend in
  check (run frontend "(check-sat)" = ["sat"]) "Resource limit recovery";
  check (count frontend > before) "Resource limit recovery count"

let disjunctive_equality_recovery () =
  let initial =
    {|(declare-sort U 0)(declare-const a U)(declare-const b U)
      (declare-const c U)(declare-const d U)(declare-const p Bool)
      (assert (=> p (or (and (= a b) (= b c))
                        (and (= a d) (= d c)))))
      (assert (distinct a c))|} in
  let expression = command "(check-sat-assuming (p))" in
  let recover frontend =
    check (run frontend
      "(check-sat)(check-sat-assuming (p))(check-sat)"
      = ["sat"; "unsat"; "sat"]) "Disjunctive equality assumption recovery";
    check (run frontend
      "(push 1)(assert p)(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"]) "Disjunctive equality scope recovery" in
  check_recovery "disjunctive equality" initial expression recover

let integer_extensions_recovery () =
  let model frontend =
    check (run frontend "(get-value ((int.pow2 x)))"
      = ["(((int.pow2 x) 170141183460469231731687303715884105728))"])
      "Integer extension model recovery" in
  atomic "integer extension model"
    "(declare-const x Int)(assert (= x 127))(check-sat)"
    "(get-value ((int.pow2 x)))" model model;
  List.iter (fun (name, initial) ->
    let initial = initial ^ "(push 1)(assert bad)" in
    let recover frontend =
      check (run frontend "(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "Integer extension check recovery" in
    check_recovery name initial (command "(check-sat)") recover)
    ["power extension",
       {|(declare-const x Int)(assert (>= x 0))
         (define-fun bad () Bool (< (int.pow2 x) x))|};
     "log extension",
       {|(declare-const x Int)(assert (= (int.log2 x) 5))
         (define-fun bad () Bool (> x 100))|};
     "bitwise extension",
       {|(declare-const k Int)(declare-const x Int)(declare-const y Int)
         (assert (> k 0))(assert (= y 1))(assert (= (mod x 2) 0))
         (define-fun bad () Bool (distinct (piand k x y) 0))|}];
  atomic "constant integer extension" "(declare-const p Bool)"
    "(assert (= (int.log2 (int.pow2 4096)) 4095))"
    (fun frontend -> check (run frontend "(check-sat)" = ["sat"])
        "Interrupted extension assertion persisted")
    (fun frontend -> check (run frontend "(check-sat)" = ["unsat"])
        "Extension assertion missing")

let small_polynomial_recovery () =
  let initial =
    "(declare-const x Int)(declare-const y Int)(assert (= (* x y) 1))" in
  let expression = command "(check-sat-assuming ((> x 0)))" in
  let recover frontend =
    check (run frontend
      "(check-sat-assuming ((< x 0)))(get-value ((* x y) (< x 0)))"
      = ["sat"; "(((* x y) 1) ((< x 0) true))"])
      "Polynomial suggestion changed assumptions" in
  let frontend = setup initial in
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  check (F.execute ~resource frontend expression = Some "sat")
    "Polynomial model fixture";
  let used = R.used resource and callbacks = !calls in
  for budget = 0 to used - 1 do
    let frontend = setup initial in
    check (F.execute ~resource:(R.create ~budget ()) frontend expression
      = Some "unknown") "Polynomial model budget did not stop";
    rejected frontend "(get-value (x))";
    recover frontend
  done;
  List.iter (fun raises ->
    for stop = 1 to callbacks do
      let frontend = setup initial in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | exception Exit when raises -> ()
      | Some "unknown" when not raises -> ()
      | _ -> failwith "Polynomial model callback did not interrupt");
      recover frontend
    done) [false; true];
  Printf.printf "%s: %d budgets, %d callback positions recovered\n%!"
    "small polynomial models" used (2 * callbacks);
  let verify frontend =
    check (run frontend "(get-value ((* x y)))" = ["(((* x y) 1))"])
      "Polynomial model access recovery" in
  atomic "polynomial model access" (initial ^ "(check-sat)")
    "(get-value ((* x y) (+ x y)))" verify verify


let equality_lattice_recovery () =
  let initial =
    {|(declare-const y Int)(declare-const z Int)
      (define-fun sum () Int (+ (* 4294967296 y) (* 3123612579 z)))
      (assert (<= sum 24))(assert (>= sum 24))(assert (< z 0))|} in
  let expression = command "(check-sat)" in
  let frontend = setup initial in
  let resource = R.create () in
  check (F.execute ~resource frontend expression = Some "sat")
    "Lattice model fixture";
  let used = R.used resource in
  let recover frontend =
    check (run frontend
      "(check-sat)(get-value (sum (< z 0)))\
       (push 1)(assert (> sum 24))(check-sat)(pop 1)(check-sat)"
      = ["sat"; "((sum 24) ((< z 0) true))"; "unsat"; "sat"])
      "Interrupted lattice model recovery" in
  for budget = 0 to used - 1 do
    let frontend = setup initial in
    check (F.execute ~resource:(R.create ~budget ()) frontend expression
      = Some "unknown") "Lattice budget did not interrupt";
    recover frontend
  done;
  let callbacks = ref 0 in
  let frontend = setup initial in
  ignore (F.execute ~resource:(R.create ~cancelled:(fun () ->
    incr callbacks; false) ()) frontend expression);
  List.iter (fun raises ->
    for stop = 1 to !callbacks do
      let frontend = setup initial and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Lattice cancellation did not interrupt");
      recover frontend
    done) [false; true];
  Printf.printf "equality lattice: %d budgets, %d callbacks recovered\n%!"
    used (2 * !callbacks)

let bounded_polynomial_recovery () =
  let fixtures = [
    "bounded polynomial",
    ["(>= x3 2)";
     "(<= (+ (* x4 x2) (* 2 x2 x0 x2) x1 (* x1 x4 x4)) 9)";
     "(= (+ (* 3 x0 x4) x2) (- 5))";
     "(= (+ x1 (* 3 x2 x3) (* x3 x4) (* (- 2) x0 x1 x0)) (- 10))";
     "(or (= x1 2) (= (* x1 x1) 0))"];
    "separable polynomial",
    ["(= (+ (* x0 x0 x0) (* x1 x1 x1) (* x2 x2 x2)) 327)"]]
  in
  List.iter (fun (name, constraints) ->
    let initial = "(set-option :produce-models true)"
      ^ String.concat "" (List.init 5 (fun index ->
          Printf.sprintf "(declare-const x%d Int)" index))
      ^ String.concat "" (List.map (fun term ->
          "(assert " ^ term ^ ")") constraints) in
    let verify frontend =
      let source = "(get-value (" ^ String.concat " " constraints ^ "))" in
      match run frontend source with
      | [response] ->
        (match S.parse response with
        | [S.List values] ->
          check (List.length values = List.length constraints
            && List.for_all (function
              | S.List [_; S.Atom "true"] -> true | _ -> false) values)
            "Polynomial proposal violated original constraints"
        | _ -> failwith "Malformed polynomial model")
      | _ -> failwith "Missing polynomial model" in
    let recover frontend =
      check (run frontend "(check-sat)" = ["sat"])
        "Polynomial proposal recovery";
      verify frontend;
      check (run frontend
        "(push 1)(assert false)(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "Polynomial proposal scope recovery";
      verify frontend in
    let selected count = List.sort_uniq Int.compare
      (0 :: (count - 1) :: List.init 65 (fun index -> count * index / 65)) in
    let expression = command "(check-sat)" in
    let frontend = setup initial in
    let callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (F.execute ~resource frontend expression = Some "sat")
      "Polynomial proposal fixture";
    verify frontend;
    let used = R.used resource and callbacks = !callbacks in
    List.iter (fun budget ->
      let frontend = setup initial in
      check (F.execute ~resource:(R.create ~budget ()) frontend expression
        = Some "unknown") "Polynomial proposal budget ignored";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun index ->
        let stop = index + 1 in
        let frontend = setup initial in
        let calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr calls;
          if !calls = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | exception Exit when raises -> ()
        | Some "unknown" when not raises -> ()
        | _ -> failwith "Polynomial proposal callback ignored");
        recover frontend) (selected callbacks)) [false; true];
    Printf.printf "%s: %d budgets, %d callback positions recovered\n%!"
      name (List.length (selected used))
      (2 * List.length (selected callbacks))) fixtures

let polynomial_residue_recovery () =
  let fixtures =
    ["(= (+ (* x x x) (* y y y) (* z z z)) 4)";
     "(= (+ (* x x) x) 1)";
     "(= (+ (* x x) (* y y) (* z z)) 7)";
     "(= (- (* x x x x) (* y y y y)) 2)";
     "(= (+ (* (- 17) x x x) (* 19 y y y)) 4)"] in
  let selected count = List.sort_uniq Int.compare
    (0 :: (count - 1) :: List.init 33 (fun index -> count * index / 33)) in
  List.iter (fun equality ->
    let initial =
      "(declare-const x Int)(declare-const y Int)(declare-const z Int)"
      ^ "(push 1)(assert " ^ equality ^ ")" in
    let recover frontend =
      check (run frontend "(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "Residue scope recovery";
      check (run frontend
        "(push 1)(assert (= x 0))(assert (= y 1))(assert (= z 2))\
         (check-sat)(get-value ((+ x y z)))(pop 1)(check-sat)"
        = ["sat"; "(((+ x y z) 3))"; "sat"])
        "Residue model recovery" in
    let expression = command "(check-sat)" in
    let callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (F.execute ~resource (setup initial) expression = Some "unsat")
      "Residue fixture";
    let used = R.used resource and callbacks = !callbacks in
    List.iter (fun budget ->
      let frontend = setup initial in
      check (F.execute ~resource:(R.create ~budget ()) frontend expression
        = Some "unknown") "Residue budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = setup initial and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Residue callback");
        recover frontend) (selected callbacks)) [false; true]) fixtures

let nary_bitvector_recovery () =
  let initial =
    {|(declare-const x (_ BitVec 4))
      (declare-const y (_ BitVec 4))
      (push 1)(assert (distinct (bvxor x y) #b1111))|} in
  let expression =
    command "(check-sat-assuming ((= (bvxor x y) #b1111)))" in
  check_recovery "n-ary bitvector gates" initial expression
    (fun frontend ->
      check (run frontend
        {|(check-sat-assuming ((= (bvxor x y) #b1111)))
          (check-sat)(pop 1)
          (check-sat-assuming ((= (bvxor x y) #b1111)))
          (get-value ((= (bvxor x y) #b1111)))|}
        = ["unsat"; "sat"; "sat";
           "(((= (bvxor x y) #b1111) true))"])
        "N-ary bitvector assumption recovery")

let integer_cut_recovery () =
  let initial =
    {|(declare-const x Int)(declare-const y Int)
      (assert (>= x 0))(assert (>= y 0))(push 1)
      (assert (= (+ (* 2 x) (* 3 y)) 1))|} in
  let setup () = let f = F.create () in ignore (F.run f initial); f in
  let command = match S.parse "(check-sat)" with
    | [command] -> command | _ -> failwith "Check command" in
  let recover f =
    check (F.run f
      "(check-sat)(pop 1)(push 1)\
       (assert (= (+ (* 2 x) (* 3 y)) 2))(check-sat)\
       (get-value ((+ (* 2 x) (* 3 y))))(pop 1)(check-sat)"
      = ["unsat"; "sat"; "(((+ (* 2 x) (* 3 y)) 2))"; "sat"])
      "Cut recovery" in
  let resource = R.create () in
  check (F.execute ~resource (setup ()) command = Some "unsat") "Cut fixture";
  let used = R.used resource in
  for budget = 0 to used - 1 do
    let f = setup () in
    check (F.execute ~resource:(R.create ~budget ()) f command = Some "unknown")
      "Cut budget did not interrupt";
    recover f
  done;
  let callbacks = ref 0 in
  ignore (F.execute ~resource:(R.create ~cancelled:(fun () ->
    incr callbacks; false) ()) (setup ()) command);
  List.iter (fun raises ->
    for stop = 1 to !callbacks do
      let f = setup () and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource f command with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Cut callback did not interrupt");
      recover f
    done) [false; true];
  Printf.printf "integer cuts: %d budgets, %d callbacks recovered\n%!"
    used (2 * !callbacks)

let alternate_integer_cut_recovery () =
  let setup () =
    let terms = T.create () in
    let solver = E.create terms in
    let integer i = T.integer terms (Z.of_int i) in
    let variable name =
      T.apply terms (T.symbol terms
        { T.name; arguments = [||]; result = T.Int; kind = T.Uninterpreted })
        [||] in
    let variables = Array.init 20 (fun i ->
      variable (Printf.sprintf "x%d" i)) in
    Array.iter (fun variable ->
      E.assert_ solver (T.builtin terms ">=" T.Bool [|variable; integer 0|]))
      variables;
    let sum = T.builtin terms "+" T.Int
        [|T.builtin terms "*" T.Int [|integer 2; variables.(0)|];
          T.builtin terms "*" T.Int [|integer 3; variables.(1)|]|] in
    E.push solver;
    E.assert_ solver (T.equal terms sum (integer 1));
    solver, terms, sum in
  let recover (solver, terms, sum) =
    check (match E.For_testing.check ~reorder_after:0 solver with
      | E.Unsat _ -> true | _ -> false) "Alternate UNSAT recovery";
    E.pop solver;
    E.push solver;
    E.assert_ solver (T.equal terms sum (T.integer terms (Z.of_int 2)));
    check (E.For_testing.check
      ~reorder_after:0 solver = E.Sat) "Alternate SAT recovery";
    check (E.value solver sum = Some (E.Integer (Z.of_int 2)))
      "Alternate model recovery";
    E.pop solver;
    check (E.For_testing.check ~reorder_after:0 solver = E.Sat)
      "Alternate parent recovery" in
  let resource = R.create () in
  let solver, _, _ = setup () in
  check (match E.For_testing.check ~resource ~reorder_after:0 solver with
    | E.Unsat _ -> true | _ -> false) "Alternate cut fixture";
  let used = R.used resource in
  for budget = 0 to used - 1 do
    let ((solver, _, _) as fixture) = setup () in
    check (match E.For_testing.check ~resource:(R.create ~budget ())
      ~reorder_after:0 solver with E.Unknown _ -> true | _ -> false)
      "Alternate budget did not interrupt";
    recover fixture
  done;
  let callbacks = ref 0 in
  let solver, _, _ = setup () in
  ignore (E.For_testing.check
    ~reorder_after:0 ~resource:(R.create ~cancelled:(fun () ->
    incr callbacks; false) ()) solver);
  List.iter (fun raises ->
    for stop = 1 to !callbacks do
      let ((solver, _, _) as fixture) = setup () in
      let calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls;
        if !calls = stop then (if raises then raise Exit else true)
        else false) () in
      (match E.For_testing.check ~resource ~reorder_after:0 solver with
      | E.Unknown _ when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Alternate callback did not interrupt");
      recover fixture
    done) [false; true];
  Printf.printf "alternate cuts: %d budgets, %d callbacks recovered\n%!"
    used (2 * !callbacks)

let power_order_recovery () =
  let power degree variable =
    "(* " ^ String.concat " " (List.init degree (fun _ -> variable)) ^ ")" in
  let fixtures =
    List.concat_map (fun degree ->
      let x = power degree "x" and y = power degree "y" in
      let equality = "(= " ^ x ^ " " ^ y ^ ")" in
      ["(and (> x 0) (> y 0) (distinct x y) " ^ equality ^ ")", "unsat";
       "(and (> x y) (>= y 0) (<= " ^ x ^ " " ^ y ^ "))", "unsat";
       "(and (= x 2) (= y (- 2)) " ^ equality ^ ")",
       if degree land 1 = 0 then "sat" else "unsat"])
      [2; 3; 4; 5; 7; 8] in
  let selected count = List.sort_uniq Int.compare
      (0 :: (count - 1) :: List.init 17 (fun index -> count * index / 17)) in
  let make assertion = setup
      ("(declare-const x Int)(declare-const y Int)(push 1)(assert "
       ^ assertion ^ ")") in
  let recover frontend expected =
    check (run frontend "(check-sat)(pop 1)(check-sat)" = [expected; "sat"])
      "Power scope recovery";
    check (run frontend "(push 1)(assert (= x 2))(assert (= y (- 2)))\
      (check-sat)(get-value ((+ x y) (* x x) (* y y)))(pop 1)"
      = ["sat"; "(((+ x y) 0) ((* x x) 4) ((* y y) 4))"])
      "Power model recovery" in
  let expression = command "(check-sat)" in
  List.iteri (fun index (assertion, expected) ->
    let callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (F.execute ~resource (make assertion) expression = Some expected)
      "Power resource fixture";
    let used = R.used resource and callbacks = !callbacks in
    List.iter (fun budget ->
      let frontend = make assertion in
      let resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Power budget did not stop";
      check (R.used resource <= budget) "Power exceeded operation budget";
      recover frontend expected) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make assertion and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Power callback did not stop");
        recover frontend expected) (selected callbacks)) [false; true];
    Printf.printf "power%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected callbacks))) fixtures


module Boolean_polynomial = struct
  let command = S.List [S.Atom "check-sat"]
  let cube = "(= (+ (* x x x) (* y y y) (* z z z)) 4)"
  let declarations =
    "(declare-const x Int)(declare-const y Int)(declare-const z Int)\
     (declare-const p Bool)(declare-const q Bool)"
  let fixtures =
    ["(or p " ^ cube ^ ")";
     "(and (= p (not q)) (or q " ^ cube ^ ")\
       (or (not p) (= (* x x) 1)))";
     "(and p (= (* x y) 30) (>= x 5) (<= x 6) (>= y 5) (<= y 6))";
     "(and (not p) (= (* (ite p x y) (ite p x y)) 25))"]
  let setup formula =
    let frontend = F.create () in
    check (F.run frontend (declarations ^ "(push 1)(assert " ^ formula ^ ")")
      = []) "Boolean proposal setup";
    frontend
  let verify frontend formula =
    match F.run frontend ("(get-value (" ^ formula ^ "))") with
    | [response] ->
      (match S.parse response with
      | [S.List [S.List [_; S.Atom "true"]]] -> ()
      | _ -> failwith "Boolean proposal violates asserted formula")
    | _ -> failwith "Boolean proposal missing model"
  let recover frontend formula =
    check (F.run frontend "(check-sat)" = ["sat"])
      "Boolean proposal check recovery";
    verify frontend formula;
    check (F.run frontend
      "(push 1)(assert false)(check-sat)(pop 1)(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"; "sat"]) "Boolean proposal scope recovery";
    check (F.run frontend
      "(push 1)(assert (not p))(assert q)(assert (= x 2))\
       (check-sat)(get-value (p q x))(pop 1)"
      = ["sat"; "((p false) (q true) (x 2))"])
      "Boolean proposal alternate model recovery"
  let selected count = List.sort_uniq Int.compare
    (0 :: (count - 1) :: List.init 33 (fun index -> count * index / 33))
  let run () =
    List.iteri (fun index formula ->
      let frontend = setup formula and callbacks = ref 0 in
      let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
      check (F.execute ~resource frontend command = Some "sat")
        "Boolean proposal fixture";
      verify frontend formula;
      let used = R.used resource and callbacks = !callbacks in
      List.iter (fun budget ->
        let frontend = setup formula in
        check (F.execute ~resource:(R.create ~budget ()) frontend command
          = Some "unknown") "Boolean proposal budget ignored";
        recover frontend formula) (selected used);
      List.iter (fun raises ->
        List.iter (fun stop ->
          let frontend = setup formula and calls = ref 0 in
          let resource = R.create ~cancelled:(fun () ->
            let index = !calls in incr calls;
            if index = stop then (if raises then raise Exit else true)
            else false) () in
          (match F.execute ~resource frontend command with
          | Some "unknown" when not raises -> ()
          | exception Exit when raises -> ()
          | _ -> failwith "Boolean proposal cancellation ignored");
          recover frontend formula) (selected callbacks)) [false; true];
      Printf.printf "fixture%d work%d: %d budgets, %d callbacks recovered\n%!"
        index used (List.length (selected used))
        (2 * List.length (selected callbacks))) fixtures;
    let frontend = F.create () in
    check (F.run frontend (declarations
      ^ "(assert (! (or p " ^ cube ^ ") :named guarded))\
         (check-sat)(check-sat-assuming ((not p)))(get-unsat-assumptions)\
         (check-sat)(push 1)(assert (! (not p) :named denied))\
         (check-sat)(get-unsat-core)(pop 1)(check-sat)")
      = ["sat"; "unsat"; "((not p))"; "sat"; "unsat";
         "(guarded denied)"; "sat"])
      "Boolean proposal assumptions or named core";
    check (F.run frontend
      "(push 1)(assert p)(assert (not p))(check-sat)(pop 1)(check-sat)"
      = ["unsat"; "sat"]) "Boolean proposal inconsistent shared variable";
    print_endline "Boolean proposal regressions passed"
end

let overloaded_commands () =
  atomic "overloaded declaration"
    "(declare-fun f (Int) Int)(push 4)\
     (set-option :global-declarations true)"
    "(declare-fun f (Bool) Int)"
    (fun frontend -> rejected frontend "(assert (= (f true) 1))")
    (fun frontend ->
      check (run frontend
        "(assert (= (f true) 1))(check-sat)(pop 4)\
         (assert (= (f true) 2))(check-sat)" = ["sat"; "sat"])
        "Overloaded declaration recovery");
  atomic "overloaded application"
    "(declare-datatype Box (par (T) ((box (value T)))))\
     (declare-fun box (Bool) Int)(declare-fun box (Int) Bool)"
    "(assert (= ((as box (Box Int)) 7) ((as box (Box Int)) 7)))"
    (fun frontend ->
      check (run frontend "(get-assertions)" = ["()"])
        "Interrupted overload assertion survived")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"])
        "Overloaded application recovery");
  atomic "overloaded model"
    "(declare-const x Int)(declare-const x Bool)\
     (assert (= (as x Int) 7))(assert (as x Bool))(check-sat)"
    "(get-model)"
    (fun frontend ->
      check (run frontend "(get-value ((as x Int) (as x Bool)))"
        = ["(((as x Int) 7) ((as x Bool) true))"])
        "Interrupted overloaded model changed values")
    (fun frontend ->
      check (run frontend "(get-model)"
        = ["((define-fun x () Int 7)\n (define-fun x () Bool true))"])
        "Overloaded model recovery")

let finite_product_recovery () =
  let fixtures =
    ["(and (>= x (- 2)) (<= x 2) (>= y (- 2)) (<= y 2))",
     "(= (* x y) 3)";
     "(and (>= x (- 5)) (<= x 4) (>= y (- 3)) (<= y 2))",
     "(or (< (* x y) (- 12)) (> (* x y) 15))";
     "(and (>= x 2) (<= x 5) (>= y (- 4)) (<= y (- 2))\
       (>= z 3) (<= z 7))",
     "(or (< (* x y z) (- 140)) (> (* x y z) (- 12)))";
     "(and (>= x 2) (<= x 5) (>= y (- 4)) (<= y (- 2))\
       (>= z 3) (<= z 7))",
     "(or (< (* (- 3) x y z) 36) (> (* (- 3) x y z) 420))";
     "(and (>= x (- 2)) (<= x 3))",
     "(or (< (* x x x) (- 18)) (> (* x x x) 27))"] in
  let selected count = List.sort_uniq Int.compare
    (0 :: (count - 1) :: List.init 25 (fun index -> count * index / 25)) in
  let make (bounds, violation) = setup
    ("(set-option :produce-unsat-cores true)\
      (declare-const x Int)(declare-const y Int)(declare-const z Int)\
      (push 1)(assert (! " ^ bounds ^ " :named box))\
      (assert (! " ^ violation ^ " :named bad))") in
  let recover frontend =
    check (run frontend "(check-sat)(get-unsat-core)(pop 1)(check-sat)"
      = ["unsat"; "(box bad)"; "sat"]) "Finite product core or scope recovery";
    check (run frontend "(push 1)(assert (= x 6))(assert (= y (- 8)))\
      (assert (= z 4))(check-sat)(get-value ((* x y) (* x y z)))(pop 1)"
      = ["sat"; "(((* x y) (- 48)) ((* x y z) (- 192)))"])
      "Finite product model recovery" in
  let expression = command "(check-sat)" in
  List.iteri (fun index fixture ->
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource (make fixture) expression = Some "unsat")
      "Finite product fixture";
    let used = R.used resource and calls = !calls in
    List.iter (fun budget ->
      let frontend = make fixture in
      let resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Finite product budget";
      check (R.used resource <= budget) "Finite product shared budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make fixture and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Finite product callback");
        recover frontend) (selected calls)) [false; true];
    Printf.printf "interval%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected calls))) fixtures

let reclamation_recovery () =
  let integer terms n = T.integer terms (Z.of_int n) in
  let variable terms name sort =
    T.apply terms (T.symbol terms
      { T.name; arguments = [||]; result = sort;
        kind = T.Uninterpreted }) [||] in
  let setup mode =
    let terms = T.create () in
    let solver = E.create terms in
    let x = variable terms "x" T.Int in
    let y = variable terms "y" T.Int in
    let z = variable terms "z" T.Int in
    List.iter (E.register solver) [x; y; z];
    let add a b = T.builtin terms "+" T.Int [|a; b|] in
    let ge a b = T.builtin terms ">=" T.Bool [|a; b|] in
    let le a b = T.builtin terms "<=" T.Bool [|a; b|] in
    let n = integer terms in
    E.assert_ solver (T.equal terms x (add y (n 1)));
    E.assert_ solver (ge y (n 0));
    E.push solver;
    E.assert_ solver (T.equal terms z (add x (n 1)));
    E.assert_ solver (le y (n 10));
    E.push solver;
    for i = 0 to 1599 do
      let p = variable terms ("p" ^ string_of_int i) T.Bool in
      let q = variable terms ("q" ^ string_of_int i) T.Bool in
      E.assert_ solver (T.or_ terms [|p; q|])
    done;
    check (E.check solver = E.Sat) "temporary scope satisfiability";
    E.pop solver;
    check (E.check solver = E.Sat) "temporary scope pop";
    (match mode with
    | 0 -> ()
    | 1 -> E.assert_ solver (ge y (n 5))
    | 2 ->
      E.pop solver;
      E.assert_ solver (T.equal terms y (n 20));
      E.assert_ solver (T.equal terms z (n 0))
    | 3 ->
      E.push solver;
      E.assert_ solver (T.equal terms z (n 8))
    | _ -> invalid_arg "probe mode");
    let validate () =
      let read term = match E.value solver term with
        | Some (E.Integer value) -> value
        | _ -> failwith "missing reconstructed integer" in
      let xv = read x and yv = read y and zv = read z in
      check (Z.equal xv (Z.add yv Z.one)) "root model equation";
      if mode = 2 then begin
        check (Z.equal yv (Z.of_int 20)) "changed root model";
        check (Z.equal zv Z.zero) "popped substitution leaked"
      end else begin
        check (Z.equal zv (Z.add xv Z.one)) "child model equation";
        check (Z.compare yv Z.zero >= 0 && Z.compare yv (Z.of_int 10) <= 0)
          "child model bounds";
        if mode = 1 then check (Z.compare yv (Z.of_int 5) >= 0)
          "changed child bound";
        if mode = 3 then check (Z.equal zv (Z.of_int 8)) "new child model"
      end in
    let recover () =
      check (E.check solver = E.Sat) "reclaim recovery";
      validate ();
      E.push solver;
      E.assert_ solver (T.not_ terms (T.equal terms x (add y (n 1))));
      check (match E.check solver with E.Unsat _ -> true | _ -> false)
        "root equality lost on rebuild";
      E.pop solver;
      check (E.check solver = E.Sat) "scope recovery";
      validate ();
      if mode = 3 then E.pop solver;
      if mode <> 2 then begin
        E.pop solver;
        E.assert_ solver (T.equal terms y (n 20));
        E.assert_ solver (T.equal terms z (n 0));
        check (E.check solver = E.Sat) "retained frame pop";
        check (E.value solver x = Some (E.Integer (Z.of_int 21)))
          "root reconstruction after retained frame pop";
        check (E.value solver z = Some (E.Integer Z.zero))
          "retained frame substitution escaped"
      end in
    solver, validate, recover in
  for mode = 0 to 3 do
    let solver, validate, recover = setup mode in
    let callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (E.check ~resource solver = E.Sat) "initial reclaim";
    validate ();
    let used = R.used resource and count = !callbacks in
    recover ();
    let budgets = List.init used Fun.id in
    List.iter (fun budget ->
      let solver, _, recover = setup mode in
      let resource = R.create ~budget () in
      check (match E.check ~resource solver with E.Unknown _ -> true
        | E.Sat | E.Unsat _ -> false) "reclaim budget did not stop";
      check (R.used resource <= budget) "shared budget exceeded";
      recover ()) budgets;
    List.iter (fun raises ->
      for stop = 1 to count do
        let solver, _, recover = setup mode in
        let calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr calls;
          if !calls = stop then (if raises then raise Exit else true)
          else false) () in
        (match E.check ~resource solver with
        | E.Unknown "cancelled" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "reclaim callback did not stop");
        recover ()
      done) [false; true];
    Printf.printf "mode%d: %d work, %d budgets, %d callbacks recovered\n%!"
      mode used (List.length budgets) (2 * count)
  done

let bitvector_polynomial_recovery () =
  atomic "bitvector polynomial assertion"
    "(declare-const x (_ BitVec 32))(declare-const y (_ BitVec 32))\
     (declare-const z (_ BitVec 32))"
    "(assert (distinct (bvmul x (bvadd y z))\
       (bvadd (bvmul x y) (bvmul x z))))"
    (fun frontend -> check (run frontend "(check-sat)" = ["sat"])
      "Polynomial assertion rollback")
    (fun frontend -> check (run frontend "(check-sat)" = ["unsat"])
      "Polynomial assertion recovery")

let core_resources () =
  let first = "n" ^ String.make 4096 'a' in
  let second = "n" ^ String.make 2048 'b' in
  let initial = "(declare-const p Bool)(assert (! p :named " ^ first
    ^ "))(assert (! (not p) :named " ^ second ^ "))(check-sat)" in
  let expected = "(" ^ first ^ " " ^ second ^ ")" in
  let verify frontend =
    check (run frontend "(get-unsat-core)" = [expected])
      "Interrupted core serialization changed core" in
  atomic "core serialization" initial "(get-unsat-core)" verify verify;
  let symbol = "p" ^ String.make 4096 'x' in
  let initial = "(declare-const " ^ symbol ^ " Bool)(declare-const q Bool)"
    ^ "(assert (or " ^ symbol ^ " q))"
    ^ "(check-sat-assuming ((not " ^ symbol ^ ") (not q)))" in
  let expected = "((not " ^ symbol ^ ") (not q))" in
  let verify frontend =
    check (run frontend "(get-unsat-assumptions)" = [expected])
      "Interrupted assumption serialization changed core" in
  atomic "assumption serialization" initial "(get-unsat-assumptions)"
    verify verify;
  let count = 32 in
  let declarations = List.init count (fun i ->
    Printf.sprintf "(declare-const p%d Bool)" i) in
  let clauses = List.init (count - 1) (fun i ->
    Printf.sprintf "(assert (! (=> p%d p%d) :named step%d))" i (i + 1) i) in
  let initial = String.concat "" (declarations @ clauses)
    ^ "(assert (! p0 :named start))"
    ^ Printf.sprintf "(assert (! (not p%d) :named finish))" (count - 1) in
  let frontend = setup initial and resource = R.create () in
  let expression = command "(check-sat)" in
  check (F.execute ~resource frontend expression = Some "unsat")
    "Core projection setup";
  let used = R.used resource in
  let expected = run frontend "(get-unsat-core)" in
  for offset = 1 to min used 128 do
    let frontend = setup initial in
    let resource = R.create ~budget:(used - offset) () in
    check (F.execute ~resource frontend expression = Some "unknown")
      "Interrupted core projection did not stop";
    rejected frontend "(get-unsat-core)";
    check (run frontend "(check-sat)(get-unsat-core)" = "unsat" :: expected)
      "Core projection did not recover"
  done;
  let frontend = setup "(assert (! false :named contradiction))(check-sat)" in
  let ticks = ref 0 in
  let resource = R.create ~clock:(fun () ->
    incr ticks; if !ticks < 3 then 0. else 2.) ~timeout:1. () in
  (match F.execute ~resource frontend (command "(get-unsat-core)") with
  | exception R.Exhausted "timeout" -> ()
  | _ -> failwith "Core deadline did not cover completion");
  check (run frontend "(get-unsat-core)" = ["(contradiction)"])
    "Core deadline changed result"

let product_pair_recovery () =
  let fixtures = [{|
(set-option :produce-unsat-cores true) (declare-fun P_2 () Int) (declare-fun
P_3 () Int) (declare-fun dz () Int) (declare-fun rz () Int) (push 1) (assert
(! (<= (* 16777216 (- 1)) P_2) :named p0)) (assert (! (<= P_2 16777215)
:named p1)) (assert (! (<= (* 2097152 (- 1)) P_3) :named p2)) (assert (! (<=
P_3 2097151) :named p3)) (assert (! (let ((?v_1 (< P_2 0)) (?v_0 (< P_3 0)))
(let ((?v_5 (not ?v_0)) (?v_2 (not ?v_1)) (?v_6 (* 1 (- 1))) (?v_3 (- P_2 (*
16777216 (- 1))))) (let ((?v_4 (- ?v_6 (ite (< ?v_3 8388608) ?v_3 (- ?v_3
16777216)))) (?v_7 (- P_3 (* 2097152 (- 1))))) (let ((?v_8 (- ?v_6 (ite (<
?v_7 1048576) ?v_7 (- ?v_7 2097152))))) (let ((?v_9 (* (ite ?v_2 P_2 (+ (ite
(>= ?v_4 0) ?v_4 (+ ?v_4 16777216)) 1)) (ite ?v_5 P_3 (+ (ite (>= ?v_8 0)
?v_8 (+ ?v_8 2097152)) 1))))) (= (+ (* 140737488355328 dz) rz) (- (ite (not
(or (and ?v_1 ?v_5) (and ?v_0 ?v_2))) ?v_9 (- ?v_9)) (* P_2 P_3))))))))
:named p4)) (assert (! (> rz 0) :named p5)) (assert (! (< rz
140737488355328) :named p6))
|}; {|
(set-option :produce-unsat-cores true) (declare-fun P_2 () Int) (declare-fun
P_3 () Int) (declare-fun dz () Int) (declare-fun rz () Int) (push 1) (assert
(! (<= (* 16 (- 1)) P_2) :named p0)) (assert (! (<= P_2 15) :named p1))
(assert (! (<= (* 8 (- 1)) P_3) :named p2)) (assert (! (<= P_3 7) :named
p3)) (assert (! (let ((?v_1 (< P_2 0)) (?v_0 (< P_3 0))) (let ((?v_5 (not
?v_0)) (?v_2 (not ?v_1)) (?v_6 (* 1 (- 1))) (?v_3 (- P_2 (* 16 (- 1)))))
(let ((?v_4 (- ?v_6 (ite (< ?v_3 8) ?v_3 (- ?v_3 16)))) (?v_7 (- P_3 (* 8 (-
1))))) (let ((?v_8 (- ?v_6 (ite (< ?v_7 4) ?v_7 (- ?v_7 8))))) (let ((?v_9
(* (ite ?v_2 P_2 (+ (ite (>= ?v_4 0) ?v_4 (+ ?v_4 16)) 1)) (ite ?v_5 P_3 (+
(ite (>= ?v_8 0) ?v_8 (+ ?v_8 8)) 1))))) (= (+ (* 512 dz) rz) (- (ite (not
(or (and ?v_1 ?v_5) (and ?v_0 ?v_2))) ?v_9 (- ?v_9)) (* P_2 P_3))))))))
:named p4)) (assert (! (> rz 0) :named p5)) (assert (! (< rz 512) :named
p6))
|}] in
  let selected count = List.sort_uniq Int.compare
    (0 :: count - 1 :: List.init 31 (fun index -> count * index / 31)) in
  let make source = setup source in
  let recover frontend =
    check (run frontend "(check-sat)" = ["unsat"])
      "Product pair check recovery";
    (match run frontend "(get-unsat-core)" with
    | [response] -> (match S.parse response with
      | [S.List (_ :: _)] -> ()
      | _ -> failwith "Product pair core missing")
    | _ -> failwith "Product pair core missing");
    check (run frontend "(pop 1)(check-sat)(push 1)\
      (assert (= P_2 8))(assert (= P_3 (- 9)))\
      (assert (= dz 7))(assert (= rz (- 3)))\
      (check-sat)(get-value (P_2 P_3 dz rz))(pop 1)"
      = ["sat"; "sat"; "((P_2 8) (P_3 (- 9)) (dz 7) (rz (- 3)))"])
      "Product pair retained popped equalities" in
  let expression = command "(check-sat)" in
  List.iteri (fun index source ->
    let frontend = make source and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Product pair fixture";
    let used = R.used resource and calls = !calls in
    recover frontend;
    List.iter (fun budget ->
      let frontend = make source and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Product pair budget";
      check (R.used resource <= budget) "Product pair exceeded budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make source and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Product pair cancellation");
        recover frontend) (selected calls)) [false; true];
    Printf.printf "pair%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected calls))) fixtures

let bitvector_rewrite_recovery () =
  let declarations =
    "(declare-const x (_ BitVec 32))(declare-const y (_ BitVec 32))\
     (declare-const z (_ BitVec 32))\
     (declare-fun f ((_ BitVec 32)) (_ BitVec 32))" in
  List.iter (fun (name, formula) ->
    atomic ("bitvector " ^ name) declarations
      ("(assert (not " ^ formula ^ "))")
      (fun frontend -> check (run frontend "(check-sat)" = ["sat"])
        "Bitvector rewrite rollback")
      (fun frontend -> check (run frontend "(check-sat)" = ["unsat"])
        "Bitvector rewrite recovery"))
    ["unsigned bound", "(= (bvlshr (bvand x y) (bvor x z)) (_ bv0 32))";
     "Boolean ring", "(= (bvor x (bvand y x)) x)";
     "congruence", "(= (bvor x (bvmul y (bvneg y)))\
       (bvor x (bvneg (bvmul y y))))";
     "shift chain", "(= (bvlshr (bvlshr x y) z)\
       (bvlshr (bvlshr x z) y))";
     "function atoms", "(distinct (f (bvmul x (bvneg y)))\
       (bvadd (f (bvneg (bvmul x y))) (_ bv1 32)))";
     "disjoint addition", "(= (bvmul z (bvor x (bvshl y x)))\
       (bvmul z (bvadd x (bvshl y x))))"]

let factor_comparison_recovery () =
  let fixtures = [
    ({smt|(set-option :produce-unsat-cores true)
(declare-fun b__8 () Int)
(declare-fun a__2 () Int)
(declare-fun a__6 () Int)
(declare-fun a__3 () Int)
(declare-fun a__7 () Int)
(declare-fun a__5 () Int)
(declare-fun a__4 () Int)
(push 1)
(assert (! (>= (+ 1 (* (- 0 1) b__8)) 0) :named c0))
(assert (! (= (+ (* b__8 a__2) (* (- 0 1) a__2)) 0) :named c1))
(assert (! (>= (+ (* b__8 a__6) (* b__8 a__3 a__7) (* b__8 a__5 a__4 a__7)
(* (- 0 1) (* b__8 a__5))) 0) :named c2))
(assert (! (>= (+ (* a__3 a__2) (* a__5 a__4 a__2) (* (- 0 1) (* a__6
a__2)) (* (- 0 1) (* a__3 a__7 a__2)) (* (- 0 1) (* a__5 a__4 a__7 a__2)))
0) :named c3))
(assert (! (>= (+ (* a__5 a__2) (* (- 0 1) (* a__3 a__2)) (* (- 0 1) (*
a__5 a__4 a__2))) 0) :named c4))
(assert (! (not (and (= (+ (* a__3 a__2) (* a__5 a__4 a__2) (* (- 0 1) (*
a__6 a__2)) (* (- 0 1) (* a__3 a__7 a__2)) (* (- 0 1) (* a__5 a__4 a__7
a__2))) 0) (= (+ (* a__5 a__2) (* (- 0 1) (* a__3 a__2)) (* (- 0 1) (*
a__5 a__4 a__2))) 0))) :named c5))
(assert (! (>= b__8 0) :named c6))
(assert (! (>= 1 b__8) :named c7))
(assert (! (>= a__2 0) :named c8))
(assert (! (>= a__6 0) :named c9))
(assert (! (>= a__3 0) :named c10))
(assert (! (>= a__7 0) :named c11))
(assert (! (>= a__5 0) :named c12))
(assert (! (>= a__4 0) :named c13))|smt},
     "(push 1)(assert (= b__8 (- 4)))(assert (= a__2 (- 3)))(assert (= a__6 \
      (- 2)))(assert (= a__3 (- 1)))(assert (= a__7 0))(assert (= a__5 \
      1))(assert (= a__4 2))(check-sat)(get-value (b__8 a__2 a__6 a__3 \
      a__7 a__5 a__4))(pop 1)",
     "((b__8 (- 4)) (a__2 (- 3)) (a__6 (- 2)) (a__3 (- 1)) (a__7 0) \
      (a__5 1) (a__4 2))");
    ({smt|(set-option :produce-unsat-cores true)
(declare-fun b__9 () Int)
(declare-fun a__2 () Int)
(declare-fun a__4 () Int)
(declare-fun a__7 () Int)
(declare-fun a__6 () Int)
(declare-fun a__3 () Int)
(declare-fun a__5 () Int)
(declare-fun a__8 () Int)
(push 1)
(assert (! (>= (+ 1 (* (- 0 1) b__9)) 0) :named c0))
(assert (! (= (+ (* b__9 a__2) (* (- 0 1) a__2)) 0) :named c1))
(assert (! (>= (+ (* a__4 a__2) (* (- 0 1) (* a__7 a__2))) 0) :named c2))
(assert (! (>= (+ (* a__4 a__2) (* (- 0 1) a__2)) 0) :named c3))
(assert (! (>= (+ (* b__9 a__6) (* b__9 a__3 a__7) (* (- 0 1) (* b__9
a__3)) (* (- 0 1) (* b__9 a__3 a__5)) (* (- 0 1) (* b__9 a__6 a__4 a__5))
(* (- 0 1) (* b__9 a__6 a__7 a__4 a__5)) (* (- 0 1) (* b__9 a__8 a__5
a__5))) 0) :named c4))
(assert (! (>= (+ (* b__9 a__4 a__7) (* (- 0 1) (* b__9 a__7 a__7 a__4
a__5))) 0) :named c5))
(assert (! (>= (+ (* b__9 a__5 a__7) (* (- 0 1) (* b__9 a__4))) 0) :named
c6))
(assert (! (>= (+ (* a__3 a__2) (* (- 0 1) (* a__6 a__2))) 0) :named c7))
(assert (! (>= (* a__3 a__2) 0) :named c8))
(assert (! (not (and (= (+ (* a__3 a__2) (* (- 0 1) (* a__6 a__2))) 0) (=
(* a__3 a__2) 0))) :named c9))
(assert (! (>= b__9 0) :named c10))
(assert (! (>= 1 b__9) :named c11))
(assert (! (>= a__2 0) :named c12))
(assert (! (>= a__4 0) :named c13))
(assert (! (>= a__7 0) :named c14))
(assert (! (>= a__6 0) :named c15))
(assert (! (>= a__3 0) :named c16))
(assert (! (>= a__5 0) :named c17))
(assert (! (>= a__8 0) :named c18))|smt},
     "(push 1)(assert (= b__9 (- 4)))(assert (= a__2 (- 3)))(assert (= a__4 \
      (- 2)))(assert (= a__7 (- 1)))(assert (= a__6 0))(assert (= a__3 \
      1))(assert (= a__5 2))(assert (= a__8 3))(check-sat)(get-value \
      (b__9 a__2 a__4 a__7 a__6 a__3 a__5 a__8))(pop 1)",
     "((b__9 (- 4)) (a__2 (- 3)) (a__4 (- 2)) (a__7 (- 1)) (a__6 0) \
      (a__3 1) (a__5 2) (a__8 3))");
    ({smt|(set-option :produce-unsat-cores true)
(declare-fun a__2 () Int)
(declare-fun a__6 () Int)
(declare-fun a__5 () Int)
(declare-fun a__3 () Int)
(declare-fun a__8 () Int)
(declare-fun a__4 () Int)
(declare-fun a__7 () Int)
(declare-fun a__9 () Int)
(push 1)
(assert (! (>= (+ a__2 (* (- 0 1) (* a__6 a__2))) 0) :named c0))
(assert (! (>= (+ (* a__5 a__3) (* (- 0 1) (* a__8 a__5 a__2))) 0) :named
c1))
(assert (! (>= (+ (* a__6 a__3) (* (- 0 1) a__3)) 0) :named c2))
(assert (! (>= (+ (* a__8 a__5 a__2) (* (- 0 1) (* a__5 a__6 a__3))) 0)
:named c3))
(assert (! (>= (+ (* a__6 a__6 a__2) (* (- 0 1) a__2)) 0) :named c4))
(assert (! (>= (+ a__3 (* (- 0 1) (* a__6 a__6 a__3))) 0) :named c5))
(assert (! (>= (+ (* a__4 a__3) (* (- 0 1) (* a__4 a__2)) (* (- 0 1) (*
a__7 a__5 a__2))) 0) :named c6))
(assert (! (>= (+ (* a__4 a__2) (* a__7 a__5 a__2) (* a__4 a__6 a__2) (*
a__7 a__5 a__6 a__2) (* a__9 a__8 a__5 a__6 a__2) (* (- 0 1) (* a__4
a__3)) (* (- 0 1) (* a__7 a__5 a__3)) (* (- 0 1) (* a__9 a__8 a__5 a__3))
(* (- 0 1) (* a__4 a__6 a__3))) 0) :named c7))
(assert (! (not (and (= (+ (* a__4 a__3) (* (- 0 1) (* a__4 a__2)) (* (- 0
1) (* a__7 a__5 a__2))) 0) (= (+ (* a__4 a__2) (* a__7 a__5 a__2) (* a__4
a__6 a__2) (* a__7 a__5 a__6 a__2) (* a__9 a__8 a__5 a__6 a__2) (* (- 0 1)
(* a__4 a__3)) (* (- 0 1) (* a__7 a__5 a__3)) (* (- 0 1) (* a__9 a__8 a__5
a__3)) (* (- 0 1) (* a__4 a__6 a__3))) 0))) :named c8))
(assert (! (>= a__2 0) :named c9))
(assert (! (>= a__6 0) :named c10))
(assert (! (>= a__5 0) :named c11))
(assert (! (>= a__3 0) :named c12))
(assert (! (>= a__8 0) :named c13))
(assert (! (>= a__4 0) :named c14))
(assert (! (>= a__7 0) :named c15))
(assert (! (>= a__9 0) :named c16))|smt},
     "(push 1)(assert (= a__2 (- 4)))(assert (= a__6 (- 3)))(assert (= a__5 \
      (- 2)))(assert (= a__3 (- 1)))(assert (= a__8 0))(assert (= a__4 \
      1))(assert (= a__7 2))(assert (= a__9 3))(check-sat)(get-value \
      (a__2 a__6 a__5 a__3 a__8 a__4 a__7 a__9))(pop 1)",
     "((a__2 (- 4)) (a__6 (- 3)) (a__5 (- 2)) (a__3 (- 1)) (a__8 0) \
      (a__4 1) (a__7 2) (a__9 3))")] in
  let selected count = List.sort_uniq Int.compare
    (0 :: count - 1 :: List.init 7 (fun index -> count * index / 7)) in
  let expression = command "(check-sat)" in
  List.iteri (fun index (source, model_query, model) ->
    let make () = setup source in
    let recover frontend =
      check (run frontend "(check-sat)" = ["unsat"])
        "Factored check recovery";
      (match run frontend "(get-unsat-core)" with
      | [response] -> (match S.parse response with
        | [S.List (_ :: _)] -> ()
        | _ -> failwith "Factored core missing")
      | _ -> failwith "Factored core missing");
      check (run frontend "(pop 1)(check-sat)" = ["sat"])
        "Factored scope recovery";
      check (run frontend model_query = ["sat"; model])
        "Factored model recovery" in
    let frontend = make () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Factored comparison fixture";
    let used = R.used resource and calls = !calls in
    recover frontend;
    List.iter (fun budget ->
      let frontend = make () and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Factored comparison budget";
      check (R.used resource <= budget) "Factored comparison exceeded budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make () and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Factored comparison cancellation");
        recover frontend) (selected calls)) [false; true];
    Printf.printf "factor%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected calls))) fixtures

let zero_divisor_recovery () =
  List.iter (fun op ->
    let initial = Printf.sprintf
      {|(set-option :produce-unsat-cores true)
        (declare-const x Int)(declare-const y Int)
        (declare-const d Int)(declare-const p Bool)
        (assert (and (<= 0 x) (<= x 1) (<= 0 y) (<= y 1)))
        (assert (or (= d 0) (= d 1)))
        (assert (= (%s x 0) 7))(assert (= (%s y d) 9))
        (assert (=> p (= x y)))|} op op in
    let expression = command "(check-sat-assuming (p))" in
    let recover frontend =
      check (run frontend
        "(check-sat)(check-sat-assuming (p))(get-unsat-assumptions)"
        = ["sat"; "unsat"; "(p)"]) "Zero-divisor assumptions";
      check (run frontend
        "(push 1)(assert (! (= x y) :named eq))(check-sat)\
         (get-unsat-core)(pop 1)(check-sat)"
        = ["unsat"; "(eq)"; "sat"]) "Zero-divisor scopes";
      check (run frontend (Printf.sprintf
        "(push 1)(assert (= x 0))(assert (= y 1))(check-sat)\
         (get-value (x y d (%s x 0) (%s y d)))(pop 1)" op op)
        = ["sat"; Printf.sprintf
          "((x 0) (y 1) (d 0) ((%s x 0) 7) ((%s y d) 9))" op op])
        "Zero-divisor model" in
    let selected count = List.sort_uniq Int.compare
      (0 :: count - 1 :: List.init 31 (fun i -> count * i / 31)) in
    let frontend = setup initial and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Zero-divisor fixture";
    let used = R.used resource and calls = !calls in
    recover frontend;
    List.iter (fun budget ->
      let frontend = setup initial and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Zero-divisor budget";
      check (R.used resource <= budget) "Zero-divisor exceeded budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = setup initial and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Zero-divisor cancellation");
        recover frontend) (selected calls)) [false; true];
    Printf.printf
      "%s zero divisor used%d: %d budgets, %d callbacks recovered\n%!"
      op used (List.length (selected used))
      (2 * List.length (selected calls)))
    ["div"; "mod"]

let model_shared_rendering () =
  let exercise depth recovery =
    let initial = dag_source depth in
    let source = "(get-value (x))" in
    let recover frontend =
      match run frontend source with
      | [response] ->
        check (String.length response < 5000) "Expanded shared model";
        (match S.parse response with
        | [S.List [S.List [S.Atom "x"; value]]] ->
          let equality = "(= x " ^ S.to_string value ^ ")" in
          check (run frontend ("(get-value (" ^ equality ^ "))")
            = ["((" ^ equality ^ " true))"]) "Shared model round trip"
        | _ -> failwith "Shared model shape")
      | _ -> failwith "Shared model response" in
    let frontend = setup initial in
    recover frontend;
    (match run frontend "(get-model)" with
    | [response] ->
      check (String.length response < 5000) "Expanded shared definition"
    | _ -> failwith "Shared model definition");
    if recovery then
      atomic "shared model serialization" initial source recover recover
  in
  exercise 30 false;
  exercise 8 true;
  let declaration =
    "(declare-datatype T ((|@arg0|) (Other)))" in
  let frontend = setup
    (declaration ^ "(declare-fun f (Int) T)\
     (assert (= (f 1) |@arg0|))(assert (= (f 2) Other))(check-sat)") in
  match run frontend "(get-model)" with
  | [response] ->
    (match S.parse response with
    | [S.List definitions] ->
      let restored = setup (declaration
        ^ String.concat "" (List.map S.to_string definitions)
        ^ "(check-sat)") in
      check (run restored
        "(get-value ((= (f 1) |@arg0|) (= (f 2) Other)))"
        = ["(((= (f 1) |@arg0|) true) ((= (f 2) Other) true))"])
        "Captured model parameter"
    | _ -> failwith "Model definition shape")
  | _ -> failwith "Model definition response"

let model_rendering_equality () =
  let terms = T.create () in
  let sort = T.Named 0 in
  let symbol name arguments = T.symbol terms
    { name; arguments; result = sort; kind = T.Constructor 0 } in
  let leaf = symbol "Leaf" [|T.Int|] in
  let next = symbol "Next" [|sort|] in
  let pair = symbol "Pair" [|sort; sort|] in
  let solver = E.create terms in
  check (E.check solver = E.Sat) "Empty model";
  let model = M.create ~terms ~solver in
  let build integer depth unary =
    let value = ref (M.Constructor (leaf, [|M.Integer (Z.of_int integer)|])) in
    for _ = 1 to depth do
      value := if unary then M.Constructor (next, [|!value|])
        else M.Constructor (pair, [|!value; !value|])
    done;
    !value in
  List.iter (fun unary ->
    let left = build 42 25 unary and right = build 42 25 unary in
    let different = build 43 25 unary in
    let equal ?resource () = M.equal ?resource model left right in
    check (equal ()) "Shared equality";
    check (not (M.equal model left different)) "Shared inequality";
    let resource = R.create () in
    check (equal ~resource ()) "Measured equality";
    let used = R.used resource in
    for budget = 0 to used - 1 do
      (match equal ~resource:(R.create ~budget ()) () with
      | exception R.Exhausted _ -> ()
      | _ -> failwith "Equality did not exhaust");
      check (equal ()) "Equality budget recovery"
    done;
    let calls = ref 0 in
    ignore (equal ~resource:(R.create
      ~cancelled:(fun () -> incr calls; false) ()) ());
    let count = !calls in
    List.iter (fun raises ->
      for stop = 1 to count do
        let calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr calls;
          if !calls = stop then (if raises then raise Exit else true)
          else false) () in
        (match equal ~resource () with
        | exception Exit when raises -> ()
        | exception R.Exhausted _ when not raises -> ()
        | _ -> failwith "Equality did not cancel");
        check (equal ()) "Equality callback recovery"
      done) [false; true];
    Printf.printf "unary=%b: %d budgets, %d callbacks recovered\n%!"
      unary used (2 * count)) [true; false];
  let left = build 42 100000 true and right = build 42 100000 true in
  check (M.equal model left right) "Deep equality";
  let leaf = build 42 0 true in
  let fields = [|leaf; leaf|] in
  let cyclic = M.Constructor (pair, fields) in
  fields.(1) <- M.Constructor (next, [|cyclic|]);
  List.iter (fun (left, right) ->
    match M.equal model left right with
    | exception M.Unavailable _ -> ()
    | _ -> failwith "Mixed cycle accepted")
    [cyclic, cyclic; cyclic, leaf; leaf, cyclic]

let monomial_recovery () =
  let fixtures = [
    ("(set-option :produce-models true) (set-option :produce-unsat-cores \
      true) (declare-fun b__8 () Int) (declare-fun a__3 () Int) \
      (declare-fun a__2 () Int) (declare-fun a__6 () Int) (declare-fun a__7 \
      () Int) (declare-fun a__5 () Int) (declare-fun a__4 () Int) (push 1) \
      (assert (! (>= (+ 1 (* (- 0 1) b__8)) 0) :named m0)) (assert (! (= (+ \
      (* b__8 a__3) (* (- 0 1) a__3)) 0) :named m1)) (assert (! (>= (+ a__2 \
      (* (- 0 1) a__3)) 0) :named m2)) (assert (! (>= (+ a__2 (* (- 0 1) (* \
      a__6 a__3))) 0) :named m3)) (assert (! (>= (+ (* (- 0 1) (* b__8 a__7 \
      a__5 a__6)) (* b__8 a__7 a__6 a__6) (* (- 0 1) (* b__8 a__4 a__5)) (* \
      (- 0 1) (* b__8 a__4 a__5 a__5)) (* (- 0 1) (* b__8 a__7 a__5 a__5 \
      a__5)) (* (- 0 1) (* b__8 a__7 a__6 a__5 a__5))) 0) :named m4)) \
      (assert (! (>= (+ (* b__8 a__5) (* (- 0 1) (* b__8 a__6 a__6))) 0) \
      :named m5)) (assert (! (>= (+ (* a__4 a__3) (* a__7 a__5 a__3) (* \
      a__7 a__6 a__3) (* (- 0 1) (* a__7 a__2))) 0) :named m6)) (assert (! \
      (>= (+ (* a__7 a__6 a__3) (* (- 0 1) (* a__4 a__2)) (* (- 0 1) (* \
      a__4 a__5 a__2)) (* (- 0 1) (* a__7 a__5 a__5 a__2)) (* (- 0 1) (* \
      a__7 a__6 a__5 a__2)) (* (- 0 1) (* a__7 a__6 a__2))) 0) :named m7)) \
      (assert (! (not (and (= (+ (* a__4 a__3) (* a__7 a__5 a__3) (* a__7 \
      a__6 a__3) (* (- 0 1) (* a__7 a__2))) 0) (= (+ (* a__7 a__6 a__3) (* \
      (- 0 1) (* a__4 a__2)) (* (- 0 1) (* a__4 a__5 a__2)) (* (- 0 1) (* \
      a__7 a__5 a__5 a__2)) (* (- 0 1) (* a__7 a__6 a__5 a__2)) (* (- 0 1) \
      (* a__7 a__6 a__2))) 0))) :named m8)) (assert (! (>= b__8 0) :named \
      m9)) (assert (! (>= 1 b__8) :named m10)) (assert (! (>= a__3 0) \
      :named m11)) (assert (! (>= a__2 0) :named m12)) (assert (! (>= a__6 \
      0) :named m13)) (assert (! (>= a__7 0) :named m14)) (assert (! (>= \
      a__5 0) :named m15)) (assert (! (>= a__4 0) :named m16))",
     "(push 1)(assert (= b__8 (- 4)))(assert (= a__3 (- 3)))(assert (= a__2 \
      (- 2)))(assert (= a__6 (- 1)))(assert (= a__7 0))(assert (= a__5 \
      1))(assert (= a__4 2))(check-sat)(get-value (b__8 a__3 a__2 a__6 a__7 \
      a__5 a__4))(pop 1)",
     "((b__8 (- 4)) (a__3 (- 3)) (a__2 (- 2)) (a__6 (- 1)) (a__7 0) (a__5 \
      1) (a__4 2))");
    ("(set-option :produce-models true) (set-option :produce-unsat-cores \
      true) (declare-fun a__4 () Int) (declare-fun a__6 () Int) \
      (declare-fun a__3 () Int) (declare-fun a__7 () Int) (declare-fun a__5 \
      () Int) (declare-fun a__2 () Int) (push 1) (assert (! (>= (+ (* a__4 \
      a__6 a__3) (* a__7 a__5 a__6 a__3) (* (- 0 1) (* a__4 a__5 a__2)) (* \
      (- 0 1) (* a__7 a__5 a__5 a__2)) (* (- 0 1) (* a__4 a__6 a__2)) (* (- \
      0 1) (* a__7 a__6 a__6 a__2)) (* (- 0 1) (* a__7 a__5 a__3)) (- 0 1)) \
      0) :named m0)) (assert (! (>= (+ (* a__5 a__2) (* (- 0 1) (* a__6 \
      a__3))) 0) :named m1)) (assert (! (>= (+ (* a__6 a__2) (* (- 0 1) (* \
      a__6 a__5 a__2))) 0) :named m2)) (assert (! (>= (+ (* a__5 a__3) (* \
      a__6 a__6 a__3) (* (- 0 1) (* a__5 a__6 a__2))) 0) :named m3)) \
      (assert (! (>= a__4 0) :named m4)) (assert (! (>= a__6 0) :named m5)) \
      (assert (! (>= a__3 0) :named m6)) (assert (! (>= a__7 0) :named m7)) \
      (assert (! (>= a__5 0) :named m8)) (assert (! (>= a__2 0) :named m9))",
     "(push 1)(assert (= a__4 (- 3)))(assert (= a__6 (- 2)))(assert (= a__3 \
      (- 1)))(assert (= a__7 0))(assert (= a__5 1))(assert (= a__2 \
      2))(check-sat)(get-value (a__4 a__6 a__3 a__7 a__5 a__2))(pop 1)",
     "((a__4 (- 3)) (a__6 (- 2)) (a__3 (- 1)) (a__7 0) (a__5 1) (a__2 2))");
    ("(set-option :produce-models true) (set-option :produce-unsat-cores \
      true) (declare-fun b__8 () Int) (declare-fun a__2 () Int) \
      (declare-fun a__3 () Int) (declare-fun a__5 () Int) (declare-fun a__7 \
      () Int) (declare-fun a__6 () Int) (declare-fun a__4 () Int) (push 1) \
      (assert (! (>= (+ 1 (* (- 0 1) b__8)) 0) :named m0)) (assert (! (= (+ \
      (* b__8 a__2) (* (- 0 1) a__2)) 0) :named m1)) (assert (! (>= (+ a__3 \
      (* (- 0 1) a__2)) 0) :named m2)) (assert (! (>= (+ a__3 (* (- 0 1) (* \
      a__5 a__2))) 0) :named m3)) (assert (! (>= (+ (* b__8 a__7 a__5 a__5) \
      (* (- 0 1) (* b__8 a__7 a__6 a__5)) (* (- 0 1) (* b__8 a__4 a__6)) (* \
      (- 0 1) (* b__8 a__4 a__6 a__6)) (* (- 0 1) (* b__8 a__7 a__5 a__6 \
      a__6)) (* (- 0 1) (* b__8 a__7 a__6 a__6 a__6))) 0) :named m4)) \
      (assert (! (>= (+ (* b__8 a__6) (* (- 0 1) (* b__8 a__5 a__5))) 0) \
      :named m5)) (assert (! (>= (+ (* a__4 a__2) (* a__7 a__5 a__2) (* \
      a__7 a__6 a__2) (* (- 0 1) (* a__7 a__3))) 0) :named m6)) (assert (! \
      (>= (+ (* a__7 a__5 a__2) (* (- 0 1) (* a__4 a__3)) (* (- 0 1) (* \
      a__7 a__5 a__3)) (* (- 0 1) (* a__4 a__6 a__3)) (* (- 0 1) (* a__7 \
      a__5 a__6 a__3)) (* (- 0 1) (* a__7 a__6 a__6 a__3))) 0) :named m7)) \
      (assert (! (not (and (= (+ (* a__4 a__2) (* a__7 a__5 a__2) (* a__7 \
      a__6 a__2) (* (- 0 1) (* a__7 a__3))) 0) (= (+ (* a__7 a__5 a__2) (* \
      (- 0 1) (* a__4 a__3)) (* (- 0 1) (* a__7 a__5 a__3)) (* (- 0 1) (* \
      a__4 a__6 a__3)) (* (- 0 1) (* a__7 a__5 a__6 a__3)) (* (- 0 1) (* \
      a__7 a__6 a__6 a__3))) 0))) :named m8)) (assert (! (>= b__8 0) :named \
      m9)) (assert (! (>= 1 b__8) :named m10)) (assert (! (>= a__2 0) \
      :named m11)) (assert (! (>= a__3 0) :named m12)) (assert (! (>= a__5 \
      0) :named m13)) (assert (! (>= a__7 0) :named m14)) (assert (! (>= \
      a__6 0) :named m15)) (assert (! (>= a__4 0) :named m16))",
     "(push 1)(assert (= b__8 (- 4)))(assert (= a__2 (- 3)))(assert (= a__3 \
      (- 2)))(assert (= a__5 (- 1)))(assert (= a__7 0))(assert (= a__6 \
      1))(assert (= a__4 2))(check-sat)(get-value (b__8 a__2 a__3 a__5 a__7 \
      a__6 a__4))(pop 1)",
     "((b__8 (- 4)) (a__2 (- 3)) (a__3 (- 2)) (a__5 (- 1)) (a__7 0) (a__6 \
      1) (a__4 2))");
    ("(set-option :produce-models true) (set-option :produce-unsat-cores \
      true) (declare-fun a__7 () Int) (declare-fun a__3 () Int) \
      (declare-fun a__4 () Int) (declare-fun a__5 () Int) (declare-fun a__2 \
      () Int) (declare-fun a__6 () Int) (push 1) (assert (! (>= (+ (* a__7 \
      a__3) (* (- 0 1) (* a__4 a__5 a__2)) (* (- 0 1) (* a__7 a__5 a__5 \
      a__2)) (* (- 0 1) (* a__4 a__6 a__5 a__2)) (* (- 0 1) (* a__7 a__5 \
      a__5 a__6 a__2)) (* (- 0 1) (* a__7 a__6 a__6 a__5 a__2)) (- 0 1)) 0) \
      :named m0)) (assert (! (>= (+ (* a__5 a__2) (* (- 0 1) a__3)) 0) \
      :named m1)) (assert (! (>= a__7 0) :named m2)) (assert (! (>= a__3 0) \
      :named m3)) (assert (! (>= a__4 0) :named m4)) (assert (! (>= a__5 0) \
      :named m5)) (assert (! (>= a__2 0) :named m6)) (assert (! (>= a__6 0) \
      :named m7))",
     "(push 1)(assert (= a__7 (- 3)))(assert (= a__3 (- 2)))(assert (= a__4 \
      (- 1)))(assert (= a__5 0))(assert (= a__2 1))(assert (= a__6 \
      2))(check-sat)(get-value (a__7 a__3 a__4 a__5 a__2 a__6))(pop 1)",
     "((a__7 (- 3)) (a__3 (- 2)) (a__4 (- 1)) (a__5 0) (a__2 1) (a__6 2))")] in
  let selected count = List.sort_uniq Int.compare
    (0 :: count - 1 :: List.init 7 (fun index -> count * index / 7)) in
  let expression = command "(check-sat)" in
  List.iteri (fun index (source, model_query, model) ->
    let make () = setup source in
    let recover frontend =
      check (run frontend "(check-sat)" = ["unsat"])
        "Monomial check recovery";
      (match run frontend "(get-unsat-core)" with
      | [response] -> (match S.parse response with
        | [S.List (_ :: _)] -> ()
        | _ -> failwith "Monomial core missing")
      | _ -> failwith "Monomial core missing");
      check (run frontend "(pop 1)(check-sat)" = ["sat"])
        "Monomial scope recovery";
      check (run frontend model_query = ["sat"; model])
        "Monomial model recovery" in
    let frontend = make () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Monomial comparison fixture";
    let used = R.used resource and calls = !calls in
    recover frontend;
    List.iter (fun budget ->
      let frontend = make () and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Monomial comparison budget";
      check (R.used resource <= budget) "Monomial comparison exceeded budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make () and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Monomial comparison cancellation");
        recover frontend) (selected calls)) [false; true];
    Printf.printf "monomial%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected calls))) fixtures


let quadratic_recovery () =
  let fixtures = [
    ("(set-option :produce-models true)(set-option :produce-unsat-cores \
      true)(declare-const p Int)(declare-const x Int)(declare-const y \
      Int)(push 1)(assert (! (>= p 4294967296) :named q0))(assert (! (>= x \
      0) :named q1))(assert (! (>= y 0) :named q2))(assert (! (< x p) \
      :named q3))(assert (! (< y p) :named q4))(assert (! (>= (* x y) (* p \
      p)) :named q5))",
     "(push 1)(assert (= p (- 2)))(assert (= x (- 1)))(assert (= y \
      0))(check-sat)(get-value (p x y))(pop 1)",
     "((p (- 2)) (x (- 1)) (y 0))");
    ("(set-option :produce-models true)(set-option :produce-unsat-cores \
      true)(declare-const p Int)(declare-const q Int)(declare-const x \
      Int)(declare-const y Int)(push 1)(assert (! (>= x 0) :named \
      q0))(assert (! (>= y 0) :named q1))(assert (! (<= x p) :named \
      q2))(assert (! (<= y q) :named q3))(assert (! (> (* x y) (* p q)) \
      :named q4))",
     "(push 1)(assert (= p (- 2)))(assert (= q (- 1)))(assert (= x \
      0))(assert (= y 1))(check-sat)(get-value (p q x y))(pop 1)",
     "((p (- 2)) (q (- 1)) (x 0) (y 1))");
    ("(set-option :produce-models true)(set-option :produce-unsat-cores \
      true)(declare-const p Int)(declare-const q Int)(declare-const x \
      Int)(declare-const y Int)(push 1)(assert (! (>= p 1) :named \
      q0))(assert (! (>= q 1) :named q1))(assert (! (>= x 0) :named \
      q2))(assert (! (>= y 0) :named q3))(assert (! (< x p) :named \
      q4))(assert (! (<= y q) :named q5))(assert (! (>= (* x y) (* p q)) \
      :named q6))",
     "(push 1)(assert (= p (- 2)))(assert (= q (- 1)))(assert (= x \
      0))(assert (= y 1))(check-sat)(get-value (p q x y))(pop 1)",
     "((p (- 2)) (q (- 1)) (x 0) (y 1))")] in
  let selected count = List.sort_uniq Int.compare
    (0 :: count - 1 :: List.init 7 (fun index -> count * index / 7)) in
  let expression = command "(check-sat)" in
  List.iteri (fun index (source, model_query, model) ->
    let make () = setup source in
    let recover frontend =
      check (run frontend "(check-sat)" = ["unsat"])
        "Quadratic check recovery";
      (match run frontend "(get-unsat-core)" with
      | [response] -> (match S.parse response with
        | [S.List (_ :: _)] -> ()
        | _ -> failwith "Quadratic core missing")
      | _ -> failwith "Quadratic core missing");
      check (run frontend "(pop 1)(check-sat)" = ["sat"])
        "Quadratic scope recovery";
      check (run frontend model_query = ["sat"; model])
        "Quadratic model recovery" in
    let frontend = make () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Quadratic comparison fixture";
    let used = R.used resource and calls = !calls in
    recover frontend;
    List.iter (fun budget ->
      let frontend = make () and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Quadratic comparison budget";
      check (R.used resource <= budget) "Quadratic comparison exceeded budget";
      recover frontend) (selected used);
    List.iter (fun raises ->
      List.iter (fun stop ->
        let frontend = make () and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          let current = !calls in incr calls;
          if current = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "Quadratic comparison cancellation");
        recover frontend) (selected calls)) [false; true];
    Printf.printf "quadratic%d used%d: %d budgets and %d callbacks recovered\n%!"
      index used (List.length (selected used))
      (2 * List.length (selected calls))) fixtures


let candidate_division_recovery () =
  let source =
    "(set-option :produce-models true)\
     (set-option :produce-unsat-cores true)\
     (declare-const i1 Int)(declare-const i2 Int)(push 1)\
     (assert (! (>= (div 63 (div i2 (* i1 67 i2))) 63) :named a))\
     (assert (! (< 67 i2) :named b))" in
  let expression = command "(check-sat)" in
  let request =
    "(get-value (i1 i2 (* i1 67 i2) (div i2 (* i1 67 i2))\
     (mod i2 (* i1 67 i2)) (div 63 (div i2 (* i1 67 i2)))\
     (mod 63 (div i2 (* i1 67 i2)))))" in
  let rec integer = function
    | S.Atom value -> Z.of_string value
    | S.List [S.Atom "-"; value] -> Z.neg (integer value)
    | _ -> failwith "Candidate division integer model" in
  let model frontend =
    let values = match run frontend request with
      | [response] -> (match S.parse response with
        | [S.List pairs] -> List.map (function
          | S.List [_; value] -> integer value
          | _ -> failwith "Candidate division value pair") pairs
        | _ -> failwith "Candidate division values")
      | _ -> failwith "Candidate division response" in
    let division x y q r =
      if not (Z.is_zero y) then begin
        let eq, er = Z.div_rem x y in
        check (Z.equal q eq && Z.equal r er) "Candidate division meaning"
      end in
    match values with
    | [i1; i2; product; q; r; outer_q; outer_r] ->
      check (Z.equal product (Z.mul (Z.mul i1 (Z.of_int 67)) i2))
        "Candidate product meaning";
      division i2 product q r;
      division (Z.of_int 63) q outer_q outer_r;
      check (Z.compare i2 (Z.of_int 67) > 0
        && Z.compare outer_q (Z.of_int 63) >= 0)
        "Candidate division assertions"
    | _ -> failwith "Candidate division model size" in
  let recover frontend =
    check (run frontend "(check-sat)" = ["sat"])
      "Candidate division check recovery";
    model frontend;
    check (run frontend "(push 1)(assert (! (<= i2 67) :named c))\
      (check-sat)" = ["unsat"]) "Candidate division contradiction";
    (match run frontend "(get-unsat-core)" with
    | [response] -> (match S.parse response with
      | [S.List names] ->
        check (List.mem (S.Atom "b") names && List.mem (S.Atom "c") names)
          "Candidate division core"
      | _ -> failwith "Candidate division core response")
    | _ -> failwith "Candidate division core missing");
    check (run frontend "(pop 1)(check-sat)" = ["sat"])
      "Candidate division scope recovery";
    model frontend in
  let selected count = List.sort_uniq Int.compare
    (0 :: count - 1 :: List.init 15 (fun i -> count * i / 15)) in
  let frontend = setup source and calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  check (F.execute ~resource frontend expression = Some "sat")
    "Candidate division fixture";
  let used = R.used resource and calls = !calls in
  recover frontend;
  List.iter (fun budget ->
    let frontend = setup source and resource = R.create ~budget () in
    check (F.execute ~resource frontend expression = Some "unknown")
      "Candidate division budget";
    check (R.used resource <= budget) "Candidate division exceeded budget";
    recover frontend) (selected used);
  List.iter (fun raises ->
    List.iter (fun stop ->
      let frontend = setup source and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        let current = !calls in incr calls;
        if current = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Candidate division cancellation");
      recover frontend) (selected calls)) [false; true];
  Printf.printf "Candidate division used%d: %d budgets, %d callbacks recovered\n%!"
    used (List.length (selected used)) (2 * List.length (selected calls))


let constant_division_proposal_recovery () =
  let formulas = [
    "(= (* (- x (* x y)) (- z 1) (- x (* x y)) (- z 1))\
       (div (- (* z (- x)) 2) 2))";
    "(and (not (= y 0)) (not (= x 0))\
       (= (* x x) (div (- (* y y)) (- 2))))";
    "(and (not (= y 0)) (not (= x 0))\
       (= (* x x) (mod (- (* y y)) 2)))";
    "(and (not (= y 0)) (not (= x 0))\
       (= (* x x) (mod (- (* y y)) (- 2))))";
    "(and (not (= y 0)) (not (= x 0))\
       (= (* x x) (div (* y y) 2)))"] in
  List.iteri (fun index formula ->
    let source = "(set-option :produce-models true)\
      (declare-const x Int)(declare-const y Int)(declare-const z Int)\
      (assert " ^ formula ^ ")" in
    let verify frontend =
      match run frontend ("(get-value (" ^ formula ^ "))") with
      | [response] ->
        (match S.parse response with
        | [S.List [S.List [_; S.Atom "true"]]] -> ()
        | _ -> failwith "Constant-division proposal model")
      | _ -> failwith "Constant-division proposal response" in
    let recover frontend =
      check (run frontend "(check-sat)" = ["sat"])
        "Constant-division proposal retry";
      verify frontend;
      check (run frontend ("(push 1)(assert (not " ^ formula ^ "))\
        (check-sat)(pop 1)(check-sat)") = ["unsat"; "sat"])
        "Constant-division proposal scopes";
      verify frontend in
    let expression = command "(check-sat)" in
    let frontend = setup source and callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (F.execute ~resource frontend expression = Some "sat")
      "Constant-division proposal fixture";
    verify frontend;
    let used = R.used resource and callbacks = !callbacks in
    let selected count = List.sort_uniq Int.compare
      (0 :: count - 1 :: List.init 31 (fun i -> count * i / 31)) in
    List.iter (fun budget ->
      let frontend = setup source and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Constant-division proposal budget";
      check (R.used resource <= budget) "Constant-division budget exceeded";
      recover frontend) (selected used);
    List.iter (fun raises -> List.iter (fun stop ->
      let frontend = setup source and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        let current = !calls in incr calls;
        if current = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Constant-division proposal cancellation");
      recover frontend) (selected callbacks)) [false; true];
    Printf.printf "constant division %d: %d work, %d callbacks recovered\n%!"
      index used callbacks) formulas

let one_bit_recovery () =
  let prefix = "(set-option :produce-models true)\
    (declare-const x (_ BitVec 4))(declare-const y (_ BitVec 4))\
    (define-fun p ((v (_ BitVec 4))) Bool\
      (= (bvand v (bvsub v (_ bv1 4))) (_ bv0 4)))" in
  let source = prefix ^ "(assert (p (bvadd x y)))" in
  let cases = [source, "sat";
    source ^ "(assert (p x))(assert (p y))\
      (assert (distinct x y))(assert (distinct x (_ bv0 4)))\
      (assert (distinct y (_ bv0 4)))", "unsat"] in
  List.iter (fun (source, expected) ->
    let expression = command "(check-sat)" in
    let make () = setup source in
    let recover frontend =
      check (run frontend "(check-sat)" = [expected])
        "One-bit recovery";
      if expected = "sat" then
        check (run frontend "(get-value ((p (bvadd x y))))"
          = ["(((p (bvadd x y)) true))"]) "One-bit model";
      check (run frontend "(push 1)(assert false)(check-sat)(pop 1)"
        = ["unsat"]) "One-bit scope conflict";
      check (run frontend "(check-sat)" = [expected]) "One-bit scope recovery"
    in
    let frontend = make () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some expected)
      "One-bit fixture";
    let used = R.used resource and callbacks = !calls in
    recover frontend;
    for budget = 0 to used - 1 do
      let frontend = make () and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "One-bit budget";
      check (R.used resource <= budget) "One-bit budget exceeded";
      recover frontend
    done;
    List.iter (fun raises ->
      for stop = 1 to callbacks do
        let frontend = make () and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr calls;
          if !calls = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "One-bit cancellation");
        recover frontend
      done) [false; true];
    Printf.printf "one-bit %s: %d budgets, %d callbacks recovered\n%!"
      expected used (2 * callbacks)) cases
let variable_division_proposal_recovery () =
  let formulas = [
    "(and (> x 0) (> y 0) (< x 5) (< y 5) (< (div x y) 3))";
    "(>= (+ (* y z (- (- 4) x)) (- 1)) 0 (div x y))";
    "(and (> y 0) (= (div (- (* x x) 1) y) (- 1)))";
    "(and (< y 0) (= (div (- (* x x) 1) y) 1))";
    "(and (< y 0) (= (mod (- (* x x) 1) y) 1))";
    "(and (> y 0) (= (mod (- (* x x) 1) y) 1))";
    "(or (= y 0) (= (div (* x x) y) 2))";
    "(and (not (= y 0)) (= (div (* 1267650600228229401496703205376 \
       x x) y) 1267650600228229401496703205376))"] in
  List.iteri (fun index formula ->
    let source = "(set-option :produce-models true)\
      (declare-const x Int)(declare-const y Int)(declare-const z Int)\
      (assert " ^ formula ^ ")" in
    let verify frontend =
      match run frontend ("(get-value (" ^ formula ^ "))") with
      | [response] ->
        (match S.parse response with
        | [S.List [S.List [_; S.Atom "true"]]] -> ()
        | _ -> failwith "Variable-division proposal model")
      | _ -> failwith "Variable-division proposal response" in
    let recover frontend =
      check (run frontend "(check-sat)" = ["sat"])
        "Variable-division proposal retry";
      verify frontend;
      check (run frontend ("(push 1)(assert (not " ^ formula ^ "))\
        (check-sat)(pop 1)(check-sat)") = ["unsat"; "sat"])
        "Variable-division proposal scopes";
      verify frontend in
    let expression = command "(check-sat)" in
    let frontend = setup source and callbacks = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr callbacks; false) () in
    check (F.execute ~resource frontend expression = Some "sat")
      "Variable-division proposal fixture";
    verify frontend;
    let used = R.used resource and callbacks = !callbacks in
    let selected count = List.sort_uniq Int.compare
      (0 :: count - 1 :: List.init 31 (fun i -> count * i / 31)) in
    List.iter (fun budget ->
      let frontend = setup source and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Variable-division proposal budget";
      check (R.used resource <= budget) "Variable-division budget exceeded";
      recover frontend) (selected used);
    List.iter (fun raises -> List.iter (fun stop ->
      let frontend = setup source and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        let current = !calls in incr calls;
        if current = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Variable-division proposal cancellation");
      recover frontend) (selected callbacks)) [false; true];
    Printf.printf "variable division %d: %d work, %d callbacks recovered\n%!"
      index used callbacks) formulas


module Projected_functions = struct
module T = Vox_smt_term
module E = Vox_smt_engine
module M = Vox_smt_model
module R = Vox_smt_resource
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message
let integer = function M.Integer z -> z | _ -> failwith "Integer model"
let variable terms name = T.apply terms (T.symbol terms
  {name;arguments=[||];result=T.Int;kind=T.Uninterpreted}) [||]
let lookup (entries,fallback) key =
  match List.find_opt (fun (args,_) -> match args with
    | [|M.Integer z|] -> Z.equal z key | _ -> false) entries with
  | Some (_,value) -> integer value | None -> integer fallback

let fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let a = variable terms "a" in
  let symbol name = T.symbol terms
    {name;arguments=[|T.Int|];result=T.Int;kind=T.Uninterpreted} in
  let f=symbol "f" and g=symbol "g" in
  let plus i = T.builtin terms "+" T.Int
    [|a;T.integer terms (Z.of_int i)|] in
  let applications = List.init 33 (fun i ->
    let rhs = T.apply terms g [|plus (i+1)|] in
    T.apply terms f [|rhs|],rhs) in
  List.iter (fun (left,right) -> E.assert_ solver (T.equal terms left right))
    applications;
  let registered = T.apply terms f [|plus 100|] in
  E.register solver registered;
  terms,solver,a,f,g,applications,registered

let audit (terms,solver,a,f,g,applications,registered) =
  let model = M.create ~terms ~solver in
  let avalue = integer (M.value model a) in
  let ftable = M.interpretation model f and gtable = M.interpretation model g in
  List.iteri (fun i (left,right) ->
    let input = Z.add avalue (Z.of_int (i+1)) in
    let gvalue = lookup gtable input in
    let fvalue = lookup ftable gvalue in
    check (Z.equal fvalue gvalue) "Returned function model violates assertion";
    check (Z.equal (integer (M.value model right)) gvalue)
      "Nested value disagrees with interpretation";
    check (Z.equal (integer (M.value model left)) fvalue)
      "Value disagrees with interpretation") applications;
  let key = Z.add avalue (Z.of_int 100) in
  check (Z.equal (integer (M.value model registered)) (lookup ftable key))
    "Registered application disagrees with function interpretation";
  model

let recover ((terms,solver,a,_,_,_,_) as fixture) =
  check (E.check solver=E.Sat) "Projection recovery";
  ignore (audit fixture);
  E.push solver;
  E.assert_ solver (T.equal terms a (T.integer terms (Z.of_int 7)));
  check (E.check solver=E.Sat) "Scoped projection recovery";
  ignore (audit fixture);
  E.pop solver;
  check (E.check solver=E.Sat) "Projection pop recovery";
  ignore (audit fixture)

let assumptions () =
  let ((terms,solver,a,_,_,_,_) as fixture) = fixture () in
  let assumption = T.equal terms a (T.integer terms (Z.of_int 9)) in
  check (E.check ~assumptions:[assumption] solver=E.Sat) "Assumption check";
  ignore (audit fixture);
  E.push solver;
  E.assert_ solver (T.equal terms a (T.integer terms (Z.of_int 11)));
  check (E.check solver=E.Sat) "Assumption leaked";
  let old = audit fixture in
  let fresh = variable terms "fresh" in
  E.register solver fresh;
  (match M.value old a with
  | exception M.Unavailable _ -> ()
  | _ -> failwith "Registration failed to invalidate model handle");
  ignore (audit fixture);
  E.pop solver;
  recover fixture

let run () =
  assumptions ();
  let ((_,solver,_,_,_,_,_) as initial) = fixture () in
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls;false) () in
  check (E.check ~resource solver=E.Sat) "Projection fixture";
  ignore (audit initial);
  let used=R.used resource and count= !calls in
  let samples = List.init used Fun.id in
  List.iter (fun budget ->
    let ((_,solver,_,_,_,_,_) as fixture)=fixture () in
    let resource=R.create ~budget () in
    (match E.check ~resource solver with
    | E.Unknown _ -> () | _ -> failwith "Missing resource exhaustion");
    check (R.used resource<=budget) "Resource budget exceeded";
    recover fixture) samples;
  List.iter (fun raises -> for stop=1 to count do
    let ((_,solver,_,_,_,_,_) as fixture)=fixture () in
    let calls=ref 0 in
    let resource=R.create ~cancelled:(fun () -> incr calls;
      if !calls=stop then (if raises then raise Exit else true) else false) () in
    (match E.check ~resource solver with
    | E.Unknown _ when not raises -> ()
    | exception Exit when raises -> ()
    | _ -> failwith "Missing cancellation or callback exception");
    recover fixture
  done) [false;true];
  Printf.printf "Projection API: %d budgets, %d callbacks recovered\n%!"
    (List.length samples) (2*count)

end

let constant_definitions () =
  atomic "constant datatype definition"
    "(declare-datatype Box (par (X) ((box (val X)))))"
    "(define-const c (Box Int) (box 17))"
    (fun frontend -> rejected frontend "(assert (= c c))")
    (fun frontend ->
      check (run frontend
        "(assert (distinct (val c) 17))(check-sat)" = ["unsat"])
        "Constant datatype definition recovery");
  atomic "annotated constant definition"
    "(declare-const x Int)(assert (= x 7))(check-sat)"
    "(define-const c Int (! (+ x 1) :named n))"
    (fun frontend ->
      rejected frontend "(assert (= c c))";
      rejected frontend "(assert (= n n))";
      check (run frontend "(get-value (x))" = ["((x 7))"])
        "Interrupted definition lost the model")
    (fun frontend ->
      check (run frontend
        "(assert (= c 8))(assert (= n c))(check-sat)" = ["sat"])
        "Constant annotation recovery");
  let frontend = F.create () in
  let script = Buffer.create 2048 in
  Buffer.add_string script
    "(declare-const x (_ BitVec 8))(define-const d0 (_ BitVec 8) x)";
  for i = 1 to 22 do
    Buffer.add_string script (Printf.sprintf
      "(define-const d%d (_ BitVec 8) (bvadd d%d (bvadd d%d x)))"
      i (i-1) (i-1))
  done;
  let expected = ref 1 in
  for _ = 1 to 22 do expected := (2 * !expected + 1) mod 256 done;
  Buffer.add_string script (Printf.sprintf
    "(assert (= x #x01))(assert (distinct d22 (_ bv%d 8)))(check-sat)"
    !expected);
  let resource = R.create ~budget:1000000 () in
  check (F.run ~resource frontend (Buffer.contents script) = ["unsat"])
    "Shared constant definitions";
  Printf.printf "shared constant definitions: %d resource units\n%!"
    (R.used resource)


let constant_definition_scopes () =
  let frontend = setup
    "(set-option :global-declarations true)(push 1)\
     (declare-const x Int)(define-const c Int (+ x 1))(pop 1)" in
  check (run frontend
    "(assert (= x 4))(check-sat)(get-value (c))\
     (reset-assertions)(assert (= x 8))(check-sat)(get-value (c))"
    = ["sat"; "((c 5))"; "sat"; "((c 9))"])
    "Global constant definition scope";
  check (run frontend
    "(reset)(declare-const c Bool)(assert c)(check-sat)" = ["sat"])
    "Constant definition reset";
  let frontend = setup
    "(push 1)(declare-const x Int)(define-const c Int (+ x 1))" in
  check (run frontend
    "(assert (= x 4))(check-sat)(get-value (c))(pop 1)\
     (declare-const x Bool)(define-const c Bool (not x))\
     (assert x)(assert c)(check-sat)"
    = ["sat"; "((c 5))"; "unsat"])
    "Local constant definition scope";
  let frontend = setup
    "(declare-const x Int)(assert (= x 8))(check-sat)" in
  rejected frontend "(define-const c Bool x)";
  check (run frontend "(get-value (x))" = ["((x 8))"])
    "Ill-typed definition changed model";
  check (run frontend
    "(define-const c Int (+ x 1))(assert (= c 9))(check-sat)"
    = ["sat"]) "Definition after type error";
  rejected frontend "(define-const c Int 11)";
  rejected frontend "(assert (c 2))";
  check (run frontend "(get-value (c))" = ["((c 9))"])
    "Rejected constant definition application changed model"

let narrowed_division_recovery () =
  let initial =
    {|(declare-const low (_ BitVec 3))(declare-const b (_ BitVec 5))
      (define-const a (_ BitVec 5) ((_ zero_extend 2) low))
      (define-const q (_ BitVec 5) (bvudiv a b))
      (define-const r (_ BitVec 5) (bvurem a b))
      (push 1)(assert (distinct b (_ bv0 5)))(assert (bvuge r b))|} in
  check_recovery "narrowed division" initial (command "(check-sat)")
    (fun frontend ->
      check (run frontend "(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "Narrowed division scope recovery";
      List.iter (fun (divisor, quotient, remainder) ->
        let script = Printf.sprintf
          "(push 1)(assert (= low (_ bv5 3)))\
           (assert (= b (_ bv%d 5)))(check-sat)\
           (get-value ((= q (_ bv%d 5)) (= r (_ bv%d 5))))(pop 1)"
          divisor quotient remainder in
        match run frontend script with
        | ["sat"; values] ->
          (match S.parse values with
          | [S.List [S.List [_; S.Atom "true"];
                     S.List [_; S.Atom "true"]]] -> ()
          | _ -> failwith "Narrowed division exact model")
        | _ -> failwith "Narrowed division model recovery")
        [0, 31, 5; 2, 2, 1; 6, 0, 5; 16, 0, 5])

let cached_definition_commands () =
  atomic "cached constant instance"
    "(declare-datatype Box (par (X) ((box (val X)))))\
     (define-const c Int (val (box 17)))"
    "(assert (distinct c 17))"
    (fun frontend ->
      check (run frontend "(check-sat)" = ["sat"])
        "Interrupted cached definition changed assertions")
    (fun frontend ->
      check (run frontend "(check-sat)" = ["unsat"])
        "Cached definition instance recovery");
  let source = Buffer.create 40000 in
  Buffer.add_string source "(define-const c0 Int 0)";
  for index = 1 to 1024 do
    Buffer.add_string source
      (Printf.sprintf "(define-const c%d Int c%d)" index (index - 1))
  done;
  let frontend = setup (Buffer.contents source) in
  let expression = command "(assert (= c1024 1))" in
  (match F.execute ~resource:(R.create ~budget:200 ()) frontend expression with
  | exception R.Exhausted _ -> ()
  | _ -> failwith "Nullary alias expansion did not poll");
  check (run frontend "(check-sat)" = ["sat"])
    "Interrupted alias expansion changed assertions";
  ignore (F.execute frontend expression);
  check (run frontend "(check-sat)" = ["unsat"])
    "Alias expansion recovery"

module Bitvector_boundary = struct
let fixture () =
  let terms = T.create () in
  let solver = E.create terms in
  let variable name = T.apply terms
      (T.symbol terms {name; arguments = [||]; result = T.Bitvec 32;
        kind = T.Uninterpreted}) [||] in
  let s = variable "s" and t = variable "t" in
  E.register solver s; E.register solver t;
  let op name a b = T.builtin terms name (T.Bitvec 32) [|a; b|] in
  let shift = op "bvshl" t s in
  let left = op "bvmul" t (op "bvlshr" s shift) in
  let right = op "bvmul" s (op "bvlshr" t shift) in
  E.assert_ solver (T.not_ terms (T.equal terms left right));
  terms, solver, s, t

let validate solver s t =
  check (E.check solver = E.Sat) "boundary witness missing";
  let read term = match E.value solver term with
    | Some (E.Bits (32, value)) -> Int64.of_string (Z.to_string value)
    | _ -> failwith "missing bitvector model" in
  let s = read s and t = read t in
  let mask = 0xffffffffL in
  let shift = if s >= 32L then 0L else
      Int64.logand mask (Int64.shift_left t (Int64.to_int s)) in
  let shr value = if shift >= 32L then 0L
      else Int64.shift_right_logical value (Int64.to_int shift) in
  let left = Int64.logand mask (Int64.mul t (shr s)) in
  let right = Int64.logand mask (Int64.mul s (shr t)) in
  check (left <> right) "invalid boundary witness"

let recover terms solver s t =
  validate solver s t;
  (match E.check ~assumptions:[T.equal terms s (T.bits terms 32 Z.zero)]
      solver with
  | E.Unsat _ -> ()
  | _ -> failwith "boundary witness ignored an assumption");
  validate solver s t;
  E.push solver;
  E.assert_ solver (T.equal terms s (T.bits terms 32 Z.zero));
  (match E.check solver with
  | E.Unsat _ -> () | _ -> failwith "boundary assignment leaked into scope");
  E.pop solver;
  validate solver s t

let run () =
  let terms, solver, s, t = fixture () in
  let callbacks = ref 0 in
  check (E.check ~cancelled:(fun () -> incr callbacks; false) solver = E.Sat)
    "boundary fixture";
  let work = E.resource_count solver in
  let _, resource_solver, _, _ = fixture () in
  let resource = R.create () in
  check (E.check ~resource resource_solver = E.Sat) "resource fixture";
  let resource_work = R.used resource in
  recover terms solver s t;
  for index = 0 to 64 do
    let terms, solver, s, t = fixture () in
    let budget = work * index / 64 in
    (match E.check ~budget solver with
    | E.Unknown _ | E.Sat -> ()
    | E.Unsat _ -> failwith "boundary budget changed satisfiability");
    recover terms solver s t
  done;
  for index = 0 to 64 do
    let terms, solver, s, t = fixture () in
    let budget = resource_work * index / 64 in
    let resource = R.create ~budget () in
    (match E.check ~resource solver with
    | E.Unknown _ | E.Sat -> ()
    | E.Unsat _ -> failwith "boundary resource changed satisfiability");
    check (R.used resource <= budget) "boundary resource exceeded its budget";
    recover terms solver s t
  done;
  for cutoff = 1 to !callbacks do
    let terms, solver, s, t = fixture () and count = ref 0 in
    check (E.check ~cancelled:(fun () -> incr count; !count = cutoff) solver
      = E.Unknown "cancelled") "boundary cancellation";
    recover terms solver s t;
    let terms, solver, s, t = fixture () and count = ref 0 in
    (match E.check ~cancelled:(fun () ->
       incr count; if !count = cutoff then raise Exit; false) solver with
    | exception Exit -> () | _ -> failwith "boundary callback exception");
    recover terms solver s t
  done;
  let _, solver, s, t = fixture () in
  let time = ref 0. in
  let resource = R.create ~timeout:0.01 ~clock:(fun () ->
    time := !time +. 0.001; !time) () in
  check (E.check ~resource solver = E.Unknown "timeout")
    "boundary timeout was not reported";
  validate solver s t;
  Printf.printf "Constant shifts and boundary model: %d work, %d callbacks\n%!"
    work !callbacks
end

let frontend_reclamation () =
  let churn frontend =
    for n = 0 to 4095 do
      ignore (run frontend (Printf.sprintf
        "(push 1)(declare-const temporary Int)\
         (assert (= temporary %d))(pop 1)" n))
    done in
  let make () =
    let frontend = setup
      "(declare-sort U 1)(declare-const u (U Int))\
       (declare-fun f ((U Int)) Int)(assert (= (f u) 11))\
       (declare-const x Int)(assert (= x 7))(push 1)\
       (declare-const y Int)(assert (= y (+ x 3)))(push 2)\
       (assert (> y 0))" in
    churn frontend;
    frontend in
  let recover frontend =
    check (run frontend "(check-sat)(get-value (x y (f u)))"
      = ["sat"; "((x 7) (y 10) ((f u) 11))"])
      "Reclaimed model";
    check (run frontend "(check-sat-assuming ((not (= y 10))))"
      = ["unsat"]) "Reclaimed assumptions";
    ignore (run frontend "(pop 2)");
    check (run frontend "(check-sat)(get-value (y))"
      = ["sat"; "((y 10))"]) "Reclaimed nested scopes";
    ignore (run frontend
      "(pop 1)(push 1)(declare-const y Bool)(assert y)");
    check (run frontend "(check-sat)(get-value (x y))"
      = ["sat"; "((x 7) (y true))"]) "Reclaimed declaration scope";
    check (run frontend "(pop 1)(assert (= x 8))(check-sat)"
      = ["unsat"]) "Reclaimed root assertions" in
  let expression = command "(check-sat)" in
  let frontend = make () and resource = R.create () in
  ignore (F.execute ~resource frontend expression);
  let work = R.used resource in
  recover frontend;
  List.iter (fun budget ->
    let frontend = make () in
    check (F.execute ~resource:(R.create ~budget ()) frontend expression
      = Some "unknown") "Reclamation budget";
    recover frontend) [0; 1; work / 4; work / 2; work - 1];
  let callbacks = ref 0 in
  ignore (F.execute ~cancelled:(fun () -> incr callbacks; false)
    (make ()) expression);
  List.iter (fun raises ->
    for stop = 1 to !callbacks do
      let frontend = make () and calls = ref 0 in
      let cancelled () =
        incr calls;
        if !calls = stop && raises then raise Exit;
        !calls = stop in
      (match F.execute ~cancelled frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Reclamation callback");
      recover frontend
    done) [false; true];
  let frontend = setup
    "(declare-const x Int)(assert (= x 7))(push 1)(pop 1)(check-sat)" in
  for n = 0 to 8191 do
    ignore (run frontend (Printf.sprintf "(get-value ((+ x %d)))" n))
  done;
  rejected frontend "(check-sat-assuming (missing))";
  check (run frontend "(get-value (x))" = ["((x 7))"])
    "Malformed check after reclamation changed the previous model";
  Printf.printf "Frontend reclamation: %d work, %d callbacks recovered\n%!"
    work (2 * !callbacks)

module Compact_distinct_resources = struct
  module F = Vox_smt_frontend
  module S = Vox_smt_sexp
  module R = Vox_smt_resource
  let check condition message = if not condition then failwith message
  let names = List.init 18 (fun i -> "x" ^ string_of_int i)
  let declarations = "(declare-sort U 0)" ^ String.concat ""
    (List.map (fun name -> "(declare-const " ^ name ^ " U)") names)
  let group = "(distinct " ^ String.concat " " names ^ ")"
  let setup source =
    let frontend = F.create () in ignore (F.run frontend source); frontend
  let command source = match S.parse source with
    | [command] -> command | _ -> failwith "One command"
  let recover_check name initial source expected afterwards =
    let expression = command source in
    let frontend = setup initial in
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend expression = Some expected) name;
    let used = R.used resource and callback_count = !calls in
    for budget = 0 to used - 1 do
      let frontend = setup initial in
      (match F.execute ~resource:(R.create ~budget ()) frontend expression with
      | Some "unknown" -> ()
      | value -> failwith (name ^ " budget did not stop: "
         ^ Option.value value ~default:"none"));
      check (F.execute frontend expression = Some expected) (name ^ " recovery");
      afterwards frontend
    done;
    List.iter (fun raises ->
      for stop = 1 to callback_count do
        let frontend = setup initial in
        let seen = ref 0 in
        let resource = R.create ~cancelled:(fun () -> incr seen;
          if !seen = stop then (if raises then raise Exit else true)
          else false) () in
        (match F.execute ~resource frontend expression with
        | exception Exit when raises -> ()
        | Some "unknown" when not raises -> ()
        | _ -> failwith (name ^ " callback did not stop"));
        check (F.execute frontend expression = Some expected)
          (name ^ " callback recovery");
        afterwards frontend
      done) [false; true];
    Printf.printf "%s: %d budgets, %d callbacks recovered\n%!"
      name used (2 * callback_count)
  let () =
    recover_check "distinct activation"
      (declarations ^ "(assert " ^ group ^ ")") "(check-sat)" "sat"
      (fun f -> check (F.run f
        "(push 1)(assert (= x0 x1))(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) "activation scopes");
    recover_check "distinct assumption"
      (declarations ^ "(assert (= x0 x1))")
      ("(check-sat-assuming (" ^ group ^ "))") "unsat"
      (fun f -> check (F.run f "(check-sat)" = ["sat"])
        "assumption persisted");
    recover_check "distinct transfer"
      (declarations ^ "(declare-fun f (U) U)(assert " ^ group ^ ")"
        ^ "(assert (= x0 (f x2)))(assert (= x1 (f x3)))"
        ^ "(assert (= x2 x3))") "(check-sat)" "unsat"
      (fun _ -> ());
    let initial = declarations ^ "(assert " ^ group ^ ")(check-sat)" in
    let expression = command ("(get-value (" ^ group ^ "))") in
    let frontend = setup initial in
    let expected = F.execute frontend expression in
    let resource = R.create () in
    ignore (F.execute ~resource (setup initial) expression);
    let used = R.used resource in
    for budget = 0 to used - 1 do
      let frontend = setup initial in
      (match F.execute ~resource:(R.create ~budget ()) frontend expression with
      | exception R.Exhausted _ -> ()
      | _ -> failwith "distinct model budget did not stop");
      check (F.execute frontend expression = expected) "distinct model recovery"
    done;
    Printf.printf "distinct model: %d budgets recovered\n%!" used

  module T = Vox_smt_term
  module E = Vox_smt_engine

  let repeated_assumptions conjunction =
    let terms = T.create () in
    let constant name sort = T.apply terms
      (T.symbol terms {name; arguments = [||]; result = sort;
        kind = T.Uninterpreted}) [||] in
    let values = Array.init 18 (fun i ->
      constant ("x" ^ string_of_int i) (T.Named 0)) in
    let group = T.builtin terms "distinct" T.Bool values in
    let b = constant "b" T.Bool in
    let assumption = if conjunction then T.and_ terms [|group; b|]
      else group in
    let solver = E.create terms in
    let run assumptions =
      let resource = R.create () in
      check (E.check ~resource ~assumptions solver = E.Sat)
        "repeated distinct assumption result";
      R.used resource in
    ignore (run [assumption]);
    let warm = run [assumption] in
    for _ = 1 to 256 do
      check (run [assumption] <= warm + 16)
        "repeated distinct assumption work grew"
    done;
    ignore (run [T.not_ terms group]);
    ignore (run [assumption]);
    E.push solver;
    E.assert_ solver (T.equal terms values.(0) values.(1));
    (match E.check ~assumptions:[assumption] solver with
    | E.Unsat _ -> ()
    | _ -> failwith "cached distinct assumption scope conflict");
    E.pop solver;
    ignore (run [assumption]);
    ignore (run [T.not_ terms group]);
    Printf.printf "Repeated %s assumptions: stable after 256 checks\n%!"
      (if conjunction then "conjunction" else "distinct")

  let () =
    repeated_assumptions false;
    repeated_assumptions true
end

module Boolean_alias_resources = struct
  module F = Vox_smt_frontend
  module S = Vox_smt_sexp
  module R = Vox_smt_resource
  let check condition message = if not condition then failwith message
  let names = List.init 65 (fun i -> "p" ^ string_of_int i)
  let declarations = String.concat "" (List.map (fun name ->
    "(declare-const " ^ name ^ " Bool)") ("a" :: "b" :: names))
  let definitions = declarations
    ^ "(assert (= a (and " ^ String.concat " " names ^ ")))"
    ^ "(assert (= b (or " ^ String.concat " " names ^ ")))"
  let setup source =
    let frontend = F.create () in ignore (F.run frontend source); frontend
  let command source = match S.parse source with
    | [command] -> command | _ -> failwith "One command expected"
  let () =
    let f = setup definitions in
    check (F.run f "(check-sat-assuming (a (not p0)))(check-sat)"
      = ["unsat"; "sat"]) "long conjunction assumption";
    check (F.run f ("(check-sat-assuming (b " ^ String.concat " "
      (List.map (fun name -> "(not " ^ name ^ ")") names) ^ "))")
      = ["unsat"]) "long disjunction assumption";
    check (F.run f
      "(push 1)(assert a)(check-sat)(get-value (a b p0 p19))"
      = ["sat"; "((a true) (b true) (p0 true) (p19 true))"])
      "long alias model";
    check (F.run f
      "(push 1)(assert (not p0))(check-sat)(pop 2)(check-sat)"
      = ["unsat"; "sat"]) "long alias scopes";
    let expression = command "(check-sat-assuming (a (not p0)))" in
    let resource = R.create () in
    check (F.execute ~resource (setup definitions) expression = Some "unsat")
      "long alias conflict";
    let work = R.used resource in
    for budget = 0 to work - 1 do
      let f = setup definitions in
      check (F.execute ~resource:(R.create ~budget ()) f expression
        = Some "unknown") "long alias budget interruption";
      check (F.execute f expression = Some "unsat") "long alias recovery";
      check (F.run f "(check-sat)" = ["sat"])
        "long alias assumptions persisted"
    done;
    let calls = ref 0 in
    ignore (F.execute ~cancelled:(fun () -> incr calls; false)
      (setup definitions) expression);
    List.iter (fun raises ->
      for stop = 1 to !calls do
        let f = setup definitions and seen = ref 0 in
        let cancelled () =
          incr seen;
          if !seen = stop then (if raises then raise Exit else true)
          else false in
        (match F.execute ~cancelled f expression with
        | Some "unknown" when not raises -> ()
        | exception Exit when raises -> ()
        | _ -> failwith "long alias cancellation");
        check (F.execute f expression = Some "unsat")
          "long alias cancellation recovery";
        check (F.run f "(check-sat)" = ["sat"])
          "long alias cancelled assumptions persisted"
      done) [false; true];
    Printf.printf "Boolean aliases: models, scopes, %d budgets, %d callbacks\n%!"
      work (2 * !calls)
end

module Nonlinear_proposal_resources = struct
  let fixture coefficient reversed =
    let variables = "factor" :: "free" ::
      List.init 7 (fun i -> "v" ^ string_of_int i) in
    let declarations = List.map (fun variable ->
      "(declare-const " ^ variable ^ " Int)") variables in
    let assertions = ["(<= (- 1) factor)"; "(<= factor 1)"] @
      List.concat_map (fun variable ->
        ["(>= " ^ variable ^ " 1)";
         "(= (+ (* factor " ^ variable ^ ") " ^ variable ^ ") 0)";
         Printf.sprintf "(>= (* free %s) (* %d %s))"
           variable coefficient variable])
        (List.init 7 (fun i -> "v" ^ string_of_int i)) in
    let declarations, assertions =
      if reversed then List.rev declarations, List.rev assertions
      else declarations, assertions in
    let initial = "(set-option :produce-models true)" ^
      String.concat "" declarations ^
      String.concat "" (List.map (fun assertion ->
        "(assert " ^ assertion ^ ")") assertions) in
    initial, assertions

  let verify frontend assertions =
    let source = "(get-value (" ^ String.concat " " assertions ^ "))" in
    match run frontend source with
    | [response] ->
      (match S.parse response with
      | [S.List values] ->
        check (List.length values = List.length assertions &&
          List.for_all (function
            | S.List [_; S.Atom "true"] -> true
            | _ -> false) values) "Nonlinear proposal model"
      | _ -> failwith "Nonlinear proposal values")
    | _ -> failwith "Nonlinear proposal response"

  let exercise name initial expected validate recover =
    let frontend = setup initial and calls = ref 0 in
    let resource = R.create ~budget:20000 ~cancelled:(fun () ->
      incr calls; false) () in
    let expression = command "(check-sat)" in
    check (F.execute ~resource frontend expression = Some expected)
      (name ^ " budgeted answer");
    validate frontend;
    let used = R.used resource and calls = !calls in
    let budgets = List.sort_uniq Int.compare
      (0 :: (used - 1) :: List.init 17 (fun i -> used * i / 17)) in
    List.iter (fun budget ->
      let frontend = setup initial in
      let resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        (name ^ " budget interruption");
      check (R.used resource <= budget) (name ^ " shared budget");
      recover frontend) budgets;
    List.iter (fun raises -> for stop = 1 to calls do
      let frontend = setup initial and seen = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr seen;
        if !seen = stop then (if raises then raise Exit else true)
        else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith (name ^ " callback interruption"));
      recover frontend
    done) [false; true];
    Printf.printf "%s: %d budgets, %d callbacks\n%!"
      name (List.length budgets) (2 * calls)

  let wide_sparse () =
    List.iter (fun count -> List.iter (fun reverse ->
      let variables = List.init count (fun i -> "v" ^ string_of_int i) in
      let assertions = List.concat_map (fun v ->
        ["(>= " ^ v ^ " 0)"; "(<= (* " ^ v ^ " " ^ v ^ ") 1)"])
        variables @ ["(= (+ " ^ String.concat " " variables ^ ") 1)"] in
      let assertions = if reverse then List.rev assertions else assertions in
      let initial = "(set-option :produce-models true)" ^
        String.concat "" (List.map (fun v ->
          "(declare-const " ^ v ^ " Int)") variables) ^
        String.concat "" (List.map (fun assertion ->
          "(assert " ^ assertion ^ ")") assertions) in
      let recover frontend =
        check (run frontend "(check-sat)" = ["sat"])
          "Sparse proposal recovery";
        verify frontend assertions;
        check (run frontend
          "(push 1)(assert false)(check-sat)(pop 1)(check-sat)"
          = ["unsat"; "sat"]) "Sparse proposal scope recovery";
        verify frontend assertions in
      exercise (Printf.sprintf "sparse proposal %d/%b" count reverse)
        initial "sat" (fun frontend -> verify frontend assertions) recover)
      [false; true]) [9; 12; 16]

  let run () =
    wide_sparse ();
    List.iter (fun coefficient -> List.iter (fun reversed ->
      let initial, assertions = fixture coefficient reversed in
      let recover frontend =
        check (run frontend "(check-sat)" = ["sat"])
          "Factor proposal recovery";
        verify frontend assertions;
        check (run frontend
          "(push 1)(assert false)(check-sat)(pop 1)(check-sat)"
          = ["unsat"; "sat"]) "Factor proposal scope recovery";
        verify frontend assertions in
      exercise (Printf.sprintf "factor boundary %d/%b" coefficient reversed)
        initial "sat" (fun frontend -> verify frontend assertions) recover)
      [false; true]) [3; 17; 50000];
    let initial, assertions = fixture 17 false in
    let initial = initial ^ "(push 1)(assert (<= v0 0))" in
    let recover frontend =
      check (run frontend "(check-sat)" = ["unsat"])
        "Failed factor proposal recovery";
      check (run frontend "(pop 1)(check-sat)" = ["sat"])
        "Failed factor proposal scope recovery";
      verify frontend assertions in
    exercise "failed factor boundary" initial "unsat" (fun _ -> ()) recover;
    List.iter (fun bound -> List.iter (fun negative ->
      let assertions =
        [Printf.sprintf "(<= (- %d) x)" (bound + 1);
         Printf.sprintf "(<= x %d)" bound;
         Printf.sprintf "(> (* x x) %d)" bound] @
        (if negative then ["(< x 0)"] else []) in
      let initial = "(set-option :produce-models true)" ^
        "(declare-const x Int)" ^ String.concat ""
          (List.map (fun assertion -> "(assert " ^ assertion ^ ")")
            assertions) in
      let recover frontend =
        check (run frontend "(check-sat)" = ["sat"])
          "Literal proposal recovery";
        verify frontend assertions;
        check (run frontend
          "(push 1)(assert false)(check-sat)(pop 1)(check-sat)"
          = ["unsat"; "sat"]) "Literal proposal scope recovery";
        verify frontend assertions in
      exercise (Printf.sprintf "literal proposal %d/%b" bound negative)
        initial "sat" (fun frontend -> verify frontend assertions) recover)
      [false; true]) [3; 17; 2147483647]

end

module Quadratic_bound_resources = struct
let query = S.List [S.Atom "check-sat"]
let declarations = "(set-option :produce-models true)" ^
  "(declare-const a Int)(declare-const b Int)(declare-const c Int)" ^
  "(declare-const x Int)"
let fixtures =
  ["sum", ["(>= c 1)"; "(>= (+ a b 1) 0)";
      "(<= (+ a b 1) (* 2 (+ c 1)))"],
      "(> (* a (+ b 1)) (* (+ c 1) (+ c 1)))";
   "strict sum", ["(>= c 1)"; "(>= (+ a b) 0)";
      "(< (+ a b) (* 2 c))"], "(>= (* a b) (* c c))";
   "ratio", ["(>= a 0)"; "(>= b 0)"; "(>= c 0)";
      "(<= (* 2 c) (+ a b))"; "(<= a (* 4 b))";
      "(<= b (* 4 a))"], "(> (* 16 c c) (* 25 a b))";
   "scaled order", ["(>= a 1)"; "(> b (* 4 a))"],
      "(<= (* a b) (* 4 a a))";
   "envelope ratio", ["(>= a 1)"; "(>= x 1)"; "(>= c 0)";
      "(= b (div x a))"; "(<= (* 2 c) (+ a b))";
      "(<= x (* a a))"; "(<= (* a a) (div (* 5 x) 3))"],
      "(> (* 60 c c) (* 64 x))";
   "envelope additive", ["(>= a 1)"; "(>= x 1)"; "(>= c 0)";
      "(= b (div x a))"; "(<= (* 2 c) (+ a b))";
      "(>= x (- (* a a) a))"; "(<= x (+ (* a a) (* 2 a)))"],
      "(> (* c c) (+ x c))"]
let run () =
  List.iter (fun (name, facts, negation) -> List.iter (fun reverse ->
    let facts = if reverse then List.rev facts else facts in
    let initial = declarations ^ String.concat "" (List.map
        (fun fact -> "(assert " ^ fact ^ ")") facts) ^
        "(push 1)(assert " ^ negation ^ ")" in
    let setup () = let solver = F.create () in
      ignore (run solver initial); solver in
    let recover solver =
      check (run solver "(check-sat)(pop 1)(check-sat)" = ["unsat"; "sat"])
        (name ^ " recovery");
      check (run solver
        "(push 1)(assert false)(check-sat)(pop 1)(check-sat)"
        = ["unsat"; "sat"]) (name ^ " repeated scope recovery") in
    let solver = setup () and calls = ref 0 in
    let resource = R.create ~budget:250000
        ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource solver query = Some "unsat") (name ^ " answer");
    let used = R.used resource and callbacks = !calls in
    recover solver;
    let budgets = List.sort_uniq Int.compare
      (used - 1 :: List.init 33 (fun i -> used * i / 33)) in
    List.iter (fun budget ->
      let solver = setup () and resource = R.create ~budget () in
      check (F.execute ~resource solver query = Some "unknown")
        (name ^ " budget");
      check (R.used resource <= budget) (name ^ " overspent");
      recover solver) budgets;
    List.iter (fun raises -> for stop = 1 to callbacks do
      let solver = setup () and calls = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr calls; if !calls = stop then
          if raises then raise Exit else true else false) () in
      (match F.execute ~resource solver query with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith (name ^ " callback"));
      recover solver
    done) [false; true];
    Printf.printf "%s/%b: %d work, %d budgets, %d callbacks\n%!"
      name reverse used (List.length budgets) (2 * callbacks))
    [false; true]) fixtures
end

module Pending_scope_resources = struct
let linear_retained () =
  List.iter (fun deferred ->
    let terms = T.create () in
    let solver = E.create terms in
    let integer n = T.integer terms (Z.of_int n) in
    let declare name =
      T.apply terms (T.symbol terms
        {name; arguments = [||]; result = T.Int;
         kind = T.Uninterpreted}) [||] in
    let x = declare "x" in
    E.register solver x;
    for index = 0 to 2047 do
      E.register solver (declare ("warm" ^ string_of_int index))
    done;
    E.assert_ solver (T.equal terms x (integer 7));
    check (E.check solver = E.Sat) "Initial linear check";
    E.push solver;
    E.assert_ solver (T.equal terms x (integer 7));
    if deferred then E.pop solver
    else begin
      E.push solver;
      E.assert_ solver (T.equal terms x (integer 7));
      check (E.check solver = E.Sat) "Nested linear check";
      E.pop solver;
      E.pop solver;
      let resource = R.create ~budget:1000 () in
      check (E.check ~resource solver = E.Sat) "Linear pop rebuilt parent"
    end;
    let resource = R.create () in
    E.register ~resource solver (declare "fresh");
    check (R.used resource > 1) "Linear pop discarded the core";
    check (E.check solver = E.Sat) "Linear retained answer")
    [false; true]

let assumption_registration () =
  let initial =
    "(set-option :global-declarations true)" ^
    "(declare-const a Int)(declare-const b Int)" ^
    "(assert (>= a 1))(assert (> b (* 4 a)))(push 1)" ^
    "(check-sat-assuming ((<= (* a b) (* 4 a a))))" ^
    "(push 1)(declare-const fresh Int)(pop 1)" in
  atomic "assumption registrations" initial "(pop 1)"
    (fun frontend ->
      check (run frontend
        "(check-sat-assuming ((<= (* a b) (* 4 a a))))" = ["unsat"])
        "Interrupted pop lost assumption scope")
    (fun frontend ->
      let resource = R.create ~budget:10000 () in
      check (F.execute ~resource frontend (command "(check-sat)")
        = Some "sat") "Assumption registrations survived pop";
      check (run frontend "(get-value (fresh (>= a 1) (> b (* 4 a))))"
        = ["((fresh 0) ((>= a 1) true) ((> b (* 4 a)) true))"])
        "Assumption pop lost declarations or parent")

let run () =
  assumption_registration ();
  linear_retained ();
  List.iter (fun balanced ->
    let initial =
      "(set-option :global-declarations true)" ^
      "(declare-const x Int)(assert (= x 7))(check-sat)" ^
      "(push 1)(declare-const y Int)(assert (= y 5))" ^
      "(push 1)(declare-const z Int)(assert (= z 9))" ^
      (if balanced then "(pop 1)" else "") in
    atomic "cancelled pending scopes" initial
      (if balanced then "(pop 1)" else "(pop 2)")
      (fun frontend ->
        check (run frontend "(check-sat)(get-value (x y))"
          = ["sat"; "((x 7) (y 5))"]) "Interrupted pending pop")
      (fun frontend ->
        check (run frontend
          "(assert (distinct y 5))(assert (distinct z 9))(check-sat)"
          = ["sat"]) "Pending declaration or assertion lost";
        check (run frontend "(get-value (x))" = ["((x 7))"])
          "Pending pop lost parent") ) [false; true]
end

let nonlinear_scope_pop () =
  List.iter (fun pending ->
    let initial =
      "(declare-const a Int)(declare-const b Int)" ^
      "(assert (>= a 1))(assert (> b (* 4 a)))" ^
      "(push 1)(assert (<= (* a b) (* 4 a a)))(check-sat)" ^
      "(push 512)(check-sat)" ^ (if pending then "(push 1)" else "") in
    atomic "nonlinear pop" initial
      (if pending then "(pop 514)" else "(pop 513)")
      (fun frontend -> check (run frontend "(check-sat)" = ["unsat"])
        "Interrupted pop changed scope")
      (fun frontend ->
        check (run frontend "(check-sat)" = ["sat"]) "Pop parent recovery";
        check (run frontend "(get-value ((>= a 1) (> b (* 4 a))))"
          = ["(((>= a 1) true) ((> b (* 4 a)) true))"])
          "Pop parent model")) [false; true]

module Interval_units_resources = struct
  module F = Vox_smt_frontend
  module S = Vox_smt_sexp
  module R = Vox_smt_resource

  let check condition message = if not condition then failwith message
  let command = match S.parse "(check-sat)" with
    | [command] -> command | _ -> failwith "One check required"
  let condition = "(< (+ x y) 10)"
  let payload = "(assert (> (ite (< (+ x y) 10) (+ x 20) (+ y 1)) 0))"
  let bounds lower upper = Printf.sprintf
    "(assert (>= (- x y) %d))(assert (<= (+ x y) %d))\
     (assert (>= y 0))(assert (<= y 4))" lower upper
  let setup () =
    let frontend = F.create () in
    ignore (F.run frontend
      ("(declare-const x Int)(declare-const y Int)(push 1)"
        ^ bounds 10 14 ^ payload));
    frontend
  let recover frontend =
    check (F.run frontend ("(check-sat)(get-value (" ^ condition ^ "))")
      = ["sat"; "(((< (+ x y) 10) false))"])
      "Interval unit recovery";
    check (F.run frontend
      ("(push 1)(assert " ^ condition
        ^ ")(check-sat)(pop 1)(check-sat)")
      = ["unsat"; "sat"])
      "Nested interval scope recovery";
    check (F.run frontend
      ("(pop 1)(push 1)" ^ bounds 0 4 ^ payload
        ^ "(check-sat)(get-value (" ^ condition ^ "))")
      = ["sat"; "(((< (+ x y) 10) true))"])
      "Interval unit survived pop";
    check (F.run frontend
      ("(pop 1)(check-sat-assuming (" ^ condition
        ^ "))(check-sat-assuming ((not " ^ condition ^ ")))")
      = ["sat"; "sat"])
      "Interval unit escaped its scope"
  let run () =
    let frontend = setup () in
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    check (F.execute ~resource frontend command = Some "sat")
      "Interval resource fixture";
    let work = R.used resource and callbacks = !calls in
    recover frontend;
    for budget = 0 to work - 1 do
      let frontend = setup () in
      let resource = R.create ~budget () in
      check (F.execute ~resource frontend command = Some "unknown")
        "Interval budget did not interrupt";
      check (R.used resource <= budget) "Interval budget overrun";
      recover frontend
    done;
    for stop = 1 to callbacks do
      List.iter (fun raises ->
        let frontend = setup () and calls = ref 0 in
        let resource = R.create ~cancelled:(fun () ->
          incr calls;
          if !calls = stop then if raises then raise Exit else true
          else false) () in
        (match F.execute ~resource frontend command with
        | exception Exit when raises -> ()
        | Some "unknown" when not raises -> ()
        | _ -> failwith "Interval callback did not interrupt");
        recover frontend) [false; true]
    done;
    Printf.printf "Interval units: %d budgets and %d callbacks recovered\n%!"
      work (2 * callbacks)
end

let protocol_option_atomicity () =
  let initial = "(set-option :produce-assertions true)" in
  let verify expected frontend =
    check (run frontend
      "(get-option :interactive-mode)(get-option :produce-assertions)"
      = [expected; expected]) "Assertion option aliases diverged"
  in
  atomic "assertion option alias" initial
    "(set-option :interactive-mode false)" (verify "true") (verify "false");
  atomic "unknown option" initial "(set-option :zzz true)"
    (verify "true") (fun frontend ->
      verify "true" frontend;
      check (run frontend "(get-option :zzz)" = ["unsupported"])
        "Unknown option was stored")

let nested_remainder_recovery () =
  let fixtures = [
    "(+ a (mod b c))", "(+ a b)";
    "(- a (mod b c))", "(- a b)";
    "(* (mod a c) (mod b c))", "(* a b)";
    "(- (mod b c))", "(- b)";
    "(- a (mod b c) (mod x c))", "(- a b x)";
    "(+ x (mod (+ a (mod b c)) c))", "(+ a b x)";
    "(+ (mod b c) (mod a (+ 1 c)))", "(+ b (mod a (+ 1 c)))";
    "(* (mod a c) (+ x (mod b c)))", "(* a (+ b x))";
  ] in
  List.iteri (fun index (left, right) ->
    let statement = "(not (or (= c 0) (= (mod " ^ left ^
      " c) (mod " ^ right ^ " c))))" in
    let initial = "(set-option :produce-models true)" ^
      "(set-option :produce-unsat-cores true)" ^
      "(declare-const a Int)(declare-const b Int)" ^
      "(declare-const c Int)(declare-const x Int)(push 1)" ^
      "(assert (! " ^ statement ^ " :named bad))" in
    let recover frontend =
      check (run frontend
        "(check-sat)(get-unsat-core)(pop 1)(check-sat)"
        = ["unsat"; "(bad)"; "sat"]) "Nested remainder scope recovery";
      check (run frontend
        ("(push 1)(assert (= a (- 7)))(assert (= b 3))" ^
        "(assert (= c (- 5)))(check-sat)") = ["sat"])
        "Nested remainder model recovery" in
    let frontend = setup initial and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
    let expression = command "(check-sat)" in
    check (F.execute ~resource frontend expression = Some "unsat")
      "Nested remainder answer";
    let used = R.used resource and calls = !calls in
    recover frontend;
    let budgets = List.sort_uniq Int.compare
      (0 :: used - 1 :: List.init 33 (fun i -> used * i / 33)) in
    List.iter (fun budget ->
      let frontend = setup initial and resource = R.create ~budget () in
      check (F.execute ~resource frontend expression = Some "unknown")
        "Nested remainder budget";
      check (R.used resource <= budget) "Nested remainder shared budget";
      recover frontend) budgets;
    List.iter (fun raises -> for stop = 1 to calls do
      let frontend = setup initial and seen = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr seen; if !seen = stop then
          (if raises then raise Exit else true) else false) () in
      (match F.execute ~resource frontend expression with
      | Some "unknown" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Nested remainder callback");
      recover frontend
    done) [false; true];
    Printf.printf "nested%d work%d: %d budgets, %d callbacks\n%!"
      index used (List.length budgets) (2 * calls)
  ) fixtures


let () =
  Quadratic_bound_resources.run ();
  Pending_scope_resources.run ();
  nonlinear_scope_pop ();
  nested_remainder_recovery ();
  protocol_option_atomicity ();
  Interval_units_resources.run ();
  Nonlinear_proposal_resources.run ();
  divisibility_commands ();
  frontend_reclamation ();
  Bitvector_boundary.run ();
  cached_definition_commands ();
  constant_definitions ();
  constant_definition_scopes ();
  narrowed_division_recovery ();
  Projected_functions.run ();
  variable_division_proposal_recovery ();
  one_bit_recovery ();
  constant_division_proposal_recovery ();
  candidate_division_recovery ();
  quadratic_recovery ();
  monomial_recovery ();
  model_shared_rendering ();
  model_rendering_equality ();
  zero_divisor_recovery ();
  factor_comparison_recovery ();
  product_pair_recovery ();
  core_resources ();
  reclamation_recovery ();
  finite_product_recovery ();
  power_order_recovery ();
  Boolean_polynomial.run ();
  polynomial_residue_recovery ();
  integer_cut_recovery ();
  alternate_integer_cut_recovery ();
  nary_bitvector_recovery ();
  bounded_polynomial_recovery ();
  equality_lattice_recovery ();
  small_polynomial_recovery ();
  integer_extensions_recovery ();
  buffered_scope_recovery ();
  difference_presolver_recovery ();
  disjunctive_equality_recovery ();
  cumulative_resources ();
  requested_equality_recovery ();
  quotient_bounds_recovery ();
  bitvector_quotient_recovery ();
  overloaded_commands ();
  bitvector_polynomial_recovery ();
  bitvector_rewrite_recovery ();
  atomic_commands ();
  uninterpreted_sort_commands ();
  annotation_commands ();
  connective_commands ();
  numeric_alias_commands ();
  conversion_commands ();
  check_budgets ();
  parser_and_constants ();
  model_resources ();
  model_recursive_products ();
  model_constructive_completion ();
  model_equal_shared_dags ();
  cancellation_and_clock ();
  command_callbacks ();
  frontend_entry_reentry ();
  engine_term_polling ();
  parser_resources ();
  pure_uf_recovery ();
  print_endline "Complete-operation resource tests passed"
