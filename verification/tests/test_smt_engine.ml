module Frontend = Vox_smt_frontend
module Engine = Vox_smt_engine
module Term = Vox_smt_term
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message

let run source =
  let polls = ref 0 in
  Frontend.run
    ~cancelled:(fun () ->
      incr polls;
      !polls > 200000)
    (Frontend.create ()) source

let expect source expected =
  let actual =
    try run source
    with exn -> failwith (Printexc.to_string exn ^ "\n" ^ source)
  in
  check (actual = expected)
    (Printf.sprintf "SMT result: [%s], expected [%s]\n%s"
       (String.concat "; " actual)
       (String.concat "; " expected)
       source)

let solver_information () =
  expect
    {|(get-info :name)(get-info :version)(get-info :authors)
      (get-info :error-behavior)(get-info :assertion-stack-levels)
      (push 2)(get-info :assertion-stack-levels)(pop 1)
      (get-info :assertion-stack-levels)(get-info :unsupported-info)|}
    ["(:name \"Vox SMT\")"; "(:version \"0.1\")";
     "(:authors \"Vox contributors\")";
     "(:error-behavior continued-execution)";
     "(:assertion-stack-levels 0)"; "(:assertion-stack-levels 2)";
     "(:assertion-stack-levels 1)"; "unsupported"];
  expect
    {|(get-info :status)(set-info :status unsat)(get-info :status)
      (check-sat)(get-info :status)(assert false)(get-info :status)
      (check-sat)(get-info :status)(reset)(get-info :status)|}
    ["(:status unknown)"; "(:status unknown)"; "sat"; "(:status sat)";
     "(:status unknown)"; "unsat"; "(:status unsat)"; "(:status unknown)"]

let information_attributes () =
  expect
    {|(set-option :print-success true)
      (set-info :status sat)(set-info :status unsat)(set-info :status unknown)
      (set-info :source "sample")(set-info :custom)
      (set-info :custom (nested data))(get-info :status)|}
    ["success"; "success"; "success"; "success"; "success"; "success";
     "success"; "(:status unknown)"];
  let frontend = Frontend.create () in
  List.iter (fun command ->
    (match Frontend.run frontend command with
    | _ -> failwith ("Accepted malformed information: " ^ command)
    | exception Frontend.Error _ -> ());
    check (Frontend.run frontend "(check-sat)" = ["sat"])
      "Malformed information changed solver state")
    ["(set-info)"; "(set-info source)"; "(set-info :source a b)";
     "(set-info :status)"; "(set-info :status true)";
     "(set-info :status \"sat\")"; "(set-info :status sat extra)";
     "(set-info : value)"]

let terminal_command () =
  expect "(set-option :print-success true)(exit)(check-sat)(unclosed"
    ["success"; "success"];
  expect "(exit)(unclosed" [];
  expect {|(echo "(exit)")(check-sat)(exit)(unclosed|}
    ["\"(exit)\""; "sat"];
  expect "; (exit)\n(check-sat)(exit)\"unclosed" ["sat"];
  let frontend = Frontend.create () in
  (match Frontend.run frontend "(exit true)(check-sat)" with
  | _ -> failwith "Accepted malformed exit"
  | exception Frontend.Error _ -> ());
  check (Frontend.run frontend "(check-sat)" = ["sat"])
    "Malformed exit changed solver state"

let output_channels () =
  expect
    {|(get-option :diagnostic-output-channel)
      (set-option :print-success true)
      (set-option :diagnostic-output-channel "stdout")
      (get-option :diagnostic-output-channel)
      (set-option :diagnostic-output-channel "logs/diagnostics.txt")
      (get-option :diagnostic-output-channel)
      (reset)(get-option :diagnostic-output-channel)(check-sat)|}
    ["\"stderr\""; "success"; "success"; "\"stdout\""; "success";
     "\"logs/diagnostics.txt\""; "\"stderr\""; "sat"];
  expect
    {|(get-option :regular-output-channel)
      (set-option :regular-output-channel "stderr")
      (get-option :regular-output-channel)
      (reset-assertions)(get-option :regular-output-channel)
      (reset)(get-option :regular-output-channel)|}
    ["\"stdout\""; "\"stderr\""; "\"stderr\""; "\"stdout\""];
  List.iter (fun option ->
    let frontend = Frontend.create () in
    ignore (Frontend.run frontend ("(set-option " ^ option ^ " \"stdout\")"));
    List.iter (fun value ->
      (match Frontend.run frontend
         ("(set-option " ^ option ^ " " ^ value ^ ")") with
      | _ -> failwith "Accepted a nonstring output channel"
      | exception Frontend.Error _ -> ());
      check
        (Frontend.run frontend ("(get-option " ^ option ^ ")(check-sat)")
         = ["\"stdout\""; "sat"])
        "Failed output channel option changed frontend state")
      ["stdout"; "true"; "0"; "(stdout)"])
    [":diagnostic-output-channel"; ":regular-output-channel"]

let declaration terms name sort =
  Term.apply terms
    (Term.symbol terms
       { name; arguments = [||]; result = sort; kind = Term.Uninterpreted })
    [||]

let integer_divisibility () =
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let y = declaration terms "y" Term.Int in
  let p = declaration terms "p" Term.Bool in
  let integer n = Term.integer terms (Z.of_int n) in
  let app op args = Term.builtin terms op Term.Int args in
  let conditional = Term.ite terms p (app "*" [|integer 4; x|])
      (integer 0) in
  let sum = app "+" [|app "*" [|y; conditional|];
                      app "*" [|x; conditional|]|] in
  let remainder value divisor = app "mod" [|value; integer divisor|] in
  let zero value = match Term.node terms value with
    | Term.Integer value -> Z.is_zero value
    | _ -> false in
  List.iter (fun divisor ->
    check (zero (remainder sum divisor)) "Conditional divisibility")
    [2; 4; -2; -4];
  check (not (zero (remainder sum 0))) "Zero divisor simplified";
  check (not (zero (remainder (app "+" [|sum; y|]) 4)))
    "Unknown summand simplified";
  check (not (zero (remainder (Term.ite terms p sum y) 4)))
    "Unknown branch simplified";
  let product = app "*" [|app "*" [|integer 2; x|];
      Term.ite terms p (integer 2) (app "*" [|integer 6; y|])|] in
  check (zero (remainder product 4)) "Divisors across product factors";
  let arguments = Array.make 4096 (app "*" [|integer 2; x|]) in
  let wide = Term.apply terms (Term.symbol terms
      { name = "+"; arguments = Array.make 4096 Term.Int;
        result = Term.Int; kind = Term.Builtin "+" }) arguments in
  let polls = ref 0 in
  ignore (Term.with_poll terms (fun () -> incr polls)
    (fun () -> remainder wide 4));
  check (!polls < 1024) "Divisibility search repeated-edge work bound"


let difference_disjunctions () =
  let setup =
    "(declare-const x Int)(declare-const y Int)\
     (declare-const z Int)(declare-const w Int)\
     (assert (> x 0))(assert (> z 0))\
     (assert (or (<= x 0) (<= w 0)))\
     (assert (or (<= y 0) (<= z 0)))\
     (assert (or (<= y 0) (<= w 0)))" in
  expect
    (setup ^ "(check-sat)(push 1)\
      (assert (or (<= x 0) (<= z 0)))\
      (assert (or (<= x 0) (<= w 0)))\
      (check-sat)(pop 1)(check-sat)\
      (reset-assertions)(check-sat)")
    ["sat"; "unsat"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (assert (> x 0))(assert (<= y 0))(assert (<= z 0))
      (assert (or (<= x 0) (<= y 0)))
      (assert (or (<= y 0) (<= z 0)))
      (assert (or (<= z 0) (<= x 0)))(check-sat)|}
    ["sat"];
  expect
    {|(declare-const a Int)(declare-const b Int)(declare-const c Int)
      (declare-const d Int)(declare-const e Int)(declare-const f Int)
      (assert (> a 0))(assert (> d 0))
      (assert (or (<= a 0) (<= e 0)))
      (assert (or (<= a 0) (<= f 0)))
      (assert (or (<= b 0) (<= d 0)))
      (assert (or (<= b 0) (<= f 0)))
      (assert (or (<= c 0) (<= d 0)))
      (assert (or (<= c 0) (<= e 0)))
      (assert (or (<= a 0) (<= e 0)))
      (assert (or (<= b 0) (<= f 0)))
      (assert (or (<= c 0) (<= d 0)))(check-sat)|}
    ["sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (or (<= x 0) (<= y 0)))
      (assert (or (>= x 1) (>= y 1)))(check-sat)|}
    ["sat"]

let boolean_alternatives () =
  expect
    "(declare-const p Bool)(declare-const q Bool)\
     (declare-const x Int)(push 1)\
     (assert (or (and p q (<= x 0)) (and p (not q) (<= x 0))))\
     (check-sat-assuming ((not p)))(check-sat)\
     (push 1)(assert (> x 0))(check-sat)(pop 2)\
     (assert (not p))(assert (> x 0))(check-sat)\
     (check-sat-assuming ((or (and p q) (and p (not q)))))\
     (check-sat)"
    ["unsat"; "sat"; "unsat"; "sat"; "unsat"; "sat"]

let term_array_ownership () =
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let y = declaration terms "y" Term.Int in
  let arguments = [|Term.Int|] in
  let f = Term.symbol terms
      { name = "f"; arguments; result = Term.Int;
        kind = Term.Uninterpreted } in
  arguments.(0) <- Term.Bool;
  (Term.signature terms f).arguments.(0) <- Term.Bool;
  let fx = Term.apply terms f [|x|] in
  (match Term.node terms fx with
  | Term.Apply (_, arguments) -> arguments.(0) <- y
  | _ -> failwith "Expected application");
  check (Term.apply terms f [|x|] = fx) "Application array ownership";
  let fy = Term.apply terms f [|y|] in
  check (fx <> fy) "Application identity after caller mutation";
  let count = Term.count terms in
  let invalid f = match f () with
    | _ -> failwith "Invalid term accepted"
    | exception Invalid_argument _ -> () in
  List.iter (fun term ->
    invalid (fun () -> Term.node terms term);
    invalid (fun () -> Term.sort terms term);
    invalid (fun () -> Term.apply terms f [|term|]);
    invalid (fun () -> Term.not_ terms term);
    invalid (fun () -> Term.and_ terms [|term|]);
    invalid (fun () -> Term.or_ terms [|term|]);
    invalid (fun () -> Term.to_string terms term);
    invalid (fun () -> Term.builtin terms "=" Term.Bool [|term; term|]))
    [-1; count; count + 1];
  check (Term.count terms = count) "Invalid term changed arena";
  let solver = Engine.create terms in
  Engine.assert_ solver (Term.equal terms fx (Term.integer terms Z.one));
  Engine.assert_ solver
    (Term.equal terms fy (Term.integer terms (Z.of_int 2)));
  check (Engine.check solver = Engine.Sat) "Term mutation changed formula"

let connective_terms () =
  let signature name arity =
    { Term.name; arguments = Array.make arity Term.Bool;
      result = Term.Bool; kind = Term.Builtin name } in
  List.iter (fun name ->
    List.iter (fun canonical_not ->
      let terms = Term.create () in
      let p = declaration terms "p" Term.Bool in
      let q = declaration terms "q" Term.Bool in
      if canonical_not then ignore (Term.not_ terms q);
      let raw_not = Term.symbol terms (signature "not" 1) in
      let negative = Term.apply terms raw_not [|p|] in
      let child = Term.builtin terms name Term.Bool [|negative; q|] in
      let result = Term.builtin terms name Term.Bool [|child; p|] in
      check (result = Term.boolean terms (name = "or"))
        "Complement through a separately declared not")
      [false; true]) ["and"; "or"];
  let terms = Term.create () in
  let p = declaration terms "p" Term.Bool in
  let q = declaration terms "q" Term.Bool in
  let r = declaration terms "r" Term.Bool in
  let raw = Term.symbol terms (signature "and" 2) in
  let count = Term.count terms in
  (match Term.with_temporary terms (fun () ->
     ignore (Term.and_ terms [|p; q|]);
     Term.with_temporary terms (fun () ->
       ignore (Term.or_ terms [|p; q|]); raise Exit)) with
  | _ -> failwith "Expected temporary term exception"
  | exception Exit -> ());
  check (Term.count terms = count) "Nested temporary term count";
  let child = Term.apply terms raw [|q; p|] in
  check (child = count) "Temporary term identifier reused";
  let result = Term.and_ terms [|child; r|] in
  check (result = Term.and_ terms [|p; q; r|])
    "Canonical connective after temporary term rollback"

let deep_boolean_connectives () =
  List.iter (fun (operator, identity) ->
    let source = Buffer.create 600000 in
    Buffer.add_string source "(declare-const p Bool)(assert (not p))(assert ";
    for depth = 1 to 50000 do
      Buffer.add_string source ("(" ^ operator ^ " ");
      if depth mod 2 = 0 then Buffer.add_string source (identity ^ " ")
    done;
    Buffer.add_string source "p";
    for depth = 50000 downto 1 do
      if depth mod 2 <> 0 then Buffer.add_string source (" " ^ identity);
      Buffer.add_char source ')'
    done;
    Buffer.add_string source ")(check-sat)";
    check (Frontend.run (Frontend.create ()) (Buffer.contents source)
      = ["unsat"]) ("Deep " ^ operator ^ " elaboration"))
    ["and", "true"; "|and|", "true";
     "or", "false"; "|or|", "false"];
  expect
    {|(declare-const p Bool)(assert (not p))
      (assert (and (and) (and p (or (or) false))))(check-sat)|}
    ["unsat"];
  expect
    {|(define-fun |and| ((p Bool) (q Bool)) Bool (or p q))
      (assert (and false (and false true)))(check-sat)|} ["sat"];
  List.iter (fun source ->
    let frontend = Frontend.create () in
    (match Frontend.run frontend source with
    | exception (Frontend.Error _ | Invalid_argument _) -> ()
    | _ -> failwith "Invalid nested connective accepted");
    check (Frontend.run frontend "(check-sat)" = ["sat"])
      "Invalid nested connective recovery")
    ["(assert (and true (and true 1)))";
     "(assert (and (! true :named |and|) (and false)))"]

let streamed_expressions () =
  let module S = Vox_smt_sexp in
  let symbol = "|" ^ String.make 10000 'x' ^ "|" in
  let text = String.make 4095 'a' ^ "\"" ^ String.make 4096 'b' in
  let boundary size =
    S.List (List.init (size - 1) (fun i ->
      if i mod 2 = 0 then S.Atom "|quoted symbol|"
      else S.String "embedded \" quote"))
  in
  let expected =
    [S.List [S.Atom "declare-const"; S.Atom symbol; S.Atom "Int"];
     S.List [S.Atom "echo"; S.String text];
     S.List (List.init 2048
       (fun _ -> S.List [S.Atom "f"; S.Atom "x"]))]
    @ List.map boundary [1023; 1024; 1025]
    @ [S.List [S.Atom "check-sat"]] in
  let path = Filename.temp_file "vox-smt-stream-" ".smt2" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let output = open_out_bin path in
    Fun.protect ~finally:(fun () -> close_out output) (fun () ->
      List.iter (fun expression ->
        check (S.parse (S.to_string expression) = [expression])
          "Parsed expression roundtrip";
        output_string output (S.to_string expression);
        output_string output "\n; comment\n") expected);
    let input = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      List.iter (fun expression ->
        check (S.read input = Some expression) "Streamed expression") expected;
      check (S.read input = None) "Streamed end of input"))

let streamed_recovery () =
  let module S = Vox_smt_sexp in
  let path = Filename.temp_file "vox-reader-recovery-" ".smt2" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let append text =
      let output = open_out_gen
        [Open_wronly; Open_append; Open_binary] 0o600 path in
      Fun.protect ~finally:(fun () -> close_out output)
        (fun () -> output_string output text)
    in
    let input = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      List.iter (fun ending ->
        append ("(and "
          ^ String.concat " " (List.init 2048 (fun _ -> "true"))
          ^ ending);
        (match S.read input with
        | _ -> failwith "Unclosed expression accepted"
        | exception S.Error _ -> ());
        append "(check-sat)\n";
        check (S.read input = Some (S.List [S.Atom "check-sat"]))
          "Reader after failed expression")
        [""; " \"unterminated"];
      check (S.read input = None) "Recovered reader end of input"))

let requested_integer_equalities () =
  expect
    {|(declare-datatype T ((box (contents Int)) (pair (first T) (second Int))))
      (declare-sort U 0)(declare-const k U)(declare-fun p (U T Int) Bool)
      (declare-const x Int)(declare-const y Int)(declare-const q Bool)
      (assert (<= 0 x))(assert (< x 16))
      (assert (<= 0 y))(assert (< y 16))
      (assert (=> q (= y (mod (+ x 32) 16))))
      (assert (p k (pair (box x) 3) 7))
      (assert (not (p k (pair (box y) 3) 7)))
      (check-sat)(check-sat-assuming (q))(check-sat)
      (push 1)(assert q)(check-sat)(pop 1)(check-sat)|}
    ["sat"; "unsat"; "sat"; "unsat"; "sat"];
  expect
    {|(declare-datatype T ((box (contents Int))))
      (declare-fun p (T Int) Bool)(declare-const x Int)
      (declare-const y Int)(assert (= x 0))(assert (= y 16))
      (assert (= (mod x 16) (mod y 16)))
      (assert (p (box x) 7))(assert (not (p (box y) 7)))
      (check-sat)|} ["sat"]

let wide_predicate_requests () =
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let integer value = Term.integer terms (Z.of_int value) in
  let remainder = Term.builtin terms "mod" Term.Int [|x; integer 5|] in
  let arity = 2048 in
  let predicate = Term.symbol terms
      { name = "p"; arguments = Array.make arity Term.Int;
        result = Term.Bool; kind = Term.Uninterpreted } in
  let solver = Engine.create terms in
  Engine.assert_ solver
    (Term.builtin terms "<" Term.Bool [|remainder; integer 5|]);
  Engine.assert_ solver (Term.apply terms predicate (Array.make arity x));
  check (Engine.check ~budget:100000 solver = Engine.Sat)
    "Wide predicate exhausted discovery budget";
  Engine.push solver;
  Engine.assert_ solver (Term.boolean terms false);
  check (Engine.check solver = Engine.Unsat []) "Wide predicate inner scope";
  Engine.pop solver;
  check (Engine.check ~budget:100000 solver = Engine.Sat)
    "Wide predicate scope recovery"

let comparison_relations () =
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const k Int)
      (declare-const q Bool)
      (assert (<= 0 x))(assert (<= x 2))
      (assert (<= 0 y))(assert (< y 5))
      (assert (=> q (= y (mod (+ x 2) 5))))
      (assert (<= k (+ x 2)))(assert (not (<= k y)))
      (check-sat-assuming (q))(check-sat)
      (push 1)(assert q)(check-sat)(pop 1)
      (check-sat-assuming ((not q)))(check-sat)|}
    ["unsat"; "sat"; "unsat"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const k Int)
      (assert (= x 4))(assert (= y (mod (+ x 2) 5)))
      (assert (<= k (+ x 2)))(assert (not (<= k y)))(check-sat)|} ["sat"]

let variable_affine_comparisons () =
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const k Int)
      (declare-const q Bool)
      (define-fun a () Int (mod (+ x (* 3 y) 3) 16))
      (define-fun b () Int (mod x 16))
      (assert (<= a k))(assert (not (<= b k)))
      (assert (=> q (and (<= 0 x) (<= x 2) (<= 1 y) (<= y 2))))
      (check-sat-assuming (q))(check-sat)
      (push 1)(assert q)(check-sat)(pop 1)
      (push 1)(assert (= x 14))(assert (= y 1))
      (assert (= k 8))(check-sat)(get-value (a b))(pop 1)
      (check-sat-assuming (q))|}
    ["unsat"; "sat"; "unsat"; "sat"; "((a 4) (b 14))"; "unsat"]

let demanded_integer_equalities () =
  List.iter (fun n ->
    expect (Printf.sprintf
      {|(declare-sort U 0)(declare-const x U)(declare-const y U)
        (declare-fun f (U) Int)(declare-const p Bool)
        (assert (=> p (= x y)))
        (assert (= (+ (* %d (f x)) (* %d (f y))) 1))
        (check-sat-assuming (p))(check-sat)
        (push 1)(assert p)(check-sat)(pop 1)
        (check-sat-assuming ((not p)))(check-sat)|} n (n + 1))
      ["unsat"; "sat"; "unsat"; "sat"; "sat"]) [1; 17; 1000];
  expect
    {|(declare-datatype T ((node (field Int))))
      (declare-const x T)(declare-const y T)(declare-const p Bool)
      (assert (=> p (= x y)))
      (assert (= (+ (* 17 (field x)) (* 18 (field y))) 1))
      (check-sat-assuming (p))(check-sat)
      (push 1)(assert p)(check-sat)(pop 1)(check-sat)|}
    ["unsat"; "sat"; "unsat"; "sat"]


let fixed_combinations () =
  List.iter
    (fun source -> expect source ["unsat"])
    [ {|(declare-const x Int)(declare-const y Int)
        (assert (>= x 1))(assert (>= y 1))
        (assert (< (* (- x 1) (- y 1)) 0))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
        (assert (> x 0))(assert (> y 0))(assert (<= z (- 1)))
        (assert (> (* x y z) (- (* x y))))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
        (assert (>= x 0))(assert (<= (- y z) 1))
        (assert (> (- (* x y) (* x z)) x))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
        (assert (<= x 0))(assert (<= (- y z) 1))
        (assert (< (- (* x y) (* x z)) x))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)
        (declare-fun f (Int) Int)
        (assert (<= x y))(assert (<= y x))
        (assert (distinct (f x) (f y)))(check-sat)|};
      {|(declare-sort U 0)(declare-const a U)(declare-const b U)
        (declare-fun f (U) Int)(declare-fun g (Int) U)
        (assert (= a b))
        (assert (distinct (g (+ (f a) 1)) (g (+ 1 (f b)))))(check-sat)|};
      {|(declare-const x (_ BitVec 4))(declare-const y (_ BitVec 4))
        (declare-fun f ((_ BitVec 4)) Int)
        (assert (= (bv2int x) (bv2int y)))
        (assert (distinct (f x) (f y)))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)
        (assert (= x (+ y 8)))
        (assert (distinct ((_ int2bv 3) x) ((_ int2bv 3) y)))
        (check-sat)|};
      {|(declare-const x Int)(declare-const y Int)
        (assert (= x y))
        (assert (distinct (div x 0) (div y 0)))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)
        (assert (= x y))
        (assert (distinct (mod x 0) (mod y 0)))(check-sat)|};
      {|(declare-const x Int)(assert (= (* 2 x) 1))(check-sat)|};
      {|(declare-const x Int)
        (assert (distinct (mod x 101) (mod x (- 101))))(check-sat)|};
      {|(declare-const x Int)(declare-const y Int)
        (assert (= x 3))(assert (= y (- 2)))
        (assert (distinct (* x y) (- 6)))(check-sat)|};
      {|(declare-sort U 0)(declare-const a U)(declare-const b U)
        (declare-fun f (U) U)(declare-const p Bool)
        (assert p)(assert (= (ite p a b) b))
        (assert (distinct (f a) (f b)))(check-sat)|};
      {|(declare-sort U 0)
        (declare-datatype R ((r (a U) (b U))))
        (declare-datatype S ((s (c R) (d U))))
        (declare-datatype Ret ((wrap (field R)) (nil) (other)))
        (declare-const x S)(declare-const y R)
        (declare-fun f (R) Ret)
        (assert (= (f (r (a (c x)) (b (c x)))) (wrap y)))
        (assert (distinct (f (c x)) (wrap y)))(check-sat)|};
      {|(declare-datatype L ((nil) (cons (head Int) (tail L))))
        (declare-const x L)(declare-const y L)
        (assert (= x nil))(assert (= x (cons 1 y)))(check-sat)|};
      {|(declare-datatype L ((nil) (cons (head Int) (tail L))))
        (declare-const x L)(assert ((_ is nil) x))
        (assert (distinct x nil))(check-sat)|};
      {|(declare-datatype D ((c (field Int)) (d)))
        (declare-const a D)(declare-const x Int)(declare-const y Int)
        (declare-const p Bool)
        (assert (or p (= a (c x))))(assert (or p (= a (c y))))
        (assert (not p))(assert (distinct x y))(check-sat)|};
      {|(declare-datatype D ((c (field Int)) (d)))
        (declare-const a D)(declare-const x Int)(declare-const p Bool)
        (assert (or p (= a (c x))))(assert (or p (= a d)))
        (assert (not p))(check-sat)|}
    ];
  List.iter
    (fun source -> expect source ["sat"])
    [ {|(declare-const x Int)(declare-const y Int)
        (declare-fun f (Int) Int)
        (assert (= x 0))(assert (= y 1))
        (assert (distinct (f x) (f y)))(check-sat)|};
      {|(declare-const x Int)(assert (= x (- 9)))
        (assert (= ((_ int2bv 3) x) #b111))(check-sat)|};
      {|(declare-const x Int)(assert (= x (- 5)))
        (assert (= (div x (- 2)) 3))
        (assert (= (mod x (- 2)) 1))(check-sat)|};
      {|(declare-const x Int)(assert (= (div x 0) 99))
        (assert (= (mod x 0) 101))(check-sat)|}
    ]

let buffered_assertion_scopes () =
  expect
    {|(declare-const x Int)(declare-fun f (Int) Int)
      (assert (> (f x) 0))(push 1)(assert (= x 1))
      (push 1)(assert (= (f 1) 0))(check-sat)
      (pop 1)(check-sat)(get-value (x))
      (pop 1)(push 1)(assert (= x 2))(assert (= (f 1) 0))
      (check-sat)(check-sat-assuming ((= (f 2) 0)))
      (pop 1)(check-sat-assuming ((<= (f x) 0)))|}
    ["unsat"; "sat"; "((x 1))"; "sat"; "unsat"; "unsat"];
  expect
    "(assert false)(push 2)(check-sat)(pop 1)(check-sat)\
     (pop 1)(check-sat)"
    ["unsat"; "unsat"; "unsat"];
  expect
    {|(declare-const x Int)(declare-fun p (Int) Bool)
      (assert (p x))(push 1)(assert (= x 1))
      (check-sat)(get-value ((p x)))
      (push 1)(assert (not (p 1)))(check-sat)(pop 2)
      (assert (not (p 1)))(check-sat)(get-value ((p x) (p 1)))
      (push 1)(assert (= x 1))(check-sat)(pop 1)(check-sat)|}
    ["sat"; "(((p x) true))"; "unsat"; "sat";
     "(((p x) true) ((p 1) false))"; "unsat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (> x 0))(push 1)(assert (= x 0))
      (push 1)(assert (= y 1))(check-sat)(pop 2)
      (push 1)(assert (= x 2))(check-sat)(get-value (x))
      (pop 1)(assert (= x 0))(check-sat)|}
    ["unsat"; "sat"; "((x 2))"; "unsat"];
  expect
    {|(declare-const x Int)(declare-const p Bool)
      (assert (=> p (> x 0)))(push 1)(assert (= x 0))
      (check-sat-assuming (p))(check-sat)
      (pop 1)(check-sat-assuming (p))
      (push 2)(assert (= x 1))(pop 2)
      (assert p)(assert (= x 0))(check-sat)|}
    ["unsat"; "sat"; "sat"; "unsat"]

let scopes_and_assumptions () =
  expect
    {|(declare-const x Int)(declare-const y Int)
      (declare-fun f (Int) Int)
      (assert (= x y))
      (check-sat)
      (push 1)(assert (distinct (f x) (f y)))(check-sat)
      (push 1)(assert false)(check-sat)
      (pop 2)(check-sat)
      (push 1)(assert (= x 7))(check-sat)
      (pop 1)(push 1)(assert (= y 8))(check-sat)(pop 1)
      (check-sat)|}
    ["sat"; "unsat"; "unsat"; "sat"; "sat"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const p Bool)(declare-const q Bool)
      (assert (=> p (= ((_ int2bv 3) x) #b001)))
      (assert (=> q (= x 2)))
      (check-sat-assuming (p q))
      (check-sat-assuming (p (not q)))
      (check-sat-assuming ((not p) q))
      (check-sat)
      (push 1)(assert p)(assert q)(check-sat)(pop 1)(check-sat)|}
    ["unsat"; "sat"; "sat"; "sat"; "unsat"; "sat"];
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let spare = declaration terms "spare" Term.Bool in
  let bound name n =
    Term.builtin terms name Term.Bool [| x; Term.integer terms (Z.of_int n) |]
  in
  let low = bound "<=" 0 and high = bound ">=" 1 in
  let solver = Engine.create terms in
  let assumptions = [low; spare; high] in
  (match Engine.check ~assumptions solver with
  | Engine.Unsat core -> (
    check
      (List.for_all (fun term -> List.mem term assumptions) core)
      "Core contains a non-assumption";
    let replay = Engine.create terms in
    match Engine.check ~assumptions:core replay with
    | Engine.Unsat _ -> ()
    | _ -> failwith "Mixed-theory assumption core is satisfiable")
  | _ -> failwith "Inconsistent arithmetic assumptions accepted");
  check
    (Engine.check ~assumptions:[low] solver = Engine.Sat)
    "Assumption leaked to next check";
  check
    (Engine.check ~cancelled:(fun () -> true) solver
     = Engine.Unknown "cancelled")
    "Cancellation reason";
  check
    (Engine.check ~budget:0 solver = Engine.Unknown "resource limit")
    "Zero budget reason";
  check
    (try ignore (Engine.check ~budget:(-1) solver); false
     with Invalid_argument _ -> true)
    "Negative resource budget accepted";
  check
    (Engine.check ~cancelled:(fun () -> true) solver
     = Engine.Unknown "cancelled")
    "Cancellation retained the previous resource-limit reason";
  check
    (Engine.check ~assumptions:[high] solver = Engine.Sat)
    "Cancellation corrupted arithmetic state"

let engine_preflight () =
  List.iter (fun (cancelled, budget, reason) ->
    let terms = Term.create () in
    let solver = Engine.create terms in
    let x = declaration terms "preflight_x" (Term.Bitvec 8) in
    let y = declaration terms "preflight_y" (Term.Bitvec 8) in
    List.iter (Engine.register solver) [x; y];
    let product = Term.builtin terms "bvmul" (Term.Bitvec 8) [|x; y|] in
    let equal n = Term.equal terms product (Term.bits terms 8 (Z.of_int n)) in
    let three = equal 3 and four = equal 4 in
    let stop assumptions =
      let statistics = Engine.statistics solver in
      let term_count = Term.count terms in
      let polls = ref 0 in
      let actual = Engine.check ~assumptions ~budget
          ~cancelled:(fun () -> incr polls; cancelled) solver in
      check (actual = Engine.Unknown reason) "Preflight reason";
      check (!polls = 1) "Preflight cancellation was not polled once";
      check (Engine.resource_count solver = 0) "Preflight retained work";
      check (Engine.statistics solver = statistics)
        "Preflight entered the SAT solver";
      check (Term.count terms = term_count) "Preflight created terms";
      check (Engine.value solver x = None) "Preflight retained a model";
      check (Engine.raw_value solver x = None)
        "Preflight retained a raw model"
    in
    Engine.push solver;
    Engine.assert_ solver three;
    let before = List.sort Int.compare (Engine.model_terms solver) in
    stop [four];
    check (List.sort Int.compare (Engine.model_terms solver) = before)
      "Preflight encoded terms";
    check (Engine.check ~assumptions:[four] solver = Engine.Unsat [four])
      "Preflight lost a pending assertion";
    check (Engine.check solver = Engine.Sat)
      "Preflight retained an assumption";
    check (Engine.value solver product = Some (Engine.Bits (8, Z.of_int 3)))
      "Preflight changed the pending assertion";
    check (Engine.resource_count solver > 0) "Preflight fixture did no work";
    stop [];
    check (Engine.For_testing.check ~reorder_after:0 solver = Engine.Sat)
      "Preflight prevented alternate solving";
    check (Engine.value solver product = Some (Engine.Bits (8, Z.of_int 3)))
      "Preflight changed the alternate model";
    stop [];
    check (Engine.For_testing.check ~reorder_after:0 solver = Engine.Sat)
      "Preflight damaged the saved alternate solver";
    Engine.pop solver;
    check (Engine.check ~assumptions:[four] solver = Engine.Sat)
      "Preflight damaged the scope stack")
    [true, 100000, "cancelled";
     false, 0, "resource limit";
     true, 0, "cancelled"]

let reordered_search ?(scoped = false) () =
  let setup () =
    let terms = Term.create () in
    let solver = Engine.create terms in
    if scoped then Engine.push solver;
    let x = declaration terms "x" Term.Int in
    let y = declaration terms "y" Term.Int in
    let p = declaration terms "p" Term.Bool in
    List.iter (Engine.register solver) [x; y; p];
    let cmp name a b = Term.builtin terms name Term.Bool [|a; b|] in
    Engine.assert_ solver (cmp "<=" x y);
    Engine.assert_ solver (cmp "<=" y x);
    terms, solver, x, y in
  let run ?(assumptions = []) solver =
    Engine.For_testing.check
      ~reorder_after:0 ~budget:100000 ~assumptions solver in
  let terms, solver, x, y = setup () in
  check (run solver = Engine.Sat) "Reordered arithmetic model";
  check ((Engine.statistics solver).solves = 2) "Alternate search not used";
  check (Engine.value solver x = Engine.value solver y)
    "Reordered equality model";
  let spare = declaration terms "spare" Term.Int in
  Engine.register solver spare;
  check (Engine.value solver spare = Some (Engine.Integer Z.zero))
    "Declaration after reordered model";
  let function_ = Term.symbol terms
      { name = "f"; arguments = [|Term.Int|]; result = Term.Int;
        kind = Term.Uninterpreted } in
  let apply x = Term.apply terms function_ [|x|] in
  Engine.push solver;
  Engine.assert_ solver (Term.not_ terms
    (Term.equal terms (apply x) (apply y)));
  (match run solver with
  | Engine.Unsat [] -> ()
  | _ -> failwith "Reordered scoped contradiction");
  let before = (Engine.statistics solver).propagations in
  Engine.pop solver;
  check ((Engine.statistics solver).propagations >= before)
    "Retired alternate statistics lost";
  check (run solver = Engine.Sat) "Reordered assertion leaked after pop";
  let equal n = Term.equal terms x (Term.integer terms (Z.of_int n)) in
  let assumptions = [equal 0; equal 1] in
  (match run ~assumptions solver with
  | Engine.Unsat core ->
    check (List.for_all (fun term -> List.mem term assumptions) core)
      "Reordered core contains a non-assumption";
    let replay = Engine.create terms in
    (match run ~assumptions:core replay with
    | Engine.Unsat _ -> ()
    | _ -> failwith "Reordered assumption core is satisfiable")
  | _ -> failwith "Reordered arithmetic assumptions accepted");
  check (run ~assumptions:[equal 3] solver = Engine.Sat)
    "Reordered assumptions leaked";
  check (Engine.value solver x = Some (Engine.Integer (Z.of_int 3)))
    "Reordered assumption model";
  Engine.assert_ solver (equal 4);
  check (Engine.value solver x = None) "Reordered model remained valid";
  check (run solver = Engine.Sat) "Reordered mutation";
  check (Engine.value solver x = Some (Engine.Integer (Z.of_int 4)))
    "Reordered mutation model";
  for budget = 1 to 5 do
    let _, solver, _, _ = setup () in
    check (Engine.For_testing.check ~reorder_after:0 ~budget solver
      = Engine.Unknown "resource limit") "Rebuild ignored resource budget";
    check (Engine.resource_count solver = budget) "Rebuild work uncounted";
    check (run solver = Engine.Sat) "Interrupted rebuild corrupted state"
  done;
  for stop = 1 to 100 do
    let _, solver, _, _ = setup () in
    let polls = ref 0 in
    (match Engine.For_testing.check ~reorder_after:0 ~budget:100000
      ~cancelled:(fun () -> incr polls; !polls >= stop) solver with
    | Engine.Sat | Engine.Unknown "cancelled" -> ()
    | _ -> failwith "Reordered cancellation result");
    check (run solver = Engine.Sat) "Reordered cancellation corrupted state"
  done

let cancellation_replay () =
  let setup =
    {|(declare-sort U 0)(declare-const a U)(declare-const b U)
      (declare-const x Int)(declare-const y Int)
      (declare-fun f (Int) Int)(declare-fun g (U) Int)(declare-fun h (Int) U)
      (assert (<= x y))(assert (<= y x))(assert (distinct (f x) (f y)))|}
  in
  for limit = 1 to 150 do
    let frontend = Frontend.create () in
    ignore (Frontend.run frontend setup);
    let polls = ref 0 in
    ignore
      (Frontend.run
         ~cancelled:(fun () ->
           incr polls;
           !polls >= limit)
         frontend "(check-sat)");
    let polls = ref 0 in
    let replay =
      Frontend.run
        ~cancelled:(fun () ->
          incr polls;
          !polls > 10000)
        frontend "(check-sat)"
    in
    check (replay = ["unsat"])
      (Printf.sprintf "Cancellation corrupted SMT state at poll %d" limit)
  done

let resource_budget_replay () =
  let frontend = Frontend.create () in
  let expect source expected =
    check (Frontend.run frontend source = expected)
      ("Resource-budget replay: " ^ source)
  in
  let exhausted () =
    match
      Frontend.run frontend
        "(set-option :rlimit 1)(check-sat)(get-info :reason-unknown)"
    with
    | ["unknown"; reason]
      when List.mem reason
        [ "(:reason-unknown \"resource limit\")";
          "(:reason-unknown \"arithmetic resource limit\")";
          "(:reason-unknown \"theory resource limit\")" ] -> ()
    | _ -> failwith "A small resource budget did not explain exhaustion"
  in
  expect
    {|(declare-const x Int)(declare-const y Int)
      (declare-fun f (Int) Int)
      (assert (<= x y))(assert (<= y x))|}
    [];
  exhausted ();
  expect "(set-option :rlimit 100000)(check-sat)" ["sat"];
  expect "(push 1)(assert (distinct (f x) (f y)))" [];
  exhausted ();
  expect "(set-option :rlimit 100000)(check-sat)" ["unsat"];
  expect "(pop 1)(push 1)(assert (>= x 0))(check-sat)" ["sat"];
  expect "(push 1)(assert (< y 0))" [];
  exhausted ();
  expect "(set-option :rlimit 100000)(check-sat)" ["unsat"];
  expect "(pop 1)(check-sat)(pop 1)(check-sat)" ["sat"; "sat"];
  expect "(set-option :rlimit 0)(check-sat)" ["sat"]

let scoped_cancellation_replay () =
  let cancelled_checks = ref 0 in
  for limit = 1 to 80 do
    let frontend = Frontend.create () in
    ignore
      (Frontend.run frontend
         {|(declare-const x Int)(declare-const y Int)
           (declare-const p Bool)(declare-fun f (Int) Int)
           (assert (<= x y))(assert (<= y x))
           (push 1)(assert (=> p (distinct (f x) (f y))))
           (push 1)(assert p)|});
    let polls = ref 0 in
    (match
       Frontend.run
         ~cancelled:(fun () -> incr polls; !polls >= limit)
         frontend "(check-sat)"
     with
    | ["unknown"] ->
      incr cancelled_checks;
      check
        (Frontend.run frontend "(get-info :reason-unknown)"
         = ["(:reason-unknown \"cancelled\")"])
        "Scoped cancellation reason"
    | ["unsat"] -> ()
    | _ -> failwith "Cancellation changed an unsatisfiable scoped query");
    check
      (Frontend.run frontend
         {|(check-sat)(pop 1)(check-sat)
           (check-sat-assuming (p))(check-sat-assuming ((not p)))
           (pop 1)(check-sat-assuming (p))|}
       = ["unsat"; "sat"; "unsat"; "sat"; "sat"])
      (Printf.sprintf "Cancellation corrupted nested scopes at poll %d" limit)
  done;
  check (!cancelled_checks > 0) "Scoped cancellation was never exercised"

let substitutions () =
  expect
    {|(declare-const x Int)(declare-const y Int)
      (declare-const a Int)(declare-const b Int)
      (assert (= x (+ a 1)))(assert (= y (+ a 2)))
      (check-sat)(get-value (x y))
      (push 1)(assert (= a (+ b 3)))(check-sat)(get-value (x))
      (push 1)(assert (= b 4))(check-sat)(get-value (x y))
      (pop 1)(check-sat)(get-value (y))
      (assert (= b 7))(check-sat)(get-value (x y))
      (pop 1)(check-sat)(get-value (x y))|}
    ["sat"; "((x 1) (y 2))"; "sat"; "((x 4))";
     "sat"; "((x 8) (y 9))"; "sat"; "((y 5))";
     "sat"; "((x 11) (y 12))"; "sat"; "((x 1) (y 2))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(assert (= x y))
      (check-sat)(get-value ((+ x 1)))
      (push 1)(assert (= y 4))(check-sat)(get-value ((+ x 1)))
      (pop 1)(assert (= y 7))(check-sat)(get-value ((+ x 1)))|}
    ["sat"; "(((+ x 1) 1))"; "sat"; "(((+ x 1) 5))";
     "sat"; "(((+ x 1) 8))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (declare-fun f (Int) Int)
      (push 1)(assert (= z (+ x 1)))(assert (= x (+ z 1)))
      (check-sat)(pop 1)
      (assert (= x (f x)))(assert (= y (+ z 1)))
      (push 1)(assert (= z (+ y 1)))(check-sat)(pop 1)
      (assert (= z 4))(check-sat)(get-value (y z))|}
    ["unsat"; "unsat"; "sat"; "((y 5) (z 4))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (assert (= x (+ y 1)))(assert (= y (+ z 2)))
      (assert (= z 3))(check-sat)(get-value (x y z))
      (push 1)(assert (distinct x 6))(check-sat)(pop 1)(check-sat)|}
    ["sat"; "((x 6) (y 5) (z 3))"; "unsat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (push 1)(assert (= x 1))(assert (= y x))(check-sat)
      (get-value (x y))(pop 1)
      (assert (= x 2))(assert (= y 3))(check-sat)(get-value (x y))|}
    ["sat"; "((x 1) (y 1))"; "sat"; "((x 2) (y 3))"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (= x y))(assert (= y (+ x 1)))(check-sat)|}
    ["unsat"];
  expect
    {|(declare-const x Int)(declare-const p Bool)
      (assert (> x 0))(assert (= x 0))(check-sat)
      (reset)(declare-const p Bool)(assert (= p false))
      (check-sat-assuming (p))(get-unsat-core)(check-sat)|}
    ["unsat"; "unsat"; "()"; "sat"];
  expect
    {|(declare-const x Int)(assert (> x 0))
      (push 1)(assert (= x 0))(pop 1)(check-sat)
      (push 1)(assert (< x 0))(pop 1)(check-sat)
      (push 1)(assert (= x 0))(check-sat)(pop 1)(check-sat)|}
    ["sat"; "sat"; "unsat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (< x y))(assert (= y 7))(assert (= x 8))(check-sat)
      (reset)(declare-const x Int)(assert (> x 0))(check-sat)
      (assert (= x 0))(check-sat)|}
    ["unsat"; "sat"; "unsat"]

let quoted_symbols () =
  expect
    {|(declare-sort |U| 0)(declare-const a U)(declare-const |b| |U|)
      (declare-const x |Int|)(declare-const |q r| Bool)
      (define-fun |f| ((|y| Int)) Int (|+| y x))
      (assert (= |x| 3))(assert (|and| |q r| |true|))
      (assert (= (f 2) 5))
      (assert (let ((|y| 4)) (= y 4)))
      (assert (= a b))(check-sat)
      (push 1)(declare-const |temporary| Int)(assert (= temporary 7))
      (check-sat)(pop 1)(declare-const temporary Int)
      (assert (= |temporary| 8))(check-sat)|}
    ["sat"; "sat"; "sat"];
  List.iter
    (fun source ->
      match run source with
      | _ -> failwith "Duplicate quoted name accepted"
      | exception Frontend.Error _ -> ())
    [ "(declare-const x Int)(declare-const |x| Int)";
      "(declare-sort |U| 0)(declare-sort U 0)";
      "(define-fun f () Int 0)(define-fun |f| () Int 1)" ]

let invalid_inputs () =
  List.iter
    (fun source ->
      match run source with
      | _ -> failwith ("Invalid SMT input accepted: " ^ source)
      | exception (Frontend.Error _ | Invalid_argument _ | Vox_smt_sexp.Error _)
        ->
        ())
    [ "(declare-const x Bool)(assert (= x 1))";
      "(declare-const x Int)(assert (bvult x x))";
      "(declare-const x (_ BitVec 0))";
      "(declare-const x (_ BitVec 3))(assert (= ((_ extract 3 0) x) x))";
      "(declare-const x Int)(assert (ite x true false))";
      "(assert ((_ divisible 0) 4))";
      "(assert ((_ divisible -2) 4))";
      "(assert ((_ divisible 2) true))";
      "(assert (= (bv2nat 3) 3))";
      "(declare-fun f (Int) Int)(assert (= (f true) 0))";
      "(declare-const x Int)(assert x)";
      "(pop 1)";
      "(set-option :timeout -1)";
      "(set-option :print-success invalid)";
      "(declare-const x (_ BitVec 0x04))";
      "(declare-const x (_ BitVec +4))";
      "(declare-const x (_ BitVec 1_0))";
      "(assert (= (_ bv-1 8) #xff))";
      Printf.sprintf
        "(declare-const x (_ BitVec 4))\
         (assert (distinct ((_ repeat %d) x) x))" (max_int / 2 + 2);
      Printf.sprintf
        "(declare-const x (_ BitVec 1))\
         (assert (= ((_ zero_extend %d) x) x))" max_int;
      "(set-option :rewriter.hi_div0 false)";
      "(set-option :smt.hi_div0 false)";
      "(set-option :global-negate true)";
      "(set-option :smt.arith.ignore_int true)";
      "(set-option :solve-int-as-bv 8)";
      "(set-option :solve-real-as-int true)";
      "(assert true" ];
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let atom = Term.builtin terms "unsupported" Term.Bool [| x |] in
  let solver = Engine.create terms in
  Engine.assert_ solver atom;
  match Engine.check solver with
  | Engine.Unknown _ -> ()
  | _ -> failwith "Unsupported theory accepted"

let timeout () =
  let elapsed = ref 0. in
  let clock () = elapsed := !elapsed +. 0.001; !elapsed in
  let frontend = Frontend.create ~clock () in
  check (Frontend.run frontend
      "(set-option :timeout 1)(assert false)(check-sat)\
       (get-info :reason-unknown)(reset)(check-sat)"
    = ["unknown"; "(:reason-unknown \"timeout\")"; "sat"])
    "Timeout reason and recovery"

let numerals n =
  if n < 0 then Printf.sprintf "(- %d)" (-n) else string_of_int n

let recursive_model_samples () =
  let source = Buffer.create 8192 in
  Buffer.add_string source
    "(declare-datatype N ((zero) (succ (pred N))))\
     (declare-fun f (N) Int)";
  for i = 0 to 126 do
    Printf.bprintf source
      "(declare-const x%d N)(assert (= (f x%d) %d))" i i i
  done;
  Buffer.add_string source
    "(check-sat)(get-value ((= x0 x1) (f x126)))";
  expect (Buffer.contents source)
    ["sat"; "(((= x0 x1) false) ((f x126) 126))"]

let fixed_models () =
  let random = Random.State.make [| 0x18e5; 0x411 |] in
  for _ = 1 to 200 do
    let x = Random.State.int random 17 - 8 in
    let y = Random.State.int random 9 - 4 in
    let width = 1 + Random.State.int random 5 in
    let delta = Random.State.int random 2 in
    let expected = if delta = 0 then "sat" else "unsat" in
    let prefix =
      Printf.sprintf
        "(declare-const x Int)(declare-const y Int)\n\
         (assert (= x %s))(assert (= y %s))\n"
        (numerals x) (numerals y)
    in
    expect
      (prefix
      ^ Printf.sprintf "(assert (= (* x y) %s))(check-sat)"
          (numerals ((x * y) + delta)))
      [expected];
    let bits = x land ((1 lsl width) - 1) lxor delta in
    expect
      (prefix
      ^ Printf.sprintf "(assert (= ((_ int2bv %d) x) (_ bv%d %d)))(check-sat)"
          width bits width)
      [expected];
    if y <> 0
    then begin
      let remainder = ((x mod abs y) + abs y) mod abs y in
      let quotient = (x - remainder) / y in
      expect
        (prefix
        ^ Printf.sprintf
            "(assert (= (div x y) %s))(assert (= (mod x y) %d))(check-sat)"
            (numerals (quotient + delta))
            remainder)
        [expected]
    end
  done

let z3 executable source =
  let input = Filename.temp_file "vox-smt-input-" ".smt2" in
  let output = Filename.temp_file "vox-smt-output-" ".txt" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove input;
      Sys.remove output)
    (fun () ->
      let channel = open_out_bin input in
      output_string channel source;
      close_out channel;
      let command =
        Printf.sprintf "%s -smt2 %s > %s"
          (Filename.quote executable)
          (Filename.quote input) (Filename.quote output)
      in
      let status = Sys.command command in
      let channel = open_in_bin output in
      Fun.protect
        ~finally:(fun () -> close_in channel)
        (fun () ->
          let rec lines acc =
            match input_line channel with
            | line -> lines (line :: acc)
            | exception End_of_file -> List.rev acc
          in
          let output = lines [] in
          check (status = 0)
            ("Z3 differential command failed:\n" ^ String.concat "\n" output
           ^ "\n" ^ source);
          output))

let guarded_bitvector_quotients executable =
  let verify source expected =
    expect source expected;
    Option.iter (fun executable ->
      check (z3 executable source = expected) "Bitvector quotient oracle")
      executable in
  let prelude =
    "(declare-const q (_ BitVec 8))\
     (define-fun r () (_ BitVec 8)\
       (bvsub (_ bv16 8) (bvmul (_ bv8 8) q)))" in
  let bounds =
    ["(bvsle (_ bv0 8) q)"; "(bvslt q (_ bv16 8))";
     "(bvsle (_ bv0 8) r)"; "(bvslt r (_ bv8 8))"] in
  let assert_all bounds = String.concat ""
      (List.map (fun bound -> "(assert " ^ bound ^ ")") bounds) in
  let reject = "(assert (distinct q (_ bv2 8)))(check-sat)" in
  List.iter (fun bounds -> verify (prelude ^ assert_all bounds ^ reject)
      ["unsat"])
    [bounds; List.rev bounds;
     ["(bvsge q (_ bv0 8))"; "(bvsgt (_ bv16 8) q)";
      "(not (bvslt r (_ bv0 8)))";
      "(not (bvsge r (_ bv8 8)))"]];
  List.iter (fun (index, replacement) ->
    let bounds = List.mapi (fun i bound ->
      if i = index then replacement else bound) bounds in
    verify (prelude ^ assert_all bounds ^ reject) ["sat"])
    [1, "true"; 0, "(bvule (_ bv0 8) q)";
     1, "(not (bvslt q (_ bv16 8)))";
     1, "(bvslt q (_ bv64 8))";
     2, "(not (bvsle (_ bv0 8) r))";
     3, "(not (bvslt r (_ bv8 8)))"];
  let guarded = prelude ^ "(declare-const p Bool)(assert (=> p (and "
      ^ String.concat " " bounds ^ ")))(assert (distinct q (_ bv2 8)))" in
  verify (guarded
    ^ "(check-sat)(check-sat-assuming (p))(check-sat)\
       (push 1)(assert p)(check-sat)(pop 1)(check-sat)")
    ["sat"; "unsat"; "sat"; "unsat"; "sat"];
  verify (prelude ^ assert_all bounds
    ^ "(declare-fun f ((_ BitVec 8)) Bool)\
       (assert (f q))(assert (not (f (_ bv2 8))))(check-sat)") ["unsat"];
  verify
    {|(declare-const q (_ BitVec 8))
      (define-fun r () (_ BitVec 8)
        (bvsub (_ bv200 8) (bvmul q (_ bv8 8))))
      (assert (bvsle (_ bv0 8) q))(assert (bvslt q (_ bv16 8)))
      (assert (bvsle (_ bv0 8) r))(assert (bvslt r (_ bv8 8)))
      (check-sat)|} ["unsat"]

let bitvector_quotient_limit () =
  let source = Buffer.create 100000 in
  for index = 0 to 269 do
    let q = Printf.sprintf "q%d" index in
    let r = Printf.sprintf "r%d" index in
    let p = Printf.sprintf "p%d" index in
    Printf.bprintf source
      "(declare-const %s (_ BitVec 5))(declare-const %s Bool)\
       (define-fun %s () (_ BitVec 5)\
         (bvsub (_ bv5 5) (bvmul (_ bv4 5) %s)))\
       (assert (=> %s (and (bvsle (_ bv0 5) %s)\
         (bvslt %s (_ bv4 5)) (bvsle (_ bv0 5) %s)\
         (bvslt %s (_ bv4 5)) (distinct %s (_ bv1 5)))))"
      q p r q p q q r r q
  done;
  Buffer.add_string source "(check-sat)";
  for index = 0 to 269 do
    Printf.bprintf source "(check-sat-assuming (p%d))" index
  done;
  Buffer.add_string source
    "(push 1)(assert false)(check-sat)(pop 1)(check-sat)";
  let expected = "sat" :: List.init 270 (fun _ -> "unsat")
      @ ["unsat"; "sat"] in
  check (Frontend.run (Frontend.create ()) (Buffer.contents source) = expected)
    "Bitvector quotient descriptor limit"

let conversion_roundtrips executable =
  let numeral value =
    if Z.sign value < 0 then "(- " ^ Z.to_string (Z.neg value) ^ ")"
    else Z.to_string value in
  List.iter (fun width ->
    let modulus = Z.pow (Z.of_int 2) width in
    let half = Z.pow (Z.of_int 2) (width - 1) in
    List.iter (fun target ->
      let target_modulus = Z.pow (Z.of_int 2) target in
      List.iter (fun value ->
        let signed = if Z.compare value half >= 0
          then Z.sub value modulus else value in
        List.iter (fun (converter, integer) ->
          let _, expected = Z.div_rem integer target_modulus in
          List.iter (fun operator ->
            expect (Printf.sprintf
              {|(declare-const b (_ BitVec %d))
                (assert (= b (_ bv%s %d)))
                (assert (distinct ((_ %s %d) (%s b)) (_ bv%s %d)))
                (check-sat)|}
              width (Z.to_string value) width operator target converter
              (Z.to_string expected) target) ["unsat"])
            ["int2bv"; "int_to_bv"])
          ["ubv_to_int", value; "bv2int", value; "sbv_to_int", signed])
        [Z.zero; Z.one; Z.sub half Z.one; half; Z.sub modulus Z.one])
      [1; 2; 8; 65; 129]) [1; 2; 8; 65; 129];
  expect
    {|(declare-const b (_ BitVec 129))(declare-const x Int)
      (push 1)(assert (< (ubv_to_int b) 0))(check-sat)(pop 1)
      (push 1)(assert (> (sbv_to_int b)
        340282366920938463463374607431768211455))
      (check-sat)(pop 1)
      (push 1)(assert (>= (mod x (- 7)) 7))(check-sat)(pop 1)
      (assert (< (mod x 0) 0))(check-sat)|}
    ["unsat"; "unsat"; "unsat"; "sat"];
  let script = Buffer.create 8192 in
  Buffer.add_string script "(declare-const x Int)";
  let cases = ref 0 in
  List.iter (fun width ->
    let modulus = Z.pow (Z.of_int 2) width in
    let half = Z.pow (Z.of_int 2) (width - 1) in
    let values =
      [Z.sub (Z.neg modulus) Z.one; Z.neg modulus;
       Z.sub (Z.neg half) Z.one; Z.neg half; Z.minus_one; Z.zero;
       Z.sub half Z.one; half; Z.sub modulus Z.one;
       modulus; Z.add modulus Z.one] in
    List.iter (fun value ->
      let _, unsigned = Z.div_rem value modulus in
      let signed = if Z.compare unsigned half >= 0
        then Z.sub unsigned modulus else unsigned in
      Printf.bprintf script
        {|(push 1)(assert (= x %s))
          (assert (or
            (distinct (ubv_to_int ((_ int_to_bv %d) x)) %s)
            (distinct (bv2int ((_ int2bv %d) x)) %s)
            (distinct (sbv_to_int ((_ int_to_bv %d) x)) %s)))
          (check-sat)(pop 1)|}
        (numeral value) width (numeral unsigned) width (numeral unsigned)
        width (numeral signed);
      incr cases) values) [1; 2; 8; 65; 129];
  let source = Buffer.contents script in
  let expected = List.init !cases (fun _ -> "unsat") in
  expect source expected;
  Option.iter (fun executable ->
    check (z3 executable source = expected) "Conversion boundary oracle")
    executable;
  List.iter (fun converter ->
    expect
      (Printf.sprintf
        {|(declare-const x Int)(declare-const b (_ BitVec 16))
          (assert (= b ((_ int_to_bv 16) x)))
          (push 1)(assert (= (+ x 1) (%s b)))(check-sat)(pop 1)
          (assert (= x (- 65537)))(check-sat)
          (get-value ((ubv_to_int b) (sbv_to_int b)))|} converter)
      ["unsat"; "sat";
       "(((ubv_to_int b) 65535) ((sbv_to_int b) (- 1)))"])
    ["ubv_to_int"; "sbv_to_int"];
  expect
    {|(declare-const x Int)(declare-const b (_ BitVec 8))
      (push 1)(assert (= b ((_ int_to_bv 8) x)))
      (assert (= (+ x 1) (ubv_to_int b)))(check-sat)(pop 1)
      (assert (= x 0))(assert (= b #x01))(check-sat)
      (get-value ((ubv_to_int b)))|}
    ["unsat"; "sat"; "(((ubv_to_int b) 1))"];
  List.iter (fun source -> match run source with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid conversion accepted: " ^ source))
    ["(assert (= (ubv_to_int ((_ int_to_bv 0) 7)) 0))";
     "(assert (= (sbv_to_int ((_ int2bv 0) 7)) 0))";
     "(assert (= (ubv_to_int ((_ int_to_bv 8) true)) 0))";
     "(assert (= (ubv_to_int 7) 0))"];
  let terms = Term.create () in
  let x = declaration terms "x" Term.Int in
  let symbol = Term.symbol terms
    { name = "int_to_bv:8"; arguments = [|Term.Int|];
      result = Term.Bitvec 8; kind = Term.Uninterpreted } in
  let bits = Term.apply terms symbol [|x|] in
  let integer = Term.builtin terms "ubv_to_int" Term.Int [|bits|] in
  let solver = Engine.create terms in
  Engine.assert_ solver (Term.equal terms x (Term.integer terms Z.zero));
  Engine.assert_ solver
    (Term.equal terms bits (Term.bits terms 8 Z.one));
  Engine.assert_ solver
    (Term.equal terms integer (Term.integer terms Z.one));
  check (Engine.check solver = Engine.Sat) "Conversion name overreach"

let model_queries executable =
  let verify ?(entailed = true) source expected =
    expect source expected;
    Option.iter
      (fun executable ->
        let outputs = run source in
        let values = List.hd (List.rev outputs) in
        let equations =
          match Vox_smt_sexp.parse values with
          | [Vox_smt_sexp.List pairs] ->
            List.map
              (function
                | Vox_smt_sexp.List [expression; value] ->
                  "(= "
                  ^ Vox_smt_sexp.to_string expression
                  ^ " "
                  ^ Vox_smt_sexp.to_string value
                  ^ ")"
                | _ -> failwith "Malformed get-value output")
              pairs
          | _ -> failwith "Malformed get-value response"
        in
        let prefix =
          String.sub source 0
            (let rec find i =
               if String.sub source i 11 = "(check-sat)"
               then i
               else find (i + 1)
             in
             find 0)
        in
        let values = "(and " ^ String.concat " " equations ^ ")" in
        let assertion = if entailed then "(not " ^ values ^ ")" else values in
        check
          (z3 executable (prefix ^ "(assert " ^ assertion ^ ")(check-sat)")
          = [if entailed then "unsat" else "sat"])
          "Z3 rejected model expression values")
      executable
  in
  verify ~entailed:false
    {|(declare-const x Int)(declare-const y Int)(declare-const p Bool)
      (assert (= x 4))(assert (= y 8))(assert p)
      (assert (= (div x 0) 9))(assert (= (mod x 0) 7))(check-sat)
      (get-value ((div (ite p x y) 0) (mod (ite p x y) 0)
        (div (+ x 1) 0) (mod (+ x 1) 0)))|}
    ["sat";
     "(((div (ite p x y) 0) 9) ((mod (ite p x y) 0) 7) \
      ((div (+ x 1) 0) 9) ((mod (+ x 1) 0) 7))"];
  verify ~entailed:false
    {|(check-sat)(get-value ((div 2 0) (mod 2 0)))|}
    ["sat"; "(((div 2 0) 0) ((mod 2 0) 0))"];
  verify
    {|(declare-const a (_ BitVec 4))(declare-const b (_ BitVec 4))
      (assert (= a #b1101))(assert (= b #b0011))(check-sat)
      (get-value ((bvudiv a b) (bvsdiv a b) (bvsrem a b) (bvsmod a b)
        (bvlshr a b) (bvashr a b) ((_ sign_extend 3) a)
        ((_ rotate_left 2) a) ((_ repeat 3) b) (bvcomp a b)
        (bvredand a) (bvredor a) (sbv_to_int a) ((_ int2bv 4) (- 19))))|}
    [ "sat";
      "(((bvudiv a b) (_ bv4 4)) ((bvsdiv a b) (_ bv15 4)) ((bvsrem a b) (_ \
       bv0 4)) ((bvsmod a b) (_ bv0 4)) ((bvlshr a b) (_ bv1 4)) ((bvashr a \
       b) (_ bv15 4)) (((_ sign_extend 3) a) (_ bv125 7)) (((_ rotate_left 2) \
       a) (_ bv7 4)) (((_ repeat 3) b) (_ bv819 12)) ((bvcomp a b) (_ bv0 1)) \
       ((bvredand a) (_ bv0 1)) ((bvredor a) (_ bv1 1)) ((sbv_to_int a) (- \
       3)) (((_ int2bv 4) (- 19)) (_ bv13 4)))" ];
  expect
    {|(declare-const a (_ BitVec 3))(declare-const p Bool)(check-sat)
      (get-value ((ite p a #b101) (bvadd a #b001)))|}
    ["sat"; "(((ite p a #b101) (_ bv5 3)) ((bvadd a #b001) (_ bv1 3)))"];
  expect
    {|(declare-sort U)(declare-const p Bool)(push)(assert p)
      (check-sat (not p))(pop)(check-sat p)(check-sat)|}
    ["unsat"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(assert (= x 1))(reset-assertions)
      (declare-const x Int)(assert (= x 2))(check-sat)
      (get-value (x))|}
    ["sat"; "((x 2))"];
  List.iter
    (fun query ->
      match run ("(declare-const x Int)(reset-assertions)" ^ query) with
      | _ -> failwith "reset-assertions preserved local declarations"
      | exception Frontend.Error _ -> ())
    ["(assert (= x 0))"];
  let verify_model sorts declarations assertions =
    let formula = "(and " ^ String.concat " " assertions ^ ")" in
    let output =
      run
        (sorts ^ declarations ^ "(assert " ^ formula
       ^ ")(check-sat)(get-model)")
    in
    let definitions =
      match output with
      | ["sat"; model] -> (
        match Vox_smt_sexp.parse model with
        | [Vox_smt_sexp.List definitions] ->
          String.concat "\n" (List.map Vox_smt_sexp.to_string definitions)
        | _ -> failwith "Malformed model")
      | _ -> failwith "Missing satisfiable model"
    in
    let replay =
      sorts ^ definitions ^ "(assert (not " ^ formula ^ "))(check-sat)"
    in
    expect replay ["unsat"];
    Option.iter
      (fun executable ->
        check
          (z3 executable replay = ["unsat"])
          "Z3 rejected a reconstructed model")
      executable
  in
  let verify_abstract () =
    let sorts = "(declare-sort U 0)" in
    let formula = "(and (distinct a b) (= (f a) 3) (= (f b) 4) (= (g a) b))" in
    let output =
      run
        (sorts ^ "(declare-const a U)(declare-const b U)"
       ^ "(declare-fun f (U) Int)(declare-fun g (U) U)" ^ "(assert " ^ formula
       ^ ")(check-sat)(get-model)")
    in
    let atoms = Hashtbl.create 8 in
    let rec replace = function
      | Vox_smt_sexp.List [Vox_smt_sexp.Atom "as"; Vox_smt_sexp.Atom name; sort]
        when String.starts_with ~prefix:"@value" name ->
        let replacement =
          match Hashtbl.find_opt atoms name with
          | Some (replacement, _) -> replacement
          | None ->
            let replacement = "v" ^ string_of_int (Hashtbl.length atoms) in
            Hashtbl.add atoms name (replacement, sort);
            replacement
        in
        Vox_smt_sexp.Atom replacement
      | Vox_smt_sexp.List xs -> Vox_smt_sexp.List (List.map replace xs)
      | atom -> atom
    in
    let definitions =
      match output with
      | ["sat"; model] -> (
        match Vox_smt_sexp.parse model with
        | [Vox_smt_sexp.List definitions] ->
          String.concat "\n"
            (List.map
               (fun definition -> Vox_smt_sexp.to_string (replace definition))
               definitions)
        | _ -> failwith "Malformed abstract model")
      | _ -> failwith "Missing abstract model"
    in
    let values = Hashtbl.fold (fun _ entry xs -> entry :: xs) atoms [] in
    let declarations =
      String.concat ""
        (List.map
           (fun (name, sort) ->
             "(declare-const " ^ name ^ " " ^ Vox_smt_sexp.to_string sort ^ ")")
           values)
    in
    let distinct =
      "(assert (distinct " ^ String.concat " " (List.map fst values) ^ "))"
    in
    let replay =
      sorts ^ declarations ^ definitions ^ distinct ^ "(assert (not " ^ formula
      ^ "))(check-sat)"
    in
    expect replay ["unsat"];
    Option.iter
      (fun executable ->
        check
          (z3 executable replay = ["unsat"])
          "Z3 rejected abstract function interpretations")
      executable
  in
  verify_abstract ();
  verify_model ""
    {|(declare-const x Int)(declare-const y Int)(declare-fun f (Int) Int)
      (declare-fun g (Bool (_ BitVec 4)) Int)|}
    [ "(= x 3)";
      "(= (f 1) 4)";
      "(= (f 2) 8)";
      "(= (g true #b0001) 3)";
      "(= (g false #b0010) 9)";
      "(= (f (+ 1 1)) 8)" ];
  let sorts = "(declare-datatype L ((nil) (cons (head Int) (tail L))))" in
  let lists = "(declare-const x L)(declare-const y L)" in
  verify_model sorts lists ["(distinct x y)"];
  verify_model sorts "(declare-const x L)(declare-const y L)"
    [ "((_ is cons) x)";
      "(= (head x) 3)";
      "((_ is nil) (tail x))";
      "(= y (cons 4 x))" ];
  verify_model
    "(declare-datatype T ((leaf (value Int)) (branch (left T) (right T))))"
    "(declare-const a T)(declare-const b T)(declare-const c T)"
    ["(distinct a b c)"; "(= c (branch a b))"];
  let random = Random.State.make [| 0x63a; 0x332 |] in
  for _ = 1 to 50 do
    let x = Random.State.int random 9 - 4 in
    let y = Random.State.int random 9 - 4 in
    let a = Random.State.int random 16 in
    let b = Random.State.int random 16 in
    let format = Printf.sprintf in
    verify_model ""
      {|(declare-const x Int)(declare-const y Int)
        (declare-fun f (Int) Int)(declare-fun g ((_ BitVec 4)) Int)
        (declare-const a (_ BitVec 4))(declare-const b (_ BitVec 4))|}
      [ format "(= x %s)" (numerals x);
        format "(= y %s)" (numerals y);
        format "(= (f %s) %s)" (numerals x) (numerals (x * x));
        format "(= (f %s) %s)" (numerals y) (numerals (y * y));
        format "(= a (_ bv%d 4))" a;
        format "(= b (_ bv%d 4))" b;
        format "(= (g a) %d)" (a + 1);
        format "(= (g b) %d)" (b + 1) ];
    verify_model sorts
      "(declare-const x L)(declare-const y L)(declare-fun h (L) Int)"
      [ "((_ is cons) x)";
        format "(= (head x) %s)" (numerals x);
        "((_ is nil) (tail x))";
        format "(= y (cons %s x))" (numerals y);
        "(distinct x y)";
        "(= (h x) 1)";
        "(= (h y) 2)" ]
  done

let parametric_datatypes executable =
  expect
    {|(get-option :produce-models)(set-option :produce-models true)
      (get-option :produce-models)(declare-const p Bool)(assert p)
      (push)(assert (not p))(get-assertions)(pop)(get-assertions)
      (reset-assertions)(get-assertions)(get-option :produce-models)
      (reset)(get-option :produce-models)|}
    ["false"; "true"; "(p (not p))"; "(p)"; "()"; "true"; "false"];
  let declarations =
    {|(declare-datatypes ((L 1))
        ((par (T) ((nil) (cons (head T) (tail (L T)))))))
      (declare-const xs (L Int))(declare-const ys (L Bool))|}
  in
  let assertions =
    {|(and (= xs (cons 4 (as nil (L Int))))
      (= ys (cons true (as nil (L Bool)))))|}
  in
  expect
    (declarations ^ "(assert " ^ assertions ^ ")(check-sat)"
   ^ "(get-value ((head xs) (tail xs) (head ys) ((_ is cons) xs)))")
    [ "sat";
      "(((head xs) 4) ((tail xs) (as nil (L Int))) ((head ys) true) (((_ is \
       cons) xs) true))" ];
  expect
    (declarations ^ "(assert (= xs (cons 4 nil)))(assert (= ys nil))"
   ^ "(check-sat)(push)(assert (= (head xs) 5))(check-sat)(pop)"
   ^ "(assert (= (tail xs) nil))(check-sat)")
    ["sat"; "unsat"; "sat"];
  let mutually_recursive =
    {|(declare-datatypes ((Tr 1) (Fr 1))
       ((par (T) ((leaf (value T)) (tree (children (Fr T)))))
        (par (T) ((empty) (trees (first (Tr T)) (rest (Fr T)))))))
      (declare-const x (Tr Int))|}
  in
  let mutually_assertion =
    "(= x (tree (trees (leaf 4) (as empty (Fr Int)))))"
  in
  expect
    (mutually_recursive ^ "(assert " ^ mutually_assertion
   ^ ")(check-sat)(get-value ((value (first (children x)))))")
    ["sat"; "(((value (first (children x))) 4))"];
  expect
    {|(declare-datatypes ((Box 1)) ((par (T) ((box (get T))))))
      (declare-const x (Box (Box Int)))
      (assert (= x (box (box 7))))(check-sat)
      (get-value ((get (get x))))|}
    ["sat"; "(((get (get x)) 7))"];
  expect
    {|(declare-datatypes (T) ((lst nil (cons (car T) (cdr lst)))))
      (declare-const x (lst Int))(declare-const y (lst Int))
      (assert (distinct x y))(check-sat)(push)
      (assert (and (is-nil x) (is-nil y)))(check-sat)(pop)
      (check-sat)|}
    ["sat"; "unsat"; "sat"];
  expect
    {|(push)(declare-datatype L (par (T) ((nil) (cons (head T) (tail (L T))))))
      (declare-const x (L Int))(assert (= x (cons 1 nil)))(check-sat)(pop)
      (declare-datatype L (par (T) ((nil) (cons (head T) (tail (L T))))))
      (declare-const x (L Bool))(assert (= x (cons true nil)))(check-sat)
      (reset-assertions)(declare-sort L 0)(declare-const x L)(check-sat)|}
    ["sat"; "sat"; "sat"];
  expect
    {|(set-option :global-declarations true)(push)
      (declare-datatype L (par (T) ((nil) (cons (head T) (tail (L T))))))
      (declare-const x (L Int))(pop)(reset-assertions)
      (assert (= x (cons 2 nil)))(check-sat)(get-value ((head x)))|}
    ["sat"; "(((head x) 2))"];
  expect
    {|(declare-datatype Optional (par (T) ((none) (some (get T)))))
      (declare-datatype A ((a (field (Optional A)))))
      (declare-const x A)(assert (= x (a (as none (Optional A)))))
      (check-sat)|}
    ["sat"];
  List.iter
    (fun source ->
      match run source with
      | _ -> failwith "Invalid parametric datatype accepted"
      | exception Frontend.Error _ -> ())
    [ "(declare-datatype Bad ((bad (child Bad))))";
      "(declare-datatype Bad (par (T) ((bad (child (Bad T))))))";
      "(declare-datatypes ((Bad 1)) ((par (T U) ((bad)))))";
      declarations ^ "(assert (= xs (cons true nil)))";
      declarations ^ "(check-sat)(get-value (nil))";
      declarations ^ "(assert (= (as nil (L Int)) (as nil (L Bool))))" ];
  let validate declarations assertion =
    let source =
      declarations ^ "(assert " ^ assertion ^ ")(check-sat)(get-model)"
    in
    let frontend = Frontend.create () in
    let model =
      match Frontend.run frontend source with
      | ["sat"; model] -> model
      | _ -> failwith "Parametric model unavailable"
    in
    let definitions =
      match Vox_smt_sexp.parse model with
      | [Vox_smt_sexp.List definitions] -> definitions
      | _ -> failwith "Malformed parametric model"
    in
    let bindings =
      List.map
        (function
          | Vox_smt_sexp.List
              [ Vox_smt_sexp.Atom "define-fun";
                Vox_smt_sexp.Atom name;
                Vox_smt_sexp.List [];
                _;
                value ] ->
            "(= " ^ name ^ " " ^ Vox_smt_sexp.to_string value ^ ")"
          | _ -> failwith "Unexpected parametric model function")
        definitions
    in
    let replay =
      declarations ^ "(assert (and " ^ String.concat " " bindings
      ^ "))(assert (not " ^ assertion ^ "))(check-sat)"
    in
    expect replay ["unsat"];
    Option.iter
      (fun executable ->
        check
          (z3 executable replay = ["unsat"])
          "Z3 rejected parametric datatype model")
      executable
  in
  validate declarations assertions;
  validate mutually_recursive mutually_assertion

let uninterpreted_sort_applications executable =
  let check_case source expected =
    expect source expected;
    Option.iter (fun executable ->
      check (z3 executable source = expected) "Sort application oracle")
      executable in
  check_case
    {|(declare-sort S 1)(declare-const a (S Int))(declare-const b (S Int))
      (declare-fun f ((S Int)) Int)(assert (= a b))
      (assert (distinct (f a) (f b)))(check-sat)|}
    ["unsat"];
  check_case
    {|(declare-sort S 1)(declare-sort T 2)
      (declare-const a (T (S Int) (S Bool)))
      (define-sort Alias () (T (S Int) (S Bool)))
      (declare-const b Alias)(assert (distinct a b))(check-sat)|}
    ["sat"];
  check_case
    {|(declare-sort S 1)
      (declare-datatype Box (par (X) ((box (val (S X))))))
      (declare-const a (S Int))(declare-const b (S Int))
      (assert (distinct a b))
      (assert (= ((as box (Box Int)) a) ((as box (Box Int)) b)))
      (check-sat)|}
    ["unsat"];
  expect
    {|(declare-sort S 1)
      (declare-datatype Box (par (X) ((box (val (S X))))))
      (declare-const a (S Int))(declare-const b (S Int))
      (assert (distinct a b))(assert (= (box a) (box b)))(check-sat)|}
    ["unsat"];
  check_case
    {|(push 1)(declare-sort S 1)(declare-const a (S Int))
      (assert (distinct a a))(check-sat)(pop 1)
      (declare-sort S 2)(declare-const b (S Bool Int))(check-sat)
      (reset)(declare-sort S 1)(declare-const c (S Bool))
      (assert (= c c))(check-sat)|}
    ["unsat"; "sat"; "sat"];
  check_case
    {|(set-option :global-declarations true)(push 1)(declare-sort S 1)
      (declare-const a (S Int))(pop 1)(assert (= a a))(check-sat)
      (reset-assertions)(assert (distinct a a))(check-sat)|}
    ["sat"; "unsat"]

let named_cores executable =
  expect
    {|(set-option :produce-unsat-cores true)
      (declare-const x Int)
      (assert (! (<= x 0) :named low))
      (push 1)(assert (! (>= x 1) :named |high bound|))
      (check-sat)(get-unsat-core)(pop 1)(check-sat)
      (push 1)(assert (! (>= x 2) :named |high bound|))
      (check-sat)(get-unsat-core)(pop 1)(check-sat)
      (reset-assertions)(check-sat)|}
    [ "unsat";
      "(low |high bound|)";
      "sat";
      "unsat";
      "(low |high bound|)";
      "sat";
      "sat" ];
  expect
    {|(declare-const p Bool)(declare-const q Bool)
      (assert (! p :named required))
      (check-sat-assuming ((not p) q))
      (get-unsat-core)(get-unsat-assumptions)
      (check-sat-assuming (p))(check-sat)|}
    ["unsat"; "(required)"; "((not p))"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(assert (! (> x 0) :named positive))
      (assert (not positive))(check-sat)(get-unsat-core)|}
    ["unsat"; "(positive)"];
  let prefix =
    {|(declare-const x Int)(declare-const y Int)(declare-const p Bool)
      (declare-const b (_ BitVec 3))(declare-fun f (Int) Int)
      (assert (and (<= (- 2) x) (<= x 2) (<= (- 2) y) (<= y 2)))|}
  in
  let random = Random.State.make [| 0xc0e; 0x338 |] in
  let pool =
    [| "(<= x 0)";
       "(>= x 1)";
       "(= x y)";
       "(distinct (f x) (f y))";
       "(= ((_ int2bv 3) x) b)";
       "(= b #b010)";
       "(=> p (= x 1))";
       "(or p (= y 2))";
       "(= (f x) 4)"
    |]
  in
  for _ = 1 to 100 do
    let named =
      List.init
        (3 + Random.State.int random 8)
        (fun i ->
          ( Printf.sprintf "|assertion %d|" i,
            pool.(Random.State.int random (Array.length pool)) ))
    in
    let names = List.map fst named in
    let assumptions =
      if Random.State.bool random then ["p"] else ["(not p)"]
    in
    let source =
      prefix
      ^ String.concat ""
          (List.map
             (fun (name, body) ->
               Printf.sprintf "(assert (! %s :named %s))" body name)
             named)
      ^ "(check-sat-assuming ("
      ^ String.concat " " assumptions
      ^ "))"
    in
    let frontend = Frontend.create () in
    match Frontend.run frontend source with
    | ["unsat"] ->
      let parse_core core = match Vox_smt_sexp.parse core with
        | [Vox_smt_sexp.List items] -> List.map Vox_smt_sexp.to_string items
        | _ -> failwith "Malformed core" in
      let core, selected_assumptions =
        match Frontend.run frontend
          "(get-unsat-core)(get-unsat-assumptions)" with
        | [core; selected] -> parse_core core, parse_core selected
        | _ -> failwith "Missing cores"
      in
      check
        (List.for_all (fun x -> List.mem x names) core)
        "Named core contains an unknown label";
      check (List.for_all (fun x -> List.mem x assumptions)
        selected_assumptions) "Assumption core contains an unknown assumption";
      let formulas =
        List.map
          (fun item ->
            match List.assoc_opt item named with
            | Some formula -> formula
            | None -> item)
          core
      in
      let replay =
        prefix
        ^ String.concat ""
            (List.map (fun formula -> "(assert " ^ formula ^ ")") formulas)
        ^ "(check-sat-assuming ("
        ^ String.concat " " selected_assumptions ^ "))"
      in
      expect replay ["unsat"];
      Option.iter
        (fun executable ->
          check
            (z3 executable replay = ["unsat"])
            "Z3 rejected a reported named core")
        executable
    | ["sat"] -> ()
    | _ -> failwith "Unexpected named assertion result"
  done

let named_expressions () =
  expect
    {|(declare-datatypes ((Box 1)) ((par (A) ((box (unbox A))))))
      (define-fun f ((unused (Box Bool))) (Box Int)
        (! (box 7) :named saved))
      (assert (= (unbox saved) 7))(check-sat)
      (get-value ((unbox saved) (unbox (f (box true)))))|}
    ["sat"; "(((unbox saved) 7) ((unbox (f (box true))) 7))"];
  expect
    {|(assert (! true :named removed))(check-sat)(get-assignment)
      (reset-assertions)(check-sat)(get-assignment)
      (assert (! false :named removed))(check-sat)|}
    ["sat"; "((removed true))"; "sat"; "()"; "unsat"];
  expect
    {|(declare-const x Int)
      (assert (! (= (! (+ x 1) :named n) 8) :named a))
      (assert (ite (! (= n 8) :named p) p false))
      (assert (= (! (+ n 1) :named m) m))
      (check-sat)(get-assignment)(get-value (n m a p))
      (push 1)(assert (! (not a) :named bad))(check-sat)
      (pop 1)(check-sat)(get-assignment)
      (push 1)(assert (! true :named temporary))(pop 1)
      (assert (! true :named temporary))(check-sat)(get-assignment)|}
    ["sat"; "((a true) (p true))";
     "((n 8) (m 9) (a true) (p true))";
     "unsat"; "sat"; "((a true) (p true))";
     "sat"; "((a true) (p true) (temporary true))"];
  expect
    {|(declare-const x Int)
      (define-fun f ((y Int)) Int (+ y (! (+ x 1) :named n)))
      (define-const g Bool (! (= n 8) :named p))
      (define-fun h ((x Int)) Int
        (! (let ((x 3)) (+ x 1)) :named independent))
      (assert g)(check-sat)(get-assignment)
      (get-value (n (f 2) (f 3) (h 99) independent))
      (assert (not (! (not p) :named q)))(check-sat)(get-assignment)|}
    ["sat"; "((p true))";
     "((n 8) ((f 2) 10) ((f 3) 11) ((h 99) 4) (independent 4))";
     "sat"; "((p true) (q false))"];
  expect
    {|(set-option :global-declarations true)(push 1)
      (declare-const p Bool)(assert (! p :named kept))(pop 1)
      (assert (not p))(check-sat)(get-assignment)
      (reset-assertions)(assert (not p))(check-sat)(get-assignment)
      (set-option :global-declarations false)(push 1)
      (assert (! true :named gone))(pop 1)(check-sat)(get-assignment)
      (reset-assertions)(assert p)(check-sat)(get-assignment)
      (reset)(assert (! true :named kept))(check-sat)(get-assignment)|}
    ["sat"; "((kept false))"; "sat"; "((kept false))";
     "sat"; "((kept false))"; "sat"; "((kept true))";
     "sat"; "((kept true))"];
  let frontend = Frontend.create () in
  let reject source = match Frontend.run frontend source with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid annotation accepted: " ^ source) in
  ignore (Frontend.run frontend
    "(declare-const x Int)(assert (= x 7))(check-sat)");
  List.iter (fun source ->
    reject source;
    check (Frontend.run frontend "(get-assignment)(get-value (x))"
      = ["()"; "((x 7))"]) "Failed annotation changed model";
    reject "(get-value (n))")
    ["(assert (and (! true :named n) missing))";
     "(assert (and (! true :named n) (! false :named n)))";
     "(define-fun n () Bool (! true :named n))";
     "(define-fun f ((y Int)) Int (! (+ y 1) :named n))";
     "(assert (let ((y x)) (= (! y :named n) 7)))";
     "(assert (! true :named n :named m))"];
  ignore (Frontend.run frontend
    "(push 1)(assert (! true :named n))(pop 1)(check-sat)");
  reject "(get-value (n))";
  ignore (Frontend.run frontend "(assert false)(check-sat)");
  reject "(get-assignment)"

let datatype_updates executable =
  let prefix =
    {|(declare-datatype D ((empty) (node (a Int) (b Bool) (c (_ BitVec 4)))))
      (declare-const x D)|} in
  let cases =
    [{|(assert ((_ is node) x))
       (assert (not ((_ is node) ((_ update-field a) x 1))))
       (check-sat)|}, "unsat";
     {|(assert (not ((_ is node) ((_ update-field a) x 1))))
       (check-sat)|}, "sat";
     {|(assert (distinct (b x) (b ((_ update-field a) x 1))))
       (check-sat)|}, "unsat";
     {|(assert (distinct (a x) (a ((_ update-field a) x 1))))
       (check-sat)|}, "sat";
     {|(assert (distinct ((_ update-field a) empty 1) empty))
       (check-sat)|}, "unsat";
     {|(assert (= x (node 7 false #b0000)))
       (assert (distinct ((_ update-field b)
           ((_ update-field c) ((_ update-field a) x 2) #b0011) true)
           (node 2 true #b0011)))(check-sat)|}, "unsat"] in
  List.iter (fun (source, result) ->
    expect (prefix ^ source) [result];
    Option.iter (fun executable ->
      check (z3 executable (prefix ^ source) = [result])
        "Z3 datatype update mismatch") executable) cases;
  expect
    {|(declare-datatype D ((empty) (node (a Int) (b Bool))))
      (declare-const x D)(assert (= x (node 7 false)))(check-sat)
      (get-value (((_ update a) x 2) ((_ update b) x true)))
      (push 1)(assert (= ((_ update a) x 2) (node 3 false)))
      (check-sat)(pop 1)(check-sat)|}
    ["sat";
     "((((_ update a) x 2) (node 2 false)) \
      (((_ update b) x true) (node 7 true)))";
     "unsat"; "sat"];
  expect
    {|(declare-datatype L (par (A) ((nil) (cons (head A) (tail (L A))))))
      (declare-const x (L Int))(assert ((_ is cons) x))
      (assert (distinct (head ((_ update head) x 3)) 3))(check-sat)|}
    ["unsat"];
  expect
    {|(declare-datatype L (par (A) ((nil) (cons (head A) (tail (L A))))))
      (assert (distinct ((_ update head) nil 3) (as nil (L Int))))
      (check-sat)(reset-assertions)
      (declare-datatype L (par (A) ((nil) (cons (head A) (tail (L A))))))
      (assert (distinct ((_ update tail) (as nil (L Int)) nil)
        (as nil (L Int))))(check-sat)|} ["unsat"; "unsat"];
  expect
    {|(declare-datatypes ((T 1))
        ((par (A) ((|C,| (|fst,| A) (|snd,| A))))))
      (declare-const x (T Int))
      (assert (distinct (|fst,| ((_ update |fst,|) x 3)) 3))
      (check-sat)|} ["unsat"];
  expect
    {|(declare-datatype A ((a (field Int))))
      (declare-datatype B ((b (field Bool))))
      (assert (distinct ((_ update field) (a 3) 4) (a 4)))
      (check-sat)(reset)
      (declare-datatype A ((a (field Int))))
      (declare-datatype B ((b (field Bool))))
      (assert (distinct ((_ update field) (b false) true) (b true)))
      (check-sat)|} ["unsat"; "unsat"];
  expect
    {|(declare-datatype D (par (A) ((c (s A)) (b (flag Bool)))))
      (assert (distinct ((_ update s) ((as b (D Int)) true) false)
        ((as b (D Int)) true)))(check-sat)|} ["unsat"];
  List.iter (fun source ->
    (match run (prefix ^ source) with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid datatype update accepted: " ^ source)))
    ["(assert (= ((_ update a) x true) x))";
     "(assert (= ((_ update a) empty true) empty))";
     "(assert (= ((_ update unknown) x 1) x))";
     "(assert (= ((_ update node) x 1) x))";
     "(assert (= ((_ update a) 0 1) x))";
     "(assert (= ((_ update a) x) x))"]

let datatype_matches executable =
  let source =
    {|(set-option :produce-assignments true)
      (declare-datatypes ((L 1))
        ((par (A) ((nil) (cons (head A) (tail (L A)))))))
      (declare-const xs (L Int))(declare-const ys (L Int))
      (define-fun first ((xs (L Int))) Int
        (match xs ((nil 0) ((cons h t) h))))
      (define-fun rest ((xs (L Int))) (L Int)
        (match xs (((cons h t) t) (other other))))
      (assert (= xs (cons 7 nil)))(assert (= ys nil))
      (push 1)(assert (or (distinct (first xs) 7)
        (distinct (first ys) 0) (distinct (rest xs) ys)))
      (check-sat)(pop 1)
      (assert (= (match xs ((nil nil) ((cons h t) (cons (+ h 1) t))))
        (cons 8 nil)))
      (assert (match ys ((nil (! true :named empty))
        ((cons h t) false))))
      (check-sat)(get-assignment)|} in
  expect source ["unsat"; "sat"; "((empty true))"];
  Option.iter (fun executable ->
    check (z3 executable source = ["unsat"; "sat"; "((empty true))"])
      "Datatype match oracle mismatch") executable;
  expect
    {|(declare-datatype D ((box (value Int))))
      (declare-const d D)(assert (= d (box 3)))
      (assert (= (match d (((box x) (match d (((box y) (+ x y))))))) 6))
      (check-sat)|} ["sat"];
  let prefix =
    "(declare-datatype D ((a) (b (first Int) (second Int))))"
    ^ "(declare-const d D)" in
  List.iter (fun expression ->
    match run (prefix ^ "(assert " ^ expression ^ ")") with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid match accepted: " ^ expression))
    ["(match d ())";
     "(match d ((a true)))";
     "(match d ((a true) (a false) ((b x y) true)))";
     "(match d ((a true) ((b x x) true)))";
     "(match d ((a true) ((b x) true)))";
     "(match d ((a true) ((missing x) true)))";
     "(match d ((other true) (a false)))";
     "(match d ((a true) ((b x y) 1)))";
     "(match 0 ((other true)))";
     "(match d ((a true) ((b x y) (! (= x y) :named n))))"]

let global_declarations () =
  expect
    {|(set-option :global-declarations true)(push 1)
      (declare-sort |U| 0)(declare-const a U)
      (declare-const x Int)(define-fun f ((y Int)) Int (+ x y))
      (assert (! (> x 0) :named positive))
      (pop 1)(assert (not positive))(assert (= (f 1) 1))(check-sat)
      (push 1)(set-option :global-declarations false)
      (declare-const local Int)(assert (= local 9))(pop 1)
      (declare-const local Int)(assert (= local 10))(check-sat)
      (push 1)(set-option :global-decls true)(declare-const kept Int)
      (set-option :global-decls false)(pop 1)
      (assert (= kept 7))(check-sat)
      (reset-assertions)(assert (= x 4))(check-sat)
      (reset)(push 1)(declare-const x Int)(pop 1)
      (declare-const x Int)(check-sat)|}
    ["sat"; "sat"; "sat"; "sat"; "sat"];
  List.iter
    (fun source ->
      match run source with
      | _ -> failwith "Invalid named declaration accepted"
      | exception Frontend.Error _ -> ())
    [ "(assert (! true :named a))(assert (! false :named |a|))";
      "(declare-const a Bool)(assert (! true :named a))";
      "(assert (! true :named 1 :named 2))";
      "(set-option :global-declarations invalid)" ]

let quotient_bounds executable =
  let verify source expected =
    expect source expected;
    Option.iter (fun executable ->
      check (z3 executable source = expected) "Quotient bounds oracle")
      executable in
  List.iter (fun divisor ->
    let numeral = if divisor < 0 then Printf.sprintf "(- %d)" (-divisor)
      else string_of_int divisor in
    let initial = Printf.sprintf
      {|(declare-const x Int)(declare-const k Int)
        (declare-const lower Bool)(declare-const upper Bool)
        (define-fun delta () Int (- x (* %s k)))
        (assert (= lower (<= 0 delta)))
        (assert (= upper (< delta %d)))
        (assert (distinct (div x %s) k))|}
      numeral (abs divisor) numeral in
    verify (initial ^
      {|(check-sat-assuming (lower upper))(check-sat)
        (check-sat-assuming (lower))(check-sat-assuming (upper))
        (push 1)(assert lower)(assert upper)(check-sat)(pop 1)
        (check-sat)(push 1)(assert (= delta 0))(check-sat)(pop 1)
        (push 1)(assert (= delta (- 1)))(check-sat)(pop 1)|})
      ["unsat"; "sat"; "sat"; "sat"; "unsat"; "sat"; "unsat"; "sat"];
    verify (initial ^ Printf.sprintf
      {|(push 1)(assert (= delta %d))(check-sat)(pop 1)
        (assert (= delta %d))(check-sat)|}
      (abs divisor - 1) (abs divisor)) ["unsat"; "sat"])
    [-17; -3; -1; 1; 2; 19];
  verify
    {|(declare-const x Int)(declare-const k Int)(declare-const p Bool)
      (define-fun delta () Int (- x (* 3 k)))
      (assert (=> p (and (<= 0 delta) (< delta 3))))
      (assert (distinct (mod x 3) delta))
      (check-sat-assuming (p))(check-sat)
      (push 1)(assert p)(check-sat)(pop 1)(check-sat)
      (check-sat-assuming (p))|}
    ["unsat"; "sat"; "unsat"; "sat"; "unsat"];
  verify
    {|(declare-const x Int)(declare-const k Int)
      (assert (= (- x (* 0 k)) 0))
      (assert (distinct (div x 0) k))(check-sat)|} ["sat"];
  verify
    {|(declare-const x Int)(declare-const k Int)
      (define-fun delta () Int
        (- x (* (- 18446744073709551617) k)))
      (assert (<= 0 delta))(assert (< delta 18446744073709551617))
      (assert (distinct (div x (- 18446744073709551617)) k))
      (check-sat)|} ["unsat"]

let conditional_division executable =
  let numeral n =
    if Z.sign n < 0 then Printf.sprintf "(- %s)" (Z.to_string (Z.neg n))
    else Z.to_string n
  in
  List.iter (fun divisor ->
    List.iter (fun value ->
      let quotient, remainder = Z.div_rem (Z.of_int (-value))
          (Z.of_int divisor) in
      let plain_quotient, _ = Z.div_rem (Z.of_int value)
          (Z.of_int divisor) in
      let _, quotient_remainder = Z.div_rem
          (Z.mul (Z.of_int 5) plain_quotient) (Z.of_int 7) in
      let source = Printf.sprintf
          {|(declare-const x Int)(declare-const p Bool)
            (declare-const d Int)
            (assert (<= %s x))(assert (<= x %s))
            (assert (<= (* %s (div x %s)) x))
            (define-fun y () Int (ite p (- x) x))
            (push 1)(assert p)
            (assert (or (distinct (div y %s) %s)
                        (distinct (mod y %s) %s)))
            (check-sat)(pop 1)
            (push 1)(assert (not p))(assert (= d %s))
            (assert (= (+ (* d (div y d)) (mod y d)) x))
            (check-sat)(pop 1)
            (push 1)
            (assert (distinct (mod (* 5 (div x %s)) 7) %s))
            (check-sat)(pop 1)
            (assert p)(assert (= d %s))
            (assert (or (distinct (div y d) %s)
                        (distinct (mod y d) %s)))(check-sat)|}
          (numerals value) (numerals value)
          (numerals divisor) (numerals divisor)
          (numerals divisor) (numeral quotient)
          (numerals divisor) (numeral remainder)
          (numerals (-divisor)) (numerals divisor)
          (numeral quotient_remainder) (numerals divisor)
          (numeral quotient) (numeral remainder)
      in
      let expected = ["unsat"; "sat"; "unsat"; "unsat"] in
      expect source expected;
      Option.iter (fun executable ->
        check (z3 executable source = expected) "Division oracle mismatch")
        executable)
      [-129; -128; -1; 0; 129])
    [-128; -3; 3; 128]

let bitvector_extensions executable =
  expect
    {|(declare-const x Int)(assert ((_ divisible 7) x))
      (push 1)(assert (= x (- 21)))(check-sat)(pop 1)
      (push 1)(assert (= x (- 20)))(check-sat)(pop 1)
      (assert (= x 0))(check-sat)|}
    ["sat"; "unsat"; "sat"];
  expect
    {|(declare-const x Int)
      (assert ((_ divisible 100000000000000000000000000000000000000) x))
      (assert (= x (- 100000000000000000000000000000000000000)))
      (check-sat)|}
    ["sat"];
  expect
    {|(declare-const x (_ BitVec 4))
      (assert (or (distinct (bv2nat x) (ubv_to_int x))
                  (distinct (bv2nat #b1111) 15)))(check-sat)|}
    ["unsat"];
  expect
    {|(declare-const x Int)(assert (= x 81))
      (assert (distinct (div x 3 3) 9))(check-sat)|}
    ["unsat"];
  expect
    {|(declare-const x (_ BitVec 8))(declare-const y (_ BitVec 8))
      (declare-const c (_ BitVec 1))
      (assert (distinct (bvite c (bvite c y x) y)
                        (bvite c y (bvite c x y))))(check-sat)|}
    ["unsat"];
  expect
    {|(declare-const x (_ BitVec 4))
      (assert (distinct (concat #b1 x #b0)
                        (concat (concat #b1 x) #b0)))(check-sat)|}
    ["unsat"];
  List.iter
    (fun op ->
      expect
        (Printf.sprintf
           "(declare-const x (_ BitVec 4))(declare-const y (_ BitVec 4))\n\
            (assert (distinct (%s x y #b0001) (%s (%s x y) #b0001)))\n\
            (check-sat)"
           op op op)
        ["unsat"])
    ["bvand"; "bvor"; "bvxor"; "bvxnor"; "bvadd"; "bvmul"];
  let frontend = Frontend.create () in
  check
    (Frontend.run
       ~cancelled:(fun () -> true)
       frontend "(check-sat)(get-info :reason-unknown)"
    = ["unknown"; "(:reason-unknown \"cancelled\")"])
    "Cancellation reason";
  List.iter
    (fun source ->
      match Frontend.run frontend source with
      | _ -> failwith "Reason-unknown survived a changed result"
      | exception Frontend.Error _ -> ())
    [ "(check-sat)(get-info :reason-unknown)";
      "(reset)(get-info :reason-unknown)" ];
  Option.iter
    (fun executable ->
      let random = Random.State.make [| 0x8f10; 0x28a4 |] in
      let predicates =
        [| "bvuaddo";
           "bvsaddo";
           "bvusubo";
           "bvssubo";
           "bvumulo";
           "bvsmulo";
           "bvsdivo"
        |]
      in
      for iteration = 1 to 100 do
        let width = 1 + Random.State.int random 6 in
        let atom () =
          if Random.State.int random 8 = 0
          then "(bvnego x)"
          else
            Printf.sprintf "(%s x y)"
              predicates.(Random.State.int random (Array.length predicates))
        in
        let literal () =
          let atom = atom () in
          if Random.State.bool random then atom else "(not " ^ atom ^ ")"
        in
        let source =
          Printf.sprintf
            "(declare-const x (_ BitVec %d))(declare-const y (_ BitVec %d))\n\
             (assert (or %s %s))(assert %s)(check-sat)(push 1)\n\
             (assert (= x (_ bv%d %d)))(assert %s)(check-sat)(pop 1)\n\
             (check-sat)"
            width width (literal ()) (literal ()) (literal ())
            (Random.State.int random (1 lsl width))
            width (literal ())
        in
        check
          (run source = z3 executable source)
          (Printf.sprintf "BV overflow differential case %d\n%s" iteration
             source)
      done)
    executable

let differential executable =
  let random = Random.State.make [| 0x19ef; 0x623 |] in
  let prefix =
    {|(set-logic ALL)(set-option :timeout 2000)
      (declare-sort U 0)(declare-const x Int)(declare-const y Int)
      (declare-const a U)(declare-const b U)(declare-const p Bool)
      (declare-const u (_ BitVec 3))(declare-const v (_ BitVec 3))
      (declare-fun f (Int) Int)(declare-fun g (U) Int)
      (declare-fun h ((_ BitVec 3)) U)(declare-fun k (U) (_ BitVec 3))
      (assert (and (<= (- 2) x) (<= x 2) (<= (- 2) y) (<= y 2)))|}
  in
  let atoms =
    [| "(= x y)";
       "(< x y)";
       "(<= (+ x 1) y)";
       "(= (f x) (f y))";
       "(<= (f x) (f y))";
       "(= a b)";
       "(= (g a) (g b))";
       "(= u v)";
       "(bvult u v)";
       "(= (h u) (h v))";
       "(= (k a) (k b))";
       "(= (g (h u)) (g (h v)))";
       "(= ((_ int2bv 3) x) u)";
       "(= ((_ int2bv 3) y) v)";
       "(= (bv2int u) (+ x 2))";
       "(= (bv2int v) (+ y 2))";
       "(= u (bvadd v #b001))";
       "(= x (ite p y 1))";
       "(= (h u) a)";
       "(= (h v) b)";
       "(= u (k a))";
       "(= v (k b))";
       "(= (f (+ x y)) (+ (f x) (f y)))"
    |]
  in
  let literal () =
    let atom = atoms.(Random.State.int random (Array.length atoms)) in
    if Random.State.bool random then atom else "(not " ^ atom ^ ")"
  in
  let expression () =
    if Random.State.int random 3 = 0
    then literal ()
    else
      let op = [| "and"; "or"; "=>" |].(Random.State.int random 3) in
      Printf.sprintf "(%s %s %s)" op (literal ()) (literal ())
  in
  let assertions count =
    List.init count (fun _ -> "(assert " ^ expression () ^ ")")
    |> String.concat "\n"
  in
  for iteration = 1 to 250 do
    let source =
      prefix
      ^ assertions (3 + Random.State.int random 7)
      ^ "(check-sat)(check-sat-assuming (p))(push 1)"
      ^ assertions (1 + Random.State.int random 3)
      ^ "(check-sat)(pop 1)(check-sat-assuming ((not p)))(check-sat)"
    in
    let actual = run source and expected = z3 executable source in
    check (actual = expected)
      (Printf.sprintf "Differential SMT case %d: [%s] <> [%s]\n%s" iteration
         (String.concat "; " actual)
         (String.concat "; " expected)
         source)
  done

let modular_polynomial_scopes () =
  expect
    {|(set-option :global-declarations true)
      (declare-const x Int)(declare-const a Int)(declare-const b Int)
      (declare-const p Bool)(declare-fun f (Int) Int)
      (push)
      (assert (= (f x) (mod (+ x 15) (- 5))))
      (assert (= a (f x)))(assert (= b (mod x 5)))
      (assert (and (<= 0 a) (< a 5) (<= 0 b) (< b 5)))
      (assert (distinct a b))(check-sat)(pop)
      (assert (= a 0))(assert (= b 1))(check-sat)
      (reset-assertions)
      (assert (= a 0))(assert (= b (+ a 5)))
      (assert (and (<= 0 a) (<= a 5) (<= 0 b) (<= b 5)))
      (assert (= (mod a (- 5)) (mod b 5)))
      (assert (distinct a b))(check-sat)
      (reset)
      (declare-const x Int)(declare-const a Int)(declare-const p Bool)
      (assert (= a (ite p (mod (+ x 15) (- 5)) (mod x 5))))
      (assert (distinct a (mod x 5)))(check-sat)
      (reset-assertions)
      (declare-const x Int)(declare-const a Int)(declare-const p Bool)
      (assert p)(assert (= x 0))
      (assert (= a (ite p (mod (+ x 1) (- 5)) (mod x 5))))
      (assert (distinct a (mod x 5)))(check-sat)|}
    ["unsat"; "sat"; "sat"; "unsat"; "sat"]

let elaboration_contexts expanded =
  let expand source =
    if not expanded then source
    else begin
      let module S = Vox_smt_sexp in
      let command = function
        | S.List [S.Atom "assert"; expression] ->
          S.List [S.Atom "assert";
            S.List (S.Atom "and" :: List.init 1024 (fun _ -> expression))]
        | expression -> expression
      in
      String.concat "\n"
        (List.map (fun expression -> S.to_string (command expression))
          (S.parse source))
    end
  in
  let expect source expected = expect (expand source) expected in
  expect
    {|(define-fun f ((x Int)) Int (+ x 1))
      (assert (and (= (f 1) 2) (= (f 2) 3)
        (= (let ((x 1)) (+ x 1)) 2)
        (= (let ((x 2)) (+ x 1)) 3)))
      (check-sat)|}
    ["sat"];
  expect
    {|(declare-datatype L (par (T) ((nil) (cons (head T) (tail (L T))))))
      (assert (and (= (as nil (L Int)) (ite true nil nil))
                   (= (as nil (L Bool)) (ite true nil nil))))
      (check-sat)|}
    ["sat"];
  expect
    {|(push)(declare-sort U 0)(declare-const x U)
      (assert (= (as x U) x))(check-sat)(pop)
      (define-sort U () Int)(declare-const x U)
      (assert (= (as x U) x))(assert (= x 3))(check-sat)
      (reset-assertions)(define-sort U () Bool)(declare-const x U)
      (assert (= (as x U) x))(assert x)(check-sat)
      (reset)(set-option :global-declarations true)(push)
      (define-sort U () Int)(declare-const x U)(pop)
      (assert (= (as x U) x))(assert (= x 7))(check-sat)
      (reset-assertions)(assert (= (as x U) x))(check-sat)|}
    ["sat"; "sat"; "sat"; "sat"; "sat"];
  let frontend = Frontend.create () in
  let execute source = Frontend.run frontend (expand source) in
  let expression =
    List.hd (Vox_smt_sexp.parse (expand "(assert (= x x))")) in
  ignore (execute "(declare-const x Int)");
  ignore (Frontend.execute frontend expression);
  check (execute "(check-sat)" = ["sat"]) "Cached integer symbol";
  ignore (execute "(reset)(declare-const x Bool)");
  ignore (Frontend.execute frontend expression);
  check (execute "(assert x)(check-sat)" = ["sat"])
    "Cached symbol after reset";
  let invalid =
    List.hd (Vox_smt_sexp.parse (expand "(assert (= missing 0))")) in
  (match Frontend.execute frontend invalid with
  | _ -> failwith "Unknown cached symbol accepted"
  | exception Frontend.Error _ -> ());
  ignore (execute "(declare-const missing Int)");
  ignore (Frontend.execute frontend invalid);
  check (execute "(check-sat)" = ["sat"]) "Cache after failed elaboration";
  ignore (execute "(reset)(push)(declare-const x Int)");
  let body = List.hd (Vox_smt_sexp.parse "(+ x 1)") in
  let define name =
    let module S = Vox_smt_sexp in
    ignore (Frontend.execute frontend
      (S.List [S.Atom "define-fun"; S.Atom name; S.List [];
               S.Atom "Int"; body])) in
  ignore (execute "(set-option :global-declarations true)");
  define "f";
  ignore (execute
    "(set-option :global-declarations false)(pop)(declare-const x Int)");
  define "g";
  check (execute
    "(assert (= x 2))(assert (and (= f 2) (= g 3)))(check-sat)" = ["sat"])
    "Captured definition environments"

let temporary_terms () =
  let build temporary =
    let terms = Term.create () in
    let sort = Term.Named 0 in
    let constructor () = Term.symbol terms
      { name = "C"; arguments = [|Term.Int|]; result = sort;
        kind = Term.Constructor 0 } in
    let constructor = constructor () in
    let selector () = Term.symbol terms
      { name = "field"; arguments = [|sort|]; result = Term.Int;
        kind = Term.Selector (constructor, 0) } in
    let original_selector = selector () in
    let original_tester = Term.tester terms constructor in
    let x = declaration terms "x" Term.Int in
    let mutate () =
      ignore (selector ());
      ignore (Term.symbol terms
        { name = "test"; arguments = [|sort|]; result = Term.Bool;
          kind = Term.Tester constructor });
      ignore (Term.symbol terms
        { name = "D"; arguments = [||]; result = sort;
          kind = Term.Constructor 0 });
      for i = 0 to 256 do
        let y = declaration terms ("temporary" ^ string_of_int i) Term.Int in
        ignore (Term.builtin terms "+" Term.Int [|x; y|])
      done
    in
    if temporary then begin
      let count = Term.count terms in
      (match Term.with_temporary terms (fun () ->
        mutate ();
        let inner_count = Term.count terms in
        Term.with_temporary terms mutate;
        check (Term.count terms = inner_count) "Nested temporary terms";
        raise Exit) with
      | _ -> failwith "Temporary exception swallowed"
      | exception Exit -> ());
      check (Term.count terms = count) "Temporary terms leaked";
      check (Term.constructors terms 0 = [constructor])
        "Temporary constructors leaked";
      check (Term.selector terms constructor 0 = original_selector)
        "Temporary selector replaced";
      check (Term.tester terms constructor = original_tester)
        "Temporary tester replaced"
    end;
    let y = declaration terms "y" Term.Int in
    let sum = Term.builtin terms "+" Term.Int [|x; y|] in
    let symbol = Term.symbol terms
      { name = "after"; arguments = [||]; result = sort;
        kind = Term.Constructor 0 } in
    Term.count terms, sum, Term.node terms sum, symbol
  in
  check (build false = build true) "Temporary allocation order changed"

let macro_definitions () =
  List.iter (fun source ->
    let frontend = Frontend.create () in
    (match Frontend.run frontend source with
    | _ -> failwith ("Invalid definition accepted: " ^ source)
    | exception (Frontend.Error _ | Invalid_argument _) -> ());
    check (Frontend.run frontend
      "(define-fun f () Bool true)(assert (not f))(check-sat)" = ["unsat"])
      ("Failed definition retained its name: " ^ source))
    ["(define-fun f () Bool 4)";
     "(define-fun f () Bool missing)";
     "(define-fun f () Bool f)";
     "(define-fun f ((x Int)) Bool (+ x 1))";
     "(define-fun f ((x Bool)) Int (+ x 1))";
     "(define-fun f ((x Int) (|x| Bool)) Bool x)";
     "(define-fun f ((x Int extra)) Bool true)";
     "(define-fun f ((x Missing)) Bool true)";
     "(define-fun f () Missing true)";
     "(define-fun f () Bool (let ((x true) (y x)) y))";
     "(define-fun f () Bool (let ((x true) (x false)) x))";
     "(define-fun f ((x Bool)) Bool (ite 4 x false))";
     "(define-fun g ((x Int)) Int x)\
      (define-fun f () Int (g true))";
     "(define-fun g ((x Int)) Int x)\
      (define-fun f () Bool (= ((as g Bool) 0) 0))";
     "(declare-datatypes ((List 1))\
       ((par (T) ((nil) (cons (head T) (tail (List T)))))))\
      (define-fun f ((x (List Int))) Bool (head x))"];
  let frontend = Frontend.create () in
  check (Frontend.run frontend
    "(declare-const x Int)(assert (= x 4))(check-sat)" = ["sat"])
    "Model before invalid definition";
  (match Frontend.run frontend "(define-fun f () Bool x)" with
  | _ -> failwith "Invalid model-preserving definition accepted"
  | exception Frontend.Error _ -> ());
  check (Frontend.run frontend "(get-value (x))" = ["((x 4))"])
    "Invalid definition discarded the current model";
  expect
    {|(define-fun identity ((x Int)) Int x)
      (define-fun f ((x Int)) Int (let ((x true) (y x)) (identity y)))
      (assert (distinct (f 4) 4))(check-sat)|} ["unsat"];
  expect
    {|(declare-datatypes ((List 1))
        ((par (T) ((nil) (cons (head T) (tail (List T)))))))
      (define-fun singleton ((x Int)) (List Int) (cons x nil))
      (define-fun empty ((x (List Int))) Bool ((_ is nil) x))
      (push)(define-fun f () Bool (empty (as nil (List Int))))
      (assert f)(check-sat)(pop)
      (define-fun f () Bool (= (head (singleton 5)) 5))
      (assert (not f))(check-sat)|} ["sat"; "unsat"]

let overloaded_datatypes executable =
  let prelude =
    "(declare-datatype A ((none) (some (val Int))))\
     (declare-datatype B ((none) (some (val Bool))))" in
  let verify body expected =
    let source = prelude ^ body in
    expect source expected;
    Option.iter (fun executable ->
      check (z3 executable source = expected) "Overloaded datatype result")
      executable
  in
  verify
    "(declare-const a A)(declare-const b B)\
     (assert (= a (as none A)))(assert (= b (as none B)))\
     (check-sat)" ["sat"];
  verify
    "(declare-const a A)(declare-const b B)\
     (assert (= a (some 4)))(assert (= b (some true)))\
     (assert (distinct (val a) 4))(check-sat)" ["unsat"];
  verify
    "(define-fun f ((x A)) Int (val x))\
     (define-fun g ((x B)) Bool (val x))\
     (assert (g (some true)))(assert (= (f (some 4)) 4))\
     (check-sat)" ["sat"];
  expect
    (prelude ^ "(declare-const a A)(declare-const b B)\
     (assert (= a (as none A)))(assert (= b (some true)))\
     (assert ((_ is none) a))(assert ((_ is some) b))(check-sat)\
     (get-value (a b))(get-model)")
    ["sat"; "((a (as none A)) (b ((as some B) true)))";
     "((define-fun a () A (as none A))\n \
      (define-fun b () B ((as some B) true)))"];
  expect
    {|(declare-datatype A ((node (val Int))))
      (declare-datatype B ((make (node Int))))
      (assert (= (node (make 4)) 4))(assert (= (val (node 3)) 3))
      (assert ((_ is node) (node 3)))(check-sat)|} ["sat"];
  expect
    {|(declare-datatype A ((none)))
      (define-fun old () A none)
      (push)(declare-datatype B ((none)))
      (assert (= old (as none A)))(check-sat)(pop)
      (assert (= old none))(check-sat)
      (push)(declare-datatype B ((none)))
      (set-option :global-declarations true)
      (declare-datatype C ((none)))
      (set-option :global-declarations false)(pop)(reset-assertions)
      (define-fun f () C none)(assert ((_ is none) f))(check-sat)|}
    ["sat"; "sat"; "sat"];
  expect
    {|(declare-datatypes ((P 1) (Q 1))
      ((par (T) ((none) (some (val T))))
       (par (T) ((none) (some (val T))))))
      (define-fun f ((x Int)) (P Int) (some x))
      (define-fun g ((x Bool)) (Q Bool) (some x))
      (assert (= (val (f 3)) 3))(assert (val (g true)))(check-sat)|}
    ["sat"];
  List.iter (fun body ->
    let frontend = Frontend.create () in
    ignore (Frontend.run frontend prelude);
    (match Frontend.run frontend ("(assert " ^ body ^ ")") with
    | _ -> failwith ("Invalid overload accepted: " ^ body)
    | exception (Frontend.Error _ | Invalid_argument _) -> ());
    check (Frontend.run frontend
      "(assert (= (some 4) ((as some A) 4)))(check-sat)" = ["sat"])
      "Overload resolution recovery")
    ["(= none none)"; "(= (some #b0) (some #b0))";
     "(= (val 4) 4)"; "((_ is val) (some 4))"];
  List.iter (fun source ->
    match run source with
    | _ -> failwith "Duplicate datatype member accepted"
    | exception Frontend.Error _ -> ())
    ["(declare-datatype A ((none) (none)))";
     "(declare-datatype A ((first (val Int)) (second (val Bool))))"]

let model_handle_lifetimes () =
  let module Model = Vox_smt_model in
  let unavailable query =
    match query () with
    | exception Model.Unavailable _ -> ()
    | _ -> failwith "Unavailable model accepted" in
  let invalid query =
    match query () with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "Invalid solver operation accepted" in
  let setup scoped =
    let terms = Term.create () in
    let solver = Engine.create terms in
    unavailable (fun () -> Model.create ~terms ~solver);
    let x = declaration terms "x" Term.Int in
    let f = Term.symbol terms
        { name = "f"; arguments = [|Term.Int|]; result = Term.Int;
          kind = Term.Uninterpreted } in
    let fx = Term.apply terms f [|x|] in
    Engine.register solver x;
    List.iter (fun (term, value) -> Engine.assert_ solver
      (Term.equal terms term (Term.integer terms (Z.of_int value))))
      [x, 4; fx, 9];
    if scoped then Engine.push solver;
    check (Engine.check solver = Engine.Sat) "Model lifetime setup";
    let model = Model.create ~terms ~solver in
    check (Model.value model x = Model.Integer (Z.of_int 4))
      "Initial model value";
    check (Model.value model fx = Model.Integer (Z.of_int 9))
      "Initial function value";
    ignore (Model.interpretation model f);
    terms, solver, x, f, model in
  List.iter (fun change ->
    let terms, solver, x, f, model = setup true in
    change terms solver;
    let stale () =
      unavailable (fun () -> Model.value model x);
      unavailable (fun () -> Model.value model (Term.integer terms Z.zero));
      unavailable (fun () -> Model.interpretation model f);
      unavailable (fun () -> Model.to_string model ~sort_name:string_of_int
        (Model.Integer Z.zero)) in
    stale ();
    check (Engine.check solver = Engine.Sat) "Model lifetime recovery";
    ignore (Model.value (Model.create ~terms ~solver) x);
    stale ())
    [ (fun terms solver -> Engine.assert_ solver (Term.boolean terms true));
      (fun _ solver -> Engine.push solver);
      (fun _ solver -> Engine.pop solver);
      (fun terms solver ->
        let fresh = declaration terms "fresh" Term.Int in
        Engine.register solver fresh;
        check (Model.value (Model.create ~terms ~solver) fresh
          = Model.Integer Z.zero) "Extended model generation");
      (fun _ solver -> ignore (Engine.check solver));
      (fun _ solver -> ignore (Engine.For_testing.check
        ~reorder_after:0 solver));
      (fun terms solver -> ignore (Engine.check
        ~assumptions:[Term.boolean terms false] solver));
      (fun _ solver -> ignore (Engine.check ~budget:0 solver));
      (fun _ solver -> ignore (Engine.check ~cancelled:(fun () -> true) solver));
      (fun _ solver ->
        match Engine.check ~cancelled:(fun () -> raise Exit) solver with
        | exception Exit -> ()
        | _ -> failwith "Model lifetime callback exception") ];
  let terms, solver, x, f, model = setup false in
  Engine.register solver x;
  List.iter invalid
    [ (fun () -> Engine.assert_ solver x);
      (fun () -> Engine.pop solver);
      (fun () -> Engine.register solver min_int);
      (fun () -> ignore (Engine.check ~budget:(-1) solver));
      (fun () -> ignore (Engine.For_testing.check ~reorder_after:(-1) solver));
      (fun () -> ignore (Engine.For_testing.check
        ~comparison_after:(-1) solver)) ];
  check (Model.value model x = Model.Integer (Z.of_int 4))
    "Rejected operation invalidated the model";
  ignore (Model.interpretation model f);
  check (Model.to_string model ~sort_name:string_of_int
    (Model.value model x) = "4") "Valid model rendering";
  unavailable (fun () -> Model.to_string model
    ~sort_name:(fun _ -> Engine.push solver; "U") (Model.Abstract (0, 0)));
  check (Engine.check ~assumptions:[Term.boolean terms false] solver
    = Engine.Unsat [Term.boolean terms false]) "Unsat lifetime fixture";
  unavailable (fun () -> Model.create ~terms ~solver)

let frontend_check_exceptions () =
  List.iter (fun (setup, solve, query, answer, values) ->
    let frontend = Frontend.create () in
    ignore (Frontend.run frontend setup);
    let expected = answer :: values in
    check (Frontend.run frontend (solve ^ query) = expected)
      "Initial frontend result";
    (match Frontend.run frontend "(check-sat-assuming (missing))" with
    | exception Frontend.Error _ -> ()
    | _ -> failwith "Invalid assumption accepted");
    check (Frontend.run frontend query = values)
      "Invalid assumption discarded the previous result";
    (match Frontend.run ~cancelled:(fun () -> raise Exit) frontend solve with
    | exception Exit -> ()
    | _ -> failwith "Cancellation callback exception was swallowed");
    List.iter (fun command ->
      match Frontend.run frontend command with
      | exception Frontend.Error _ -> ()
      | _ -> failwith ("Stale result after exceptional check: " ^ command))
      ["(get-value (x))"; "(get-model)"; "(get-unsat-core)";
       "(get-unsat-assumptions)"];
    check (Frontend.run frontend (solve ^ query) = expected)
      "Frontend did not recover after exceptional check")
    [ "(declare-const x Int)(assert (= x 4))",
      "(check-sat)", "(get-value (x))", "sat", ["((x 4))"];
      "(declare-const x Int)(declare-const p Bool)"
      ^ "(assert (! (not p) :named bad))",
      "(check-sat-assuming (p))",
      "(get-unsat-core)(get-unsat-assumptions)",
      "unsat", ["(bad)"; "(p)"] ]

let disjunctive_equality_cuts () =
  let prefix =
    {|(declare-sort U 0)
      (declare-const a U)(declare-const b U)(declare-const c U)
      (declare-const d U)(declare-const p Bool)
      (define-fun diamond () Bool
        (or (and (= a b) (= b c)) (and (= a d) (= d c))))|} in
  expect (prefix ^
    {|(check-sat-assuming (diamond (distinct a c)))
      (check-sat-assuming ((not diamond) (distinct a c)))
      (push 1)(assert diamond)(assert (distinct a c))(check-sat)(pop 1)
      (assert (distinct a c))(check-sat)|})
    ["unsat"; "sat"; "unsat"; "sat"];
  expect (prefix ^
    {|(assert (or (and (= a b) (= b c)) p))
      (assert (distinct a c))(check-sat)|}) ["sat"];
  expect (prefix ^
    {|(assert (or (and (= a b) (distinct b c)) (and (= a d) (= d c))))
      (assert (distinct a c))(check-sat)|}) ["sat"];
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "(declare-sort U 0)";
  for i = 0 to 80 do
    List.iter (fun name -> Printf.bprintf buffer "(declare-const %s%d U)" name i)
      ["x"; "y"; "z"]
  done;
  for i = 0 to 79 do
    Printf.bprintf buffer
      "(assert (or (and (= x%d y%d) (= y%d x%d)) \
                   (and (= x%d z%d) (= z%d x%d))))"
      i i i (i+1) i i i (i+1)
  done;
  Buffer.add_string buffer "(assert (distinct x0 x80))(check-sat)";
  expect (Buffer.contents buffer) ["unsat"]

let indexed_integer_extensions () =
  expect
    {|(check-sat)(get-value (((_ iand 4) (- 1) 5)
      ((_ iand 4) (- 2) (- 3)) ((_ iand 129) (- 1) 5)
      (mod_total 7 0) (mod_total (- 7) 0)
      (mod_total (- 7) (- 3)) (mod_total 7 (- 3))))|}
    ["sat";
     "((((_ iand 4) (- 1) 5) 5) (((_ iand 4) (- 2) (- 3)) 12) \
      (((_ iand 129) (- 1) 5) 5) ((mod_total 7 0) 7) \
      ((mod_total (- 7) 0) (- 7)) ((mod_total (- 7) (- 3)) 2) \
      ((mod_total 7 (- 3)) 1))"];
  let buffer = Buffer.create 16384 in
  Buffer.add_string buffer "(declare-const x Int)(declare-const y Int)";
  let int n = if n < 0 then Printf.sprintf "(- %d)" (-n)
    else string_of_int n in
  List.iter (fun width ->
    for x = -4 to 4 do
      for y = -4 to 4 do
        let result = (x land y) land ((1 lsl width) - 1) in
        let remainder = if y = 0 then x else
          ((x mod abs y) + abs y) mod abs y in
        Printf.bprintf buffer
          "(push 1)(assert (= x %s))(assert (= y %s))\
           (assert (or (distinct ((_ iand %d) x y) %d)\
             (distinct (mod_total x y) %s)))(check-sat)(pop 1)"
          (int x) (int y) width result (int remainder)
      done
    done) [1; 3; 5];
  expect (Buffer.contents buffer) (List.init 243 (fun _ -> "unsat"));
  expect
    {|(declare-const x Int)(assert (or (distinct (mod_total 0 x) 0)
      (distinct (mod_total x x) 0)))(check-sat)|} ["unsat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (= (mod_total x y) x))(assert (= y 0))(check-sat)
      (push 1)(assert (= ((_ iand 4) x y) 1))(check-sat)(pop 1)
      (check-sat)|} ["sat"; "unsat"; "sat"];
  List.iter (fun source ->
    (match run source with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid integer extension accepted: " ^ source)))
    ["(assert (= ((_ iand 0) 1 1) 1))";
     "(assert (= ((_ iand 3) true 1) 1))";
     "(assert (= ((_ iand 3) 1) 1))";
     "(assert (= (mod_total 1) 1))";
     "(assert (= (mod_total true 1) 1))"];
  expect
    {|(declare-fun mod_total (Bool) Bool)(assert (mod_total true))
      (check-sat)|} ["sat"]

let arithmetic_extensions executable =
  List.iter (fun source ->
    (match run source with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Malformed arithmetic extension accepted: " ^ source)))
    ["(assert (= (~ 1 2) 0))"; "(assert (~ true))";
     "(assert (iff 1 2))"; "(assert (iff true))";
     "(assert (implies true))"; "(assert (rem 1 true))";
     "(assert (= (rem 1) 0))"];
  expect
    {|(check-sat)(get-value ((~ 4) (~ (- 4))
      (rem 7 3) (rem (- 7) 3) (rem 7 (- 3)) (rem (- 7) (- 3))))|}
    ["sat";
     "(((~ 4) (- 4)) ((~ (- 4)) 4) ((rem 7 3) 1) \
      ((rem (- 7) 3) 2) ((rem 7 (- 3)) (- 1)) \
      ((rem (- 7) (- 3)) (- 2)))"];
  let sources =
    [{|(declare-const x Int)(declare-const y Int)
       (assert (distinct (rem x y)
         (ite (< y 0) (- (mod x y)) (mod x y))))(check-sat)|};
     {|(declare-const x Int)(assert (distinct (rem x 0) (mod x 0)))
       (check-sat)|};
     {|(declare-const p Bool)(declare-const q Bool)(declare-const r Bool)
       (assert (or (distinct (iff p q r) (= p q r))
         (distinct (implies p q r) (=> p q r))
         (distinct (if_then_else p q r) (ite p q r))))(check-sat)|};
     {|(declare-datatype L (par (A) ((nil) (cons (head A) (tail (L A))))))
       (declare-const p Bool)(declare-const l (L Int))
       (assert (distinct (if_then_else p nil l) (ite p nil l)))
       (check-sat)|}]
  in
  List.iter (fun source ->
    expect source ["unsat"];
    Option.iter (fun executable ->
      check (z3 executable source = ["unsat"])
        "Arithmetic extension semantics disagree with Z3") executable)
    sources;
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer "(declare-const x Int)(declare-const y Int)";
  for x = -5 to 5 do
    for y = -4 to 4 do
      if y <> 0 then begin
        let int n = if n < 0 then Printf.sprintf "(- %d)" (-n)
          else string_of_int n in
        let r = ((x mod abs y) + abs y) mod abs y in
        let r = if y < 0 then -r else r in
        Printf.bprintf buffer
          "(push 1)(assert (= x %s))(assert (= y %s))\
           (assert (distinct (rem x y) %s))(check-sat)(pop 1)"
          (int x) (int y) (int r)
      end
    done
  done;
  let source = Buffer.contents buffer in
  let expected = List.init 88 (fun _ -> "unsat") in
  expect source expected;
  Option.iter (fun executable ->
    check (z3 executable source = expected) "Signed remainder matrix")
    executable;
  expect
    {|(declare-fun ~ (Bool Bool) Bool)
      (declare-fun rem (Bool) Bool)(declare-fun iff (Int) Bool)
      (declare-fun implies (Int) Int)
      (declare-fun if_then_else (Int Int Int) Int)
      (assert (~ true false))(assert (rem false))(assert (iff 1))
      (assert (= (implies 2) 3))(assert (= (if_then_else 1 2 3) 2))
      (check-sat)|} ["sat"]

let unbounded_integer_extensions () =
  expect
    {|(declare-const x Int)(push 1)
      (assert (> (int.pow2 1048576) x))(check-sat)
      (pop 1)(assert (= x 0))(check-sat)|} ["unknown"; "sat"];
  expect
    {|(declare-const k Int)(declare-const x Int)(declare-const y Int)
      (assert (> k 0))(assert (<= 0 (piand k x y)))(check-sat)|} ["sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (< x y))(assert (= (* y y) 4))
      (assert (<= (int.pow2 y) (int.pow2 x)))(check-sat)
      (get-value ((int.pow2 y) (int.pow2 x)))|}
    ["sat"; "(((int.pow2 y) 0) ((int.pow2 x) 0))"];
  expect
    {|(declare-const k Int)(declare-const x Int)(declare-const y Int)
      (assert (= k 5))(assert (= x (- 1)))(assert (= y (- 2)))
      (assert (= (piand k x y) 30))(check-sat)
      (get-value ((piand k x y) (int.pow2 k) (int.log2 (int.pow2 k))))
      (push 1)(assert (distinct (piand k x y) 30))(check-sat)
      (pop 1)(check-sat)(reset-assertions)
      (declare-const k Int)(declare-const x Int)(declare-const y Int)
      (assert (< k 0))
      (assert (distinct (piand k x y) 0))(check-sat)|}
    ["sat"; "(((piand k x y) 30) ((int.pow2 k) 32) \
      ((int.log2 (int.pow2 k)) 5))"; "unsat"; "sat"; "unsat"];
  expect
    {|(declare-const x Int)(assert (= (int.log2 x) 5))
      (push 1)(assert (> x 100))(check-sat)(pop 1)
      (assert (< x 100))(check-sat)
      (get-value ((int.log2 x)))|}
    ["unsat"; "sat"; "(((int.log2 x) 5))"];
  expect
    {|(declare-fun int.pow2 (Int) Int)(assert (= (int.pow2 3) 7))
      (declare-fun int.log2 (Int) Int)(assert (= (int.log2 16) 99))
      (declare-fun piand (Int Int Int) Int)(assert (= (piand 0 1 1) 8))
      (check-sat)|} ["sat"];
  expect
    {|(declare-const x Int)(assert (= x 7))(check-sat)
      (get-value ((int.pow2 x) (int.log2 x) (piand 5 x (- 1))
        (int.pow2 (- 1)) (int.log2 0) (piand (- 1) (- 1) (- 1))))|}
    ["sat"; "(((int.pow2 x) 128) ((int.log2 x) 2) \
      ((piand 5 x (- 1)) 7) ((int.pow2 (- 1)) 0) ((int.log2 0) 0) \
      ((piand (- 1) (- 1) (- 1)) 0))"]

let small_polynomial_models () =
  expect
    {|(set-option :rlimit 20000)
      (declare-const x Int)(declare-const y Int)(declare-const z Int)
      (assert (= (+ (* x x x) (* y y y) (* z z z)) 6))
      (check-sat)(get-value ((+ (* x x x) (* y y y) (* z z z))))|}
    ["sat"; "(((+ (* x x x) (* y y y) (* z z z)) 6))"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (= (* x y) 1))(check-sat)
      (check-sat-assuming ((< x 0)))(get-value ((* x y) (< x 0)))
      (check-sat-assuming ((= x 2)))(check-sat)
      (push 1)(assert (= x 0))(check-sat)(pop 1)(check-sat)
      (get-value ((* x y)))|}
    ["sat"; "sat"; "(((* x y) 1) ((< x 0) true))"; "unsat"; "sat";
     "unsat"; "sat"; "(((* x y) 1))"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (assert (= x (+ y 8)))(assert (= (* y y) 1))
      (check-sat)(get-value ((- x y) (* y y)))|}
    ["sat"; "(((- x y) 8) ((* y y) 1))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (assert (= (* x y) 49))(assert (= x z))(assert (= z 7))
      (check-sat)(get-value ((+ x y z)))|}
    ["sat"; "(((+ x y z) 21))"]


let equality_lattice_scopes () =
  expect
    {|(declare-const y Int)(declare-const z Int)(declare-fun f (Int) Int)
      (define-fun sum () Int (+ (* 4294967296 y) (* 3123612579 z)))
      (assert (> (f y) 0))(check-sat)(push 1)
      (assert (<= sum 24))(assert (>= sum 24))(assert (< z 0))
      (check-sat)(get-value (sum (< z 0)))
      (check-sat-assuming ((> sum 24)))(check-sat)(pop 1)
      (push 1)(assert (= sum 25))(assert (> z 0))
      (check-sat)(get-value (sum (> z 0)))(pop 1)
      (assert (= y 0))(assert (= z 0))(check-sat)(get-value (sum))|}
    ["sat"; "sat"; "((sum 24) ((< z 0) true))"; "unsat"; "sat";
     "sat"; "((sum 25) ((> z 0) true))"; "sat"; "((sum 0))"]

let polynomial_residues () =
  expect
    {|(declare-const x Int)(declare-const p Bool)
      (define-fun impossible () Bool (= (+ (* x x) x) 1))
      (assert (! (or p impossible) :named choice))
      (check-sat)(get-value (p impossible))
      (check-sat-assuming ((not p)))
      (get-unsat-core)(get-unsat-assumptions)(check-sat)
      (push 1)(assert (! impossible :named square))
      (check-sat)(get-unsat-core)(pop 1)(check-sat)
      (assert (not impossible))(check-sat)|}
    ["sat"; "((p true) (impossible false))";
     "unsat"; "(choice)"; "((not p))"; "sat";
     "unsat"; "(square)"; "sat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-fun f (Int) Int)
      (assert (= (+ (* x x) x (* x y)) 1))
      (assert (= x 1))(assert (= y (- 1)))
      (assert (= (+ (* (f x) (f x)) (f x)) 2))
      (check-sat)(get-value ((+ (* x x) x (* x y))
        (+ (* (f x) (f x)) (f x))))|}
    ["sat"; "(((+ (* x x) x (* x y)) 1) " ^
     "((+ (* (f x) (f x)) (f x)) 2))"];
  expect
    {|(declare-const x Int)
      (assert (= (* (- 18446744073709551616) x x)
        18446744073709551617))(check-sat)|} ["unsat"]

let small_polynomial_model_resources () =
  let module F = Vox_smt_frontend in
  let module R = Vox_smt_resource in
  let module S = Vox_smt_sexp in
  let require expected actual =
    if expected <> actual then failwith (String.concat "; " actual)
  in
  let setup () =
    let source = Buffer.create 512 in
    for index = 0 to 7 do
      Printf.bprintf source "(declare-const x%d Int)" index
    done;
    Buffer.add_string source "(define-fun squares () Int (+";
    for index = 0 to 7 do
      Printf.bprintf source " (* x%d x%d)" index index
    done;
    Buffer.add_string source "))(define-fun product () Int (*";
    for index = 0 to 7 do Printf.bprintf source " x%d" index done;
    Buffer.add_string source
      "))(assert (= squares 6))(assert (= product 0))";
    let frontend = F.create () in
    require [] (F.run frontend (Buffer.contents source));
    frontend
  in
  let command = S.List [S.Atom "check-sat"] in
  let recover frontend =
    require ["sat"; "((squares 6) (product 0))"; "unsat"; "sat"]
      (F.run frontend
         "(check-sat)(get-value (squares product))\
          (push 1)(assert (= squares 5))(check-sat)(pop 1)(check-sat)")
  in
  let resource = R.create () in
  let frontend = setup () in
  if F.execute ~resource frontend command <> Some "sat" then
    failwith "seeded model missing";
  recover frontend;
  let used = R.used resource in
  for budget = 0 to used do
    let frontend = setup () and resource = R.create ~budget () in
    (match F.execute ~resource frontend command with
    | Some ("unknown" | "sat") -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "seeded budget answer");
    if R.used resource > budget then failwith "seeded budget exceeded";
    recover frontend
  done;
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  if F.execute ~resource (setup ()) command <> Some "sat" then
    failwith "charged seeded model missing";
  for stop = 1 to !calls do
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; !seen >= stop) () in
    (match F.execute ~resource frontend command with
    | Some "unknown" -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "seeded cancellation ignored");
    recover frontend;
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; if !seen = stop then raise Exit else false) () in
    (match F.execute ~resource frontend command with
    | exception Exit -> ()
    | _ -> failwith "seeded callback exception ignored");
    recover frontend
  done

let cubic_polynomial_models () =
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (define-fun cubes () Int (+ (* x x x) (* y y y) (* z z z)))
      (assert (= cubes 159))(assert (> x 0))(assert (< y 0))
      (assert (> z 0))(check-sat)
      (get-value (cubes (> x 0) (< y 0) (> z 0)))
      (push 1)(assert (= cubes 160))(check-sat)(pop 1)(check-sat)|}
    ["sat"; "((cubes 159) ((> x 0) true) ((< y 0) true) ((> z 0) true))";
     "unsat"; "sat"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (define-fun cubes () Int
        (+ (* (- 7) (- x) x x) (- (* 7 y y y)) (* 7 z z z)))
      (assert (= (- 5 cubes) (- 1108)))(check-sat)
      (get-value (cubes))|}
    ["sat"; "((cubes 1113))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (declare-const p Bool)
      (define-fun cubes () Int (+ (* x x x) (* y y y) (* z z z)))
      (assert (or (and p (= cubes 813)) (and (not p) (= cubes 159))))
      (assert (ite p (< x 0) (> x 0)))(check-sat)
      (get-value ((or (= cubes 813) (= cubes 159))
        (ite p (< x 0) (> x 0))))|}
    ["sat"; "(((or (= cubes 813) (= cubes 159)) true) " ^
     "((ite p (< x 0) (> x 0)) true))"]

let cubic_polynomial_resources () =
  let module F = Vox_smt_frontend in
  let module R = Vox_smt_resource in
  let module S = Vox_smt_sexp in
  let require expected actual =
    if expected <> actual then failwith (String.concat "; " actual)
  in
  let setup () =
    let frontend = F.create () in
    require [] (F.run frontend
      "(declare-const x Int)(declare-const y Int)(declare-const z Int)\
       (define-fun cubes () Int (+ (* x x x) (* y y y) (* z z z)))\
       (assert (= cubes 159))");
    frontend
  in
  let command = S.List [S.Atom "check-sat"] in
  let recover frontend =
    require ["sat"; "((cubes 159))"; "unsat"; "sat"]
      (F.run frontend
         "(check-sat)(get-value (cubes))\
          (push 1)(assert (= cubes 160))(check-sat)(pop 1)(check-sat)")
  in
  let resource = R.create () in
  if F.execute ~resource (setup ()) command <> Some "sat" then
    failwith "cubic model missing";
  let used = R.used resource in
  List.iter (fun budget ->
    let frontend = setup () and resource = R.create ~budget () in
    (match F.execute ~resource frontend command with
    | Some ("unknown" | "sat") -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "cubic budget answer");
    if R.used resource > budget then failwith "cubic budget exceeded";
    recover frontend)
    [0; 1; 255; 256; 4095; 4096; used / 4; used / 2; used - 1; used];
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  if F.execute ~resource (setup ()) command <> Some "sat" then
    failwith "charged cubic model missing";
  List.iter (fun stop ->
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; !seen >= stop) () in
    (match F.execute ~resource frontend command with
    | Some "unknown" -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "cubic cancellation ignored");
    recover frontend;
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; if !seen = stop then raise Exit else false) () in
    (match F.execute ~resource frontend command with
    | exception Exit -> ()
    | _ -> failwith "cubic callback exception ignored");
    recover frontend)
    [1; 2; !calls / 2; !calls - 1; !calls];
  let frontend = setup () in
  require [] (F.run frontend
    "(push 1)(declare-const w Int)(assert (= (div x w) y))");
  (match F.execute ~resource:(R.create ~budget:300000 ()) frontend command with
  | Some ("sat" | "unknown") -> ()
  | exception R.Exhausted _ -> ()
  | _ -> failwith "cubic zero divisor answer");
  require [] (F.run frontend "(pop 1)");
  recover frontend

let partition_scopes () =
  List.iter (fun sign ->
    let number n = if sign < 0 then Printf.sprintf "(- %d)" n
      else string_of_int n in
    expect
      (Printf.sprintf
        {|(declare-const d Int)
          (define-fun i () Int (+ (* %s (div (* 3 d) %s))
            (mod (+ (* 3 d) 2) %s)))
          (define-fun r () Int (mod (* d i (- 2 (* d i))) %s))
          (assert (distinct r 1))
          (push 1)(assert (= (mod d 2) 1))(check-sat)(pop 1)
          (push 1)(assert (= d 2))(check-sat)(get-value (r))(pop 1)
          (push 1)(assert (= d (- 3)))(check-sat)(pop 1)
          (push 1)(assert (= d 0))(check-sat)(get-value (r))(pop 1)
          (assert (= (mod d 2) 1))(check-sat)|}
        (number 4) (number 4) (number 4) (number 256))
      ["unsat"; "sat"; "((r 208))"; "unsat"; "sat"; "((r 0))"; "unsat"])
    [1; -1];
  expect
    {|(declare-const d Int)
      (define-fun i () Int (+ (* 4 (div (* 3 d) 4))
        (mod (+ (* 3 d) 2) 4)))
      (define-fun r () Int (mod (* d i (- 3 (* d i))) 256))
      (assert (= d 3))(assert (distinct r 1))(check-sat)(get-value (r))|}
    ["sat"; "((r 34))"];
  expect
    {|(declare-const d Int)(assert (= d 1))
      (assert (= (mod (div (* 8 d) 4) 8) 2))(check-sat)|} ["sat"];
  expect
    {|(declare-const d Int)(assert (= d 8))
      (assert (distinct (mod (* (div d 4) (- (div d 4) 1)) 256) 0))
      (check-sat)|} ["sat"]

let partition_resources () =
  let module F = Vox_smt_frontend in
  let module R = Vox_smt_resource in
  let module S = Vox_smt_sexp in
  let require expected actual =
    if expected <> actual then failwith (String.concat "; " actual) in
  let setup () =
    let frontend = F.create () in
    require [] (F.run frontend
      "(declare-const d Int)\
       (define-fun i () Int (+ (* 4 (div (* 3 d) 4))\
          (mod (+ (* 3 d) 2) 4)))\
       (define-fun r () Int (mod (* d i (- 2 (* d i))) 256))\
       (assert (distinct r 1))(push 1)(assert (= (mod d 2) 1))");
    frontend in
  let command = S.List [S.Atom "check-sat"] in
  let recover frontend =
    require ["unsat"; "sat"; "((r 208))"; "unsat"]
      (F.run frontend
         "(check-sat)(pop 1)(push 1)(assert (= d 2))\
          (check-sat)(get-value (r))(pop 1)(push 1)\
          (assert (= (mod d 2) 1))(check-sat)") in
  let resource = R.create () in
  if F.execute ~resource (setup ()) command <> Some "unsat" then
    failwith "partition conflict missing";
  let used = R.used resource in
  let budgets =
    [0; 1; 255; 256; 4095; 4096; used / 4; used / 2; used - 1; used] in
  List.iter (fun budget ->
    let frontend = setup () and resource = R.create ~budget () in
    (match F.execute ~resource frontend command with
    | Some ("unknown" | "unsat") -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "partition budget answer");
    if R.used resource > budget then failwith "partition budget exceeded";
    recover frontend) budgets;
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  if F.execute ~resource (setup ()) command <> Some "unsat" then
    failwith "charged partition conflict missing";
  let stops = List.init !calls (fun i -> i + 1) in
  List.iter (fun stop ->
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; !seen >= stop) () in
    (match F.execute ~resource frontend command with
    | Some "unknown" -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "partition cancellation ignored");
    recover frontend;
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; if !seen = stop then raise Exit else false) () in
    (match F.execute ~resource frontend command with
    | exception Exit -> ()
    | _ -> failwith "partition callback exception ignored");
    recover frontend) stops

let symbolic_modular_scopes () =
  List.iter (fun sign ->
    expect
      (Printf.sprintf
        {|(declare-const d Int)(declare-const i Int)(declare-const j Int)
          (declare-const k Int)(declare-const s Int)
          (assert (= (* d i) (+ (* k s) 1)))
          (assert (= j (* i (- 2 (* d i)))))
          (define-fun r () Int (mod (* d j) (* %s s s)))
          (assert (distinct r 1))
          (push 1)(assert (> s 1))(check-sat)(pop 1)
          (push 1)(assert (< s (- 1)))(check-sat)(pop 1)
          (assert (= s 1))(check-sat)(get-value (r))|} sign)
      ["unsat"; "unsat"; "sat"; "((r 0))"])
    ["1"; "(- 1)"];
  expect
    {|(declare-const d Int)(declare-const i Int)(declare-const j Int)
      (declare-const k Int)(declare-const s Int)
      (assert (= (* d i) (+ (* k s) 1)))
      (assert (= j (* i (- 2 (* d i)))))
      (define-fun r () Int (mod (* d j) (* 2 s s)))
      (assert (= d 3))(assert (= i 1))(assert (= k 1))(assert (= s 2))
      (assert (distinct r 1))(check-sat)(get-value (r))|}
    ["sat"; "((r 5))"]

let symbolic_modular_resources () =
  let module F = Vox_smt_frontend in
  let module R = Vox_smt_resource in
  let module S = Vox_smt_sexp in
  let require expected actual =
    if expected <> actual then failwith (String.concat "; " actual)
  in
  let setup () =
    let frontend = F.create () in
    require [] (F.run frontend
      "(declare-const d Int)(declare-const i Int)(declare-const j Int)\
       (declare-const k Int)(declare-const s Int)\
       (assert (= (* d i) (+ (* k s) 1)))\
       (assert (= j (* i (- 2 (* d i)))))\
       (define-fun r () Int (mod (* d j) (* s s)))\
       (assert (distinct r 1))(push 1)(assert (> s 1))");
    frontend
  in
  let command = S.List [S.Atom "check-sat"] in
  let recover frontend =
    require ["unsat"; "sat"; "((r 0))"; "unsat"]
      (F.run frontend
         "(check-sat)(pop 1)(push 1)(assert (= s 1))\
          (check-sat)(get-value (r))(pop 1)(push 1)\
          (assert (> s 1))(check-sat)")
  in
  let resource = R.create () in
  if F.execute ~resource (setup ()) command <> Some "unsat" then
    failwith "symbolic modular conflict missing";
  let used = R.used resource in
  List.iter (fun budget ->
    let frontend = setup () and resource = R.create ~budget () in
    (match F.execute ~resource frontend command with
    | Some ("unknown" | "unsat") -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "symbolic modular budget answer");
    if R.used resource > budget then
      failwith "symbolic modular budget exceeded";
    recover frontend)
    [0; 1; 255; 256; 4095; 4096; used / 4; used / 2; used - 1; used];
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  if F.execute ~resource (setup ()) command <> Some "unsat" then
    failwith "charged symbolic modular conflict missing";
  List.iter (fun stop ->
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; !seen >= stop) () in
    (match F.execute ~resource frontend command with
    | Some "unknown" -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "symbolic modular cancellation ignored");
    recover frontend;
    let frontend = setup () and seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen; if !seen = stop then raise Exit else false) () in
    (match F.execute ~resource frontend command with
    | exception Exit -> ()
    | _ -> failwith "symbolic modular callback exception ignored");
    recover frontend)
    [1; 2; !calls / 2; !calls - 1; !calls]

let bounded_polynomial_models () =
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (assert (= (+ (* x x x) (* y y y) (* z z z)) 327))
      (check-sat)(get-value ((+ (* x x x) (* y y y) (* z z z))))|}
    ["sat"; "(((+ (* x x x) (* y y y) (* z z z)) 327))"];
  expect
    {|(declare-const y0 Int)(declare-const x1 Int)(declare-const x2 Int)
      (declare-const x3 Int)(declare-const x4 Int)
      (define-fun x0 () Int (ite (< y0 0) (- y0) y0))
      (define-fun p () Bool
        (and (>= x3 2)
          (<= (+ (* x4 x2) (* 2 x2 x0 x2) x1 (* x1 x4 x4)) 9)
          (= (+ (* 3 x0 x4) x2) (- 5))
          (= (+ x1 (* 3 x2 x3) (* x3 x4) (* (- 2) x0 x1 x0)) (- 10))
          (or (= x1 2) (= (* x1 x1) 0))))
      (assert p)(check-sat)(get-value (p))|}
    ["sat"; "((p true))"];
  expect
    {|(declare-const x Int)(assert (> x 0))
      (assert (< (* 1099511627776 x x) 0))(check-sat)|} ["unsat"]

let boolean_polynomial_models () =
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const z Int)
      (declare-const p Bool)
      (assert (or p (= (+ (* x x x) (* y y y) (* z z z)) 4)))
      (check-sat)(get-value (p))|} ["sat"; "((p true))"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (declare-const p Bool)(declare-const q Bool)
      (define-fun sum () Int (+ (* 7 x x x) (* 7 y y)))
      (assert (and (= p q) (or p (= sum (- 273)))
        (or (not q) (= sum (- 273)))))
      (check-sat)(get-value (sum (= p q)))|}
    ["sat"; "((sum (- 273)) ((= p q) true))"];
  expect
    {|(declare-const x Int)(declare-const y Int)
      (declare-const p Bool)(declare-const q Bool)
      (define-fun sum () Int (+ (* (- 7) x x x x) (* (- 1) y y)))
      (assert (and (= (not p) q) (not p)
        (or (not q) (= sum (- 16811)))))
      (check-sat)(get-value (sum p q))|}
    ["sat"; "((sum (- 16811)) (p false) (q true))"];
  expect
    {|(declare-const x Int)(declare-const y Int)(declare-const p Bool)
      (define-fun sum () Int (+ (* 3 x x x x) (* (- 1) y y)))
      (assert (and (not p) (= (ite (not p) sum 0) 3887)))
      (check-sat)(get-value (sum p))|}
    ["sat"; "((sum 3887) (p false))"]

let declared_overloads executable =
  let verify source expected =
    expect source expected;
    Option.iter (fun executable ->
      check (Vox_smt_sexp.parse
        (String.concat "\n" (z3 executable source))
        = Vox_smt_sexp.parse (String.concat "\n" expected))
        "Declared overload result")
      executable in
  verify
    {|(declare-datatype Enum ((cons (val Int)) (none)))
      (declare-fun cons (Int Int) Int)(declare-fun cons (Enum) Bool)
      (declare-fun cons (Bool) Int)
      (assert (cons (cons 0)))
      (assert (= (cons (cons true) (cons false)) 4))(check-sat)|} ["sat"];
  verify
    {|(declare-fun f (Bool) Int)(declare-fun f (Int) Bool)
      (assert (= (f true) 4))(assert (f 4))(check-sat)
      (get-value ((f true) (f 4)))
      (push)(assert (not (f (f true))))(check-sat)(pop)
      (check-sat)|} ["sat"; "(((f true) 4) ((f 4) true))"; "unsat"; "sat"];
  verify
    {|(declare-fun f (Int) Int)(declare-fun f (Int) Bool)
      (assert (= ((as f Int) 4) 6))(assert ((as f Bool) 4))
      (check-sat)(get-value (((as f Int) 4) ((as f Bool) 4)))|}
    ["sat"; "((((as f Int) 4) 6) (((as f Bool) 4) true))"];
  expect
    {|(declare-fun node (Bool) Bool)
      (declare-datatype Box ((node (value Int))))
      (assert (node true))(assert ((_ is node) (node 4)))
      (assert (= (value (node 4)) 4))(check-sat)|} ["sat"];
  verify
    {|(declare-fun f (Int) Int)
      (define-fun old ((x Int)) Int (f x))
      (push)(declare-fun f (Bool) Int)(assert (= (f true) 4))
      (assert (= (old 3) (f 3)))(check-sat)(pop)
      (assert (distinct (old 3) (f 3)))(check-sat)|} ["sat"; "unsat"];
  expect
    {|(declare-fun f (Int) Int)(push)(declare-fun f (Bool) Int)
      (set-option :global-declarations true)
      (declare-fun f ((_ BitVec 1)) Int)
      (set-option :global-declarations false)(pop)
      (assert (= (f 0) 1))(assert (= (f #b0) 2))(check-sat)
      (reset-assertions)(assert (= (f #b0) 3))(check-sat)|}
    ["sat"; "sat"];
  expect
    {|(set-option :global-declarations true)
      (declare-const x Int)(declare-const x Bool)
      (assert (= (as x Int) 9))(assert (as x Bool))(check-sat)
      (get-value ((as x Int) (as x Bool)))(get-model)
      (reset-assertions)(assert (= (as x Int) 9))
      (assert (as x Bool))(check-sat)
      (get-value ((as x Int) (as x Bool)))|}
    ["sat"; "(((as x Int) 9) ((as x Bool) true))";
     "((define-fun x () Int 9)\n (define-fun x () Bool true))";
     "sat"; "(((as x Int) 9) ((as x Bool) true))"];
  expect
    {|(declare-datatype Box ((box (value Int))))
      (declare-fun value (Bool) Int)
      (assert (= (value true) 3))
      (assert (= (value ((_ update value) (box 1) 4)) 4))
      (check-sat)|} ["sat"];
  let frontend = Frontend.create () in
  ignore (Frontend.run frontend
    "(declare-fun f (Int) Int)(declare-fun f (Bool) Int)\
     (declare-const x Int)(declare-const x Bool)\
     (assert (= (f true) 5))(check-sat)");
  List.iter (fun source ->
    (match Frontend.run frontend source with
    | exception Frontend.Error _ -> ()
    | _ -> failwith ("Invalid declared overload accepted: " ^ source));
    check (Frontend.run frontend "(get-value ((f true)))"
      = ["(((f true) 5))"]) "Declared overload failure changed model")
    ["(declare-fun f (Bool) Int)"; "(declare-const x Int)";
     "(get-value ((f #b0)))"; "(get-value (x))";
     "(get-value (((as f Bool) 1)))"]

let bitvector_congruence_model_repair () =
  List.iter (fun width ->
    let setup = Printf.sprintf
      {|(set-logic QF_UFBV)
        (declare-const x (_ BitVec %d))
        (declare-const y (_ BitVec %d))
        (declare-const z (_ BitVec %d))
        (declare-fun f ((_ BitVec %d)) (_ BitVec %d))|}
      width width width width width in
    let one = Printf.sprintf "(_ bv1 %d)" width in
    List.iter (fun (left, right) ->
      expect (setup ^ Printf.sprintf
        {|(assert (bvult (f %s) (_ bv10 %d)))
          (push 1)
          (assert (bvugt (f %s) (_ bv20 %d)))
          (check-sat)(pop 1)(check-sat)|}
        left width right width)
        ["unsat"; "sat"])
      [Printf.sprintf "(bvadd (bvsub x %s) %s)" one one, "x";
       "(bvor x (bvand y z))", "(bvand (bvor x y) (bvor x z))";
       "(bvlshr (bvlshr x y) z)", "(bvlshr (bvlshr x z) y)"])
    [8; 63]

let zero_divisor_congruence executable =
  let verify source expected =
    expect source expected;
    Option.iter (fun executable ->
      check (z3 executable source = expected) "Zero-divisor oracle mismatch")
      executable in
  List.iter (fun op ->
    let initial = Printf.sprintf
      {|(declare-const x Int)(declare-const y Int)
        (declare-const d Int)(declare-const p Bool)
        (assert (and (<= 0 x) (<= x 1) (<= 0 y) (<= y 1)))
        (assert (or (= d 0) (= d 1)))
        (assert (= (%s x 0) 7))(assert (= (%s y d) 9))
        (assert (=> p (= x y)))|} op op in
    verify (initial ^
      {|(check-sat)(check-sat-assuming (p))(check-sat)
        (push 1)(assert (= x y))(check-sat)(pop 1)(check-sat)
        (push 1)(assert (= d 1))(check-sat)(pop 1)(check-sat)|})
      ["sat"; "unsat"; "sat"; "unsat"; "sat"; "unsat"; "sat"];
    let source = initial ^ Printf.sprintf
      {|(assert (= x 0))(assert (= y 1))(check-sat)
        (get-value (x y d (%s x 0) (%s y d)))|} op op in
    expect source ["sat"; Printf.sprintf
      "((x 0) (y 1) (d 0) ((%s x 0) 7) ((%s y d) 9))" op op])
    ["div"; "mod"];
  verify
    {|(declare-const x Int)(declare-const y Int)(declare-const d Int)
      (assert (= (div x 0) 7))(assert (= (mod x 0) 9))
      (assert (= (div y d) 11))(assert (= (mod y d) 13))
      (push 1)(assert (= x y))(assert (= d 0))(check-sat)(pop 1)
      (assert (distinct x y))(assert (= d 0))(check-sat)|}
    ["unsat"; "sat"]

let bitvector_completion_models () =
  List.iter (fun width ->
    let setup = Printf.sprintf
      {|(declare-const x (_ BitVec %d))
        (declare-const y (_ BitVec %d))
        (declare-fun f ((_ BitVec %d)) Bool)
        (assert (f x))(check-sat)|} width width width in
    expect (setup ^
      {|(push 1)(assert (not (f y)))(check-sat)
        (get-value ((distinct x y)))
        (push 1)(assert (= x y))(check-sat)(pop 1)(check-sat)
        (pop 1)(assert (= x y))(check-sat)(get-value ((f y)))|})
      ["sat"; "sat"; "(((distinct x y) true))"; "unsat"; "sat";
       "sat"; "(((f y) true))"];
    expect (setup ^
      {|(push 1)(assert (not (f y)))(check-sat)
        (get-value ((distinct x y)))
        (pop 1)(assert (= x y))(assert (not (f y)))(check-sat)|})
      ["sat"; "sat"; "(((distinct x y) true))"; "unsat"])
    [1; 8; 63; 128];
  expect
    {|(declare-const x (_ BitVec 8))(declare-const y (_ BitVec 8))
      (declare-fun f ((_ BitVec 8)) Int)
      (assert (= (f x) 1))(assert (= (f y) 2))(check-sat)
      (get-value ((distinct x y)))
      (push 1)(assert (= x y))(check-sat)(pop 1)(check-sat)
      (assert (= (ubv_to_int x) 7))(assert (= (ubv_to_int y) 9))
      (check-sat)(get-value (x y))|}
    ["sat"; "(((distinct x y) true))"; "unsat"; "sat"; "sat";
     "((x (_ bv7 8)) (y (_ bv9 8)))"]

let finite_domain_symmetry () =
  expect
    {|(set-option :global-declarations true)(declare-sort U 0)
      (declare-const a U)(declare-const b U)(declare-const c U)
      (declare-const x U)(declare-const y U)
      (declare-fun f (U) U)(declare-fun g (U U) U)
      (define-fun domain ((v U)) Bool (or (= v a) (= v b) (= v c)))
      (define-fun base () Bool
        (and (distinct a b c) (domain x) (domain y)
          (domain (f x)) (domain (f y)) (domain (g x y))
          (distinct x y) (= (f x) y) (= (f y) x) (= (g x y) x)))
      (assert base)(check-sat)(get-value (base))
      (push 1)(assert (= x c))(check-sat)(get-value (base (= x c)))
      (push 1)(assert (= y c))(check-sat)(pop 1)(check-sat)(pop 1)
      (check-sat-assuming ((= x b) (= y a)))
      (get-value (base (= x b) (= y a)))
      (check-sat-assuming ((= x a) (= y a)))(check-sat)
      (reset-assertions)(assert (distinct a b c))
      (assert (= x a))(assert (= y a))(check-sat)
      (get-value ((= x y)))|}
    ["sat"; "((base true))"; "sat"; "((base true) ((= x c) true))";
     "unsat"; "sat"; "sat";
     "((base true) ((= x b) true) ((= y a) true))";
     "unsat"; "sat"; "sat"; "(((= x y) true))"];
  expect
    {|(declare-sort U 0)(declare-const a U)(declare-const b U)
      (declare-fun f (U) U)
      (assert (distinct a b))
      (assert (or (= (f a) a) (= (f a) b)))
      (assert (or (= (f b) a) (= (f b) b)))
      (assert (= (f a) b))(assert (= (f b) a))
      (check-sat)(get-value ((= (f a) b) (= (f b) a)))|}
    ["sat"; "(((= (f a) b) true) ((= (f b) a) true))"];
  expect
    {|(declare-sort U 0)
      (declare-const a U)(declare-const b U)(declare-const c U)
      (declare-const x U)(declare-const y U)
      (declare-const z U)(declare-const w U)
      (define-fun domain ((v U)) Bool (or (= v a) (= v b) (= v c)))
      (assert (distinct a b c))
      (assert (and (domain x) (domain y) (domain z) (domain w)))
      (assert (distinct x y z w))(check-sat)|} ["unsat"]

let finite_evaluation () =
  let module F = Frontend in
  let module R = Vox_smt_resource in
  let exception Probe in
  let verify expected source =
    let actual = F.run (F.create ()) source in
    if actual <> expected then failwith
      (String.concat ";" actual ^ " <> " ^ String.concat ";" expected)
  in
  let table =
    {|(declare-fun f (Int Int) Int)
      (declare-const x Int)(declare-const y Int)
      (assert (= (f 0 0) 0))(assert (= (f 0 1) 1))
      (assert (= (f 1 0) 1))(assert (= (f 1 1) 0))
      (assert (or (= x 0) (= x 1)))
      (assert (or (= y 0) (= y 1)))|}
  in
  let impossible =
    "(assert (distinct (f x y) (mod (+ x y) 2)))"
  in
  verify ["unsat"] (table ^ impossible ^ "(check-sat)");
  verify ["sat"; "(((= (f x y) 1) true))"]
    (table ^ "(assert (= (f x y) 1))(check-sat)"
     ^ "(get-value ((= (f x y) 1)))");
  verify ["sat"; "unsat"; "sat"]
    (table ^ "(check-sat)(push 1)" ^ impossible
     ^ "(check-sat)(pop 1)(check-sat)");
  verify ["sat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x 0) (= x 1)))
      (assert (or (= (f 0) 0) (= (f 0) 1)))
      (assert (= (f 1) 1))(assert (= (f x) 0))(check-sat)|};
  verify ["sat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x 0) (= x 1)))
      (assert (= (f 0) 0))(assert (= (f x) 1))(check-sat)|};
  verify ["unsat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x 0) (= x 1)))
      (assert (or (= x 1) (= x 2)))
      (assert (= (f 0) 0))(assert (= (f 1) 1))
      (assert (distinct (f x) 1))(check-sat)|};
  verify ["unsat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x (- 2)) (= x (- 1)) (= x 0) (= x 1)))
      (assert (= (f (- 2)) (- 1)))(assert (= (f (- 1)) (- 1)))
      (assert (= (f 0) 0))(assert (= (f 1) 0))
      (assert (distinct (f x) (div x 2)))(check-sat)|};
  verify ["unsat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x 999999999999999999999999999999)
                  (= x 999999999999999999999999999998)))
      (assert (= (f 999999999999999999999999999999) 9))
      (assert (= (f 999999999999999999999999999998) 8))
      (assert (= (f x) 7))(check-sat)|};
  verify ["sat"]
    {|(declare-fun f (Int) Int)(declare-fun g (Int) Int)
      (declare-const x Int)(assert (or (= x 0) (= x 1)))
      (assert (= (f 0) 0))(assert (= (f 1) 1))
      (assert (= (g x) (f x)))(check-sat)|};
  verify ["sat"; "((p true))"; "unsat"; "((not p))"; "sat"]
    (table ^ "(declare-const p Bool)"
     ^ "(assert (or p (distinct (f x y) (mod (+ x y) 2))))"
     ^ "(check-sat)(get-value (p))"
     ^ "(check-sat-assuming ((not p)))(get-unsat-assumptions)"
     ^ "(check-sat)");
  verify ["unsat"; "(bad)"; "sat"]
    (table ^ "(push 1)"
     ^ "(assert (! (distinct (f x y) (mod (+ x y) 2)) :named bad))"
     ^ "(check-sat)(get-unsat-core)(pop 1)(check-sat)");
  verify ["unsat"]
    (table ^ "(declare-fun g (Int) Bool)"
     ^ "(assert (or (and (g x) (= (f x y) 2))"
     ^ "            (and (not (g x)) (= (f x y) 3))))(check-sat)");
  verify ["sat"]
    (table ^ "(declare-fun g (Int) Bool)"
     ^ "(assert (and (or (g x) (< (f x y) 2))"
     ^ "             (or (not (g x)) (< (f x y) 2))))(check-sat)");
  verify ["unsat"]
    {|(declare-fun f (Int) Int)(declare-const x Int)
      (assert (or (= x 0) (= x 1)))
      (assert (or (= x 2) (= x 3)))
      (assert (= (f 0) 0))(check-sat)|};
  let frontend = F.create () in
  ignore (F.run frontend (table ^ impossible));
  let measurement = R.create () in
  if F.run ~resource:measurement frontend "(check-sat)" <> ["unsat"]
  then failwith "measurement";
  let total = R.used measurement in
  for budget = 0 to min total 3000 do
    let frontend = F.create () in
    ignore (F.run frontend (table ^ impossible));
    let resource = R.create ~budget () in
    (match F.run ~resource frontend "(check-sat)" with
    | ["unknown"] | ["unsat"] -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "budget result");
    if F.run frontend "(check-sat)" <> ["unsat"]
    then failwith "budget recovery"
  done;
  for at = 1 to 30 do
    let frontend = F.create () in
    ignore (F.run frontend (table ^ impossible));
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls; if !calls = at then raise Probe; false) () in
    (match F.run ~resource frontend "(check-sat)" with
    | _ -> () | exception Probe -> ());
    if F.run frontend "(check-sat)" <> ["unsat"]
    then failwith "callback recovery"
  done;
  let membership start count =
    "(assert (or " ^ String.concat " " (List.init count (fun i ->
      Printf.sprintf "(= x %d)" (start + i))) ^ "))" in
  let overlapping =
    "(declare-const x Int)(declare-fun f (Int) Int)"
    ^ membership 0 120 ^ membership 60 120
    ^ "(assert (= (f 0) 0))(assert (= (f x) 1))" in
  let large_key n = "999999999999999999999999999999999999"
    ^ Printf.sprintf "%04d" n in
  let large_table =
    "(declare-const x Int)(declare-fun f (Int) Int)"
    ^ "(assert (or (= x " ^ large_key 0 ^ ") (= x " ^ large_key 199 ^ ")))"
    ^ String.concat "" (List.init 200 (fun n ->
        "(assert (= (f " ^ large_key n ^ ") 0))"))
    ^ "(assert (= (f x) 1))" in
  List.iter (fun (source, expected) ->
    verify [expected] (source ^ "(check-sat)");
    List.iter (fun budget ->
      let frontend = F.create () in
      ignore (F.run frontend source);
      let resource = R.create ~budget () in
      (match F.run ~resource frontend "(check-sat)" with
      | ["unknown"] -> ()
      | [actual] when actual = expected -> ()
      | exception R.Exhausted _ -> ()
      | _ -> failwith "large budget result");
      if F.run frontend "(check-sat)" <> [expected]
      then failwith "large budget recovery")
      [0; 1; 17; 101; 500; 1000; 2000; 4000; 8000; 16000])
    [overlapping, "sat"; large_table, "unsat"]

let finite_coloring () =
  let module F = Frontend in
  let module R = Vox_smt_resource in
  let exception Probe in
  let verify expected source =
    let actual = F.run (F.create ()) source in
    if actual <> expected then failwith
      (String.concat ";" actual ^ " <> " ^ String.concat ";" expected) in
  let graph =
    {|(declare-datatype C ((a)(b)(c)))
      (declare-const x C)(declare-const y C)(declare-const z C)
      (assert (or (= x a) (= x b)))
      (assert (or (= y a) (= y b)))
      (assert (distinct x y))(assert (distinct y z))|} in
  verify ["sat"; "(((distinct x y) true) ((distinct y z) true))"]
    (graph ^ "(check-sat)(get-value ((distinct x y) (distinct y z)))");
  verify ["sat"; "unsat"; "sat"]
    (graph ^ "(check-sat)(push 1)(assert (= x y))"
     ^ "(check-sat)(pop 1)(check-sat)");
  verify ["sat"; "unsat"; "(bad)"; "sat"]
    (graph ^ "(check-sat)(push 1)(assert (! (= x y) :named bad))"
     ^ "(check-sat)(get-unsat-core)(pop 1)(check-sat)");
  verify ["sat"; "unsat"; "sat"]
    (graph ^ "(check-sat)(check-sat-assuming ((= x y)))(check-sat)");
  verify ["sat"; "(((distinct (f u) (f v)) true))"]
    {|(declare-datatype A ((u)(v)))(declare-datatype C ((a)(b)))
      (declare-fun f (A) C)(assert (distinct (f u) (f v)))
      (check-sat)(get-value ((distinct (f u) (f v))))|};
  verify ["unsat"]
    {|(declare-datatype C ((a)(b)))
      (declare-const x C)(declare-const y C)(declare-const z C)
      (assert (distinct x y z))(check-sat)|};
  verify ["unsat"]
    {|(declare-datatype C ((a)(b)))(declare-fun f (C) C)
      (declare-const x C)(declare-const y C)
      (assert (= x y))(assert (distinct (f x) (f y)))(check-sat)|};
  List.iter (fun colors ->
    let palette = String.concat " " (List.init colors (fun i ->
      Printf.sprintf "(c%d)" i)) in
    let values = [0; colors / 2; colors - 1] in
    let domain name = "(assert (or " ^ String.concat " "
      (List.map (fun color ->
        Printf.sprintf "(= %s c%d)" name color) values) ^ "))" in
    let source = "(declare-datatype C (" ^ palette ^ "))"
      ^ "(declare-const x C)(declare-const y C)(declare-const z C)"
      ^ domain "x" ^ domain "y" ^ domain "z"
      ^ "(assert (distinct x y z))" in
    verify ["sat"; "(((distinct x y z) true))"]
      (source ^ "(check-sat)(get-value ((distinct x y z)))");
    List.iter (fun budget ->
      let frontend = F.create () in
      ignore (F.run frontend source);
      let resource = R.create ~budget () in
      (match F.run ~resource frontend "(check-sat)" with
      | ["sat"] | ["unknown"] -> ()
      | exception R.Exhausted _ -> ()
      | _ -> failwith "wide budget result");
      if F.run frontend "(check-sat)" <> ["sat"]
      then failwith "wide budget recovery")
      [1; 64; 512; 1024; 2048; 4096; 8192])
    [61; 62; 63; 64; 121; 122; 123; 257; 511; 512];
  let palette = String.concat " " (List.init 125 (fun i ->
    Printf.sprintf "(c%d)" i)) in
  let rollback = "(declare-datatype C (" ^ palette ^ "))"
    ^ "(declare-const x C)(declare-const y C)(declare-const z C)"
    ^ String.concat "" (List.map (fun name ->
        Printf.sprintf "(assert (or (= %s c0) (= %s c122)))" name name)
        ["x"; "y"; "z"])
    ^ "(assert (distinct x y z))" in
  verify ["unsat"] (rollback ^ "(check-sat)");
  List.iter (fun budget ->
    let frontend = F.create () in
    ignore (F.run frontend rollback);
    let resource = R.create ~budget () in
    (match F.run ~resource frontend "(check-sat)" with
    | ["unsat"] | ["unknown"] -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "multiword rollback budget result");
    if F.run frontend "(check-sat)" <> ["unsat"]
    then failwith "multiword rollback budget recovery")
    [1; 64; 128; 256; 512; 768; 1024; 1536; 2048; 4096; 8192];
  List.iter (fun at ->
    let frontend = F.create () in
    ignore (F.run frontend rollback);
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls; if !calls = at then raise Probe; false) () in
    (match F.run ~resource frontend "(check-sat)" with
    | _ -> () | exception Probe -> ());
    if F.run frontend "(check-sat)" <> ["unsat"]
    then failwith "multiword rollback callback recovery")
    [1; 2; 4; 8; 16; 32; 64; 128];
  let palette = String.concat " " (List.init 513 (fun i ->
    Printf.sprintf "(c%d)" i)) in
  verify ["sat"]
    ("(declare-datatype C (" ^ palette ^ "))"
     ^ "(declare-const x C)(declare-const y C)"
     ^ "(assert (= x c0))(assert (= y c512))"
     ^ "(assert (distinct x y))(check-sat)");
  let frontend = F.create () in
  ignore (F.run frontend graph);
  let resource = R.create () in
  if F.run ~resource frontend "(check-sat)" <> ["sat"]
  then failwith "measurement";
  let total = R.used resource in
  for budget = 0 to min total 2000 do
    let frontend = F.create () in
    ignore (F.run frontend graph);
    let resource = R.create ~budget () in
    (match F.run ~resource frontend "(check-sat)" with
    | ["sat"] | ["unknown"] -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "budget result");
    if F.run frontend "(check-sat)" <> ["sat"] then failwith "budget recovery"
  done;
  for at = 1 to 30 do
    let frontend = F.create () in
    ignore (F.run frontend graph);
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls; if !calls = at then raise Probe; false) () in
    (match F.run ~resource frontend "(check-sat)" with
    | _ -> () | exception Probe -> ());
    if F.run frontend "(check-sat)" <> ["sat"] then failwith "callback recovery"
  done;
  let declarations = List.init 64 (fun i ->
    Printf.sprintf "(declare-const x%d C)" i) in
  let edges = List.init 32 (fun i -> List.init 32 (fun j ->
    Printf.sprintf "(assert (distinct x%d x%d))" i (32 + j)))
    |> List.concat in
  let dense = "(declare-datatype C ((a)(b)))"
    ^ String.concat "" (declarations @ edges) in
  verify ["sat"] (dense ^ "(check-sat)");
  List.iter (fun budget ->
    let frontend = F.create () in
    ignore (F.run frontend dense);
    let resource = R.create ~budget () in
    (match F.run ~resource frontend "(check-sat)" with
    | ["sat"] | ["unknown"] -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "dense budget result");
    if F.run frontend "(check-sat)" <> ["sat"]
    then failwith "dense budget recovery")
    [1; 2048; 4096; 8192; 10000; 12000; 16000; 25000; 50000; 100000];
  List.iter (fun at ->
    let frontend = F.create () in
    ignore (F.run frontend dense);
    let calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls; if !calls = at then raise Probe; false) () in
    (match F.run ~resource frontend "(check-sat)" with
    | _ -> () | exception Probe -> ());
    if F.run frontend "(check-sat)" <> ["sat"]
    then failwith "dense callback recovery") [64; 256; 512; 1024; 2048]

let finite_product_bounds () =
  let declarations =
    "(declare-const x Int)(declare-const y Int)"
    ^ "(declare-fun observe (Int) Int)(assert (= (observe x) 0))" in
  List.iter (fun (lower, upper) ->
    let bounds = "(assert " ^ lower ^ ")(assert " ^ upper ^ ")"
      ^ "(assert (>= y (- 3)))(assert (<= y 3))" in
    expect
      (declarations ^ "(push 1)" ^ bounds
       ^ "(assert (= (+ (* x x) (* x y)) 7))(check-sat)(pop 1)"
       ^ bounds ^ "(assert (> x 0))"
       ^ "(assert (= (+ (* x x) (* x y)) 10))(check-sat)"
       ^ "(get-value ((+ (* x x) (* x y))))")
      ["unsat"; "sat"; "(((+ (* x x) (* x y)) 10))"];
    expect
      (declarations ^ bounds ^ "(assert (< x 0))"
       ^ "(assert (= (+ (* x x) (* x y)) 10))(check-sat)")
      ["sat"])
    ["(>= (* (- 2) x) (- 5))", "(<= (* (- 2) x) 5)";
     "(> (* (- 2) x) (- 6))", "(< (* (- 2) x) 6)";
     "(<= (- 5) (* 2 x))", "(>= 5 (* 2 x))";
     "(< (- 6) (- (* (- 2) x)))", "(> 6 (- (* (- 2) x)))"];
  expect
    (declarations ^ "(assert (>= (* (- 2) x) (- 4)))"
     ^ "(assert (<= (* (- 2) x) 4))"
     ^ "(assert (= (* x x) 8))(check-sat)")
    ["unsat"];
  expect
    (declarations
     ^ "(declare-const p Bool)"
     ^ "(assert (or p (and (>= x (- 2)) (<= x 2))))"
     ^ "(assert (>= x 0))(assert (= (* x x) 9))(check-sat)"
     ^ "(check-sat-assuming ((<= x 2)))(get-unsat-assumptions)"
     ^ "(check-sat)(get-value (x))")
    ["sat"; "unsat"; "((<= x 2))"; "sat"; "((x 3))"];
  expect
    (declarations
     ^ "(push 1)(assert (>= x 0))(assert (= (* x x) 9))"
     ^ "(assert (! (<= x 2) :named bound))(check-sat)"
     ^ "(get-unsat-core)(pop 1)(assert (= x 3))"
     ^ "(assert (= (* x x) 9))(check-sat)")
    ["unsat"; "(bound)"; "sat"]

let finite_product_replay () =
  let module R = Vox_smt_resource in
  let setup () =
    let frontend = Frontend.create () in
    ignore (Frontend.run frontend
      {|(declare-const x Int)(declare-const y Int)
        (declare-fun observe (Int) Int)(assert (= (observe x) 0))
        (push 1)(assert (= (+ (* x x) (* x y)) 7))
        (assert (>= (* (- 2) x) (- 5)))
        (assert (<= (* (- 2) x) 5))
        (assert (>= y (- 3)))(assert (<= y 3))|});
    frontend in
  let recover frontend =
    check (Frontend.run frontend "(check-sat)" = ["unsat"])
      "Finite product check did not recover";
    check (Frontend.run frontend
      {|(pop 1)(assert (= (+ (* x x) (* x y)) 7))
        (assert (= x 1))(assert (= y 6))(check-sat)
        (get-value ((+ (* x x) (* x y))))|}
      = ["sat"; "(((+ (* x x) (* x y)) 7))"])
      "Finite product bounds escaped their scope" in
  List.iter (fun budget ->
    let frontend = setup () in
    let resource = R.create ~budget () in
    (match Frontend.run ~resource frontend "(check-sat)" with
    | ["unsat"] | ["unknown"] -> ()
    | exception R.Exhausted _ -> ()
    | _ -> failwith "Unexpected finite product budget result");
    check (R.used resource <= budget) "Finite product budget exceeded";
    recover frontend) [0; 1; 16; 128; 512; 1024; 2048; 4096];
  List.iter (fun stop ->
    let frontend = setup () in
    let polls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr polls;
      if !polls = stop then raise Exit;
      false) () in
    (match Frontend.run ~resource frontend "(check-sat)" with
    | ["unsat"] -> ()
    | exception Exit -> ()
    | _ -> failwith "Unexpected finite product interruption result");
    recover frontend) [1; 2; 4; 8; 16; 32]

module Compact_distinct_frontend = struct
  module F = Vox_smt_frontend

  let check condition message = if not condition then failwith message
  let names = List.init 18 (fun i -> "x" ^ string_of_int i)
  let declarations = "(set-logic QF_UF)(declare-sort U 0)"
    ^ String.concat "" (List.map (fun name ->
        "(declare-const " ^ name ^ " U)") names)
  let distinct names = "(distinct " ^ String.concat " " names ^ ")"
  let group = distinct names
  let expect body expected =
    let actual = F.run (F.create ()) (declarations ^ body) in
    check (actual = expected)
      (Printf.sprintf "[%s] expected [%s]\n%s"
        (String.concat ";" actual) (String.concat ";" expected) body)
  let () =
    expect ("(assert " ^ group ^ ")(check-sat)"
      ^ "(push 1)(assert (= x0 x1))(check-sat)(pop 1)(check-sat)")
      ["sat"; "unsat"; "sat"];
    expect ("(push 1)(assert " ^ group ^ ")(check-sat)(pop 1)"
      ^ "(assert (= x0 x1))(check-sat)") ["sat"; "sat"];
    expect ("(assert (= x0 x1))(push 1)(assert " ^ group
      ^ ")(check-sat)(pop 1)(check-sat)") ["unsat"; "sat"];
    expect ("(assert " ^ distinct ("x0" :: names) ^ ")(check-sat)")
      ["unsat"];
    expect ("(assert " ^ group ^ ")(assert (not " ^ group
      ^ "))(check-sat)") ["unsat"];
    expect ("(assert (not " ^ group ^ "))(check-sat)") ["sat"];
    List.iter (fun expression ->
      expect ("(assert (= x0 x1))(assert " ^ expression ^ ")(check-sat)")
        ["unsat"])
      ["(or false " ^ group ^ ")";
       "(= true " ^ group ^ ")";
       "(ite true " ^ group ^ " false)";
       "(let ((d " ^ group ^ ")) d)";
       "(and true " ^ group ^ ")"];
    expect ("(declare-const b Bool)(assert (= x0 x1))"
      ^ "(assert (or b " ^ group ^ "))(check-sat-assuming ((not b)))"
      ^ "(check-sat)") ["unsat"; "sat"];
    expect ("(assert (= x0 x1))(check-sat-assuming (" ^ group
      ^ "))(check-sat)(check-sat-assuming ((and true " ^ group ^ ")))")
      ["unsat"; "sat"; "unsat"];
    expect ("(define-fun d () Bool " ^ group ^ ")(assert d)(check-sat)"
      ^ "(push 1)(assert (not d))(check-sat)(pop 1)(check-sat)")
      ["sat"; "unsat"; "sat"];
    expect ("(set-option :produce-unsat-cores true)(assert (! " ^ group
      ^ " :named d))(assert (! (= x0 x1) :named e))(check-sat)"
      ^ "(get-unsat-core)") ["unsat"; "(d e)"];
    expect ("(set-option :produce-models true)(assert " ^ group
      ^ ")(check-sat)(get-value (" ^ group ^ "))")
      ["sat"; "((" ^ group ^ " true))"];
    expect ("(assert " ^ group ^ ")(declare-const i Int)(assert (= i 0))"
      ^ "(assert (= x0 x1))(check-sat)") ["unsat"];
    expect ("(declare-fun f (U) U)(assert " ^ group ^ ")"
      ^ "(assert (= x0 (f x2)))(assert (= x1 (f x3)))"
      ^ "(assert (= x2 x3))(check-sat)") ["unsat"];
    expect ("(assert " ^ distinct (List.tl names) ^ ")"
      ^ "(assert " ^ distinct (List.rev names) ^ ")"
      ^ "(assert (= x0 x17))(check-sat)") ["unsat"];
    print_endline "Distinct frontend contexts, models, cores and scopes passed"
end

module Pure_uf = struct
  module F = Frontend

  let expect frontend source expected =
    let actual = F.run frontend source in
    check (actual = expected)
      (Printf.sprintf "%s: expected [%s], received [%s]" source
         (String.concat "; " expected) (String.concat "; " actual))

  let setup source =
    let frontend = F.create () in
    ignore (F.run frontend source);
    frontend

  let boolean_arguments () =
    List.iter (fun result ->
      List.iter (fun truth ->
        let frontend = setup
            ("(declare-sort U 0)(declare-const a Bool)"
             ^ "(declare-const b Bool)(declare-fun f (Bool) "
             ^ result ^ ")") in
        let application = if result = "Bool" then "(assert (f a))"
          else "(declare-const c U)(assert (distinct (f a) c))" in
        let assignments = if truth then "(assert a)(assert b)"
          else "(assert (not a))(assert (not b))" in
        expect frontend (assignments ^ application ^ "(check-sat)") ["sat"];
        expect frontend
          "(get-value ((= (f a) (f b))))"
          ["(((= (f a) (f b)) true))"];
        expect frontend
          "(push)(assert (distinct (f a) (f b)))(check-sat)"
          ["unsat"];
        expect frontend "(pop)(check-sat)" ["sat"])
        [false; true]) ["U"; "Bool"];
    List.iter (fun truth ->
      let frontend = setup
        ("(declare-sort U 0)(declare-const a Bool)(declare-const b Bool)"
         ^ "(declare-fun f (Bool Bool) U)(assert (= a b))") in
      let condition = if truth then "a" else "(not a)" in
      expect frontend
        ("(assert " ^ condition ^ ")(assert (distinct "
         ^ "(f (and a b) (or a b)) (f a b)))(check-sat)")
        ["unsat"]) [false; true]

  let transition =
    {|(declare-sort U 0)(declare-const u U)(declare-const v U)
      (declare-const p Bool)(declare-const q Bool)
      (declare-fun f (Bool) U)(assert p)(assert q)
      (assert (distinct u v))(check-sat)
      (push)(declare-datatype D ((empty) (box (contents U))))
      (declare-const d D)(assert (= d (box u)))|}

  let incremental_datatypes () =
    let frontend = setup transition in
    expect frontend "(check-sat)" ["sat"];
    expect frontend
      "(get-value ((= (contents d) u) (= (f p) (f q))))"
      ["(((= (contents d) u) true) ((= (f p) (f q)) true))"];
    expect frontend
      "(push)(assert (= (contents d) v))(check-sat)(pop)(check-sat)"
      ["unsat"; "sat"];
    expect frontend "(pop)(check-sat)" ["sat"];
    expect frontend
      {|(declare-datatype E ((node (flag Bool)) (leaf)))
        (declare-const e E)(assert (= e (node p)))(check-sat)
        (push)(assert (not (flag e)))(check-sat)(pop)(check-sat)|}
      ["sat"; "unsat"; "sat"]

  let run () =
    boolean_arguments ();
    incremental_datatypes ()
end

module Protocol_options = struct
  let options () =
    expect
      {|(set-option :interactive-mode true)
        (get-option :interactive-mode)(get-option :produce-assertions)
        (set-option :produce-assertions false)
        (get-option :interactive-mode)(get-option :produce-assertions)
        (set-option :produce-assertions true)(push 1)(pop 1)
        (reset-assertions)(get-option :interactive-mode)
        (reset)(get-option :interactive-mode)
        (get-option :produce-assertions)|}
      ["true"; "true"; "false"; "false"; "true"; "false"; "false"];
    expect
      {|(set-option :global-decls true)(get-option :global-declarations)
        (set-option :global-declarations false)(get-option :global-decls)
        (set-option :reproducible-resource-limit 1)(get-option :rlimit)
        (set-option :rlimit 0)(get-option :reproducible-resource-limit)
        (reset)(get-option :global-decls)
        (get-option :reproducible-resource-limit)|}
      ["true"; "false"; "1"; "0"; "false"; "0"];
    expect
      {|(set-option :zzz true)(get-option :zzz)
        (set-option :zzz "value")(get-option :zzz)
        (set-option :produce-proofs true)(get-option :produce-proofs)
        (set-option :print-success true)
        (set-option :unknown-option false)(get-option :unknown-option)
        (check-sat)|}
      ["unsupported"; "unsupported"; "unsupported"; "unsupported";
       "unsupported"; "false"; "success"; "unsupported"; "unsupported";
       "sat"];
    List.iter (fun name ->
      let frontend = Frontend.create () in
      ignore (Frontend.run frontend ("(set-option " ^ name ^ " true)"));
      List.iter (fun value ->
        (match Frontend.run frontend
            ("(set-option " ^ name ^ " " ^ value ^ ")") with
        | exception Frontend.Error _ -> ()
        | _ -> failwith "Accepted an invalid Boolean option");
        check (Frontend.run frontend ("(get-option " ^ name ^ ")")
               = ["true"]) "Invalid option changed its value")
        ["invalid"; "1"; "\"true\""; "(true)"])
      [":produce-models"; ":produce-assignments"; ":produce-unsat-cores";
       ":produce-unsat-assumptions"; ":produce-assertions";
       ":interactive-mode"; ":incremental"]

  let strict_retrievals () =
    let frontend = Frontend.create ~require_enabled_retrievals:true () in
    let run = Frontend.run frontend in
    let rejects source =
      match run source with
      | exception Frontend.Error _ -> ()
      | _ -> failwith ("Accepted disabled retrieval: " ^ source)
    in
    ignore (run "(declare-const x Int)(assert (! (= x 7) :named n))");
    check (run "(check-sat)" = ["sat"]) "Strict check";
    List.iter rejects
      ["(get-model)"; "(get-value (x))"; "(get-assignment)";
       "(get-assertions)"];
    ignore (run "(set-option :produce-models true)\
      (set-option :produce-assignments true)\
      (set-option :interactive-mode true)");
    check (run "(get-value (x))(get-assignment)(get-assertions)"
           = ["((x 7))"; "((n true))"; "((! (= x 7) :named n))"])
      "Enabled retrieval lost model or assertions";
    check (List.length (run "(get-model)") = 1) "Enabled model retrieval";
    ignore (run "(set-option :produce-models false)");
    rejects "(get-model)";
    rejects "(get-value (x))";
    ignore (run "(set-option :produce-models true)");
    check (run "(get-value (x))" = ["((x 7))"])
      "Rejected retrieval invalidated model";
    check (run "(check-sat-assuming (true))(get-assertions)"
           = ["sat"; "((! (= x 7) :named n))"])
      "Assumptions leaked into assertions";
    check (run "(check-sat-assuming ((distinct x 7)))" = ["unsat"])
      "Strict unsat check";
    rejects "(get-unsat-core)";
    rejects "(get-unsat-assumptions)";
    ignore (run "(set-option :produce-unsat-cores true)\
      (set-option :produce-unsat-assumptions true)");
    check (run "(get-unsat-core)(get-unsat-assumptions)"
           = ["(n)"; "((distinct x 7))"])
      "Enabled unsat retrieval";
    ignore (run "(reset-assertions)");
    check (run "(get-assertions)" = ["()"])
      "Reset assertions disabled retrieval";
    ignore (run "(reset)(check-sat)");
    List.iter rejects
      ["(get-model)"; "(get-value (true))"; "(get-assignment)";
       "(get-assertions)"];
    ignore (run "(set-option :produce-models true)");
    check (run "(get-value (true))" = ["((true true))"])
      "Reset lost strict frontend recovery";
    expect "(declare-const x Int)(assert (= x 7))(check-sat)(get-value (x))"
      ["sat"; "((x 7))"]

  let run () = options (); strict_retrievals ()
end

module Boolean_cardinality = struct
module T = Vox_smt_term
module E = Vox_smt_engine
module R = Vox_smt_resource

let ensure condition message = if not condition then failwith message

let fixture size groups edges =
  let terms = T.create () in
  let atoms = Array.init size (fun i ->
    let symbol = T.symbol terms
        { T.name = "p" ^ string_of_int i; arguments = [||];
          result = T.Bool; kind = T.Uninterpreted } in
    T.apply terms symbol [||]) in
  let solver = E.create terms in
  Array.iter (E.register solver) atoms;
  let positives = List.map (fun group ->
    T.or_ terms (Array.of_list (List.map (Array.get atoms) group))) groups in
  let exclusions = List.map (fun (left, right) ->
    T.or_ terms [|T.not_ terms atoms.(left); T.not_ terms atoms.(right)|])
      edges in
  terms, solver, positives, exclusions

let pigeons count holes =
  let groups = List.init count (fun p ->
    List.init holes (fun h -> p * holes + h)) in
  let edges = ref [] in
  for h = 0 to holes - 1 do
    for p = 0 to count - 1 do
      for q = p + 1 to count - 1 do
        edges := (p * holes + h, q * holes + h) :: !edges
      done
    done
  done;
  count * holes, groups, !edges

let setup ?(scoped = false) (size, groups, edges) =
  let terms, solver, positives, exclusions = fixture size groups edges in
  if scoped then E.push solver;
  List.iter (E.assert_ solver) (positives @ exclusions);
  terms, solver

let brute size groups edges =
  let rec search mask =
    if mask = 1 lsl size then false else
    let value term = mask land (1 lsl term) <> 0 in
    if List.for_all (List.exists value) groups
       && List.for_all (fun (left, right) ->
         not (value left && value right)) edges then true
    else search (mask + 1) in
  search 0

let soundness () =
  let random = Random.State.make [|122; 637; 39|] in
  for index = 0 to 1999 do
    let size = 4 + Random.State.int random 6 in
    let groups = List.init (3 + Random.State.int random 7) (fun _ ->
      List.init (2 + Random.State.int random 4)
        (fun _ -> Random.State.int random size)) in
    let edges = ref [] in
    for left = 0 to size - 1 do
      for right = left + 1 to size - 1 do
        if Random.State.bool random then edges := (left, right) :: !edges
      done
    done;
    let _, solver = setup (size, groups, !edges) in
    let result = E.check solver in
    ensure (result = if brute size groups !edges then E.Sat else E.Unsat [])
      ("Cardinality brute-force disagreement " ^ string_of_int index)
  done;
  let size, groups, edges = pigeons 4 3 in
  List.iteri (fun omitted _ ->
    let edges = List.filteri (fun i _ -> i <> omitted) edges in
    let _, solver = setup (size, groups, edges) in
    ensure (E.check solver = E.Sat) "Missing exclusion lost SAT") edges;
  let _, solver = setup (size, List.tl groups, edges) in
  ensure (E.check solver = E.Sat) "Missing group lost SAT";
  let selected = List.tl groups in
  let _, solver = setup (size, List.hd selected :: selected, edges) in
  ensure (E.check solver = E.Sat) "Duplicate clause inflated lower bound";
  Printf.printf "2000 random cases and deletion/duplicate cases passed\n%!"

let scopes () =
  let size, groups, edges = pigeons 11 10 in
  let _, solver, positives, exclusions = fixture size groups edges in
  List.iter (E.assert_ solver) positives;
  ensure (E.check solver = E.Sat) "Positive rows should be SAT";
  E.push solver;
  List.iter (E.assert_ solver) exclusions;
  let before = E.statistics solver in
  ensure (E.check solver = E.Unsat []) "Scoped cardinality contradiction";
  let after = E.statistics solver in
  ensure (after.decisions = before.decisions)
    "Cardinality contradiction entered search";
  E.pop solver;
  ensure (E.check solver = E.Sat) "Cardinality conflict survived pop";
  E.push solver;
  List.iter (E.assert_ solver) exclusions;
  ensure (E.check solver = E.Unsat []) "Cardinality replay";
  E.pop solver;
  ensure (E.check solver = E.Sat) "Second cardinality conflict survived pop";
  Printf.printf "Scoped 11-by-10 contradiction passed without decisions\n%!"

let resources () =
  let initial = pigeons 4 3 in
  let _, solver = setup ~scoped:true initial in
  let resource = R.create () in
  ensure (E.check ~resource solver = E.Unsat []) "Resource fixture";
  let used = R.used resource in
  for budget = 0 to used - 1 do
    let _, solver = setup ~scoped:true initial in
    let resource = R.create ~budget () in
    (match E.check ~resource solver with
    | E.Unknown _ -> ()
    | _ -> failwith "Cardinality resource limit was ignored");
    ensure (R.used resource <= budget) "Cardinality budget overrun";
    ensure (E.check solver = E.Unsat []) "Interrupted cardinality recovery";
    E.pop solver;
    ensure (E.check solver = E.Sat) "Interrupted cardinality scope recovery"
  done;
  let _, solver = setup ~scoped:true initial in
  let calls = ref 0 in
  let resource = R.create ~cancelled:(fun () -> incr calls; false) () in
  ensure (E.check ~resource solver = E.Unsat []) "Count callbacks";
  for stop = 1 to !calls do
    List.iter (fun raises ->
      let _, solver = setup ~scoped:true initial in
      let count = ref 0 in
      let resource = R.create ~cancelled:(fun () ->
        incr count;
        if !count = stop then if raises then raise Exit else true else false)
          () in
      (match E.check ~resource solver with
      | E.Unknown "cancelled" when not raises -> ()
      | exception Exit when raises -> ()
      | _ -> failwith "Cardinality callback did not interrupt");
      ensure (E.check solver = E.Unsat []) "Callback recovery";
      E.pop solver;
      ensure (E.check solver = E.Sat) "Callback scope recovery")
      [false; true]
  done;
  Printf.printf "Resource recovery passed (%d work, %d callback positions)\n%!"
    used !calls

let run () = soundness (); scopes (); resources ()
end

let () =
  integer_divisibility ();
  finite_product_bounds ();
  finite_product_replay ();
  finite_coloring ();
  finite_evaluation ();
  finite_domain_symmetry ();
  bitvector_completion_models ();
  bitvector_congruence_model_repair ();
  boolean_polynomial_models ();
  polynomial_residues ();
  small_polynomial_model_resources ();
  partition_scopes ();
  partition_resources ();
  symbolic_modular_scopes ();
  symbolic_modular_resources ();
  cubic_polynomial_models ();
  cubic_polynomial_resources ();
  bounded_polynomial_models ();
  equality_lattice_scopes ();
  small_polynomial_models ();
  unbounded_integer_extensions ();
  disjunctive_equality_cuts ();
  expect {|(echo "sat")(echo "")(echo "a""b")(echo "\u{65}")|}
    ["\"sat\""; "\"\""; "\"a\"\"b\""; "\"\\u{65}\""];
  model_handle_lifetimes ();
  frontend_check_exceptions ();
  temporary_terms ();
  boolean_alternatives ();
  difference_disjunctions ();
  term_array_ownership ();
  connective_terms ();
  deep_boolean_connectives ();
  macro_definitions ();
  streamed_expressions ();
  streamed_recovery ();
  elaboration_contexts false;
  elaboration_contexts true;
  modular_polynomial_scopes ();
  expect
    {|(declare-sort U 0)(declare-const p Bool)
      (declare-const a U)(declare-const b U)(declare-fun f (U) U)
      (assert p)(assert (distinct (f (ite p a b)) (f a)))
      (check-sat)|} ["unsat"];
  expect
    {|(declare-const p Bool)(assert p)
      (assert (< (ite p 0 1) 0))(check-sat)|} ["unsat"];
  let executable =
    match Sys.getenv_opt "VOX_TEST_Z3" with
    | Some "true" -> Some "z3"
    | None | Some ("false" | "") -> None
    | Some executable -> Some executable
  in
  expect
    {|(declare-const x Int)(assert (= x 3))(check-sat)
      (get-value ((+ x 1) (> (* x x) 7) (- x)))|}
    ["sat"; "(((+ x 1) 4) ((> (* x x) 7) true) ((- x) (- 3)))"];
  requested_integer_equalities ();
  demanded_integer_equalities ();
  comparison_relations ();
  variable_affine_comparisons ();
  wide_predicate_requests ();
  fixed_combinations ();
  buffered_assertion_scopes ();
  scopes_and_assumptions ();
  engine_preflight ();
  reordered_search ();
  reordered_search ~scoped:true ();
  cancellation_replay ();
  resource_budget_replay ();
  scoped_cancellation_replay ();
  substitutions ();
  quoted_symbols ();
  invalid_inputs ();
  timeout ();
  recursive_model_samples ();
  fixed_models ();
  guarded_bitvector_quotients executable;
  bitvector_quotient_limit ();
  conversion_roundtrips executable;
  model_queries executable;
  declared_overloads executable;
  overloaded_datatypes executable;
  parametric_datatypes executable;
  uninterpreted_sort_applications executable;
  named_cores executable;
  indexed_integer_extensions ();
  arithmetic_extensions executable;
  zero_divisor_congruence executable;
  conditional_division executable;
  quotient_bounds executable;
  bitvector_extensions executable;
  named_expressions ();
  datatype_updates executable;
  datatype_matches executable;
  global_declarations ();
  solver_information ();
  information_attributes ();
  terminal_command ();
  output_channels ();
  Option.iter differential executable;
  Pure_uf.run ();
  Boolean_cardinality.run ();
  Protocol_options.run ();
  print_endline "SMT engine tests passed"

module Polynomial_sign_safety = struct
module F = Vox_smt_frontend
module S = Vox_smt_sexp
module R = Vox_smt_resource

let check condition message = if not condition then failwith message
let query = S.List [S.Atom "check-sat"]
let integer n = if n < 0 then Printf.sprintf "(- %d)" (-n)
  else string_of_int n

let fixture coefficient negative reverse operator =
  let magnitude = abs coefficient and sign = compare coefficient 0 in
  let declarations = ["u"; "a"; "b"; "c"; "r"] |> List.map (fun name ->
    "(declare-const " ^ name ^ " Int)") in
  let group = Printf.sprintf "(+ (* %d c) (* %s a))"
      magnitude (integer (7 * sign)) in
  let assertions = [Printf.sprintf "(= (+ (* %s u) (* a b)) 0)"
      (integer coefficient);
    "(>= r 1)";
    (if negative then "(<= b 0)" else "(>= b 0)");
    "(" ^ operator ^ " (- (* c b) (* 7 u) r) 0)"] in
  let bound = "(" ^ (if negative then ">=" else "<=") ^ " "
      ^ group ^ " 0)" in
  let source = "(set-option :produce-unsat-cores true)" ^
    String.concat "" (if reverse then List.rev declarations else declarations)
    ^ String.concat "" (List.mapi (fun index assertion ->
      Printf.sprintf "(assert (! %s :named p%d))" assertion index)
      (if reverse then List.rev assertions else assertions)) ^
    "(push 1)(assert (! " ^ bound ^ " :named bound))" in
  let setup () = let frontend = F.create () in
    ignore (F.run frontend source); frontend in
  let recover frontend =
    check (F.run frontend "(check-sat)" = ["unsat"]) "Sign retry";
    (match F.run frontend "(get-unsat-core)" with
    | [core] ->
      (match S.parse core with
      | [S.List literals] -> check (List.mem (S.Atom "bound") literals)
          "Sign core omitted contradictory bound"
      | _ -> failwith "Sign core")
    | _ -> failwith "Sign core response");
    ignore (F.run frontend "(pop 1)");
    let b = if negative then -magnitude else magnitude in
    let c = if negative then -10 else 10 in
    let u = -(sign * (if negative then -1 else 1)) in
    let r = if operator = "=" then c * b - 7 * u else 1 in
    let concrete = Printf.sprintf
        "(assert (= a 1))(assert (= b %s))(assert (= c %s))\
         (assert (= u %s))(assert (= r %s))(check-sat)"
        (integer b) (integer c) (integer u) (integer r) in
    check (F.run frontend concrete = ["sat"]) "Sign scope recovery";
    let assertions = assertions @ ["(= a 1)";
      "(= b " ^ integer b ^ ")"; "(= c " ^ integer c ^ ")";
      "(= u " ^ integer u ^ ")"; "(= r " ^ integer r ^ ")"] in
    (match F.run frontend
        ("(get-value (" ^ String.concat " " assertions ^ "))") with
    | [response] ->
      (match S.parse response with
      | [S.List values] -> check (List.for_all (function
          | S.List [_; S.Atom "true"] -> true | _ -> false) values)
          "Sign SAT model"
      | _ -> failwith "Sign values")
    | _ -> failwith "Sign model response") in
  setup, recover

let exercise coefficient negative reverse operator =
  let setup, recover = fixture coefficient negative reverse operator in
  let frontend = setup () and callbacks = ref 0 in
  let resource = R.create ~budget:200000
      ~cancelled:(fun () -> incr callbacks; false) () in
  check (F.execute ~resource frontend query = Some "unsat") "Sign answer";
  let used = R.used resource and callbacks = !callbacks in
  recover frontend;
  let budgets = List.sort_uniq Int.compare
    (0 :: (used - 1) :: List.init 17 (fun i -> used * i / 17)) in
  List.iter (fun budget ->
    let frontend = setup () and resource = R.create ~budget () in
    check (F.execute ~resource frontend query = Some "unknown")
      "Sign budget interruption";
    check (R.used resource <= budget) "Sign shared budget overrun";
    recover frontend) budgets;
  List.iter (fun raises -> for stop = 1 to callbacks do
    let frontend = setup () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls;
      if !calls = stop then
        if raises then raise Exit else true
      else false) () in
    (match F.execute ~resource frontend query with
    | Some "unknown" when not raises -> ()
    | exception Exit when raises -> ()
    | _ -> failwith "Sign cancellation interruption");
    recover frontend
  done) [false; true];
  Printf.printf "sign %d negative=%b reverse=%b op=%s: %d work, \
    %d budgets, %d callbacks\n%!" coefficient negative reverse operator used
    (List.length budgets) (2 * callbacks)

let () =
  List.iter (fun coefficient -> List.iter (fun negative ->
    List.iter (fun reverse -> exercise coefficient negative reverse ">")
      [false; true]) [false; true]) [-3; 3];
  exercise (-3) false true ">=";
  exercise 3 true false "=";
  let frontend = F.create () in
  check (F.run frontend
    {|(declare-const u Int)(declare-const a Int)(declare-const b Int)
      (declare-const c Int)
      (assert (= (+ (* (- 3) u) (* a b)) 0))
      (assert (= b 0))(assert (= a 1))(assert (= c 0))
      (assert (>= (- (* c b) (* 7 u)) 0))(check-sat)
      (get-value ((<= (+ (* 3 c) (* (- 7) a)) 0)))|}
    = ["sat"; "(((<= (+ (* 3 c) (* (- 7) a)) 0) true))"])
    "Zero factor boundary";
  print_endline "Polynomial sign safety passed"
end

module Sparse_relation_safety = struct
module E = Vox_smt_engine
module T = Vox_smt_term
module R = Vox_smt_resource
module Z = Vox_smt_integer.Z

let check condition message = if not condition then failwith message

let fixture negative reverse =
  let terms = T.create () in
  let solver = E.create terms in
  let integer n = T.integer terms (Z.of_int n) in
  let declare name = T.apply terms (T.symbol terms
      {name; arguments = [||]; result = T.Int; kind = T.Uninterpreted})
      [||] in
  let names = ["a"; "b"; "c"; "d"; "x"; "y"] in
  let variables = List.map (fun name -> name, declare name)
      (if reverse then List.rev names else names) in
  let variable name = List.assoc name variables in
  let op name arguments = T.builtin terms name T.Int arguments in
  let compare name left right = T.builtin terms name T.Bool [|left; right|] in
  let a = variable "a" and b = variable "b" and c = variable "c"
  and d = variable "d" and x = variable "x" and y = variable "y" in
  let sign = if negative then -1 else 1 in
  let domain term sign = T.or_ terms (Array.of_list (List.map (fun n ->
      T.equal terms term (integer (sign * n))) [1; 2; 4; 8; 16])) in
  let assertions = [domain a sign; domain b sign; domain c 1; domain d sign;
    T.equal terms x (op "+" [|op "*" [|integer 7; a|]; b|]);
    T.equal terms y (op "-" [|op "*" [|x; c|]; d|]);
    compare ">" x (integer (if negative then -200 else 0));
    compare "<" x (integer (if negative then 0 else 200));
    T.equal terms y (integer (sign * 944))] in
  List.iter (fun (_, term) -> E.register solver term) variables;
  List.iter (E.assert_ solver)
    (if reverse then List.rev assertions else assertions);
  let value term = match E.value solver term with
    | Some (E.Integer value) ->
      (match Z.to_int_opt value with Some value -> value
      | None -> failwith "Non-native model value")
    | _ -> failwith "Missing integer model" in
  let validate () =
    let av = value a and bv = value b and cv = value c and dv = value d
    and xv = value x and yv = value y in
    let belongs value sign = List.mem value
      (List.map (fun n -> sign * n) [1; 2; 4; 8; 16]) in
    check (belongs av sign && belongs bv sign && belongs cv 1
      && belongs dv sign) "Sparse domain model";
    check (xv = 7 * av + bv && yv = xv * cv - dv && yv = sign * 944)
      "Sparse relation model";
    List.iter (fun assertion ->
      check (E.value solver assertion = Some (E.Boolean true))
        "False assertion in model") assertions in
  let recover () =
    check (E.check solver = E.Sat) "Sparse relation retry";
    validate ();
    let generation = match E.model_generation solver with
      | Some generation -> generation | None -> failwith "Missing generation" in
    E.push solver;
    check (not (E.model_is_current solver generation)) "Push kept generation";
    E.assert_ solver (T.equal terms y (integer 0));
    (match E.check solver with E.Unsat _ -> ()
    | _ -> failwith "Conflicting target accepted");
    check (E.model_generation solver = None) "Unsat retained generation";
    E.pop solver;
    check (E.check solver = E.Sat) "Sparse relation pop";
    validate ();
    check (not (E.model_is_current solver generation))
      "Pop revived generation" in
  solver, validate, recover

let interruption negative reverse =
  let solver, validate, recover = fixture negative reverse in
  let calls = ref 0 in
  let resource = R.create ~budget:2000000
      ~cancelled:(fun () -> incr calls; false) () in
  check (E.check ~resource solver = E.Sat) "Budgeted relation answer";
  validate ();
  let used = R.used resource and callbacks = !calls in
  recover ();
  let budgets = List.sort_uniq Int.compare
      (0 :: (used - 1) :: List.init 33 (fun i -> used * i / 33)) in
  List.iter (fun budget ->
    let solver, _, recover = fixture negative reverse in
    let resource = R.create ~budget () in
    (match E.check ~resource solver with E.Unknown _ -> ()
    | _ -> failwith "Relation budget did not interrupt");
    check (R.used resource <= budget) "Relation shared budget overrun";
    check (E.model_generation solver = None) "Interrupted generation";
    recover ()) budgets;
  List.iter (fun raises -> for stop = 1 to callbacks do
    let solver, _, recover = fixture negative reverse in
    let seen = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr seen;
      if !seen = stop then if raises then raise Exit else true else false) () in
    (match E.check ~resource solver with
    | E.Unknown _ when not raises -> ()
    | exception Exit when raises -> ()
    | _ -> failwith "Relation cancellation did not interrupt");
    check (E.model_generation solver = None) "Cancelled generation";
    recover ()
  done) [false; true];
  let solver, validate, _ = fixture negative reverse in
  check (E.check solver = E.Sat) "Warm relation check";
  let generation = Option.get (E.model_generation solver) in
  (match E.check ~budget:0 solver with E.Unknown _ -> ()
  | _ -> failwith "Warm zero budget");
  check (not (E.model_is_current solver generation)) "Check kept generation";
  check (E.check solver = E.Sat) "Warm retry";
  validate ();
  Printf.printf "relations negative=%b reverse=%b: %d work, %d budgets, \
    %d callbacks\n%!" negative reverse used (List.length budgets)
    (2 * callbacks)

let standalone source expected =
  let frontend = Vox_smt_frontend.create () in
  let replies = Vox_smt_frontend.run frontend source in
  check (replies = expected) "Standalone relation safety"

let () =
  List.iter (fun negative -> List.iter (interruption negative) [false; true])
    [false; true];
  standalone {|(declare-const a Int)(declare-const b Int)
    (assert (or (= a 1) (= a 2)))
    (assert (or (= a 4) (= a 8)))
    (assert (or (= b 1) (= b 2)))
    (assert (= (* a b) 8))(check-sat)|} ["unsat"];
  standalone {|(declare-const a Int)(declare-const b Int)
    (assert (or (= a 1) (= a 2)))
    (assert (or (= b 1) (= b 2)))
    (assert (= a 1))(assert (= a 2))
    (assert (= (* a b) 8))(check-sat)|} ["unsat"];
  standalone {|(declare-const a Int)(declare-const b Int)
    (declare-const x Int)
    (assert (or (= a 1) (= a 2)))
    (assert (or (= b 1) (= b 2)))
    (assert (= x 1000000000000000000000000000000000000000))
    (assert (= a 1))(assert (= b 2))
    (assert (= (* x a) x))(check-sat)
    (push 1)(assert (= (* x b) x))(check-sat)
    (pop 1)(check-sat)|} ["sat"; "unsat"; "sat"];
  print_endline "Sparse relation safety passed"
end

module Affine_factor_safety = struct
module F = Vox_smt_frontend
module S = Vox_smt_sexp
module R = Vox_smt_resource

let check condition message = if not condition then failwith message
let query = S.List [S.Atom "check-sat"]

let fixture coefficient multiply reverse affine =
  let variables = ["bounded"; "alias"; "free"; "fixed"] @
      List.init 7 (fun i -> "v" ^ string_of_int i) in
  let opposite = if multiply then "(* (- 1) alias)" else "(- alias)" in
  let assertions = ["(>= bounded (- 1))"; "(<= bounded 1)";
      "(= bounded " ^ opposite ^ ")"; "(= fixed 0)"] @
    List.concat_map (fun variable ->
      ["(>= " ^ variable ^ " 1)";
       "(= (+ (* alias " ^ variable ^ ") " ^ variable ^ ") 0)";
       if affine then Printf.sprintf "(< (* (+ free %d fixed) %s) 0)"
         coefficient variable
       else Printf.sprintf "(>= (* free %s) (* %d %s))"
         variable coefficient variable])
      (List.init 7 (fun i -> "v" ^ string_of_int i)) in
  let declarations = List.map (fun variable ->
      "(declare-const " ^ variable ^ " Int)") variables @
      ["(declare-const flag Bool)"] in
  let assertions = assertions @ ["(or flag (not flag))"] in
  let source = String.concat ""
      (if reverse then List.rev declarations else declarations) ^
      String.concat "" (List.map (fun assertion ->
        "(assert " ^ assertion ^ ")")
        (if reverse then List.rev assertions else assertions)) in
  let setup () = let frontend = F.create () in
    ignore (F.run frontend source); frontend in
  let validate frontend =
    match F.run frontend ("(get-value (" ^ String.concat " " assertions
        ^ "))") with
    | [response] ->
      (match S.parse response with
      | [S.List values] ->
        check (List.length values = List.length assertions
          && List.for_all (function
            | S.List [_; S.Atom "true"] -> true | _ -> false) values)
          "Factor model assertions"
      | _ -> failwith "Factor values")
    | _ -> failwith "Factor response" in
  let recover frontend =
    check (F.run frontend "(check-sat)" = ["sat"]) "Factor retry";
    validate frontend;
    let contradiction = if affine then "(>= free 0)" else "(< free 0)" in
    check (F.run frontend ("(push 1)(assert " ^ contradiction
        ^ ")(check-sat)(pop 1)(check-sat)") = ["unsat"; "sat"])
      "Factor scope recovery";
    validate frontend in
  setup, validate, recover

let exercise coefficient multiply reverse affine =
  let setup, validate, recover = fixture coefficient multiply reverse affine in
  let frontend = setup () and callbacks = ref 0 in
  let resource = R.create ~budget:500000
      ~cancelled:(fun () -> incr callbacks; false) () in
  check (F.execute ~resource frontend query = Some "sat") "Factor answer";
  validate frontend;
  let used = R.used resource and callbacks = !callbacks in
  recover frontend;
  let budgets = List.sort_uniq Int.compare
      (0 :: (used - 1) :: List.init 17 (fun i -> used * i / 17)) in
  List.iter (fun budget ->
    let frontend = setup () and resource = R.create ~budget () in
    check (F.execute ~resource frontend query = Some "unknown")
      "Factor budget interruption";
    check (R.used resource <= budget) "Factor shared budget";
    recover frontend) budgets;
  List.iter (fun raises -> for stop = 1 to callbacks do
    let frontend = setup () and calls = ref 0 in
    let resource = R.create ~cancelled:(fun () ->
      incr calls;
      if !calls = stop then
        if raises then raise Exit else true
      else false) () in
    (match F.execute ~resource frontend query with
    | Some "unknown" when not raises -> ()
    | exception Exit when raises -> ()
    | _ -> failwith "Factor cancellation interruption");
    recover frontend
  done) [false; true];
  Printf.printf "factor %d multiply=%b reverse=%b affine=%b: %d work, \
    %d budgets, %d callbacks\n%!" coefficient multiply reverse affine used
    (List.length budgets) (2 * callbacks)

let () =
  List.iter (fun affine -> List.iter (fun multiply ->
      List.iter (fun reverse -> exercise 17 multiply reverse affine)
        [false; true]) [false; true]) [false; true];
  exercise 50000 true true true;
  print_endline "Affine factor safety passed"
end
