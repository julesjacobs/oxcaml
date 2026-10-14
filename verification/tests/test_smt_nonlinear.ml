module T = Vox_smt_term
module N = Vox_smt_nonlinear
module Z = Vox_smt_integer.Z

let require condition message = if not condition then failwith message

let variable terms name =
  T.apply terms (T.symbol terms
      { name; arguments = [||]; result = T.Int; kind = T.Uninterpreted }) [||]

let rec evaluate terms assignment term =
  match T.node terms term with
  | T.Integer number -> number
  | T.Apply (symbol, arguments) ->
    let signature = T.signature terms symbol in
    (match signature.kind with
    | T.Uninterpreted -> assignment signature.name
    | T.Builtin "ite" ->
      evaluate terms assignment
        arguments.(if truth terms assignment arguments.(0) then 1 else 2)
    | T.Builtin operator ->
      let values = Array.map (evaluate terms assignment) arguments in
      (match operator with
      | "+" -> Array.fold_left Z.add Z.zero values
      | "*" -> Array.fold_left Z.mul Z.one values
      | "-" when Array.length values = 1 -> Z.neg values.(0)
      | "-" -> Array.fold_left Z.sub values.(0)
          (Array.sub values 1 (Array.length values - 1))
      | "div" -> if Z.is_zero values.(1) then Z.zero
          else fst (Z.div_rem values.(0) values.(1))
      | "mod" -> if Z.is_zero values.(1) then Z.zero
          else snd (Z.div_rem values.(0) values.(1))
      | _ -> failwith "unexpected integer builtin")
    | _ -> failwith "unexpected integer term")
  | _ -> failwith "expected integer"

and truth terms assignment term =
  match T.node terms term with
  | T.Boolean value -> value
  | T.Apply (symbol, arguments) ->
    let signature = T.signature terms symbol in
    (match signature.kind with
    | T.Builtin "not" -> not (truth terms assignment arguments.(0))
    | T.Builtin "and" -> Array.for_all (truth terms assignment) arguments
    | T.Builtin "or" -> Array.exists (truth terms assignment) arguments
    | T.Builtin "=" when T.sort terms arguments.(0) = T.Bool ->
      truth terms assignment arguments.(0)
      = truth terms assignment arguments.(1)
    | T.Builtin operator ->
      let left = evaluate terms assignment arguments.(0) in
      let right = evaluate terms assignment arguments.(1) in
      let comparison = Z.compare left right in
      (match operator with
      | "=" -> comparison = 0 | "<" -> comparison < 0
      | "<=" -> comparison <= 0 | ">" -> comparison > 0
      | ">=" -> comparison >= 0
      | _ -> failwith "unexpected Boolean builtin")
    | _ -> failwith "unexpected Boolean term")
  | _ -> failwith "expected Boolean"

let normalize_tests () =
  let terms = T.create () in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun _ -> failwith "normalization emitted a clause") () in
  let variables = Array.init 3 (fun i -> variable terms (string_of_int i)) in
  let random = Random.State.make [|0x8371; 0x128|] in
  let rec expression depth =
    if depth = 0 then
      if Random.State.bool random then variables.(Random.State.int random 3)
      else T.integer terms (Z.of_int (Random.State.int random 9 - 4))
    else
      let operator = [|"+"; "-"; "*"|].(Random.State.int random 3) in
      T.builtin terms operator T.Int
        [|expression (depth - 1); expression (depth - 1)|]
  in
  for _ = 1 to 400 do
    let original = expression 4 in
    match N.normalize solver original with
    | None -> ()
    | Some normalized ->
      require (N.normalize solver normalized = Some normalized)
        "polynomial normalization is not idempotent";
      for a = -2 to 2 do
        for b = -2 to 2 do
          for c = -2 to 2 do
            let assignment = function
              | "0" -> Z.of_int a | "1" -> Z.of_int b
              | "2" -> Z.of_int c | _ -> failwith "unknown variable" in
            require
              (Z.equal (evaluate terms assignment original)
                 (evaluate terms assignment normalized))
              "polynomial normalization changed its value"
          done
        done
      done
  done

let lemma_tests print =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" and a = variable terms "a" in
  let b = variable terms "b" in
  let q = variable terms "q" and r = variable terms "r" in
  let q2 = variable terms "q2" and r2 = variable terms "r2" in
  let mul left right = T.builtin terms "*" T.Int [|left; right|] in
  let xyz = T.builtin terms "*" T.Int [|x; y; z|] in
  let abz = T.builtin terms "*" T.Int [|a; b; z|] in
  let products =
    [mul x y; mul x z; mul x x; mul y z; mul a z; mul x b; xyz; abz] in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) products;
  N.register_division solver ~dividend:a ~divisor:x ~quotient:q ~remainder:r;
  N.register_division solver ~dividend:a ~divisor:y
    ~quotient:q2 ~remainder:r2;
  let assignment = function
    | "x" -> Z.of_int 2 | "y" -> Z.of_int (-3) | "z" -> Z.one
    | "a" -> Z.of_int 7 | "q" -> Z.of_int 3 | "r" -> Z.one
    | "b" -> Z.of_int 2
    | "q2" -> Z.of_int (-2) | "r2" -> Z.one
    | _ -> failwith "unknown variable" in
  let model term =
    if List.mem term products then Some (Z.of_int 17)
    else Some (evaluate terms assignment term) in
  let run representative explain =
    for _ = 0 to 2 do
      ignore (N.check solver ~integer:model ~representative ~explain)
    done in
  run Fun.id (fun _ _ -> None);
  let explanation = [|T.equal terms y z + 1|] in
  run (fun term -> if term = y then z else term)
    (fun left right ->
      if left = y && right = z then Some explanation else None);
  let xy = mul x y in
  let representative term = if term = a then xy else term in
  for _ = 0 to 2 do
    ignore (N.check solver ~integer:model ~representative
      ~explain:(fun left right ->
        if left = a && right = xy
        then Some [|T.equal terms a xy + 1|] else None))
  done;
  let yz = mul y z in
  run (fun term -> if term = a then xy else if term = b then yz else term)
    (fun left right ->
      if left = a && right = xy || left = xy && right = a
      then Some [|T.equal terms a xy + 1|]
      else if left = b && right = yz || left = yz && right = b
      then Some [|T.equal terms b yz + 1|] else None);
  let association =
    [-(T.equal terms a xy + 1); -(T.equal terms b yz + 1);
     T.equal terms (mul a z) (mul x b) + 1]
    |> List.sort Int.compare |> Array.of_list in
  require (List.mem association !clauses) "missing product associativity";
  let sum = T.builtin terms "+" T.Int [|mul x y; mul a b|] in
  let one = T.integer terms Z.one in
  run (fun term -> if term = sum then one else term)
    (fun left right ->
      if left = sum && right = one || left = one && right = sum
      then Some [|T.equal terms sum one + 1|] else None);
  let expanded = T.builtin terms "+" T.Int [|xyz; abz|] in
  let multiplied = [-(T.equal terms sum one + 1);
                    T.equal terms expanded z + 1]
      |> List.sort Int.compare |> Array.of_list in
  require (List.mem multiplied !clauses) "missing multiplied equality";
  let two = T.integer terms (Z.of_int 2) in
  let sum = T.builtin terms "+" T.Int [|mul x y; mul two (mul a b)|] in
  run (fun term -> if term = sum then one else term)
    (fun left right ->
      if left = sum && right = one || left = one && right = sum
      then Some [|T.equal terms sum one + 1|] else None);
  let scaled = T.builtin terms "*" T.Int [|two; a; b; z|] in
  let expanded = T.builtin terms "+" T.Int [|xyz; scaled|] in
  let multiplied = [-(T.equal terms sum one + 1);
                    T.equal terms expanded z + 1]
      |> List.sort Int.compare |> Array.of_list in
  require (List.mem multiplied !clauses) "missing scaled multiplied equality";
  require (N.check solver ~integer:model ~representative:Fun.id
             ~explain:(fun _ _ -> None) <> N.Done)
    "accepted an invalid nonlinear model";
  let rec valid_model remaining =
    require (remaining > 0) "nonlinear model did not settle";
    match N.check solver
        ~integer:(fun term -> Some (evaluate terms assignment term))
        ~representative:Fun.id ~explain:(fun _ _ -> None) with
    | N.Done -> ()
    | N.Continue -> valid_model (remaining - 1)
    | N.Incomplete _ -> failwith "rejected a valid nonlinear model" in
  valid_model 10;
  require (!clauses <> []) "no nonlinear lemmas";
  for xv = -3 to 3 do
    for yv = -3 to 3 do
      for zv = -3 to 3 do
        for av = -5 to 5 do
          for bv = -2 to 2 do
          let quotient, remainder =
            if xv = 0 then Z.of_int yv, Z.of_int zv
            else Z.div_rem (Z.of_int av) (Z.of_int xv) in
          let quotient2, remainder2 =
            if yv = 0 then Z.of_int xv, Z.of_int zv
            else Z.div_rem (Z.of_int av) (Z.of_int yv) in
          let assignment = function
            | "x" -> Z.of_int xv | "y" -> Z.of_int yv
            | "z" -> Z.of_int zv | "a" -> Z.of_int av
            | "b" -> Z.of_int bv
            | "q" -> quotient | "r" -> remainder
            | "q2" -> quotient2 | "r2" -> remainder2
            | _ -> failwith "unknown variable" in
          List.iter (fun clause ->
              require (Array.exists (fun literal ->
                  truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "invalid nonlinear lemma") !clauses
          done
        done
      done
    done
  done;
  if print then begin
    print_endline "(set-logic ALL)";
    List.iter (fun name -> Printf.printf "(declare-const %s Int)\n" name)
      ["x"; "y"; "z"; "a"; "b"];
    print_endline "(define-fun q () Int (div a x))";
    print_endline "(define-fun r () Int (mod a x))";
    print_endline "(define-fun q2 () Int (div a y))";
    print_endline "(define-fun r2 () Int (mod a y))";
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

exception Interrupted

let interruption_tests () =
  let run stop =
    let terms = T.create () in
    let x = variable terms "x" and y = variable terms "y" in
    let z = variable terms "z" and b = variable terms "b" in
    let a = variable terms "a" and q = variable terms "q" in
    let r = variable terms "r" in
    let q2 = variable terms "q2" and r2 = variable terms "r2" in
    let product = T.builtin terms "*" T.Int [|x; y|] in
    let yz = T.builtin terms "*" T.Int [|y; z|] in
    let calls = ref 0 and armed = ref true and clauses = ref [] in
    let event () =
      incr calls;
      if !armed && !calls = stop then begin
        armed := false;
        raise Interrupted
      end in
    let solver = N.create ~terms ~poll:event
        ~boolean:(fun term -> event (); term + 1)
        ~clause:(fun clause ->
          event ();
          let literals = Array.to_list (Array.map (fun literal ->
              let term = T.to_string terms (abs literal - 1) in
              if literal > 0 then term else "(not " ^ term ^ ")") clause) in
          clauses := List.sort String.compare literals :: !clauses) () in
    N.register_product solver product;
    N.register_product solver yz;
    N.register_product solver (T.builtin terms "*" T.Int [|a; z|]);
    N.register_product solver (T.builtin terms "*" T.Int [|x; b|]);
    N.register_division solver ~dividend:a ~divisor:x ~quotient:q ~remainder:r;
    N.register_division solver ~dividend:a ~divisor:y
      ~quotient:q2 ~remainder:r2;
    let assignment = function
      | "x" -> Z.of_int 2 | "y" -> Z.of_int 3 | "a" -> Z.of_int 7
      | "q" -> Z.of_int 3 | "r" -> Z.one
      | "q2" -> Z.of_int 2 | "r2" -> Z.one
      | "z" -> Z.of_int 2 | "b" -> Z.of_int 6
      | _ -> failwith "unknown interruption variable" in
    let representative term =
      if term = a then product else if term = b then yz else term in
    let explain left right =
      if left = a && right = product || left = product && right = a
      then Some [|T.equal terms a product + 1|]
      else if left = b && right = yz || left = yz && right = b
      then Some [|T.equal terms b yz + 1|] else None in
    let integer term =
      if term = product then Some (Z.of_int 11)
      else Some (evaluate terms assignment term) in
    let rec settle remaining =
      require (remaining > 0) "nonlinear interruption did not converge";
      ignore (N.propagate_products solver ~representative ~explain ~integer);
      match N.check solver ~representative ~explain ~integer with
      | N.Continue -> settle (remaining - 1)
      | N.Incomplete _ -> ()
      | N.Done -> failwith "interrupted nonlinear model accepted" in
    (try settle 10 with Interrupted -> settle 10);
    List.sort_uniq compare !clauses, !calls
  in
  let expected, calls = run max_int in
  for stop = 1 to calls do
    let actual, _ = run stop in
    require (actual = expected) "interruption lost a nonlinear lemma"
  done

let modular_tests print =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let n = variable terms "n" in
  let builtin name args = T.builtin terms name T.Int args in
  let mul x y = builtin "*" [|x; y|] in
  let rem x = builtin "mod" [|x; n|] in
  let products = [mul x y; mul (rem x) (rem y); mul (rem x) y] in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) products;
  let definitions = ref [] in
  let remainders = List.mapi (fun index dividend ->
      let qname = "q" ^ string_of_int index in
      let rname = "r" ^ string_of_int index in
      let quotient = variable terms qname in
      let remainder = variable terms rname in
      definitions :=
        (qname, builtin "div" [|dividend; n|])
        :: (rname, rem dividend) :: !definitions;
      N.register_division solver ~dividend ~divisor:n ~quotient ~remainder;
      remainder) (x :: y :: products) in
  let rec assignment xv yv nv = function
    | "x" -> Z.of_int xv | "y" -> Z.of_int yv | "n" -> Z.of_int nv
    | name -> evaluate terms (assignment xv yv nv)
        (List.assoc name !definitions) in
  let rec settle fuel =
    require (fuel > 0) "modular arithmetic did not converge";
    match N.check solver ~representative:Fun.id ~explain:(fun _ _ -> None)
      ~integer:(fun term -> Some (evaluate terms (assignment 2 3 5) term)) with
    | N.Continue -> settle (fuel - 1)
    | N.Done -> ()
    | N.Incomplete _ -> failwith "valid modular model rejected" in
  settle 10;
  let equal = T.equal terms (List.nth remainders 2)
      (List.nth remainders 3) in
  let nonzero = T.not_ terms (T.equal terms n (T.integer terms Z.zero)) in
  let expected = [equal + 1; -(nonzero + 1)]
      |> List.sort Int.compare |> Array.of_list in
  require (List.mem expected !clauses) "missing modular product congruence";
  for xv = -5 to 5 do
    for yv = -5 to 5 do
      for nv = -3 to 3 do
        List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms (assignment xv yv nv) (abs literal - 1)
                = (literal > 0)) clause) "invalid modular product lemma")
          !clauses
      done
    done
  done;
  if print then begin
    print_endline "(set-logic ALL)";
    List.iter (fun name -> Printf.printf "(declare-const %s Int)\n" name)
      ["x"; "y"; "n"];
    List.iter (fun (name, expression) ->
        Printf.printf "(define-fun %s () Int %s)\n" name
          (T.to_string terms expression)) !definitions;
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

let modular_polynomial_tests print =
  List.iter (fun divisor ->
    List.iter (fun conditional ->
      let terms = T.create () in
      let x = variable terms "x" and a = variable terms "a" in
      let int n = T.integer terms (Z.of_int n) in
      let op name args = T.builtin terms name T.Int args in
      let cmp name left right = T.builtin terms name T.Bool [|left; right|] in
      let shifted = op "+" [|x; int 10|] in
      let remainder = op "mod" [|shifted; int divisor|] in
      let expression =
        if conditional then T.ite terms (cmp "<=" x (int 0))
            remainder (op "mod" [|x; int (-5)|])
        else shifted in
      let target = T.equal_atom terms a x in
      let guard = T.equal_atom terms a expression in
      let reason = -(T.not_ terms guard + 1) in
      let clauses = ref [] in
      let solver = N.create ~terms ~boolean:(fun term -> term + 1)
          ~clause:(fun clause -> clauses := clause :: !clauses) () in
      let register () =
        for term = T.count terms - 1 downto 0 do
          N.register_modular_term solver term;
          N.register_modular_term solver term
        done in
      let representative term = if term = a then expression else term in
      let explain left right =
        if left = a && right = expression || left = expression && right = a
        then Some [|reason|] else None in
      let run av explain =
        ignore (N.propagate_products solver ~representative ~explain
          ~integer:(fun term -> Some (Z.of_int (if term = a then av else 0))))
      in
      register ();
      run 1 (fun _ _ -> None);
      require (!clauses = []) "modular alias without an explanation";
      run 5 explain;
      require (!clauses = []) "modular interval includes an extra integer";
      run 1 explain;
      let expected = [target + 1; -reason;
        -(cmp ">=" a (int 0) + 1); -(cmp "<=" a (int 4) + 1);
        -(cmp ">=" x (int 0) + 1); -(cmp "<=" x (int 4) + 1)]
        |> List.sort_uniq Int.compare |> Array.of_list in
      require (List.mem expected !clauses) "missing guarded modular lemma";
      let count = List.length !clauses in
      register ();
      run 1 explain;
      require (List.length !clauses = count) "duplicate modular lemma";
      for xv = -12 to 12 do
        for av = -1 to 10 do
          let assignment = function
            | "x" -> Z.of_int xv | "a" -> Z.of_int av
            | _ -> failwith "unknown modular variable" in
          List.iter (fun clause ->
              require (Array.exists (fun literal ->
                  truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "invalid modular polynomial lemma") !clauses
        done
      done;
      if print then begin
        print_endline "(reset)(set-logic ALL)";
        List.iter (Printf.printf "(declare-const %s Int)\n") ["x"; "a"];
        List.iter (fun clause ->
            let literals = Array.to_list (Array.map (fun literal ->
                let term = T.to_string terms (abs literal - 1) in
                if literal > 0 then term else "(not " ^ term ^ ")") clause) in
            Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
              (String.concat " " literals)) !clauses
      end) [false; true]) [5; -5]

let requested_modular_tests () =
  let terms = T.create () in
  let x = variable terms "x" and a = variable terms "a" in
  let int n = T.integer terms (Z.of_int n) in
  let op name args = T.builtin terms name T.Int args in
  let cmp name left right = T.builtin terms name T.Bool [|left; right|] in
  let expression = op "mod" [|op "+" [|x; int 10|]; int 5|] in
  let guard = T.equal_atom terms a expression in
  let reason = guard + 1 in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  for term = T.count terms - 1 downto 0 do
    N.register_modular_term solver term
  done;
  let representative term = if term = a then expression else term in
  let explain left right =
    if left = a && right = expression || left = expression && right = a
    then Some [|reason|] else None in
  let request explain = N.request_modular_equalities solver
      ~representative ~explain [a, x] in
  let before = T.count terms in
  require (not (request (fun _ _ -> None))) "unexplained modular request";
  require (T.count terms = before) "unproved request allocated terms";
  require (request explain) "missing requested modular equality";
  let target = T.equal terms a x in
  let expected = [target + 1; -reason;
    -(cmp ">=" a (int 0) + 1); -(cmp "<=" a (int 4) + 1);
    -(cmp ">=" x (int 0) + 1); -(cmp "<=" x (int 4) + 1)]
    |> List.sort_uniq Int.compare |> Array.of_list in
  require (List.mem expected !clauses) "requested equality guards";
  require (not (request explain)) "duplicate requested modular equality";
  for xv = -12 to 12 do
    for av = -1 to 10 do
      let assignment = function
        | "x" -> Z.of_int xv | "a" -> Z.of_int av
        | _ -> failwith "unknown modular request variable" in
      List.iter (fun clause ->
        require (Array.exists (fun literal ->
          truth terms assignment (abs literal - 1) = (literal > 0)) clause)
          "invalid requested modular equality") !clauses
    done
  done

let requested_affine_tests () =
  List.iter (fun offset ->
    let terms = T.create () in
    let x = variable terms "x" and a = variable terms "a" in
    let int n = T.integer terms (Z.of_int n) in
    let op name args = T.builtin terms name T.Int args in
    let cmp name left right = T.builtin terms name T.Bool [|left; right|] in
    let expression = op "mod" [|op "+" [|x; int (10 + offset)|]; int 5|] in
    let guard = T.equal_atom terms a expression in
    let reason = guard + 1 in
    let clauses = ref [] in
    let solver = N.create ~terms ~boolean:(fun term -> term + 1)
        ~clause:(fun clause -> clauses := clause :: !clauses) () in
    for term = T.count terms - 1 downto 0 do
      N.register_modular_term solver term
    done;
    let representative term = if term = a then expression else term in
    let explain left right =
      if left = a && right = expression || left = expression && right = a
      then Some [|reason|] else None in
    let request affine explain = N.request_modular_equalities ~affine solver
        ~representative ~explain [a, x] in
    let before = T.count terms in
    require (not (request true (fun _ _ -> None))) "unexplained affine request";
    require (T.count terms = before) "unproved affine request allocated terms";
    require (not (request false explain)) "affine request not opted in";
    require (request true explain) "missing requested affine equality";
    let centered = if offset > 2 then offset - 5 else offset in
    let shifted = op "+" [|x; int centered|] in
    let target = T.equal terms a shifted in
    let expected = [target + 1; -reason;
      -(cmp ">=" a (int 0) + 1); -(cmp "<=" a (int 4) + 1);
      -(cmp ">=" shifted (int 0) + 1); -(cmp "<=" shifted (int 4) + 1)]
      |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem expected !clauses) "shifted affine interval guards";
    require (not (request true explain)) "duplicate affine request";
    for xv = -12 to 12 do
      for av = -1 to 10 do
        let assignment = function
          | "x" -> Z.of_int xv | "a" -> Z.of_int av
          | _ -> failwith "unknown affine variable" in
        List.iter (fun clause ->
          require (Array.exists (fun literal ->
            truth terms assignment (abs literal - 1) = (literal > 0)) clause)
            "invalid requested affine equality") !clauses
      done
    done) [-2; -1; 1; 2; 3]

let affine_run ~factor ~aliased stop =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" in
  let int n = T.integer terms (Z.of_int n) in
  let op name a b = T.builtin terms name T.Int [|a; b|] in
  let wrap value = op "-" (op "mod" (op "+" value (int 8))
      (int 16)) (int 8) in
  let right = wrap x in
  let original = op "+" (op "*" (int factor) y) (int 3) in
  let left = wrap (op "+" (if aliased then z else x) original) in
  let explanation = T.equal_atom terms z right + 1 in
  let events = ref 0 and armed = ref true and clauses = ref [] in
  let poll () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll
      ~boolean:(fun term -> poll (); term + 1)
      ~clause:(fun clause -> poll (); clauses := clause :: !clauses) () in
  let seen = Hashtbl.create 32 in
  let rec register term =
    if not (Hashtbl.mem seen term) then begin
      Hashtbl.add seen term ();
      N.register_modular_term solver term;
      match T.node terms term with
      | T.Apply (_, arguments) -> Array.iter register arguments
      | _ -> ()
    end in
  register left; register right;
  List.iter (fun term -> register
      (T.builtin terms ">=" T.Bool [|term; int (-8)|])) [left; right];
  let representative term = if aliased && term = right then z else term in
  let explain a b =
    if aliased && ((a = z && b = right) || (a = right && b = z))
    then Some [|explanation|] else None in
  let run () = ignore (N.request_modular_equalities ~affine:true solver
      ~representative ~explain [left, right]) in
  (try run () with Exit -> run ());
  require (!clauses <> []) "missing affine modular lemma";
  let serialize clause = Array.to_list clause |> List.map (fun literal ->
      let text = T.to_string terms (abs literal - 1) in
      if literal > 0 then text else "(not " ^ text ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare
    (List.map serialize !clauses), !events

let affine_tests () =
  let seen = Hashtbl.create 32 and interruptions = ref 0 in
  let validate terms clauses = List.iter (fun clause ->
    let serial = Array.to_list clause |> List.map (fun literal ->
      let text = T.to_string terms (abs literal - 1) in
      if literal > 0 then text else "(not " ^ text ^ ")")
      |> List.sort String.compare in
    if not (Hashtbl.mem seen serial) then begin
      Hashtbl.add seen serial ();
      for x = -10 to 10 do
        for y = -10 to 10 do
          for z = -10 to 10 do
            let assignment name = Z.of_int (match name with
              | "x" -> x | "y" -> y | "z" -> z
              | _ -> failwith "unknown affine variable") in
            require (Array.exists (fun literal ->
              truth terms assignment (abs literal - 1) = (literal > 0))
              clause) "invalid affine modular lemma"
          done
        done
      done
    end) clauses in
  List.iter (fun (factor, aliased) ->
    let terms, clauses, expected, events =
      affine_run ~factor ~aliased max_int in
    validate terms clauses;
    for stop = 1 to events do
      incr interruptions;
      let terms, clauses, actual, _ = affine_run ~factor ~aliased stop in
      require (List.for_all (fun clause -> List.mem clause actual) expected)
        "interruption lost affine lemma";
      validate terms clauses
    done) [3,false; -3,false; 15,false; 3,true; -3,true];
  Printf.printf "Affine modular lemmas: %d lemmas, %d interruptions\n%!"
    (Hashtbl.length seen) !interruptions


let affine_rejected_cases () =
  List.iter (fun indicator ->
    let terms = T.create () in
    let x = variable terms "x" and y = variable terms "y" in
    let int n = T.integer terms (Z.of_int n) in
    let op name a b = T.builtin terms name T.Int [|a; b|] in
    let delta = if indicator then
      T.ite terms (T.builtin terms "<" T.Bool [|y; int 0|])
        (int 3) (int 5)
      else op "*" y y in
    let left = op "mod" (op "+" x delta) (int 16) in
    let right = op "mod" x (int 16) in
    let clauses = ref [] in
    let solver = N.create ~terms ~boolean:(fun term -> term + 1)
        ~clause:(fun clause -> clauses := clause :: !clauses) () in
    let rec register term =
      N.register_modular_term solver term;
      match T.node terms term with
      | T.Apply (_, arguments) -> Array.iter register arguments
      | _ -> () in
    register left; register right;
    ignore (N.request_modular_equalities ~affine:true solver
      ~representative:Fun.id ~explain:(fun _ _ -> None) [left,right]);
    require (!clauses = []) "nonlinear or indicator affine delta accepted")
    [false; true]


let modular_choice_tests print =
  List.iter (fun divisor ->
    List.iter (fun choice ->
      let terms = T.create () in
      let x = variable terms "x" and a = variable terms "a" in
      let b = variable terms "b" and c = variable terms "c" in
      let d = variable terms "d" in
      let int n = T.integer terms (Z.of_int n) in
      let op name args = T.builtin terms name T.Int args in
      let cmp name left right = T.builtin terms name T.Bool [|left; right|] in
      let guard = cmp ">=" c (int 0) in
      let first = T.ite terms guard (op "+" [|x; int 2|]) x in
      let second = match choice with
        | 0 -> T.ite terms guard (int (-2)) (int 0)
        | 1 -> T.ite terms (T.not_ terms guard) (int 0) (int (-2))
        | 2 -> T.ite terms (cmp ">=" d (int 0)) (int (-2)) (int 0)
        | _ -> T.ite terms guard (op "-" [|x|]) (int 0) in
      let rem value = op "mod" [|value; int divisor|] in
      let left = rem (op "+" [|first; second|]) and right = rem x in
      let target = T.equal_atom terms a b in
      let aliases = [a, left; b, right] in
      let representative term =
        Option.value (List.assoc_opt term aliases) ~default:term in
      let reasons = List.map (fun (left, right) ->
        -(T.not_ terms (T.equal_atom terms left right) + 1)) aliases in
      let explain left right =
        List.find_map (fun ((x, y), reason) ->
          if left = x && right = y || left = y && right = x
          then Some [|reason|] else None) (List.combine aliases reasons) in
      let clauses = ref [] in
      let solver = N.create ~terms ~boolean:(fun term -> term + 1)
          ~clause:(fun clause -> clauses := clause :: !clauses) () in
      for term = T.count terms - 1 downto 0 do
        N.register_modular_term solver term
      done;
      let run value explain =
        ignore (N.propagate_products solver ~representative ~explain
          ~integer:(fun term ->
            Some (Z.of_int (if term = a then value else 0))))
      in
      run 1 (fun _ _ -> None);
      require (!clauses = []) "unexplained shared-choice alias";
      run 5 explain;
      require (!clauses = []) "shared-choice interval includes its endpoint";
      run 1 explain;
      let expected = target + 1 :: List.map Int.neg reasons @
        [-(cmp ">=" a (int 0) + 1); -(cmp "<=" a (int 4) + 1);
         -(cmp ">=" b (int 0) + 1); -(cmp "<=" b (int 4) + 1)]
        |> List.sort_uniq Int.compare |> Array.of_list in
      require (List.mem expected !clauses = (choice < 2))
        "shared-choice guard distinction";
      let count = List.length !clauses in
      run 1 explain;
      run 1 (fun _ _ -> None);
      require (List.length !clauses = count) "shared-choice cache lifetime";
      for xv = -6 to 6 do
        for av = -1 to 6 do
          for bv = -1 to 6 do
            List.iter (fun (cv, dv) ->
              let assignment = function
                | "x" -> Z.of_int xv | "a" -> Z.of_int av
                | "b" -> Z.of_int bv | "c" -> Z.of_int cv
                | "d" -> Z.of_int dv
                | _ -> failwith "unknown shared-choice variable" in
              List.iter (fun clause ->
                require (Array.exists (fun literal ->
                  truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "invalid shared-choice lemma") !clauses)
              [-1, -1; -1, 1; 1, -1; 1, 1]
          done
        done
      done;
      if print then begin
        print_endline "(reset)(set-logic ALL)";
        List.iter (Printf.printf "(declare-const %s Int)\n")
          ["x"; "a"; "b"; "c"; "d"];
        List.iter (fun clause ->
          let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
          Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
            (String.concat " " literals)) !clauses
      end) [0; 1; 2; 3]) [5; -5]

let integer_bound_tests () =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" in
  let int n = T.integer terms (Z.of_int n) in
  let op name sort x y = T.builtin terms name sort [|x; y|] in
  let cmp name x y = op name T.Bool x y in
  let xy = op "*" T.Int x y and xz = op "*" T.Int x z in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) [xy; xz];
  let check xv yv zv product =
    let assignment = function
      | "x" -> Z.of_int xv | "y" -> Z.of_int yv
      | "z" -> Z.of_int zv | _ -> failwith "unknown variable" in
    let model term = Some
        (if term = xy || term = xz then Z.of_int product
         else evaluate terms assignment term) in
    for _ = 0 to 2 do
      ignore (N.check solver ~integer:model ~representative:Fun.id
        ~explain:(fun _ _ -> None))
    done in
  let contains premises conclusion =
    let literals = (conclusion + 1)
      :: List.map (fun term -> -(term + 1)) premises
      |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem literals !clauses) "missing integer product bound" in
  check 2 3 4 20;
  contains [cmp ">=" x (int 1); cmp ">=" y (int 0)] (cmp ">=" xy y);
  check 2 (-3) (-2) 0;
  contains [cmp ">=" x (int 1); cmp "<=" y (int 0)] (cmp "<=" xy y);
  check 2 3 4 20;
  contains [cmp ">=" x (int 0); cmp "<" y z]
    (cmp "<=" (op "+" T.Int xy x) xz);
  check (-2) 3 4 (-20);
  contains [cmp "<=" x (int 0); cmp "<" y z]
    (cmp ">=" (op "+" T.Int xy x) xz);
  for xv = -3 to 3 do
    for yv = -3 to 3 do
      for zv = -3 to 3 do
        let assignment = function
          | "x" -> Z.of_int xv | "y" -> Z.of_int yv
          | "z" -> Z.of_int zv | _ -> failwith "unknown variable" in
        List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms assignment (abs literal - 1) = (literal > 0))
                clause) "invalid integer product bound") !clauses
      done
    done
  done

let grouped_product_tests print =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" in
  let int n = T.integer terms (Z.of_int n) in
  let op name sort args = T.builtin terms name sort args in
  let mul args = op "*" T.Int args in
  let xy = mul [|x; y|] and xyz = mul [|x; y; z|] in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) [xy; xyz];
  let assignment xv yv zv = function
    | "x" -> Z.of_int xv | "y" -> Z.of_int yv
    | "z" -> Z.of_int zv
    | _ -> failwith "unknown grouped product variable" in
  let run x y z =
    ignore (N.propagate_products solver ~representative:Fun.id
      ~explain:(fun _ _ -> None)
      ~integer:(fun term -> Some (evaluate terms (assignment x y z) term)));
    ignore (N.check solver ~representative:Fun.id ~explain:(fun _ _ -> None)
      ~integer:(fun term -> Some (evaluate terms (assignment x y z) term))) in
  run 2 3 (-2);
  run 2 3 2;
  run (-2) 3 (-2);
  run (-2) 3 2;
  let contains premises conclusion =
    let literals = (conclusion + 1)
      :: List.map (fun term -> -(term + 1)) premises
      |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem literals !clauses) "missing grouped product lemma" in
  let cmp name a b = op name T.Bool [|a; b|] in
  contains [cmp "<=" z (int (-1)); cmp ">=" xy (int 0)]
    (cmp "<=" xyz (op "-" T.Int [|int 0; xy|]));
  contains [cmp ">=" z (int 1); cmp ">=" xy (int 0)] (cmp ">=" xyz xy);
  contains [cmp "<=" z (int (-1)); cmp "<=" xy (int 0)]
    (cmp ">=" xyz (op "-" T.Int [|int 0; xy|]));
  contains [cmp ">=" z (int 1); cmp "<=" xy (int 0)] (cmp "<=" xyz xy);
  for xv = -2 to 2 do
    for yv = -2 to 2 do
      for zv = -2 to 2 do
        List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms (assignment xv yv zv) (abs literal - 1)
                = (literal > 0)) clause) "invalid grouped product lemma")
          !clauses
      done
    done
  done;
  if print then begin
    print_endline "(reset)(set-logic ALL)";
    List.iter (Printf.printf "(declare-const %s Int)\n") ["x"; "y"; "z"];
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

let product_difference_tests print =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" and w = variable terms "w" in
  let int n = T.integer terms (Z.of_int n) in
  let op name sort args = T.builtin terms name sort args in
  let mul a b = op "*" T.Int [|a; b|] in
  let xy = mul x y and wz = mul w z in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) [xy; wz];
  let representative term = if term = w then x else term in
  let explanation = T.equal terms x w in
  let explain left right =
    if left = x && right = w || left = w && right = x
    then Some [|explanation + 1|] else None in
  let check xv yv zv lhs rhs =
    let assignment = function
      | "x" | "w" -> Z.of_int xv | "y" -> Z.of_int yv
      | "z" -> Z.of_int zv | _ -> failwith "unknown variable" in
    let integer term = Some (if term = xy then Z.of_int lhs
        else if term = wz then Z.of_int rhs
        else evaluate terms assignment term) in
    ignore (N.propagate_products solver ~integer ~representative ~explain) in
  let contains sign order =
    let cmp name a b = op name T.Bool [|a; b|] in
    let premise = cmp "<=" y (op "+" T.Int [|z; int 1|]) in
    let bound = op "+" T.Int [|wz; x|] in
    let conclusion = cmp order xy bound in
    let literals = [conclusion + 1; -(explanation + 1); -(premise + 1);
      -(cmp sign x (int 0) + 1)]
      |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem literals !clauses) "missing product difference bound" in
  check 2 4 3 20 0;
  contains ">=" "<=";
  check (-2) 4 3 (-20) 0;
  contains "<=" ">=";
  check 2 3 3 1 0;
  for xv = -3 to 3 do
    for yv = -3 to 3 do
      for zv = -3 to 3 do
        for wv = -3 to 3 do
          let assignment = function
            | "x" -> Z.of_int xv | "y" -> Z.of_int yv
            | "z" -> Z.of_int zv | "w" -> Z.of_int wv
            | _ -> failwith "unknown variable" in
          List.iter (fun clause ->
              require (Array.exists (fun literal ->
                  truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "invalid product difference bound") !clauses
        done
      done
    done
  done;
  if print then begin
    print_endline "(reset)(set-logic ALL)";
    List.iter (Printf.printf "(declare-const %s Int)\n") ["x"; "y"; "z"; "w"];
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

let ring_fixture () =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" and a = variable terms "a" in
  let b = variable terms "b" and c = variable terms "c" in
  let builtin name args = T.builtin terms name T.Int args in
  let mul args = builtin "*" args in
  let ab = mul [|a; b|] and bc = mul [|b; c|] in
  let ac = mul [|a; c|] and xy = mul [|x; y|] in
  let bbz = mul [|b; b; z|] in
  let target = T.equal terms xy bbz in
  let aliases = [ab, x; bc, y; ac, z] in
  let representative term =
    Option.value (List.assoc_opt term aliases) ~default:term in
  let explain left right =
    if List.mem (left, right) aliases || List.mem (right, left) aliases
    then Some [|T.equal terms left right + 1|] else None in
  let two = T.integer terms (Z.of_int 2) in
  let next = builtin "+" [|a; T.integer terms Z.one|] in
  let half = builtin "div" [|a; two|] in
  let other = builtin "div" [|next; two|] in
  let halves = T.equal terms (builtin "+" [|half; other|]) a in
  let divisions = List.map (fun divisor ->
      divisor, builtin "div" [|x; divisor|], builtin "mod" [|x; divisor|])
      [y; z] in
  let register solver =
    List.iter (N.register_product solver) [ab; bc; ac; xy; bbz];
    List.iter (fun (divisor, quotient, remainder) ->
        N.register_division solver ~dividend:x ~divisor ~quotient ~remainder)
      divisions in
  let integer term =
    Some (evaluate terms (fun _ -> Z.of_int 2) term) in
  terms, target, halves, representative, explain, register, integer

let ring_tests print =
  let terms, target, halves, representative, explain, register, integer =
    ring_fixture () in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  register solver;
  ignore (N.check solver ~representative ~explain ~integer);
  require (List.exists (Array.mem (target + 1)) !clauses)
    "missing polynomial equality consequence";
  require (List.exists (Array.mem (halves + 1)) !clauses)
    "missing consecutive halves identity";
  let variables = [|"x"; "y"; "z"; "a"; "b"; "c"|] in
  let values = Array.make 6 0 in
  let rec assignments index =
    if index = 6 then begin
      let assignment name =
        let value = ref Z.zero in
        Array.iteri (fun i variable ->
            if variable = name then value := Z.of_int values.(i)) variables;
        !value in
      List.iter (fun clause ->
          require (Array.exists (fun literal ->
              truth terms assignment (abs literal - 1) = (literal > 0)) clause)
            "invalid polynomial equality consequence") !clauses
    end else
      for value = -2 to 2 do
        values.(index) <- value;
        assignments (index + 1)
      done in
  assignments 0;
  if print then begin
    print_endline "(set-logic ALL)";
    Array.iter (Printf.printf "(declare-const %s Int)\n") variables;
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

let halves_guard_tests print =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let d = variable terms "d" in
  let int n = T.integer terms (Z.of_int n) in
  let builtin name args = T.builtin terms name T.Int args in
  let next = builtin "+" [|x; int 1|] in
  let first = builtin "div" [|x; d|] in
  let second = builtin "div" [|y; d|] in
  let target = T.equal terms (builtin "+" [|first; second|]) x in
  let assumptions = [y, next; d, int 2] in
  let representative term =
    Option.value (List.assoc_opt term assumptions) ~default:term in
  let explain left right =
    if List.mem (left, right) assumptions
       || List.mem (right, left) assumptions
    then Some [|T.equal terms left right + 1|] else None in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  N.register_product solver (builtin "*" [|x; x|]);
  let assignment xv yv dv = function
    | "x" -> Z.of_int xv | "y" -> Z.of_int yv | "d" -> Z.of_int dv
    | _ -> failwith "unknown halves variable" in
  ignore (N.check solver ~representative ~explain
      ~integer:(fun term -> Some (evaluate terms (assignment 3 4 2) term)));
  let expected = (target + 1)
    :: List.map (fun (left, right) -> -(T.equal terms left right + 1))
      assumptions |> List.sort_uniq Int.compare |> Array.of_list in
  require (List.mem expected !clauses) "missing guarded halves identity";
  for xv = -5 to 5 do
    for yv = -5 to 5 do
      for dv = -3 to 3 do
        List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms (assignment xv yv dv) (abs literal - 1)
                = (literal > 0)) clause) "invalid guarded halves identity")
          !clauses
      done
    done
  done;
  if print then begin
    print_endline "(reset)(set-logic ALL)";
    List.iter (Printf.printf "(declare-const %s Int)\n") ["x"; "y"; "d"];
    List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
            let term = T.to_string terms (abs literal - 1) in
            if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) !clauses
  end

let ring_interruption_tests () =
  let run stop =
    let terms, _, _, representative, explain, register, integer =
      ring_fixture () in
    let calls = ref 0 and armed = ref true and clauses = ref [] in
    let event () =
      incr calls;
      if !armed && !calls = stop then begin
        armed := false;
        raise Interrupted
      end in
    let solver = N.create ~terms ~poll:event
        ~boolean:(fun term -> event (); term + 1)
        ~clause:(fun clause ->
          event ();
          let literals = Array.to_list (Array.map (fun literal ->
              let term = T.to_string terms (abs literal - 1) in
              if literal > 0 then term else "(not " ^ term ^ ")") clause) in
          clauses := List.sort String.compare literals :: !clauses) () in
    register solver;
    let check () = ignore (N.check solver ~representative ~explain ~integer) in
    (try check () with Interrupted -> check ());
    List.sort_uniq compare !clauses, !calls in
  let expected, calls = run max_int in
  for stop = 1 to calls do
    let actual, _ = run stop in
    require (List.for_all (fun clause -> List.mem clause actual) expected)
      "interruption lost a polynomial lemma"
  done

let square_secant_tests () =
  List.iter (fun (lower, upper) ->
    for form = 0 to 3 do
      let terms = T.create () in
      let x = variable terms "x" in
      let integer n = T.integer terms (Z.of_int n) in
      let builtin op sort arguments = T.builtin terms op sort arguments in
      let comparison op left right = builtin op T.Bool [|left; right|] in
      let square = builtin "*" T.Int [|x; x|] in
      let lower_bound, upper_bound = match form with
        | 0 -> comparison ">=" x (integer lower),
               comparison "<=" x (integer upper)
        | 1 -> comparison "<=" (integer lower) x,
               comparison ">=" (integer upper) x
        | 2 -> comparison ">" x (integer (lower - 1)),
               comparison "<" x (integer (upper + 1))
        | _ -> comparison "<" (integer (lower - 1)) x,
               comparison ">" (integer (upper + 1)) x in
      let clauses = ref [] in
      let solver = N.create ~terms ~boolean:(fun term -> term + 1)
          ~clause:(fun clause -> clauses := clause :: !clauses) () in
      N.register_product solver square;
      N.register_modular_term solver lower_bound;
      N.register_modular_term solver upper_bound;
      let midpoint = (lower + upper) / 2 in
      let assignment = function "x" -> Z.of_int midpoint
        | _ -> failwith "unknown square variable" in
      let model term =
        if term = square then
          Some (Z.of_int ((lower + upper) * midpoint - lower * upper + 1))
        else Some (evaluate terms assignment term) in
      ignore (N.propagate_products solver ~integer:model
        ~representative:Fun.id ~explain:(fun _ _ -> None));
      let line = builtin "-" T.Int
          [|builtin "*" T.Int [|integer (lower + upper); x|];
            integer (lower * upper)|] in
      let expected =
        [-(comparison ">=" x (integer lower) + 1);
         -(comparison "<=" x (integer upper) + 1);
         comparison "<=" square line + 1]
        |> List.sort_uniq Int.compare |> Array.of_list in
      require (List.mem expected !clauses) "missing square secant";
      for value = lower - 3 to upper + 3 do
        List.iter (fun clause ->
          require (Array.exists (fun literal ->
            truth terms (fun _ -> Z.of_int value) (abs literal - 1)
            = (literal > 0)) clause) "invalid square secant") !clauses
      done
    done) [-7, -3; -4, 6; 0, 8; 3, 9]

let power_order_tests () =
  List.iter (fun degree ->
    let terms = T.create () in
    let x = variable terms "x" and y = variable terms "y" in
    let a = variable terms "a" and b = variable terms "b" in
    let power term = T.builtin terms "*" T.Int (Array.make degree term) in
    let px = power x and py = power y in
    let mixed = T.builtin terms "*" T.Int
        (Array.init degree (fun index -> if index = 0 then a else x)) in
    let other = T.builtin terms "*" T.Int
        (Array.init degree (fun index -> if index = 0 then b else y)) in
    let clauses = ref [] in
    let solver = N.create ~terms ~boolean:(fun term -> term + 1)
        ~clause:(fun clause -> clauses := clause :: !clauses) () in
    List.iter (N.register_product solver) [px; py; mixed; other];
    List.iter (fun (xv, yv) ->
      List.iter (fun order ->
        let assignment = function
          | "x" | "a" -> Z.of_int xv | "y" | "b" -> Z.of_int yv
          | _ -> failwith "Unknown power variable" in
        let integer term =
          Some (if term = px then Z.of_int order
                else if term = py then Z.zero
                else evaluate terms assignment term) in
        let representative term =
          if term = a then x else if term = b then y else term in
        let explain left right =
          if left = a && right = x || left = x && right = a
            || left = b && right = y || left = y && right = b
          then Some [|T.equal terms left right + 1|] else None in
        ignore (N.propagate_products solver ~integer ~representative ~explain);
        ignore (N.check solver ~integer ~representative ~explain)) [-1; 0; 1])
      [-3, -2; -2, -3; -2, 2; 2, -2; 0, 2; 2, 0; 2, 3; 3, 2; 2, 2];
    let equality =
      if degree land 1 = 0 then T.or_ terms
          [|T.equal terms x y;
            T.equal terms x (T.builtin terms "-" T.Int
              [|T.integer terms Z.zero; y|])|]
      else T.equal terms x y in
    let expected = [-(T.equal terms px py + 1); equality + 1]
      |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem expected !clauses) "Missing power injectivity";
    for xv = -5 to 5 do
      for yv = -5 to 5 do
        for av = -1 to 1 do
          for bv = -1 to 1 do
            let assignment = function
              | "x" -> Z.of_int xv | "y" -> Z.of_int yv
              | "a" -> Z.of_int av | "b" -> Z.of_int bv
              | _ -> failwith "Unknown power variable" in
            List.iter (fun clause ->
              require (Array.exists (fun literal ->
                  truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "Invalid power lemma") !clauses
          done
        done
      done
    done) [2; 3; 4; 5; 7; 8]

let product_interval_tests () =
  let terms = T.create () in
  let x = variable terms "x" and y = variable terms "y" in
  let z = variable terms "z" in
  let integer n = T.integer terms (Z.of_int n) in
  let product args = T.builtin terms "*" T.Int args in
  let xy = product [|x; y|] in
  let products = [xy; product [|x; x|]; product [|x; y; z|];
    product [|integer (-2); x; y; z|]; product [|x; y; x|];
    product (Array.make 16 x)] in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) products;
  let register op a b =
    N.register_modular_term solver (T.builtin terms op T.Bool [|a; b|]) in
  List.iter (fun (term, lower, upper) ->
    register ">" term (integer (lower - 1));
    register "<" term (integer (upper + 1))) [x, -3, 2; y, 1, 4; z, -2, -1];
  List.iter (fun value ->
    ignore (N.propagate_products solver ~representative:Fun.id
      ~explain:(fun _ _ -> None) ~integer:(fun term ->
        if List.mem term products then Some (Z.of_int value)
        else match T.node terms term with
          | T.Integer value -> Some value
          | _ when term = x -> Some Z.zero
          | _ when term = y -> Some (Z.of_int 2)
          | _ when term = z -> Some (Z.of_int (-1))
          | _ -> None))) [-100000; 100000];
  let bound op term value = T.builtin terms op T.Bool [|term; integer value|] in
  let guards = [bound ">=" x (-3); bound "<=" x 2;
                bound ">=" y 1; bound "<=" y 4] in
  List.iter (fun (op, value) ->
    let expected = bound op xy value + 1
        :: List.map (fun term -> -(term + 1)) guards
        |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem expected !clauses) "Missing finite product bound")
    [">=", -12; "<=", 8];
  for xv = -5 to 5 do
    for yv = -5 to 5 do
      for zv = -5 to 5 do
        let assignment = function
          | "x" -> Z.of_int xv | "y" -> Z.of_int yv
          | "z" -> Z.of_int zv | _ -> failwith "Unknown interval variable" in
        List.iter (fun clause ->
          require (Array.exists (fun literal ->
            truth terms assignment (abs literal - 1) = (literal > 0)) clause)
            "Invalid finite product bound") !clauses
      done
    done
  done

let extension_scan_resources () =
  let module R = Vox_smt_resource in
  let count = 300 in
  let terms = T.create () in
  let resource = ref (R.create ~budget:(8 * count) ()) in
  let solver = N.create ~terms ~poll:(fun () -> R.poll !resource)
      ~boolean:(fun _ -> failwith "Unexpected Boolean encoding")
      ~clause:(fun _ -> failwith "Unexpected extension lemma") () in
  for index = 1 to count do
    let operand = variable terms (string_of_int index) in
    let term = T.builtin terms "piand" T.Int
        [|T.integer terms Z.one; operand; operand|] in
    N.register_extension solver term
  done;
  let integer term = match T.node terms term with
    | T.Integer value -> Some value | _ -> Some Z.zero in
  let run () = N.check solver ~integer ~representative:Fun.id
      ~explain:(fun _ _ -> None) in
  (match run () with
  | exception R.Exhausted "resource limit" -> ()
  | _ -> failwith "Skipped extension pairs bypassed the work budget");
  resource := R.create ();
  require (run () = N.Done) "Extension scan did not recover";
  require (R.used !resource <= 15 * count)
    "Skipped extension pairs consumed quadratic work"

let product_pair_tests () =
  List.iter (fun (offset, negative) ->
    let terms = T.create () in
    let x = variable terms "x" and y = variable terms "y" in
    let a = variable terms "a" and b = variable terms "b" in
    let number n = T.integer terms (Z.of_int n) in
    let sum term n = T.builtin terms "+" T.Int [|term; number n|] in
    let neg term = T.builtin terms "-" T.Int [|number 0; term|] in
    let ax = sum x offset and by = sum y (-offset) in
    let av = if negative then neg ax else ax in
    let left = T.builtin terms "*" T.Int [|a; b|] in
    let right = T.builtin terms "*" T.Int [|ax; by|] in
    let ga = T.equal terms a av and gb = T.equal terms b by in
    let representative term =
      if term = av then a else if term = by then b else term in
    let explain l r =
      if l = av && r = a || l = a && r = av then Some [|ga + 1|]
      else if l = by && r = b || l = b && r = by then Some [|gb + 1|]
      else None in
    let clauses = ref [] in
    let solver = N.create ~terms ~boolean:(fun term -> term + 1)
        ~clause:(fun clause -> clauses := clause :: !clauses) () in
    N.register_product solver left;
    N.register_product solver right;
    for _ = 0 to 3 do
      ignore (N.check solver ~representative ~explain ~integer:(fun term ->
      Some (if term = left then Z.of_int 7 else if term = right then Z.of_int 3
        else evaluate terms (fun _ -> Z.one) term)))
    done;
    let targets = if negative then
        [T.equal terms left (neg right); T.equal terms right (neg left)]
      else [T.equal terms left right; T.equal terms right left] in
    require (List.exists (fun clause ->
      Array.exists (fun literal -> literal > 0
        && List.mem (literal - 1) targets) clause) !clauses)
      "Missing explained product pair equality";
    for xv = -3 to 3 do
      for yv = -3 to 3 do
        for av = -3 to 3 do
          for bv = -3 to 3 do
            let assignment = function
              | "x" -> Z.of_int xv | "y" -> Z.of_int yv
              | "a" -> Z.of_int av | "b" -> Z.of_int bv
              | _ -> failwith "Unknown product pair variable" in
            List.iter (fun clause ->
              require (Array.exists (fun literal ->
                truth terms assignment (abs literal - 1) = (literal > 0))
                  clause) "Invalid explained product pair equality") !clauses
          done
        done
      done
    done) [-2, false; 0, false; 3, false; -2, true; 0, true; 3, true]

let factor_fixture mode operator () =
  let terms = T.create () in
  let a = variable terms "a" and b = variable terms "b" in
  let x = variable terms "x" and y = variable terms "y" in
  let number n = T.integer terms (Z.of_int n) in
  let builtin op arguments = T.builtin terms op T.Int arguments in
  let factor = match mode with
    | 0 -> a
    | 1 -> builtin "*" [|a; a|]
    | 2 -> builtin "*" [|a; b|]
    | _ -> builtin "*" [|number (-2); a; a; b|] in
  let reduced = builtin "+" [|x; builtin "*" [|number (-2); y|]; number 3|] in
  let product = builtin "*" [|factor; reduced|] in
  let target = T.builtin terms operator T.Bool [|product; number 0|] in
  terms, product, target

let factor_comparison_tests () =
  for mode = 0 to 3 do
    List.iter (fun operator ->
      let terms, product, target = factor_fixture mode operator () in
      let clauses = ref [] in
      let solver = N.create ~terms ~boolean:(fun term -> term + 1)
          ~clause:(fun clause -> clauses := clause :: !clauses) () in
      N.register_product solver product;
      N.register_modular_term solver target;
      ignore (N.check solver ~representative:Fun.id
        ~explain:(fun _ _ -> None) ~integer:(fun term ->
          Some (evaluate terms (fun _ -> Z.one) term)));
      require (List.exists (Array.mem (target + 1)) !clauses)
        "Missing factored comparison";
      for av = -2 to 2 do
        for bv = -2 to 2 do
          for xv = -2 to 2 do
            for yv = -2 to 2 do
              let assignment = function
                | "a" -> Z.of_int av | "b" -> Z.of_int bv
                | "x" -> Z.of_int xv | "y" -> Z.of_int yv
                | _ -> failwith "Unknown factor variable" in
              List.iter (fun clause ->
                require (Array.exists (fun literal ->
                    truth terms assignment (abs literal - 1) = (literal > 0))
                    clause) "Invalid factored comparison") !clauses
            done
          done
        done
      done) ["="; "<"; "<="; ">"; ">="]
  done

let factor_interruption_tests () =
  let run stop =
    let terms, product, target = factor_fixture 3 "<=" () in
    let calls = ref 0 and armed = ref true and clauses = ref [] in
    let event () =
      incr calls;
      if !armed && !calls = stop then begin
        armed := false;
        raise Interrupted
      end in
    let solver = N.create ~terms ~poll:event
        ~boolean:(fun term -> event (); term + 1)
        ~clause:(fun clause ->
          event ();
          let literals = Array.to_list (Array.map (fun literal ->
              let term = T.to_string terms (abs literal - 1) in
              if literal > 0 then term else "(not " ^ term ^ ")") clause) in
          clauses := List.sort String.compare literals :: !clauses) () in
    N.register_product solver product;
    N.register_modular_term solver target;
    let rec check remaining =
      require (remaining > 0) "Factor interruption did not settle";
      match N.check solver ~representative:Fun.id ~explain:(fun _ _ -> None)
          ~integer:(fun term ->
            Some (evaluate terms (fun _ -> Z.one) term)) with
      | N.Done -> ()
      | N.Continue -> check (remaining - 1)
      | N.Incomplete _ -> failwith "Factor coherent model rejected" in
    (try check 10 with Interrupted -> check 10);
    List.sort_uniq compare !clauses, !calls in
  let expected, calls = run max_int in
  for stop = 1 to calls do
    let actual, _ = run stop in
    require (actual = expected) "Interruption lost a factored comparison"
  done

let monomial_fixture () =
  let terms = T.create () in
  let a = variable terms "a" and b = variable terms "b" in
  let c = variable terms "c" and d = variable terms "d" in
  let mul factors = T.builtin terms "*" T.Int (Array.of_list factors) in
  let products = List.map mul
      [[a;a;b]; [b;b;b]; [a;c;c]; [a;a;b;c]; [a;a;a;b];
       [a;b;c]; [b;b;c;c]; [a;a;b;d]; [a;b;b;d];
       [b;b]; [c;c]; [a;b;b]] in
  terms, products

let monomial_run stop =
  let terms, products = monomial_fixture () in
  let events = ref 0 and armed = ref true and clauses = ref [] in
  let event () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll:event
      ~boolean:(fun term -> event (); term + 1)
      ~clause:(fun clause -> event (); clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) products;
  let relation = T.builtin terms "<=" T.Bool
      [|List.nth products 9; List.nth products 10|] in
  N.register_modular_term solver relation;
  let scalar = function
    | "a" -> Z.of_int 2 | "b" -> Z.of_int 3
    | "c" -> Z.of_int 4 | "d" -> Z.of_int 5
    | _ -> failwith "Unknown monomial variable" in
  let integer term =
    Some (match List.find_index ((=) term) products with
      | Some index -> Z.of_int (if index >= 9 then [|1;2;10000|].(index-9)
          else [|-3; 17; 2|].(index mod 3))
      | None -> evaluate terms scalar term) in
  let rec check remaining =
    require (remaining > 0) "Monomial fixture did not settle";
    match N.check solver ~integer ~representative:Fun.id
        ~explain:(fun _ _ -> None) with
    | N.Continue -> check (remaining - 1)
    | N.Done | N.Incomplete _ -> () in
  (try check 20 with Exit -> check 20);
  let serial clause = Array.to_list clause |> List.map (fun literal ->
      let term = T.to_string terms (abs literal - 1) in
      if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare (List.map serial !clauses),
  !events

let monomial_tests print =
  let terms, clauses, expected, events = monomial_run max_int in
  let wanted = List.sort String.compare
      ["(not (<= (* b b) (* c c)))"; "(not (>= a 0))";
       "(<= (* a b b) (* a c c))"] in
  require (List.mem wanted expected) "Missing monomial order";
  require (List.mem (List.sort String.compare
      ["(not (>= b 0))"; "(>= (* a a b) 0)"]) expected)
    "Missing monomial sign";
  for a = -2 to 2 do
    for b = -2 to 2 do
      for c = -2 to 2 do
        for d = -2 to 2 do
          let assignment = function
            | "a" -> Z.of_int a | "b" -> Z.of_int b
            | "c" -> Z.of_int c | "d" -> Z.of_int d
            | _ -> failwith "Unknown monomial variable" in
          List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms assignment (abs literal - 1) = (literal > 0))
                clause) "Invalid monomial inequality") clauses
        done
      done
    done
  done;
  if print then begin
    List.iter (Printf.printf "(declare-const %s Int)\n") ["a";"b";"c";"d"];
    List.iter (fun clause ->
      Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
        (String.concat " " clause)) expected
  end else begin
    for stop = 1 to events do
      let _, _, actual, _ = monomial_run stop in
      require (actual = expected) "Monomial interruption lost a lemma"
    done;
    ()
  end

let quadratic_fixture () =
  let terms = T.create () in
  let a = variable terms "a" and b = variable terms "b" in
  let c = variable terms "c" and d = variable terms "d" in
  let mul factors = T.builtin terms "*" T.Int (Array.of_list factors) in
  let products = List.map mul [[a;b]; [c;c]; [a;c]; [b;d]; [d;d]] in
  terms, products

let quadratic_run stop =
  let terms, products = quadratic_fixture () in
  let events = ref 0 and armed = ref true and clauses = ref [] in
  let event () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll:event
      ~boolean:(fun term -> event (); term + 1)
      ~clause:(fun clause -> event (); clauses := clause :: !clauses) () in
  List.iter (N.register_product solver) products;
  let scalar = function
    | "a" -> Z.of_int 2 | "b" -> Z.of_int 3
    | "c" -> Z.of_int 4 | "d" -> Z.of_int 5
    | _ -> failwith "Unknown quadratic variable" in
  let integer term =
    Some (match List.find_index ((=) term) products with
      | Some index -> Z.of_int [|17;2;-3;13;1|].(index)
      | None -> evaluate terms scalar term) in
  let rec check remaining =
    require (remaining > 0) "Quadratic fixture did not settle";
    match N.check solver ~integer ~representative:Fun.id
        ~explain:(fun _ _ -> None) with
    | N.Continue -> check (remaining - 1)
    | N.Done | N.Incomplete _ -> () in
  (try check 20 with Exit -> check 20);
  let serial clause = Array.to_list clause |> List.map (fun literal ->
      let term = T.to_string terms (abs literal - 1) in
      if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare (List.map serial !clauses),
  !events

let quadratic_tests print =
  let terms, clauses, expected, events = quadratic_run max_int in
  let wanted = List.sort String.compare
      ["(not (>= a 0))"; "(not (>= b 0))";
       "(not (< a c))"; "(not (<= b c))"; "(not (>= c 1))";
       "(< (* a b) (* c c))"] in
  require (List.mem wanted expected) "Missing strict product order";
  for a = -2 to 2 do
    for b = -2 to 2 do
      for c = -2 to 2 do
        for d = -2 to 2 do
          let assignment = function
            | "a" -> Z.of_int a | "b" -> Z.of_int b
            | "c" -> Z.of_int c | "d" -> Z.of_int d
            | _ -> failwith "Unknown quadratic variable" in
          List.iter (fun clause ->
            require (Array.exists (fun literal ->
                truth terms assignment (abs literal - 1) = (literal > 0))
                clause) "Invalid quadratic inequality") clauses
        done
      done
    done
  done;
  if print then begin
    List.iter (Printf.printf "(declare-const %s Int)\n") ["a";"b";"c";"d"];
    List.iter (fun clause ->
      Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
        (String.concat " " clause)) expected
  end else begin
    for stop = 1 to events do
      let _, _, actual, _ = quadratic_run stop in
      require (actual = expected) "Quadratic interruption lost a lemma"
    done;
    ()
  end

let candidate_division_tests () =
  let terms = T.create () in
  let x = variable terms "cx" and y = variable terms "cy" in
  let q0 = variable terms "cq0" and r0 = variable terms "cr0" in
  let q1 = variable terms "cq1" and r1 = variable terms "cr1" in
  let zero = T.integer terms Z.zero in
  let product = T.builtin terms "*" T.Int [|x; y|] in
  let power = T.builtin terms "int.pow2" T.Int [|x|] in
  let logarithm = T.builtin terms "int.log2" T.Int [|y|] in
  let and_ = T.builtin terms "piand" T.Int [|x; x; y|] in
  let bindings = [x, 3; y, 5; q0, 19; r0, -8; q1, 1; r1, 2;
    product, 15; power, 8; logarithm, 2; and_, 1] in
  let lookup term = match List.assoc_opt term bindings with
    | Some value -> Some (Z.of_int value)
    | None -> (match T.node terms term with
      | T.Integer value -> Some value | _ -> None) in
  let make poll =
    let clauses = ref 0 in
    let solver = N.create ~terms ~boolean:(fun term -> term + 1)
        ~clause:(fun _ -> incr clauses) ~poll () in
    N.register_product solver product;
    List.iter (N.register_extension solver) [power; logarithm; and_];
    N.register_division solver ~dividend:x ~divisor:zero
      ~quotient:q0 ~remainder:r0;
    N.register_division solver ~dividend:y ~divisor:x
      ~quotient:q1 ~remainder:r1;
    solver, clauses in
  let run solver model = N.check solver ~integer:model
      ~representative:Fun.id ~explain:(fun _ _ -> None) in
  let solver, clauses = make (fun () -> ()) in
  require (run solver lookup = N.Done) "Exact candidate division model";
  require (!clauses = 0) "Exact candidate model emitted a lemma";
  List.iter (fun (term, _) ->
    List.iter (fun missing ->
      let solver, _ = make (fun () -> ()) in
      let model current = if current = term then
          (if missing then None else
            Option.map (fun value -> Z.add value Z.one) (lookup term))
        else lookup current in
      require (run solver model <> N.Done)
        "Inconsistent candidate division model accepted") [false; true])
    [x, 3; y, 5; q1, 1; r1, 2; product, 15;
     power, 8; logarithm, 2; and_, 1];
  List.iter (fun term ->
    let solver, _ = make (fun () -> ()) in
    let model current = if current = term then None else lookup current in
    require (run solver model <> N.Done)
      "Missing zero-division model value accepted") [q0; r0];
  let polls = ref 0 in
  let solver, _ = make (fun () -> incr polls) in
  require (run solver lookup = N.Done) "Candidate poll fixture";
  let count = !polls in
  for stop = 0 to count - 1 do
    let current = ref 0 and active = ref true in
    let solver, clauses = make (fun () ->
      if !active then begin
        let index = !current in incr current;
        if index = stop then raise Exit
      end) in
    (match run solver lookup with
    | exception Exit -> ()
    | _ -> failwith "Candidate model failed to interrupt");
    active := false;
    require (run solver lookup = N.Done) "Candidate model recovery";
    require (!clauses = 0) "Interrupted candidate emitted a lemma"
  done;
  Printf.printf "Candidate division model: %d interruptions recovered\n%!"
    count

let cyclic_modular_run stop =
  let terms = T.create () in
  let x = variable terms "cx" and y = variable terms "cy" in
  let int n = T.integer terms (Z.of_int n) in
  let op name a b = T.builtin terms name T.Int [|a; b|] in
  let wrap value = op "mod" value (int 5) in
  let self = wrap (op "+" x (int 5)) in
  let impossible = op "+" x (int 1) in
  let other = op "+" y (int 5) in
  let back = op "-" x (int 5) in
  let targets = [wrap (op "-" (wrap (op "-" x (int 1))) (int 1)),
                wrap (op "-" x (int 2));
                wrap x, wrap (op "+" y (int 5))] in
  let self_reason = T.equal_atom terms x self + 1 in
  let impossible_reason = T.equal_atom terms x impossible + 1 in
  let left_reason = T.equal_atom terms x other + 1 in
  let right_reason = T.equal_atom terms y back + 1 in
  let clauses = ref [] and events = ref 0 and armed = ref true in
  let event () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll:event
      ~boolean:(fun term -> event (); term + 1)
      ~clause:(fun clause -> event (); clauses := clause :: !clauses) () in
  for term = T.count terms - 1 downto 0 do
    N.register_modular_term solver term
  done;
  let request phase =
    let representative term =
      if term = x then
        (if phase = 0 then self else if phase = 1 then other else impossible)
      else if phase = 1 && term = y then back else term in
    let explain a b =
      if a = x && b =
          (if phase = 0 then self else if phase = 1 then other else impossible)
      then Some [|if phase = 0 then self_reason
                  else if phase = 1 then left_reason else impossible_reason|]
      else if phase = 1 && a = y && b = back then Some [|right_reason|]
      else None in
    ignore (N.request_modular_equalities solver ~representative ~explain
      targets) in
  for phase = 0 to 2 do
    (try request phase with Exit -> request phase)
  done;
  require (List.exists (Array.exists ((=) (-impossible_reason))) !clauses)
    "Impossible cyclic alias was not exercised";
  let serial clause = Array.to_list clause |> List.map (fun literal ->
      let term = T.to_string terms (abs literal - 1) in
      if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare (List.map serial !clauses),
  !events

let cyclic_modular_tests () =
  let terms, clauses, expected, events = cyclic_modular_run max_int in
  require (List.length expected >= 2) "Cyclic modular aliases lost lemmas";
  for x = -12 to 12 do
    for y = -12 to 12 do
      let assignment = function
        | "cx" -> Z.of_int x | "cy" -> Z.of_int y
        | _ -> failwith "Unknown cyclic modular variable" in
      List.iter (fun clause ->
        require (Array.exists (fun literal ->
            truth terms assignment (abs literal - 1) = (literal > 0)) clause)
          "Invalid cyclic modular lemma") clauses
    done
  done;
  for stop = 1 to events do
    let _, _, actual, _ = cyclic_modular_run stop in
    require (actual = expected) "Cyclic modular interruption lost a lemma"
  done;
  Printf.printf "Cyclic modular aliases: %d lemmas, %d interruptions recovered\n%!"
    (List.length expected) events

let symbolic_modular_run stop =
  let terms = T.create () in
  let d = variable terms "d" and i = variable terms "i" in
  let j = variable terms "j" and k = variable terms "k" in
  let s = variable terms "s" in
  let int n = T.integer terms (Z.of_int n) in
  let op name a b = T.builtin terms name T.Int [|a; b|] in
  let di = op "*" d i in
  let ks1 = op "+" (op "*" k s) (int 1) in
  let inv = op "*" i (op "-" (int 2) di) in
  let divisor = op "*" s s and dividend = op "*" d j in
  let quotient = op "div" dividend divisor in
  let remainder = op "mod" dividend divisor in
  let first = T.equal_atom terms di ks1 + 1 in
  let second = T.equal_atom terms j inv + 1 in
  let clauses = ref [] and events = ref 0 and armed = ref true in
  let event () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll:event
      ~boolean:(fun term -> event (); term + 1)
      ~clause:(fun clause -> event (); clauses := clause :: !clauses) () in
  let products = [di; inv; divisor; dividend] in
  List.iter (N.register_product solver) products;
  N.register_division solver ~dividend ~divisor ~quotient ~remainder;
  let representative term =
    if term = ks1 then di else if term = inv then j else term in
  let explain a b =
    if (a = di && b = ks1) || (a = ks1 && b = di) then Some [|first|]
    else if (a = j && b = inv) || (a = inv && b = j) then Some [|second|]
    else None in
  let assignment = function
    | "d" -> Z.of_int 3 | "i" -> Z.one | "j" -> Z.of_int (-1)
    | "k" -> Z.one | "s" -> Z.of_int 2
    | _ -> failwith "unknown modular variable" in
  let model term = Some (if term = remainder then Z.zero
    else if List.mem term products then Z.of_int 37
    else evaluate terms assignment term) in
  let guard = T.builtin terms "or" T.Bool
      [|T.builtin terms ">" T.Bool [|divisor; int 1|];
        T.builtin terms "<" T.Bool [|divisor; op "-" (int 0) (int 1)|]|] in
  let expected = [T.equal terms remainder (int 1) + 1;
    -(guard + 1); -first; -second]
    |> List.sort_uniq Int.compare |> Array.of_list in
  let run () =
    ignore (N.check solver ~integer:model ~representative ~explain) in
  (try run () with Exit -> run ());
  require (List.mem expected !clauses) "missing symbolic modular lemma";
  let serial clause = Array.to_list clause |> List.map (fun literal ->
      let term = T.to_string terms (abs literal - 1) in
      if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare (List.map serial !clauses),
  !events

let symbolic_modular_tests print =
  let terms, clauses, expected, events = symbolic_modular_run max_int in
  let seen = Hashtbl.create 32 in
  let validate terms clause =
    let serial = Array.to_list clause |> List.map (fun literal ->
        let term = T.to_string terms (abs literal - 1) in
        if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
    if not (Hashtbl.mem seen serial) then begin
      Hashtbl.add seen serial ();
      for d = -3 to 3 do
        for i = -3 to 3 do
          for j = -5 to 5 do
            for k = -3 to 3 do
              for s = -3 to 3 do
                let assignment name = Z.of_int (match name with
                  | "d" -> d | "i" -> i | "j" -> j | "k" -> k | "s" -> s
                  | _ -> failwith "unknown modular variable") in
                require (Array.exists (fun literal ->
                    truth terms assignment (abs literal - 1) = (literal > 0))
                    clause) "invalid symbolic modular lemma"
              done
            done
          done
        done
      done
    end in
  List.iter (validate terms) clauses;
  for stop = 1 to events do
    let terms, clauses, actual, _ = symbolic_modular_run stop in
    require (List.for_all (fun clause -> List.mem clause actual) expected)
      "symbolic modular interruption lost lemmas";
    List.iter (validate terms) clauses
  done;
  if print then
    Hashtbl.iter (fun clause () -> Printf.printf "(assert (not (or %s)))\n"
      (String.concat " " clause)) seen;
  Printf.printf "Symbolic modular: %d lemmas, %d interruptions recovered\n%!"
    (Hashtbl.length seen) events

let partition_run ~sign ~aliased stop =
  let terms = T.create () in
  let d = variable terms "d" and i = variable terms "i" in
  let int n = T.integer terms (Z.of_int n) in
  let op name a b = T.builtin terms name T.Int [|a; b|] in
  let first = op "*" (int 3) d in
  let inverse = op "+" (op "*" (int (sign * 4))
      (op "div" first (int (sign * 4))))
      (op "mod" (op "+" first (int 2)) (int (sign * 4))) in
  let next = if aliased then i else inverse in
  let next = op "*" next (op "-" (int 2) (op "*" d next)) in
  let dividend = op "*" d next in
  let remainder = op "mod" dividend (int (sign * 256)) in
  let explanation = T.equal_atom terms i inverse + 1 in
  let clauses = ref [] and events = ref 0 and armed = ref true in
  let event () =
    incr events;
    if !armed && !events = stop then begin armed := false; raise Exit end in
  let solver = N.create ~terms ~poll:event
      ~boolean:(fun term -> event (); term + 1)
      ~clause:(fun clause -> event (); clauses := clause :: !clauses) () in
  let rec register term =
    N.register_modular_term solver term;
    match T.node terms term with
    | T.Apply (_, arguments) -> Array.iter register arguments
    | _ -> () in
  register remainder; if aliased then register inverse;
  let representative term = if aliased && term = inverse then i else term in
  let explain a b =
    if aliased && ((a = i && b = inverse) || (a = inverse && b = i))
    then Some [|explanation|] else None in
  let assignment _ = Z.of_int 3 in
  let model term = Some (if term = remainder then Z.zero
    else evaluate terms assignment term) in
  let guard = T.equal terms (op "mod" d (int 2)) (int 1) in
  let expected = (T.equal terms remainder (int 1) + 1) :: -(guard + 1) ::
    (if aliased then [-explanation] else [])
    |> List.sort_uniq Int.compare |> Array.of_list in
  let run () = ignore (N.propagate_products solver ~integer:model
      ~representative ~explain) in
  (try run () with Exit -> run ());
  require (List.mem expected !clauses) "missing residue partition lemma";
  let serial clause = Array.to_list clause |> List.map (fun literal ->
      let term = T.to_string terms (abs literal - 1) in
      if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
  terms, !clauses, List.sort_uniq Stdlib.compare (List.map serial !clauses),
  !events

let partition_tests print =
  let seen = Hashtbl.create 32 and interruptions = ref 0 in
  let validate terms clause =
    let serial = Array.to_list clause |> List.map (fun literal ->
        let term = T.to_string terms (abs literal - 1) in
        if literal > 0 then term else "(not " ^ term ^ ")")
      |> List.sort String.compare in
    if not (Hashtbl.mem seen serial) then begin
      Hashtbl.add seen serial ();
      for d = -32 to 32 do
        for i = -32 to 32 do
          let assignment name = Z.of_int (match name with
            | "d" -> d | "i" -> i | _ -> failwith "unknown variable") in
          require (Array.exists (fun literal ->
              truth terms assignment (abs literal - 1) = (literal > 0))
              clause) "invalid residue partition lemma"
        done
      done
    end in
  List.iter (fun (sign, aliased) ->
    let terms, clauses, expected, events = partition_run ~sign ~aliased max_int in
    List.iter (validate terms) clauses;
    let stops = List.init events (fun i -> i + 1) in
    List.iter (fun stop ->
      incr interruptions;
      let terms, clauses, actual, _ = partition_run ~sign ~aliased stop in
      require (List.for_all (fun clause -> List.mem clause actual) expected)
        "partition interruption lost lemmas";
      List.iter (validate terms) clauses) stops)
    [1, false; -1, false; 1, true; -1, true];
  if print then Hashtbl.iter (fun clause () -> Printf.printf "(assert (not (or %s)))\n"
      (String.concat " " clause)) seen;
  Printf.printf "Partitions: %d lemmas, %d interruptions recovered\n%!"
    (Hashtbl.length seen) !interruptions


let partition_boundary_test kind =
  let terms = T.create () in
  let d = variable terms "d" and i = variable terms "i" in
  let int n = T.integer terms (Z.of_int n) in
  let op name a b = T.builtin terms name T.Int [|a; b|] in
  let q = op "div" d (int 4) in
  let cycle = op "div" i (int 2) in
  let dividend, modulus = match kind with
    | 0 -> op "div" (op "*" (int 8) d) (int 4), 8
    | 1 -> op "*" (int 256) q, 256
    | 2 -> op "*" q (op "-" q (int 1)), 256
    | 3 -> op "+" d i, 256
    | _ -> op "div" d (int 17), 256 in
  let remainder = T.apply terms (T.symbol terms
      { name = "mod"; arguments = [|T.Int; T.Int|];
        result = T.Int; kind = T.Builtin "mod" }) [|dividend; int modulus|] in
  let clauses = ref [] and events = ref 0 in
  let solver = N.create ~terms ~poll:(fun () -> incr events)
      ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  let rec register term =
    N.register_modular_term solver term;
    match T.node terms term with
    | T.Apply (_, arguments) -> Array.iter register arguments
    | _ -> () in
  register remainder; register q;
  if kind = 3 then register cycle;
  let representative term = if kind = 3 && term = cycle then i else term in
  let explain a b =
    if kind = 3 && ((a = i && b = cycle) || (a = cycle && b = i)) then
      Some [|T.equal_atom terms i cycle + 1|] else None in
  let model term = Some (if term = remainder then Z.of_int (-1)
      else evaluate terms (fun _ -> Z.of_int 3) term) in
  ignore (N.propagate_products solver ~integer:model ~representative ~explain);
  require (!events < 100000) "partition cycle did not terminate";
  if kind >= 2 then require (!clauses = []) "unproved residue was emitted";
  if kind = 0 then begin
    let guard = T.equal terms (op "mod" d (int 4)) (int 1) in
    let expected = [T.equal terms remainder (int 2) + 1; -(guard + 1)]
        |> List.sort_uniq Int.compare |> Array.of_list in
    require (List.mem expected !clauses) "quotient precision lost"
  end;
  if kind = 1 then require (List.exists (fun clause ->
      Array.mem (T.equal terms remainder (int 0) + 1) clause) !clauses)
      "all-residue result missing";
  for d = -64 to 64 do
    for i = -8 to 8 do
      let assignment name = Z.of_int (if name = "d" then d else i) in
      List.iter (fun clause ->
        require (Array.exists (fun literal ->
            truth terms assignment (abs literal - 1) = (literal > 0)) clause)
          "invalid partition boundary lemma") !clauses
    done
  done


let inferred_modulus_tests print =
  List.iter (fun (modulus, shifted) ->
    let terms = T.create () in
    let x = variable terms "x" and q = variable terms "q" in
    let c = variable terms "c" in
    let integer = T.integer terms in
    let op name args = T.builtin terms name T.Int args in
    let cmp name a b = T.builtin terms name T.Bool [|a; b|] in
    let modulus = Z.of_string modulus in
    let twice = Z.add modulus modulus in
    let left = if shifted then op "-" [|x; integer modulus|] else x in
    let right = op "-" [|x; integer twice|] in
    let choice = T.ite terms (cmp ">=" c (integer Z.zero)) left right in
    let divisor = if shifted then modulus else twice in
    let reduced = op "-" [|x; op "*" [|integer divisor; q|]|] in
    let target = T.equal_atom terms choice reduced in
    let low = Z.neg (Z.abs divisor) in
    let high = Z.sub (Z.add low (Z.abs divisor)) Z.one in
    let lower = cmp ">=" choice (integer low) in
    let upper = cmp "<=" choice (integer high) in
    let expected = [target + 1; -(lower + 1); -(upper + 1);
      -(cmp ">=" reduced (integer low) + 1);
      -(cmp "<=" reduced (integer high) + 1)]
      |> List.sort_uniq Int.compare |> Array.of_list in
    let initial_count = T.count terms in
    let run stop =
      let remaining = ref stop in
      let exception Stop in
      let polls = ref 0 and clauses = ref [] in
      let poll () =
        incr polls;
        match !remaining with
        | None -> ()
        | Some 0 -> remaining := None; raise Stop
        | Some n -> remaining := Some (n - 1) in
      let solver = N.create ~terms ~poll ~boolean:(fun term -> term + 1)
          ~clause:(fun clause -> clauses := clause :: !clauses) () in
      let attempt () =
        for term = initial_count - 1 downto 0 do
          N.register_modular_term solver term
        done;
        ignore (N.propagate_products solver ~integer:(fun _ -> None)
          ~representative:Fun.id ~explain:(fun _ _ -> None)) in
      (try attempt () with Stop -> attempt ());
      require (List.mem expected !clauses)
        "missing inferred modular equality after interruption";
      !polls, !clauses in
    let polls, clauses = run None in
    if not print then
      for stop = 0 to polls do ignore (run (Some stop)) done;
    let candidates = [Z.zero; Z.one; (Z.neg Z.one); low; high;
      Z.sub low Z.one; Z.add high Z.one; divisor; twice] in
    List.iter (fun xv ->
      for qv = -3 to 3 do
        List.iter (fun cv ->
          let assignment = function
            | "x" -> xv | "q" -> Z.of_int qv | "c" -> Z.of_int cv
            | _ -> failwith "unknown inferred modulus variable" in
          List.iter (fun clause ->
            require (Array.exists (fun literal ->
              truth terms assignment (abs literal - 1) = (literal > 0)) clause)
              "invalid inferred modular equality") clauses) [-1; 1]
      done) candidates;
    if print then begin
      print_endline "(reset)(set-logic QF_LIA)";
      List.iter (Printf.printf "(declare-const %s Int)\n") ["x"; "q"; "c"];
      List.iter (fun clause ->
        let literals = Array.to_list (Array.map (fun literal ->
          let term = T.to_string terms (abs literal - 1) in
          if literal > 0 then term else "(not " ^ term ^ ")") clause) in
        Printf.printf "(push 1)(assert (not (or %s)))(check-sat)(pop 1)\n"
          (String.concat " " literals)) clauses
    end)
    ["2", false; "5", true; "-7", true;
     "18446744073709551616000000000000000000000000000000000", false]

let inferred_modulus_precedence_tests () =
  let terms = T.create () in
  let x = variable terms "x" and c = variable terms "c" in
  let int n = T.integer terms (Z.of_int n) in
  let op name args = T.builtin terms name T.Int args in
  let condition = T.builtin terms ">=" T.Bool [|c; int 0|] in
  let clauses = ref [] in
  let solver = N.create ~terms ~boolean:(fun term -> term + 1)
      ~clause:(fun clause -> clauses := clause :: !clauses) () in
  List.iter (fun modulus ->
    let choice = T.ite terms condition x (op "-" [|x; int modulus|]) in
    N.register_modular_term solver choice) [0; 1; -1];
  require (not (N.has_moduli solver)) "trivial inferred modulus";
  List.iter (fun modulus ->
    let choice = T.ite terms condition x (op "-" [|x; int modulus|]) in
    N.register_modular_term solver choice)
    [2; 3; 5; 7; 11; 17; 19; 23; 29];
  let remainder = op "mod" [|x; int 13|] in
  for term = T.count terms - 1 downto 0 do
    N.register_modular_term solver term
  done;
  require (N.request_modular_equalities solver ~representative:Fun.id
    ~explain:(fun _ _ -> None) [remainder, x])
    "inferred moduli displaced an explicit modulus";
  let target = T.equal terms remainder x in
  require (List.exists (Array.mem (target + 1)) !clauses)
    "missing explicit modular equality"

let product_monotonicity_tests () =
  List.iter (fun (xv, yv) ->
    List.iter (fun offset ->
      List.iter (fun repeated ->
        let terms = T.create () in
        let x = variable terms "x" and y = variable terms "y" in
        let other = if repeated then x else y in
        let int n = T.integer terms (Z.of_int n) in
        let mul a b = T.builtin terms "*" T.Int [|a; b|] in
        let compare op a b = T.builtin terms op T.Bool [|a; b|] in
        let product = mul x other in
        let assignment = function
          | "x" -> Z.of_int xv | "y" -> Z.of_int yv
          | _ -> failwith "Unknown monotonicity variable" in
        let actual = Z.add (evaluate terms assignment product)
            (Z.of_int offset) in
        let clauses = ref [] in
        let solver = N.create ~terms ~boolean:(fun term -> term + 1)
            ~clause:(fun clause -> clauses := clause :: !clauses) () in
        N.register_product solver product;
        for _ = 1 to 3 do
          ignore (N.check solver ~representative:Fun.id
            ~explain:(fun _ _ -> None) ~integer:(fun term ->
              Some (if term = product then actual
                else evaluate terms assignment term)))
        done;
        let greater = offset < 0 in
        let nonnegative = (if repeated then xv else yv) >= 0 in
        let lower = compare (if greater = nonnegative then ">=" else "<=")
            x (int xv) in
        let sign = compare (if nonnegative then ">=" else "<=")
            other (int 0) in
        let bound = compare (if greater then ">=" else "<=")
            product (mul (int xv) other) in
        let expected = [-(lower + 1); -(sign + 1); bound + 1]
            |> List.sort_uniq Int.compare |> Array.of_list in
        require (List.mem expected !clauses)
          "Missing product monotonicity bound";
        for a = -7 to 7 do
          for b = -7 to 7 do
            let assignment = function
              | "x" -> Z.of_int a | "y" -> Z.of_int b
              | _ -> failwith "Unknown monotonicity variable" in
            List.iter (fun clause ->
              require (Array.exists (fun literal ->
                truth terms assignment (abs literal - 1) = (literal > 0))
                clause) "Invalid product monotonicity bound") !clauses
          done
        done) [false; true]) [-7; 7])
    [-6, -3; -6, 0; -6, 3; 0, -3; 0, 0; 0, 3;
     6, -3; 6, 0; 6, 3]

module Multiplied_bounds = struct
  exception Interrupted

  let fixture ?(affine_bounds = true) ~poll ~boolean_event ~clause_event
      mode op sign offset =
    let terms = T.create () in
    let x = variable terms "x" and y = variable terms "y" in
    let z = variable terms "z" and a = variable terms "a" in
    let w = variable terms "w" in
    let int n = T.integer terms (Z.of_int n) in
    let add args = T.builtin terms "+" T.Int args in
    let mul args = T.builtin terms "*" T.Int args in
    let xz = mul [|x; z|] and yz = mul [|y; z|] in
    let coefficient = int (if mode land 1 = 0 then 3 else -2) in
    let first = if mode >= 2 && mode <= 4 then a else x in
    let expression = add [|mul [|coefficient; first|]; y;
        (if mode = 5 then w else int 3)|] in
    let assignment = function
      | "x" -> Z.of_int 2
      | "a" -> Z.of_int (if mode = 4 then 7 else 2) | "y" -> Z.of_int (-1)
      | "z" -> Z.of_int sign | "w" -> Z.of_int 3
      | _ -> failwith "model variable" in
    let threshold = Z.add (evaluate terms assignment expression)
      (Z.of_int (if op = ">" || op = ">=" then -1 else 1)) in
    let bound = T.builtin terms op T.Bool
        [|expression; T.integer terms threshold|] in
    let equality = T.equal terms a x in
    let clauses = ref [] in
    let solver = N.create ~terms ~poll
      ~boolean:(fun term -> boolean_event (); term + 1)
      ~clause:(fun clause ->
        clause_event (); clauses := clause :: !clauses) () in
    List.iter (N.register_product solver) [xz; yz];
    N.register_modular_term solver bound;
    if mode = 2 || mode = 4 then N.register_modular_term solver equality;
    let representative term = if mode = 3 && term = a then x else term in
    let explain left right =
      if mode = 3 && (left = a && right = x || left = x && right = a)
      then Some [|equality + 1|] else None in
    let integer term = Some
        (if term = xz then Z.add (evaluate terms assignment term)
           (Z.of_int offset) else evaluate terms assignment term) in
    let run () = ignore (N.propagate_products ~affine_bounds solver
      ~integer ~representative ~explain) in
    terms, clauses, run

  let exact_checks terms clauses =
    let values = [-7; -1; 0; 1; 7] in
    List.iter (fun xv -> List.iter (fun yv -> List.iter (fun zv ->
      List.iter (fun av -> List.iter (fun wv ->
        let assignment = function
          | "x" -> Z.of_int xv | "y" -> Z.of_int yv
          | "z" -> Z.of_int zv | "a" -> Z.of_int av
          | "w" -> Z.of_int wv | _ -> failwith "sample variable" in
        List.iter (fun clause -> require (Array.exists (fun literal ->
          truth terms assignment (abs literal - 1) = (literal > 0)) clause)
          "invalid multiplied bound") clauses) [3; -3]) [xv; 3-xv])
      values) values) values

  let run () =
    let total = ref 0 and extra = ref 0 in
    for mode = 0 to 3 do
      List.iter (fun op -> List.iter (fun sign -> List.iter (fun offset ->
        let terms, clauses, run = fixture ~poll:ignore ~boolean_event:ignore
          ~clause_event:ignore mode op sign offset in
        run (); exact_checks terms !clauses;
        let _, baseline, run = fixture ~affine_bounds:false ~poll:ignore
          ~boolean_event:ignore ~clause_event:ignore mode op sign offset in
        run ();
        extra := !extra + List.length !clauses - List.length !baseline;
        total := !total + List.length !clauses)
        [-100; 100]) [-3; 0; 3]) ["<"; "<="; ">"; ">="]
    done;
    List.iter (fun mode ->
      let _, clauses, run = fixture ~poll:ignore ~boolean_event:ignore
          ~clause_event:ignore mode "<=" 3 100 in
      run ();
      let _, previous, run = fixture ~affine_bounds:false ~poll:ignore
        ~boolean_event:ignore ~clause_event:ignore mode "<=" 3 100 in
      run ();
      require (List.sort_uniq Stdlib.compare !clauses
        = List.sort_uniq Stdlib.compare !previous)
        "unsupported substitution emitted bounds") [4; 5];
    require (!extra > 0) "no multiplied bounds emitted";
    let interruptions = ref 0 in
    List.iter (fun mode ->
      let trial stop =
        let calls = ref 0 and armed = ref false in
        let event () = incr calls;
          if !armed && !calls = stop then
            (armed := false; raise Interrupted) in
        let terms, clauses, run = fixture ~poll:event ~boolean_event:event
            ~clause_event:event mode "<=" 3
            (if mode land 1 = 0 then 100 else -100) in
        calls := 0; armed := true;
        (try run () with Interrupted -> incr interruptions; run ());
        exact_checks terms !clauses;
        !calls, List.sort_uniq Stdlib.compare !clauses in
      let count, baseline = trial 0 in
      require (baseline <> []) "empty replay fixture";
      for stop = 1 to count do
        let _, actual = trial stop in
        require (actual = baseline) "interrupted bound emission changed clauses"
      done) [0; 2; 3];
    Printf.printf "Multiplied bounds: %d clauses, %d interruptions\n%!"
      !total !interruptions
end

module Nested_remainders = struct
  exception Interrupted

  let fixture ~poll ~boolean_event ~clause_event mode =
    let terms = T.create () in
    let a = variable terms "a" and b = variable terms "b" in
    let c = variable terms "c" and x = variable terms "x" in
    let int n = T.integer terms (Z.of_int n) in
    let add xs = T.builtin terms "+" T.Int xs in
    let sub xs = T.builtin terms "-" T.Int xs in
    let mul xs = T.builtin terms "*" T.Int xs in
    let divisions = ref [] in
    let modulo value modulus =
      let remainder = T.builtin terms "mod" T.Int [|value; modulus|] in
      let quotient = T.builtin terms "div" T.Int [|value; modulus|] in
      divisions := (value, modulus, quotient, remainder) :: !divisions;
      remainder in
    let ac = modulo a c and bc = modulo b c and xc = modulo x c in
    let left, right = match mode with
      | 0 -> add [|a; bc|], add [|a; b|]
      | 1 -> sub [|a; bc|], sub [|a; b|]
      | 2 -> mul [|ac; bc|], mul [|a; b|]
      | 3 -> sub [|bc|], sub [|b|]
      | 4 -> sub [|a; bc; xc|], sub [|a; b; x|]
      | 5 -> add [|modulo (add [|a; bc|]) c; x|], add [|a; b; x|]
      | 6 -> let other = modulo a (add [|c; int 1|]) in
        add [|other; bc|], add [|other; b|]
      | 7 -> mul [|ac; add [|bc; x|]|], mul [|a; add [|b; x|]|]
      | 8 -> add [|a; modulo b (add [|c; int 1|])|], add [|a; b|]
      | _ -> failwith "fixture mode" in
    let left = modulo left c and right = modulo right c in
    let expected = List.sort Int.compare
      [-(T.not_ terms (T.equal terms c (int 0)) + 1);
        T.equal terms left right + 1] |> Array.of_list in
    let clauses = ref [] in
    let solver = N.create ~terms ~poll
      ~boolean:(fun term -> boolean_event (); term + 1)
      ~clause:(fun clause -> clause_event (); clauses := clause :: !clauses) () in
    List.iter (fun (dividend, divisor, quotient, remainder) ->
      N.register_division solver ~dividend ~divisor ~quotient ~remainder)
      !divisions;
    let run () = for _ = 1 to 3 do
      ignore (N.check solver ~integer:(fun _ -> None)
        ~representative:Fun.id ~explain:(fun _ _ -> None)) done in
    terms, clauses, expected, run

  let exact terms clauses =
    let total = ref 0 in
    List.iter (fun a -> List.iter (fun b -> List.iter (fun c ->
      List.iter (fun x ->
        let assignment = function
          | "a" -> Z.of_int a | "b" -> Z.of_int b
          | "c" -> Z.of_int c | "x" -> Z.of_int x
          | _ -> failwith "variable" in
        List.iter (fun clause ->
          incr total;
          require (Array.exists (fun literal ->
            truth terms assignment (abs literal - 1) = (literal > 0)) clause)
            "invalid nested remainder clause") clauses)
        [-3; 0; 4]) [-4; -1; 0; 1; 5]) [-7; 0; 8]) [-6; 0; 9];
    !total

  let run () =
    let checks = ref 0 and interruptions = ref 0 in
    for mode = 0 to 8 do
      let terms, clauses, expected, run = fixture ~poll:ignore
        ~boolean_event:ignore ~clause_event:ignore mode in
      run ();
      require (List.mem expected !clauses = (mode <> 8))
        "nested remainder lemma presence";
      checks := !checks + exact terms !clauses;
      let trial stop =
        let count = ref 0 and armed = ref false in
        let event () = incr count;
          if !armed && !count = stop then
            (armed := false; raise Interrupted) in
        let _, clauses, _, run = fixture ~poll:event ~boolean_event:event
            ~clause_event:event mode in
        count := 0; armed := true;
        (try run () with Interrupted -> incr interruptions; run ());
        !count, List.sort_uniq Stdlib.compare !clauses in
      let count, baseline = trial 0 in
      for stop = 1 to count do
        let _, recovered = trial stop in
        require (baseline = recovered) "nested remainder recovery"
      done;
      Printf.printf "mode%d: %d clauses, %d callbacks recovered\n%!"
        mode (List.length !clauses) count
    done;
    Printf.printf "checks%d interruptions%d\n%!" !checks !interruptions
end

let () =
  Nested_remainders.run ();
  Multiplied_bounds.run ();
  product_monotonicity_tests ();
  inferred_modulus_precedence_tests ();
  inferred_modulus_tests false;
  cyclic_modular_tests ();
  if Array.length Sys.argv > 1
      && Sys.argv.(1) = "--symbolic-modular-lemmas" then
    begin symbolic_modular_tests true; exit 0 end;
  candidate_division_tests ();
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--quadratic-lemmas" then
    begin quadratic_tests true; exit 0 end;
  quadratic_tests false;
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--monomial-lemmas" then
    begin monomial_tests true; exit 0 end;
  monomial_tests false;
  factor_comparison_tests ();
  factor_interruption_tests ();
  product_pair_tests ();
  product_interval_tests ();
  power_order_tests ();
  extension_scan_resources ();
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--ring-lemmas" then begin
    ring_tests true; halves_guard_tests true;
    product_difference_tests true; grouped_product_tests true; exit 0
  end;
  normalize_tests ();
  let print = Array.length Sys.argv > 1 && Sys.argv.(1) = "--lemmas" in
  let modular = Array.length Sys.argv > 1
      && Sys.argv.(1) = "--modular-lemmas" in
  if not modular then lemma_tests print;
  if not print then begin
    modular_tests modular;
    modular_polynomial_tests modular;
    modular_choice_tests modular
  end;
  if not (print || modular) then begin
    symbolic_modular_tests false;
    partition_tests false;
    List.iter partition_boundary_test [0; 1; 2; 3; 4];
    requested_modular_tests ();
    requested_affine_tests ();
    affine_tests ();
    affine_rejected_cases ();
    ring_tests false;
    halves_guard_tests false;
    ring_interruption_tests ();
    integer_bound_tests ();
    square_secant_tests ();
    product_difference_tests false;
    grouped_product_tests false;
    interruption_tests ();
    print_endline "Nonlinear arithmetic tests passed"
  end
