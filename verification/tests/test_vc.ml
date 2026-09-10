open Vox_smt

let queries source =
  Language_extension.enable Refinement_types ();
  Typecore.reset_delayed_checks ();
  let parsed = Parse.implementation (Lexing.from_string source) in
  let tree, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) parsed
  in
  Typecore.force_delayed_checks ();
  let result = ref [] in
  Vox_vc.generate tree ~prove:(fun _ query ->
      check ~int_width:63 query;
      result := query :: !result);
  List.rev !result

let prelude =
  "external ge : int -> int -> bool @@ total = \"%greaterequal\"\n\
   external add : int -> int -> int @@ total = \"%addint\"\n\
   type nonnegative = {n : int | ge n 0}\n"

let size count body =
  let steps = String.concat "" (List.init count (fun _ -> body)) in
  match
    queries
      (prelude ^ "let f (b : bool) : nonnegative = let x = 0 in\n" ^ steps
     ^ "refine_ x")
  with
  | [q] -> String.length (to_smtlib ~int_width:63 ~timeout_ms:5000 q)
  | qs ->
    failwith (Printf.sprintf "Expected one query, got %d" (List.length qs))

let () =
  List.iter
    (fun body ->
      let small = size 20 body in
      let large = size 80 body in
      assert (large < 5 * small))
    [ "(if b then () else ());\n";
      "let x = if b then add x 1 else add x 2 in\n";
      "(match b with true when b -> () | false -> () | _ -> ());\n" ];
  let source =
    prelude
    ^ "let f () =\n\
       let x = 0 in\n\
       let (_ : nonnegative) = refine_ x in\n\
       let (_ : nonnegative) = refine_ x in ()\n"
  in
  assert (List.length (queries source) = 1);
  assert (
    List.length
      (queries (source ^ "let g () : nonnegative = let x = 1 in refine_ x"))
    = 2);
  let independent count =
    let parameters =
      String.concat " "
        (List.init count (fun i -> Printf.sprintf "(b%d : bool)" i))
    in
    let steps =
      String.concat ""
        (List.init count (fun i ->
             Printf.sprintf "let x = if b%d then add x 1 else add x 2 in\n" i))
    in
    match
      queries
        (prelude ^ "let f " ^ parameters ^ " : nonnegative = let x = 0 in\n"
       ^ steps ^ "refine_ x")
    with
    | [q] -> q
    | _ -> failwith "Expected one query for independent joins"
  in
  let q = independent 20 in
  let rec count_ites = function
    | App (op, args) ->
      (if op = Ite then 1 else 0)
      + List.fold_left (fun n t -> n + count_ites t) 0 args
    | _ -> 0
  in
  assert (List.fold_left (fun n f -> n + count_ites f.term) 0 q.facts = 20);
  let result =
    Vox_smt_solver.check
      ~config:{ Vox_smt_solver.default_config with executable = Sys.argv.(1) }
      ~int_width:63 q
  in
  assert (result.validity = Valid);
  print_endline "VC sharing and batching tests passed"

let () =
  let solve source =
    match queries (prelude ^ source) with
    | [q] ->
      (Vox_smt_solver.check
         ~config:
           { Vox_smt_solver.default_config with executable = Sys.argv.(1) }
         ~int_width:63 q)
        .validity
    | _ -> failwith "Expected one function-join query"
  in
  assert (
    solve
      "let f (b : bool) = let g x = x in let h x = add x 1 in let chosen = if \
       b then g else h in let a = chosen 0 in let c = chosen 0 in let (_ : {r \
       : int | r === c}) = refine_ a in ()"
    = Valid);
  assert (
    match
      solve
        "let f (b : bool) = let g x = x in let h x = add x 1 in let chosen = \
         if b then g else h in let a = chosen 0 in let c = g 0 in let (_ : {r \
         : int | r === c}) = refine_ a in ()"
    with
    | Invalid _ -> true
    | _ -> false)

let () =
  let copy_query_size count =
    let source =
      "external append : int iarray -> int iarray -> int iarray = \
       \"caml_array_append\"\n"
      ^ "external get : int iarray -> int -> int = \"%array_safe_get\"\n"
      ^ "external eq : int -> int -> bool @@ total = \"%equal\"\n"
      ^ "let f (index : int) : {value : int | eq value value} =\n"
      ^ "let array = [: 7 :] in\n"
      ^ String.concat ""
          (List.init count (fun _ ->
               "let copy = append array [: :] in\n"
               ^ "let array = append array copy in\n"))
      ^ "let value = get array index in refine_ value"
    in
    match queries source with
    | [query] -> String.length (to_smtlib ~int_width:63 ~timeout_ms:5000 query)
    | _ -> failwith "Expected one array-copy query"
  in
  let small = copy_query_size 12 in
  let large = copy_query_size 24 in
  assert (large < 3 * small);
  assert (large < 500_000);
  print_endline "Array-copy query expansion is bounded"

let () =
  let solve source =
    match queries (prelude ^ source) with
    | [q] ->
      (Vox_smt_solver.check
         ~config:
           { Vox_smt_solver.default_config with executable = Sys.argv.(1) }
         ~int_width:63 q)
        .validity
    | _ -> failwith "Expected one ghost-lambda query"
  in
  let application value =
    "let f () = let p = ghost_ (fun (x : int) -> ge x 0) in\n" ^ "let value = "
    ^ value ^ " in let u = () in\n"
    ^ "let (_ : {u : unit | p value}) = refine_ u in ()"
  in
  assert (solve (application "1") = Valid);
  let opaque_alias =
    "let f () = let p (x : int) = ge x 0 in let q = ghost_ p in\n"
    ^ "let value = 1 in let u = () in\n"
    ^ "let (_ : {u : unit | q value}) = refine_ u in ()"
  in
  assert (match solve opaque_alias with Invalid _ -> true | _ -> false);
  assert (match solve (application "-1") with Invalid _ -> true | _ -> false);
  let checked_body =
    "let f () = let _p = ghost_ (fun (x : int) ->\n"
    ^ "let negative = -1 in let (_ : nonnegative) = refine_ negative in\n"
    ^ "ge x 0) in ()"
  in
  assert (match solve checked_body with Invalid _ -> true | _ -> false);
  let array_observation operation expected =
    "external length : int iarray @ immutable total -> int @@ total = "
    ^ "\"%array_length\"\n"
    ^ "external get : int iarray @ immutable total -> int -> int @@ total = "
    ^ "\"%array_safe_get\"\n" ^ "let f () = let p = ghost_ (fun (x : int) -> "
    ^ operation ^ ") in let input = 7 in let u = () in\n"
    ^ "let (_ : {u : unit | p input === " ^ string_of_int expected
    ^ "}) = refine_ u in ()"
  in
  assert (solve (array_observation "length [: x; 20 :]" 2) = Valid);
  assert (solve (array_observation "get [: x; 20 :] 0" 7) = Valid);
  assert (
    match solve (array_observation "get [: x; 20 :] 0" 20) with
    | Invalid _ -> true
    | _ -> false);
  let lambda_size count =
    let source =
      prelude ^ "let f (x : int) = let p = ghost_ (fun (y : int) ->\n"
      ^ String.concat "" (List.init count (fun _ -> "let y = add y y in\n"))
      ^ "y) in let u = () in\n"
      ^ "let (_ : {u : unit | ge (p x) 0}) = refine_ u in ()"
    in
    match queries source with
    | [q] -> String.length (to_smtlib ~int_width:63 ~timeout_ms:5000 q)
    | _ -> failwith "Expected one shared ghost-lambda query"
  in
  let small = lambda_size 20 in
  let large = lambda_size 80 in
  assert (large < 5 * small);
  print_endline "Ghost lambdas preserve obligations and share substitutions"

let () =
  let nested_copy expected =
    "external copy : (a : int iarray iarray) -> "
    ^ "{b : int iarray iarray | b === a} @ total = \"%obj_dup\"\n"
    ^ "external row : int iarray iarray -> int -> int iarray @ total = "
    ^ "\"%array_safe_get\"\n"
    ^ "external get : int iarray -> int -> int @ total = "
    ^ "\"%array_safe_get\"\n" ^ "let f () : {n : int | n === "
    ^ string_of_int expected ^ "} =\n" ^ "let source = [: [: 10; 20 :] :] in\n"
    ^ "let refine_ values = copy source in\n"
    ^ "let selected = row values 0 in\n"
    ^ "let value = get selected 1 in refine_ value"
  in
  let solve expected =
    match queries (nested_copy expected) with
    | [query] ->
      (Vox_smt_solver.check
         ~config:
           { Vox_smt_solver.default_config with executable = Sys.argv.(1) }
         ~int_width:63 query)
        .validity
    | _ -> failwith "Expected one nested-array query"
  in
  assert (solve 20 = Valid);
  assert (match solve 10 with Invalid _ -> true | _ -> false);
  print_endline "Iarray equality transports nested observations"
