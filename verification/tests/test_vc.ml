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
