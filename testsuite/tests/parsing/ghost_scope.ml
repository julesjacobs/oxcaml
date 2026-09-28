(* TEST
 include ocamlcommon;
 expect;
*)

let () =
  let parse s = Parse.expression (Lexing.from_string s) in
  let normalize =
    let open Ast_mapper in
    { default_mapper with
      location = (fun _ _ -> Location.none);
      expr = (fun self e ->
        let e = default_mapper.expr self e in
        { e with pexp_loc_stack = [] }) }
  in
  let equal a b = normalize.expr normalize a = normalize.expr normalize b in
  let pairs = [
    "ghost_ (lemma x); run ()", "(ghost_ (lemma x)); run ()";
    "ghost_ begin lemma1 x; lemma2 x end; run ()",
      "(ghost_ (lemma1 x; lemma2 x)); run ()";
    "ghost_ x + y", "(ghost_ x) + y";
    "[ghost_ x; y]", "[(ghost_ x); y]";
  ] in
  List.iter (fun (source, expected) ->
    assert (equal (parse source) (parse expected))) pairs;
  let cases = List.map fst pairs @ [
    "ghost_ (lemma x; lemma2 x)";
    "(ghost_ f) x";
    "ghost_ (f x)";
    "ghost_ (fun x -> x)";
    "ghost_ (let x = y in x)";
    "ghost_ (if b then x else y)";
    "ghost_ ((f x)[@attribute])";
  ] in
  List.iter (fun source ->
    let ast = parse source in
    let printed = Format.asprintf "%a" Pprintast.expression ast in
    assert (equal ast (parse printed))) cases;
  List.iter (fun source ->
    match parse source with
    | _ -> assert false
    | exception Syntaxerr.Error _ -> ()) ["ghost_ f x"; "ghost_ fun x -> x"];
  Format.printf "ghost scope and printer round-trips passed@."
;;
[%%expect{|
ghost scope and printer round-trips passed
|}]
