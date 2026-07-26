(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement
module Facts = Vox_vc.Fact_env

let next_type_id = ref 20_000

let fresh_type_id () =
  incr next_type_id;
  !next_type_id

let arrow argument result =
  create_expr
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy, None),
         argument,
         result,
         commu_ok ))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let int_type = Predef.type_int
let bool_type = Predef.type_bool
let stdlib_path = Path.Pident (Ident.create_persistent "Stdlib")
let bigint_path = Path.Pdot (stdlib_path, "Bigint")

let bigint_type =
  create_expr
    (Tconstr (Path.Pdot (bigint_path, "t"), [], ref Mnil))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let option_type = Predef.type_option int_type
let bool_option_type = Predef.type_option bool_type
let loc = Location.in_file "standalone_lean.ml"
let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let node type_ rexp_desc = R.create ~loc ~type_ rexp_desc
let int value = node int_type (Rexp_constant (Const_int value))
let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))

let bool value =
  node bool_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_bool;
           rconstr_name = if value then "true" else "false";
         },
         [] ))

let free type_ name = node type_ (Rexp_ident (Rfree (Rfun name)))

let primitive type_ name =
  let path = Path.Pdot (stdlib_path, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let bigint_primitive type_ name =
  let path = Path.Pdot (bigint_path, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let apply type_ function_ arguments =
  node type_
    (Rexp_apply
       (function_, List.map (fun argument -> Nolabel, argument) arguments))

let binary name argument_type result_type left right =
  let function_type = arrow argument_type (arrow argument_type result_type) in
  apply result_type (primitive function_type name) [left; right]

let equal type_ left right = binary "=" type_ bool_type left right
let not_equal type_ left right = binary "<>" type_ bool_type left right
let greater left right = binary ">" int_type bool_type left right
let greater_equal left right = binary ">=" int_type bool_type left right
let less_equal left right = binary "<=" int_type bool_type left right

let bigint_binary name result_type left right =
  let function_type = arrow bigint_type (arrow bigint_type result_type) in
  apply result_type (bigint_primitive function_type name) [left; right]

let bigint_of_int value =
  apply bigint_type
    (bigint_primitive (arrow int_type bigint_type) "of_int")
    [int value]

let bigint_comparison name left right =
  bigint_binary name bool_type left right

let bigint_is_zero value =
  apply bool_type
    (bigint_primitive (arrow bigint_type bool_type) "is_zero")
    [value]

let x =
  { rb_id = Ident.create_scoped ~scope:1 "x";
    rb_type = int_type;
  }

let view =
  { rb_id = Ident.create_scoped ~scope:2 "view";
    rb_type = int_type;
  }

let positive_refinement =
  { ref_skeleton = int_type;
    ref_view = view;
    ref_pred = greater (bound view) (int 0);
  }

let positive_x =
  Vox_vc.instantiate ~refinement:positive_refinement ~with_:(bound x)

let nonnegative_x = greater_equal (bound x) (int 0)

let test_origin =
  Vox_vc.{ kind = "binder"; name = Some "x"; span = Some loc }

let fact expression =
  Vox_vc.
    { expression; location = Some loc; scope = None; origin = test_origin }

let vc ?(facts = []) goal = Vox_vc.create ~loc ~facts ~goal

let () =
  let outer = Facts.empty in
  let premature = Facts.add ~origin:test_origin ~loc positive_x outer in
  let with_x = Facts.enter x.rb_id outer in
  let with_fact = Facts.add ~origin:test_origin ~loc positive_x with_x in
  assert (Facts.facts outer = []);
  assert (Facts.facts (Facts.enter x.rb_id premature) = []);
  assert (List.length (Facts.facts with_fact) = 1);
  let after_x = Facts.leave x.rb_id with_fact in
  assert (Facts.facts after_x = []);
  begin
    match Facts.snapshot ~loc ~goal:nonnegative_x with_fact with
    | Ok condition -> assert (List.length condition.facts = 1)
    | Error _ -> failwith "in-scope goal was rejected"
  end;
  begin
    match Facts.snapshot ~loc ~goal:nonnegative_x after_x with
    | Error { escaped = [_]; _ } -> ()
    | Error _ | Ok _ -> failwith "escaped goal was not attributed"
  end;
  print_endline "fact environment: by-value scope filtering";
  print_endline "escaped goal: rejected"

type parsed_term =
  | Name of string
  | Integer of string
  | Apply of string * parsed_term list

let refinement_predicate source =
  let lexbuf = Lexing.from_string ("int{ " ^ source ^ " }") in
  match (Parse.core_type lexbuf).ptyp_desc with
  | Ptyp_extension
      (_, PStr [{ pstr_desc = Pstr_eval (expression, _); _ }]) ->
    begin match expression.pexp_desc with
    | Pexp_constraint (predicate, _, _) -> predicate
    | _ -> failwith "unexpected refinement constraint"
    end
  | _ -> failwith "unexpected refinement parse tree"

let rec parsed_term (expression : Parsetree.expression) =
  match expression.pexp_desc with
  | Pexp_hole -> Name "_"
  | Pexp_ident { txt = Lident name; _ } -> Name name
  | Pexp_construct ({ txt = Lident name; _ }, None)
    when String.equal name "true" || String.equal name "false" ->
    Name name
  | Pexp_construct ({ txt = Lident name; _ }, Some argument) ->
    Apply (name, [parsed_term argument])
  | Pexp_constant { pconst_desc = Pconst_integer (value, None); _ } ->
    Integer value
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, arguments) ->
    Apply (name, List.map (fun (_, argument) -> parsed_term argument) arguments)
  | _ -> failwith "unexpected displayed predicate"

let check_display expected expression expected_term =
  let display = Vox_verify.render_display ~env expression in
  assert (String.equal display expected);
  assert (parsed_term (refinement_predicate display) = expected_term)

let op name left right = Apply (name, [left; right])
let name name = Name name
let integer value = Integer value

let less left right = binary "<" int_type bool_type left right
let add left right = binary "+" int_type int_type left right
let multiply left right = binary "*" int_type int_type left right
let bit_and left right = binary "land" int_type int_type left right
let bit_or left right = binary "lor" int_type int_type left right
let bit_xor left right = binary "lxor" int_type int_type left right
let conjunction left right = binary "&&" bool_type bool_type left right
let disjunction left right = binary "||" bool_type bool_type left right

let some value =
  node option_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_option;
           rconstr_name = "Some";
         },
         [value] ))

let some_bool value =
  node bool_option_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_option;
           rconstr_name = "Some";
         },
         [value] ))

let negate argument =
  apply bool_type (primitive (arrow bool_type bool_type) "not") [argument]

let () =
  let hole =
    { rb_id = Ident.create_scoped ~scope:4 "_";
      rb_type = int_type;
    }
  in
  let comparison =
    conjunction (greater (bound hole) (int 0)) (less (bound x) (int 3))
  in
  check_display "_ > 0 && x < 3" comparison
    (op "&&" (op ">" (name "_") (integer "0"))
       (op "<" (name "x") (integer "3")));
  let arithmetic = multiply (add (bound x) (int 1)) (int 2) in
  check_display "(x + 1) * 2" arithmetic
    (op "*" (op "+" (name "x") (integer "1")) (integer "2"));
  let mixed = conjunction (disjunction (bool true) (bool false)) (bool true) in
  check_display "(true || false) && true" mixed
    (op "&&" (op "||" (name "true") (name "false")) (name "true"));
  let negated = negate comparison in
  check_display "not (_ > 0 && x < 3)" negated
    (Apply ("not",
       [op "&&" (op ">" (name "_") (integer "0"))
          (op "<" (name "x") (integer "3"))]));
  let grouped_addition = add (bound x) (add (int 1) (int 2)) in
  check_display "x + (1 + 2)" grouped_addition
    (op "+" (name "x") (op "+" (integer "1") (integer "2")));
  let pred = free (arrow option_type bool_type) "pred" in
  let constructed = some (bound x) in
  let raw_construct = Vox_verify.render_display ~env constructed in
  let prefix_construct = apply bool_type pred [constructed] in
  let display = Vox_verify.render_display ~env prefix_construct in
  assert (String.equal display ("pred (" ^ raw_construct ^ ")"));
  assert (
    parsed_term (refinement_predicate "pred (Some x)")
    = Apply ("pred", [Apply ("Some", [Name "x"])]));
  let operator =
    { rb_id = Ident.create_scoped ~scope:5 "++";
      rb_type = arrow int_type int_type;
    }
  in
  check_display "(++)" (bound operator) (name "++");
  print_endline "source display: precedence round trips"

let tautology = vc (equal int_type (bound x) (bound x))
let entailment = vc ~facts:[fact positive_x] nonnegative_x
let not_proved = vc (greater (bound x) (int 0))
let disproved = vc (equal int_type (int 1) (int 2))

let datatype =
  let value = some (int 7) in
  vc (equal option_type value value)

let compound =
  let local =
    { rb_id = Ident.create_scoped ~scope:3 "local";
      rb_type = int_type;
    }
  in
  let local_equality = equal int_type (bound local) (bound local) in
  let let_expression =
    node bool_type
      (Rexp_let
         ( [{ rbind_binder = local; rbind_expr = int 3 }],
           local_equality ))
  in
  let lambda =
    node (arrow int_type bool_type)
      (Rexp_function
         { arg_label = Nolabel;
           param = local;
           body = local_equality;
         })
  in
  let lambda_application = apply bool_type lambda [int 4] in
  let pair_type = create_expr
      (Ttuple [None, int_type; None, bool_type])
      ~level:0 ~scope:0 ~id:(fresh_type_id ())
  in
  let pair = node pair_type (Rexp_tuple [None, int 5; None, bool true]) in
  let tuple_equality = equal pair_type pair pair in
  let conditional =
    node bool_type
      (Rexp_ifthenelse (bool true, tuple_equality, Some (bool false)))
  in
  let conjunction left right =
    binary "&&" bool_type bool_type left right
  in
  vc (conjunction let_expression
        (conjunction lambda_application conditional))

let emit condition =
  match Vox_lean.emit ~env condition with
  | Ok text -> text
  | Error error -> failwith error.message

let contains text pattern =
  let text_length = String.length text in
  let pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > text_length then false
    else if String.sub text index pattern_length = pattern then true
    else loop (index + 1)
  in
  pattern_length = 0 || loop 0

let comparison_application =
  let parameter =
    { rb_id = Ident.create_scoped ~scope:6 "condition";
      rb_type = bool_type;
    }
  in
  let identity =
    node (arrow bool_type bool_type)
      (Rexp_function
         { arg_label = Nolabel;
           param = parameter;
           body = bound parameter;
         })
  in
  vc (apply bool_type identity [less (int 1) (int 2)])

let comparison_constructor =
  let zero = bigint_of_int 0 in
  vc
    (equal bool_option_type
       (some_bool (bigint_is_zero zero))
       (some_bool (bool true)))
let nested_bitwise =
  bit_and (bit_or (int 1) (int 2)) (bit_xor (int 3) (int 1))

let nested_bitwise_condition =
  vc
    ~facts:[fact (equal int_type nested_bitwise (int 2))]
    (equal int_type nested_bitwise (int 2))

let postfix_selector_condition =
  let selected = bit_and (int (-1)) (int 1) in
  vc
    ~facts:[fact (greater selected (int 0))]
    (greater selected (int 0))

let () =
  let first = emit entailment in
  let second = emit entailment in
  assert (String.equal first second);
  let first = emit datatype in
  let second = emit datatype in
  assert (String.equal first second);
  let emitted = emit comparison_application in
  assert
    (contains emitted
       "((fun (l_0 : Bool) => l_0) \
         (decide ((BitVec.ofInt 63 1).toInt < (BitVec.ofInt 63 2).toInt)))");
  let emitted = emit comparison_constructor in
  assert
    (contains emitted
       ".Some (decide ((BitVec.ofInt 63 0).toInt = 0)))");
  let exact_decide_terms =
    (* An ordinary [int] is a signed 63-bit bitvector: equality is decided
       on the words themselves, while the orders read their signed value.
       A [Bigint.t] lifted from a literal is the mathematical integer that
       literal denotes, so its equality is decided there too. *)
    [ equal int_type (int 10) (int 10),
        "(decide ((BitVec.ofInt 63 10) = (BitVec.ofInt 63 10)))";
      not_equal int_type (int 11) (int 12),
        "(!(decide ((BitVec.ofInt 63 11) = (BitVec.ofInt 63 12))))";
      less (int 13) (int 14),
        "(decide ((BitVec.ofInt 63 13).toInt < (BitVec.ofInt 63 14).toInt))";
      less_equal (int 15) (int 16),
        "(decide ((BitVec.ofInt 63 15).toInt ≤ (BitVec.ofInt 63 16).toInt))";
      greater (int 18) (int 17),
        "(decide ((BitVec.ofInt 63 18).toInt > (BitVec.ofInt 63 17).toInt))";
      greater_equal (int 20) (int 19),
        "(decide ((BitVec.ofInt 63 20).toInt ≥ (BitVec.ofInt 63 19).toInt))";
      bigint_comparison "equal" (bigint_of_int 21) (bigint_of_int 21),
        "(decide ((BitVec.ofInt 63 21).toInt = (BitVec.ofInt 63 21).toInt))";
      bigint_comparison "lt" (bigint_of_int 22) (bigint_of_int 23),
        "(decide ((BitVec.ofInt 63 22).toInt < (BitVec.ofInt 63 23).toInt))";
      bigint_comparison "le" (bigint_of_int 24) (bigint_of_int 25),
        "(decide ((BitVec.ofInt 63 24).toInt ≤ (BitVec.ofInt 63 25).toInt))";
      bigint_comparison "gt" (bigint_of_int 27) (bigint_of_int 26),
        "(decide ((BitVec.ofInt 63 27).toInt > (BitVec.ofInt 63 26).toInt))";
      bigint_comparison "ge" (bigint_of_int 29) (bigint_of_int 28),
        "(decide ((BitVec.ofInt 63 29).toInt ≥ (BitVec.ofInt 63 28).toInt))";
      bigint_is_zero (bigint_of_int 0),
        "(decide ((BitVec.ofInt 63 0).toInt = 0))";
    ]
  in
  List.iter
    (fun (term, expected) -> assert (contains (emit (vc term)) expected))
    exact_decide_terms;
  let emitted = emit nested_bitwise_condition in
  assert
    (contains emitted
       "(BitVec.and (BitVec.or (BitVec.ofInt 63 1) (BitVec.ofInt 63 2)) \
        (BitVec.xor (BitVec.ofInt 63 3) (BitVec.ofInt 63 1)))");
  let emitted = emit postfix_selector_condition in
  assert
    (contains emitted
       "(BitVec.and (BitVec.ofInt 63 (-1)) \
        (BitVec.ofInt 63 1)).toInt > (BitVec.ofInt 63 0).toInt");
  print_endline "Lean emission: byte-identical"

let check expected condition =
  let result = Vox_lean.discharge ~env condition in
  assert (result.verdict = expected);
  assert (String.equal result.location.loc_start.pos_fname
            loc.loc_start.pos_fname)

let () =
  if Vox_lean.lean_available () then begin
    check Vox_lean.Proved tautology;
    check Vox_lean.Proved entailment;
    check Vox_lean.Not_proved not_proved;
    check Vox_lean.Disproved disproved;
    check Vox_lean.Proved datatype;
    check Vox_lean.Proved compound;
    check Vox_lean.Proved comparison_application;
    check Vox_lean.Proved comparison_constructor;
    check Vox_lean.Proved nested_bitwise_condition;
    check Vox_lean.Proved postfix_selector_condition
  end;
  print_endline "real Lean subprocess cases: completed or skipped"

let () =
  let function_type = arrow int_type (arrow int_type int_type) in
  let user_add = free function_type "add" in
  let user_result = apply int_type user_add [int 1; int 2] in
  let user_add_condition = vc (equal int_type user_result (int 3)) in
  let emitted = emit user_add_condition in
  assert (not (String.contains emitted '+'));
  let left = Ident.create_local "same_name" in
  let right = Ident.create_local "same_name" in
  let global id =
    node int_type (Rexp_ident (Rfree (Rglobal (Path.Pident id))))
  in
  let distinct_condition = vc (equal int_type (global left) (global right)) in
  let emitted = emit distinct_condition in
  assert (String.contains emitted '0');
  assert (String.contains emitted '1');
  if Vox_lean.lean_available () then begin
    check Vox_lean.Not_proved user_add_condition;
    check Vox_lean.Not_proved distinct_condition
  end;
  let parameter =
    { rb_id = Ident.create_local "quantified";
      rb_type = int_type;
    }
  in
  let predicate =
    node (arrow int_type bool_type)
      (Rexp_function
         { arg_label = Nolabel;
           param = parameter;
           body = bool true;
         })
  in
  let quantifier =
    apply bool_type
      (free (arrow predicate.rexp_type bool_type) "forall_")
      [predicate]
  in
  begin
    match Vox_lean.emit ~env (vc quantifier) with
    | Error { message; _ }
      when String.starts_with ~prefix:"quantifier combinator" message -> ()
    | Error error -> failwith error.message
    | Ok _ -> failwith "quantifier combinator was silently emitted"
  end;
  print_endline "reference identity and quantifier guards: checked"

let () =
  assert ((emit tautology |> String.length) > 0);
  assert ((emit (vc (bool true)) |> String.length) > 0)
