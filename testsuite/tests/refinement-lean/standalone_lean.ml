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
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy),
         argument,
         result,
         commu_ok ))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let int_type = Predef.type_int
let bool_type = Predef.type_bool
let option_type = Predef.type_option int_type
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
  let stdlib = Path.Pident (Ident.create_persistent "Stdlib") in
  let path = Path.Pdot (stdlib, name) in
  node type_ (Rexp_ident (Rfree (Rapp path)))

let apply type_ function_ arguments =
  node type_
    (Rexp_apply
       (function_, List.map (fun argument -> Nolabel, argument) arguments))

let binary name argument_type result_type left right =
  let function_type = arrow argument_type (arrow argument_type result_type) in
  apply result_type (primitive function_type name) [left; right]

let equal type_ left right = binary "=" type_ bool_type left right
let greater left right = binary ">" int_type bool_type left right
let greater_equal left right = binary ">=" int_type bool_type left right

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

let fact expression = Vox_vc.{ expression; location = Some loc }

let vc ?(facts = []) goal = Vox_vc.create ~loc ~facts ~goal

let () =
  let outer = Facts.empty in
  let premature = Facts.add ~loc positive_x outer in
  let with_x = Facts.enter x.rb_id outer in
  let with_fact = Facts.add ~loc positive_x with_x in
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

let tautology = vc (equal int_type (bound x) (bound x))
let entailment = vc ~facts:[fact positive_x] nonnegative_x
let not_proved = vc (greater (bound x) (int 0))
let disproved = vc (equal int_type (int 1) (int 2))

let some value =
  node option_type
    (Rexp_construct
       ( { rconstr_type_path = Predef.path_option;
           rconstr_name = "Some";
         },
         [value] ))

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

let () =
  let first = emit entailment in
  let second = emit entailment in
  assert (String.equal first second);
  let first = emit datatype in
  let second = emit datatype in
  assert (String.equal first second);
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
    check Vox_lean.Proved compound
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
