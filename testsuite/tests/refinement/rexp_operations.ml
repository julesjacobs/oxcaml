(* TEST
 include ocamlcommon;
*)

open Types

module R = Types.Refinement

let next_type_id = ref 10_000

let fresh_type_id () =
  incr next_type_id;
  !next_type_id

let named_type name =
  let path = Path.Pident (Ident.create_persistent name) in
  create_expr
    (Tconstr (path, [], ref Mnil))
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

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

let tuple_type fields =
  create_expr
    (Ttuple fields)
    ~level:0
    ~scope:0
    ~id:(fresh_type_id ())

let int_type = named_type "int"
let bool_type = named_type "bool"
let pair_type = tuple_type [None, int_type; None, int_type]

let bool_path =
  match get_desc bool_type with
  | Tconstr (path, [], _) -> path
  | _ -> assert false

let pair_path = Path.Pident (Ident.create_persistent "pair")
let module_path = Path.Pident (Ident.create_persistent "M")

let node type_ rexp_desc =
  R.create ~loc:Location.none ~type_ rexp_desc

let int value = node int_type (Rexp_constant (Const_int value))

let bool value =
  let name = if value then "true" else "false" in
  node bool_type
    (Rexp_construct
       ({ rconstr_type_path = bool_path; rconstr_name = name }, []))

let bound binder = node binder.rb_type (Rexp_ident (Rbound binder.rb_id))

let free type_ reference = node type_ (Rexp_ident (Rfree reference))

let apply type_ function_ arguments =
  node type_
    (Rexp_apply
       (function_, List.map (fun argument -> Nolabel, argument) arguments))

let eq_int_type = arrow int_type (arrow int_type bool_type)
let eq_int = free eq_int_type (Rfun "eq_int")

let equal_int left right = apply bool_type eq_int [left; right]

let validate expression =
  R.validate ~equal_type:eq_type ~bool_type expression

let expect_valid name expression =
  match validate expression with
  | Ok () -> Printf.printf "validator accepts: %s\n" name
  | Error error ->
    failwith
      (Format.asprintf "validator rejected %s: %a"
         name R.print_validation_error error)

let expect_error name expression =
  match validate expression with
  | Ok () -> failwith ("validator unexpectedly accepted " ^ name)
  | Error error ->
    Format.printf "validator rejects: %s (%a)@."
      name R.print_validation_error error

let pair_binder =
  { rb_id = Ident.create_scoped ~scope:1 "pair"; rb_type = pair_type }

let pair =
  node pair_type (Rexp_tuple [None, int 1; None, int 2])

let first =
  node int_type
    (Rexp_field
       ( bound pair_binder,
         { rfield_type_path = pair_path; rfield_name = "first" } ))

let predicate =
  let condition = equal_int first (int 1) in
  let conditional =
    node bool_type (Rexp_ifthenelse (condition, bool true, Some (bool false)))
  in
  node bool_type
    (Rexp_let
       ([{ rbind_binder = pair_binder; rbind_expr = pair }], conditional))

let lambda_binder =
  { rb_id = Ident.create_scoped ~scope:1 "x"; rb_type = int_type }

let lambda =
  let body = equal_int (bound lambda_binder) (bound lambda_binder) in
  node (arrow int_type bool_type)
    (Rexp_function { arg_label = Nolabel; param = lambda_binder; body })

let forall_type = arrow lambda.rexp_type bool_type
let quantified_predicate =
  apply bool_type (free forall_type (Rapp (Path.Pdot (module_path, "forall"))))
    [lambda]

let () =
  expect_valid "let/tuple/field/apply/if/construct" predicate;
  expect_valid "single-parameter function" quantified_predicate;
  print_endline
    "constructors: ident constant let function apply tuple construct field ifthenelse";
  Format.printf "printer: %a@." R.print (equal_int (int 1) (int 2))

let () =
  let references =
    [ Rfun "absolute";
      Rsibling "measure";
      Rapp (Path.Pdot (module_path, "operation"));
      Rglobal (Path.Pdot (module_path, "value")) ]
  in
  List.iter
    (fun reference ->
      Format.printf "reference: %a@." R.print (free bool_type reference))
    references

let () =
  let x = { rb_id = Ident.create_scoped ~scope:1 "x"; rb_type = int_type } in
  let y = { rb_id = Ident.create_scoped ~scope:1 "y"; rb_type = int_type } in
  let open_expression =
    node (arrow int_type int_type)
      (Rexp_function
         { arg_label = Nolabel; param = y; body = bound x })
  in
  let replacement = bound y in
  let substituted = R.subst ~id:x.rb_id ~by:replacement open_expression in
  match substituted.rexp_desc with
  | Rexp_function { param; body = { rexp_desc = Rexp_ident (Rbound body); _ };
                    _ } ->
    assert (not (Ident.same param.rb_id y.rb_id));
    assert (Ident.same body y.rb_id);
    print_endline "substitution: capture avoided"
  | _ -> assert false

let identity_type = arrow int_type int_type
let shadowing_type = arrow int_type identity_type

let make_shadowing outer inner use_inner =
  let body = bound (if use_inner then inner else outer) in
  let inner_function =
    node identity_type
      (Rexp_function { arg_label = Nolabel; param = inner; body })
  in
  node shadowing_type
    (Rexp_function
       { arg_label = Nolabel; param = outer; body = inner_function })

let () =
  let left_outer =
    { rb_id = Ident.create_scoped ~scope:1 "x"; rb_type = int_type }
  in
  let left_inner =
    { rb_id = Ident.create_scoped ~scope:1 "x"; rb_type = int_type }
  in
  let right_outer =
    { rb_id = Ident.create_scoped ~scope:1 "y"; rb_type = int_type }
  in
  let right_inner =
    { rb_id = Ident.create_scoped ~scope:1 "y"; rb_type = int_type }
  in
  let left = make_shadowing left_outer left_inner true in
  let right = make_shadowing right_outer right_inner true in
  let wrong = make_shadowing right_outer right_inner false in
  assert (R.alpha_equal ~equal_type:eq_type left right);
  assert (not (R.alpha_equal ~equal_type:eq_type left wrong));
  print_endline "alpha-equality: threaded shadowing pairs"

let () =
  let freshened = R.freshen_binders lambda in
  assert (R.alpha_equal ~equal_type:eq_type lambda freshened);
  match freshened.rexp_desc with
  | Rexp_function
      { param; body = { rexp_desc = Rexp_apply (_, (_, occurrence) :: _); _ };
        _ } ->
    begin match occurrence.rexp_desc with
    | Rexp_ident (Rbound occurrence) ->
      assert (Ident.same param.rb_id occurrence);
      assert (not (Ident.same param.rb_id lambda_binder.rb_id))
    | _ -> assert false
    end;
    print_endline "freshening: every imported binder renamed"
  | _ -> assert false

let () =
  let unbound =
    { rb_id = Ident.create_scoped ~scope:1 "free"; rb_type = bool_type }
  in
  expect_error "unbound binder" (bound unbound);
  expect_error "non-bool root" (int 0);
  let duplicate =
    { rb_id = Ident.create_scoped ~scope:1 "duplicate"; rb_type = int_type }
  in
  let duplicate_let =
    node bool_type
      (Rexp_let
         ( [ { rbind_binder = duplicate; rbind_expr = int 1 };
             { rbind_binder = duplicate; rbind_expr = int 2 } ],
           bool true ))
  in
  expect_error "duplicate binder" duplicate_let;
  let mismatched =
    node bool_type
      (Rexp_let
         ( [{ rbind_binder = lambda_binder; rbind_expr = int 1 }],
           node bool_type (Rexp_ident (Rbound lambda_binder.rb_id)) ))
  in
  expect_error "bound occurrence type" mismatched;
  expect_error "empty sibling name" (free bool_type (Rsibling ""))
