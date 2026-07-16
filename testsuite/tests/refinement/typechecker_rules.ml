(* TEST
 include ocamlcommon;
*)

open Typedtree
open Types

let source = {|
let local =
  let x = (1 : int{ true }) in
  x

let parameter = fun (x : int{ true }) -> x

let contract (x : int{ true }) = x
let contract_use = contract 1

module M : sig val x : int{ true } end = struct
  let x = (1 : int{ true })
end
let module_use = M.x

let matching = ((1 : int{ true }) : int{ true })

let mutable_case () =
  let mutable x : int{ true } = 1 in
  x
|}

let typed_structure =
  let parsed = Parse.implementation (Lexing.from_string source) in
  let structure, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) parsed
  in
  structure

let rec is_refined type_ =
  match get_desc type_ with
  | Trefine _ -> true
  | Tpoly (type_, []) -> is_refined type_
  | _ -> false

let rec is_int type_ =
  match get_desc type_ with
  | Tconstr (path, _, _) -> Path.same path Predef.path_int
  | Tpoly (type_, []) -> is_int type_
  | _ -> false

let refined_constraint_marks expression =
  List.fold_left
    (fun count (extra, _, _) ->
      match extra with
      | Texp_constraint core_type when is_refined core_type.ctyp_type ->
        count + 1
      | _ -> count)
    0 expression.exp_extra

let binding_name binding =
  match binding.vb_pat.pat_desc with
  | Tpat_var { id; _ } -> Ident.name id
  | _ -> failwith "expected a variable binding"

let bindings =
  List.filter_map
    (fun item ->
      match item.str_desc with
      | Tstr_value (_, [binding]) -> Some (binding_name binding, binding)
      | _ -> None)
    typed_structure.str_items

let binding name = List.assoc name bindings

let find_identifier name expression =
  let found = ref None in
  let super = Tast_iterator.default_iterator in
  let iterator =
    { super with
      expr =
        (fun self expression ->
          begin match expression.exp_desc with
          | Texp_ident { lid = { txt = Longident.Lident found_name; _ }; _ }
            when String.equal name found_name ->
            if Option.is_none !found then found := Some expression
          | _ -> ()
          end;
          super.expr self expression)
    }
  in
  iterator.expr iterator expression;
  Option.get !found

let function_parts expression =
  match expression.exp_desc with
  | Texp_function
      { params = [{ fp_kind = Tparam_pat pattern; _ }];
        body = Tfunction_body body;
        _ } ->
    pattern, body
  | _ -> failwith "expected a single-parameter function"

let () =
  let local = (binding "local").vb_expr in
  begin match local.exp_desc with
  | Texp_let (Nonrecursive, [inner], body) ->
    assert (is_refined inner.vb_pat.pat_type);
    assert (is_refined inner.vb_expr.exp_type);
    assert (refined_constraint_marks inner.vb_expr = 1);
    let use = find_identifier "x" body in
    assert (is_int use.exp_type);
    begin match use.exp_desc with
    | Texp_ident { desc; _ } -> assert (is_int desc.val_type)
    | _ -> assert false
    end
  | _ -> failwith "expected the local let"
  end;
  print_endline "local binder: pattern refined; environment and use skeleton"

let () =
  let parameter = (binding "parameter").vb_expr in
  let pattern, body = function_parts parameter in
  assert (is_refined pattern.pat_type);
  begin match get_desc parameter.exp_type with
  | Tarrow (_, domain, _, _) -> assert (is_refined domain)
  | _ -> failwith "expected an arrow"
  end;
  let use = find_identifier "x" body in
  assert (is_int use.exp_type);
  begin match use.exp_desc with
  | Texp_ident { desc; _ } -> assert (is_int desc.val_type)
  | _ -> assert false
  end;
  print_endline "parameter: pattern and arrow refined; body use skeleton"

let () =
  let matching = (binding "matching").vb_expr in
  assert (is_refined matching.exp_type);
  assert (refined_constraint_marks matching = 1);
  print_endline "annotations: introduction marked; matching refinement unmarked"

let () =
  let application = (binding "contract_use").vb_expr in
  begin match application.exp_desc with
  | Texp_apply (function_, [Nolabel, Arg (argument, _)], _, _, _) ->
    assert (is_int argument.exp_type);
    begin match get_desc function_.exp_type with
    | Tarrow (_, domain, _, _) -> assert (is_refined domain)
    | _ -> failwith "contract application lost its refined domain"
    end
  | _ -> failwith "expected the contract application"
  end;
  print_endline "contract: refined domain retained; argument checked at skeleton"

let () =
  let use = (binding "module_use").vb_expr in
  assert (is_int use.exp_type);
  begin match use.exp_desc with
  | Texp_ident { desc; _ } -> assert (is_refined desc.val_type)
  | _ -> failwith "expected the qualified interface value"
  end;
  print_endline "interface: signature refined; qualified use skeleton"

let () =
  let _, body = function_parts (binding "mutable_case").vb_expr in
  begin match body.exp_desc with
  | Texp_letmutable (inner, use) ->
    assert (is_refined inner.vb_pat.pat_type);
    assert (is_refined inner.vb_expr.exp_type);
    assert (refined_constraint_marks inner.vb_expr = 1);
    assert (is_refined use.exp_type);
    begin match inner.vb_pat.pat_desc, use.exp_desc with
    | Tpat_var { id; _ }, Texp_mutvar use_id ->
      assert (Ident.same id use_id.txt);
      let description =
        Env.find_value (Path.Pident id) use.exp_env
        |> Subst.Lazy.force_value_description
      in
      assert (is_refined description.val_type)
    | _ -> failwith "expected a mutable variable"
    end
  | _ -> failwith "expected a mutable let"
  end;
  print_endline "mutable binder: refinement is not stripped"
