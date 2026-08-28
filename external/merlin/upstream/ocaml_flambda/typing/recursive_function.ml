open Typedtree

exception Invalid of Location.t * string

let reject loc reason = raise (Invalid (loc, reason))

let parameters exp =
  match exp.exp_desc with
  | Texp_function {params; body = Tfunction_body body; _} ->
      let params = List.map (fun param ->
        match param.fp_arg_label, param.fp_kind with
        | Nolabel, Tparam_pat ({pat_desc = Tpat_var {id; _}; _} as pat) ->
            id, pat
        | _ -> reject param.fp_loc
            "checked recursion requires simple unlabelled parameters") params
      in
      params, body
  | _ -> reject exp.exp_loc
      "checked recursion requires a function with variable parameters"

let check_predicates self exp =
  let seen = ref Btype.TypeSet.empty in
  let check_type loc ty =
    if not (Btype.TypeSet.mem ty !seen) then begin
      seen := Btype.TypeSet.add ty !seen;
      if Ctype.refinement_ident_occurs self ty then
        reject loc "the recursive function occurs in a type predicate"
    end
  in
  let default = Tast_iterator.default_iterator in
  let it =
    { default with
      typ = (fun it ty ->
        check_type ty.ctyp_loc ty.ctyp_type;
        default.typ it ty);
      pat = (fun it pat ->
        check_type pat.pat_loc pat.pat_type;
        default.pat it pat);
      expr = (fun it exp ->
        check_type exp.exp_loc exp.exp_type;
        default.expr it exp) }
  in
  it.expr it exp

let check_uses self exp =
  let params, body = parameters exp in
  check_predicates self exp;
  let default = Tast_iterator.default_iterator in
  let rec iterator allow_calls =
    { default with
      module_expr = (fun it m ->
        match m.mod_desc with
        | Tmod_functor _ -> default.module_expr (iterator false) m
        | _ -> default.module_expr it m);
      class_expr = (fun it cl ->
        match cl.cl_desc with
        | Tcl_fun _ | Tcl_structure _ ->
            default.class_expr (iterator false) cl
        | _ -> default.class_expr it cl);
      binding_op = (fun it op ->
        if Path.same op.bop_op_path (Path.Pident self) then
          reject op.bop_loc
            "the recursive function cannot be used as a binding operator";
        default.binding_op it op);
      expr = (fun it exp ->
        match exp.exp_desc with
        | Texp_apply
            ({exp_desc = Texp_ident {path = Path.Pident id; _}; _} as fn,
             args, _, _, _, _) when Ident.same id self ->
            if not allow_calls then
              reject fn.exp_loc
                "the recursive function occurs in a delayed body";
            if List.length args <> List.length params then
              reject exp.exp_loc
                "recursive calls must supply every value parameter";
            List.iter (function
              | Types.Nolabel, Arg (arg, _) -> it.expr it arg
              | _ -> reject exp.exp_loc
                  "recursive calls must be unlabelled and fully applied") args
        | Texp_ident {path = Path.Pident id; _} when Ident.same id self ->
            reject exp.exp_loc "the recursive function must be called directly"
        | Texp_function _ | Texp_lazy _ | Texp_quote _ | Texp_letop _ ->
            default.expr (iterator false) exp
        | _ -> default.expr it exp)
    }
  in
  let it = iterator true in
  it.expr it body
