open Types
open Btype

let logical_type type_ =
  let visited = TypeHash.create 17 in
  let rec copy type_ =
    match TypeHash.find_opt visited type_ with
    | Some copy -> copy
    | None ->
      begin match get_desc type_ with
      | Trefine { ref_skeleton; _ } ->
        let stub =
          newgenstub ~scope:(get_scope type_)
            (Jkind.Builtin.any ~why:Dummy_jkind)
        in
        TypeHash.add visited type_ stub;
        Transient_expr.set_stub_desc stub (Tlink (copy ref_skeleton));
        stub
      | Tarrow (label, argument, result, commu) ->
        let stub =
          newgenstub ~scope:(get_scope type_)
            (Jkind.Builtin.any ~why:Dummy_jkind)
        in
        TypeHash.add visited type_ stub;
        Transient_expr.set_stub_desc stub
          (Tarrow (label, copy argument, copy result, commu));
        stub
      | Tpoly (body, variables) ->
        let stub =
          newgenstub ~scope:(get_scope type_)
            (Jkind.Builtin.any ~why:Dummy_jkind)
        in
        TypeHash.add visited type_ stub;
        Transient_expr.set_stub_desc stub (Tpoly (copy body, variables));
        stub
      | _ -> type_
      end
  in
  copy type_

let mentions binder type_ =
  with_type_mark (fun mark ->
    let found = ref false in
    let rec visit type_ =
      if not !found && try_mark_node mark type_ then begin
        begin match get_desc type_ with
        | Trefine refinement ->
          if Ident.Set.mem binder
               (Refinement.free_bound_identifiers refinement.ref_pred)
          then found := true
        | _ -> ()
        end;
        if not !found then iter_type_expr visit type_
      end
    in
    visit type_;
    !found)

let mentions_identifier identifier type_ =
  with_type_mark (fun mark ->
    let found = ref false in
    let rec visit type_ =
      if not !found && try_mark_node mark type_ then begin
        begin match get_desc type_ with
        | Trefine refinement ->
          if Ident.Set.mem identifier
               (Refinement.free_bound_identifiers refinement.ref_pred)
          then found := true
          else
            let value_path path =
              begin match path with
              | Path.Pident occurrence
                when Ident.same identifier occurrence ->
                found := true
              | _ -> ()
              end;
              path
            in
            ignore
              (Refinement.map_paths
                 ~value_path ~type_path:Fun.id refinement.ref_pred)
        | _ -> ()
        end;
        if not !found then iter_type_expr visit type_
      end
    in
    visit type_;
    !found)

let instantiate ~binder ~with_ type_ =
  Subst.type_expr
    (Subst.add_refinement_bound binder with_ Subst.identity)
    type_

let rename ~binder ~as_ codomain =
  Subst.type_expr
    (Subst.add_refinement_bound_renaming binder as_ Subst.identity)
    codomain

let validate_scopes type_ = Vox_scope.validate_scopes type_
let validate_signature = Vox_scope.validate_signature
