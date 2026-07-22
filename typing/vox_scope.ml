open Types
open Btype

type scope_error =
  | Value of Ident.t
  | Module of Ident.t

let validate_scopes_internal ?(program_scope = Ident.Set.empty) ?stable_scope
    ?module_in_scope type_ =
  let visited = ref [] in
  let seen type_ bound =
    List.exists
      (fun (other, other_bound) ->
        eq_type type_ other && Ident.Set.equal bound other_bound)
      !visited
  in
  let rec visit bound type_ =
    if seen type_ bound then Ok ()
    else begin
      visited := (type_, bound) :: !visited;
      match get_desc type_ with
      | Tarrow ((_, _, _, binder), domain, codomain, _) ->
        begin match visit bound domain with
        | Error _ as error -> error
        | Ok () ->
          begin match binder with
          | Some binder when Ident.Set.mem binder bound -> Error (Value binder)
          | None -> visit bound codomain
          | Some binder -> visit (Ident.Set.add binder bound) codomain
          end
        end
      | Trefine refinement ->
        let predicate_bound =
          Ident.Set.add refinement.ref_view.rb_id
            (Ident.Set.union bound program_scope)
        in
        let escaped =
          Ident.Set.diff
            (Refinement.free_bound_identifiers refinement.ref_pred)
            predicate_bound
        in
        let escaped_stable =
          match stable_scope with
          | None -> None
          | Some stable_scope ->
            let escaped = ref None in
            let value_path path =
              begin match path with
              | Path.Pident id
                when not (Ident.is_global_or_predef id)
                     && not (Ident.Set.mem id stable_scope) ->
                if Option.is_none !escaped then escaped := Some id
              | Path.Pident _ | Path.Pdot _ | Path.Papply _
              | Path.Pextra_ty _ -> ()
              end;
              path
            in
            ignore
              (Refinement.map_paths ~value_path ~type_path:Fun.id
                 refinement.ref_pred);
            !escaped
        in
        let escaped_module =
          match module_in_scope with
          | None -> None
          | Some module_in_scope ->
            let escaped = ref None in
            let value_path path =
              begin match path with
              | Path.Pident _ -> ()
              | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ ->
                List.iter
                  (fun id ->
                    if Option.is_none !escaped && not (module_in_scope id)
                    then escaped := Some id)
                  (Path.heads path)
              end;
              path
            in
            ignore
              (Refinement.map_paths ~value_path ~type_path:Fun.id
                 refinement.ref_pred);
            !escaped
        in
        begin
          match
            Ident.Set.choose_opt escaped, escaped_stable, escaped_module
          with
          | Some binder, _, _ -> Error (Value binder)
          | None, Some value, _ -> Error (Value value)
          | None, None, Some module_ -> Error (Module module_)
          | None, None, None ->
          let result = ref (Ok ()) in
          let visit_if_open nested =
            match !result with
            | Error _ -> ()
            | Ok () -> result := visit bound nested
          in
          visit_if_open refinement.ref_skeleton;
          visit_if_open refinement.ref_view.rb_type;
          Refinement.iter_types visit_if_open refinement.ref_pred;
          !result
        end
      | _ ->
        let result = ref (Ok ()) in
        iter_type_expr
          (fun nested ->
            match !result with
            | Error _ -> ()
            | Ok () -> result := visit bound nested)
          type_;
        !result
    end
  in
  visit Ident.Set.empty type_

let validate_scopes ?program_scope ?stable_scope type_ =
  match validate_scopes_internal ?program_scope ?stable_scope type_ with
  | Ok () -> Ok ()
  | Error (Value id | Module id) -> Error id

let validate_scopes_with_modules ~module_in_scope ?program_scope ?stable_scope
    type_ =
  validate_scopes_internal ~module_in_scope ?program_scope ?stable_scope type_

let validate_signature signature =
  let rec validate enclosing_scope enclosing_modules signature =
    let stable_scope =
      List.fold_left
        (fun stable_scope -> function
          | Sig_value (id, _, Exported) -> Ident.Set.add id stable_scope
          | Sig_value (_, _, Hidden)
          | Sig_type _ | Sig_typext _ | Sig_module _ | Sig_modtype _
          | Sig_class _ | Sig_class_type _ | Sig_jkind _ -> stable_scope)
        enclosing_scope signature
    in
    let module_scope =
      List.fold_left
        (fun module_scope -> function
          | Sig_module (id, _, _, _, Exported) ->
            Ident.Set.add id module_scope
          | Sig_module (_, _, _, _, Hidden)
          | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_modtype _
          | Sig_class _ | Sig_class_type _ | Sig_jkind _ -> module_scope)
        enclosing_modules signature
    in
    let result = ref (Ok ()) in
    let super = type_iterators_without_type_expr in
    let update_result check =
      match !result with
      | Error _ -> ()
      | Ok () -> result := check ()
    in
    let rec iterator_for module_scope =
      { super with
        it_signature =
          (fun _ nested_signature ->
            update_result (fun () ->
              validate stable_scope module_scope nested_signature));
        it_module_type =
          (fun _ module_type -> visit_module_type module_scope module_type);
        it_type_expr =
          (fun _ type_ ->
            let module_in_scope id =
              Ident.is_global_or_predef id
              || Ident.Set.mem id module_scope
            in
            update_result (fun () ->
              match
                validate_scopes_internal ~stable_scope ~module_in_scope type_
              with
              | Ok () -> Ok ()
              | Error (Value id | Module id) -> Error id));
      }
    and visit_module_type module_scope = function
      | Mty_functor (Named (id, parameter, _), result_type, _) ->
        visit_module_type module_scope parameter;
        let result_scope =
          Option.fold ~none:module_scope
            ~some:(fun id -> Ident.Set.add id module_scope)
            id
        in
        visit_module_type result_scope result_type
      | Mty_functor (Unit, result_type, _) ->
        visit_module_type module_scope result_type
      | module_type ->
        let iterator = iterator_for module_scope in
        super.it_module_type iterator module_type
    in
    let iterator = iterator_for module_scope in
    List.iter (iterator.it_signature_item iterator) signature;
    !result
  in
  validate Ident.Set.empty Ident.Set.empty signature
