(* TEST
 include ocamlcommon;
 expect;
*)

let () =
  let open Types in
  Language_extension.enable Refinement_types ();
  let source = {|
    external eq : int -> int -> bool @@ total = "%equal"
    external g : (n:int) -> {r:int | eq r n} @@ total = "g"
    type t = {x:int | let n=x in let refine_ y=g n in eq y n}
  |} in
  let _, signature, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial)
      (Parse.implementation (Lexing.from_string source))
  in
  let ty =
    List.find_map (function
      | Sig_type (id, td, _, _) when Ident.name id = "t" -> td.type_manifest
      | _ -> None) signature |> Option.get
  in
  let check label ty =
    match get_desc ty with
    | Trefine {ref_pred={rexp_desc=Rexp_let
        (n, {rexp_desc=Rexp_let (y, _); _}); _}; _} ->
      begin match get_desc y.rb_expr.rexp_type with
      | Trefine r ->
        Format.printf "%s: %b@." label
          (Option.is_some (Refinement_predicate.find_ident
             (Ident.Set.singleton n.rb_ident) r.ref_pred))
      | _ -> assert false
      end
    | _ -> assert false
  in
  check "original" ty;
  let copy = Subst.type_expr Subst.identity ty in
  check "copy" copy;
  check "copy of copy" (Subst.type_expr Subst.identity copy);
  check "saved" (Subst.type_expr
    (Subst.with_additional_action Prepare_for_saving Subst.identity) ty)
;;
[%%expect{|
File "_none_", line 3, characters 4-59:
Warning 228 [trusted-external]: The verifier assumes this external's refinement;
  nothing checks it.
original: true
copy: true
copy of copy: true
saved: true
|}]

let () =
  let module _ = struct
    open Types

    let check result message = if not result then failwith message

    let () = Language_extension.enable Refinement_types ()

    let _, signature, _, _, _, env =
      Typemod.type_structure (Lazy.force Env.initial)
        (Parse.implementation (Lexing.from_string {|
          external eq : int -> int -> bool @@ total = "%equal"
          external add : int -> int -> int @@ total = "%addint"
          let ambient = 0
          type left =
            (x:int) -> (y:{v:int | eq v x}) ->
            (z:{v:int | eq v y}) -> {r:int | eq r (add x (add y z))}
          type right =
            (a:int) -> (b:{v:int | eq v a}) ->
            (c:{v:int | eq v b}) -> {s:int | eq s (add a (add b c))}
          type capture_left =
            (x:int) -> (y:int) -> {r:int | eq r (add x (add y ambient))}
          type nested_left =
            (x:int) -> (y:((n:int) -> {r:int | eq r (add n x)})) ->
            (z:int) -> {r:int | eq r (add x z)}
          type nested_right =
            (a:int) -> (b:((m:int) -> {s:int | eq s (add m a)})) ->
            (c:int) -> {s:int | eq s (add a c)}
        |}))

    let ty name =
      List.find_map (function
        | Sig_type (id, declaration, _, _) when Ident.name id = name ->
            declaration.type_manifest
        | _ -> None) signature |> Option.get

    let arrow ty = match get_desc ty with
      | Tarrow ((_, _, _, Some binder), argument, codomain, _) ->
          binder, argument, codomain
      | _ -> failwith "Dependent arrow required"

    let align left right =
      let source, _, left = arrow left in
      let target, _, right = arrow right in
      Subst.align_arrow_codomains source left target right, right

    let rec matching_binders left right =
      match get_desc left, get_desc right with
      | Tarrow ((_, _, _, Some left_id), _, left_ret, _),
        Tarrow ((_, _, _, Some right_id), _, right_ret, _) ->
          check (Ident.same left_id right_id) "Unaligned internal binder";
          1 + matching_binders left_ret right_ret
      | _ -> 0

    let () =
      let left = ty "left" and right = ty "right" in
      let _, _, original = arrow left in
      let original_id, _, _ = arrow original in
      let result, target = align left right in
      check (matching_binders result target = 2) "Incomplete batch";
      Ctype.equal env false [result] [target];
      let after_id, _, _ = arrow original in
      check (Ident.same original_id after_id) "Source binder mutated";
      let copied_id, _, _ = arrow result in
      check (not (Ident.same original_id copied_id)) "Source binder retained";
      let ordinary = Subst.type_expr Subst.identity result in
      let ordinary_id, _, _ = arrow ordinary in
      check (not (Ident.same copied_id ordinary_id))
        "Ordinary substitution stopped freshening";
      List.iter (fun compare ->
        compare (Subst.type_expr Subst.identity left)
          (Subst.type_expr Subst.identity right))
        [ (fun left right -> Ctype.equal env false [left] [right]);
          Ctype.unify env;
          (fun left right -> ignore (Ctype.moregeneral env false [] [] left right));
          (fun left right -> Ctype.subtype env left right ()) ];
      Format.printf "Batched dependent arrows preserve all type relations@."

    let () =
      let left = ty "nested_left" and right = ty "nested_right" in
      let _, _, original = arrow left in
      let argument ty = match get_desc ty with
        | Tarrow (_, argument, _, _) -> Btype.tpoly_get_mono argument
        | _ -> failwith "Function argument required" in
      let old_argument_binder, _, _ = arrow (argument original) in
      let result, target = align left right in
      let argument_binder, _, _ = arrow (argument result) in
      check (not (Ident.same old_argument_binder argument_binder))
        "Unselected argument binder was retained";
      Ctype.equal env false [result] [target];
      Format.printf "Nested argument binders retain ordinary freshening@."

    let () =
      let ambient = List.find_map (function
        | Sig_value (id, _, _) when Ident.name id = "ambient" -> Some id
        | _ -> None) signature |> Option.get in
      let source, _, source_body = arrow (ty "capture_left") in
      let ambient_low =
        Ident.create_scoped ~scope:Ident.lowest_scope "ambient" in
      let source_body = Subst.type_expr
          (Subst.add_bound_value ambient ambient_low Subst.identity)
          source_body in
      let ambient = ambient_low in
      check (Ident.scope ambient = Ident.lowest_scope)
        "Capture collision must pass the collection scope guard";
      let target, _, target_body = arrow (ty "right") in
      let target_body = match get_desc target_body with
        | Tarrow ((label, argument_mode, result_mode, _), argument, result, comm) ->
            Btype.newgenty (Tarrow
              ((label, argument_mode, result_mode, Some ambient),
               argument, result, comm))
        | _ -> failwith "Capture target arrow required" in
      let copied = Subst.align_arrow_codomains source source_body
          target target_body in
      let copied_binder, _, copied_body = arrow copied in
      check (not (Ident.same copied_binder ambient)) "Captured ambient value";
      let expected = Subst.type_expr
          (Subst.add_bound_value source target Subst.identity) source_body in
      Ctype.equal env false [copied] [expected];
      (match get_desc copied_body with
      | Trefine { ref_pred; _ } ->
          check (Option.is_some (Refinement_predicate.find_ident
            (Ident.Set.singleton ambient) ref_pred)) "Ambient reference lost"
      | _ -> failwith "Capture predicate required");
      Format.printf "Capture collision falls back to ordinary substitution@."

    let () =
      let source, _, source_body = arrow (ty "left") in
      let target, _, target_body = arrow (ty "right") in
      let source_binder, source_argument, _ = arrow source_body in
      let target_binder, target_argument, _ = arrow target_body in
      let make binder argument =
        let recursive = Btype.newgenty (Ttuple []) in
        let label, _, _, comm = match get_desc source_body with
          | Tarrow (label, argument, result, comm) -> label, argument, result, comm
          | _ -> failwith "Recursive arrow template required" in
        let label, argument_mode, result_mode, _ = label in
        Types.set_type_desc recursive (Tarrow
          ((label, argument_mode, result_mode, Some binder),
           argument, recursive, comm));
        recursive in
      let left = make source_binder source_argument in
      let right = make target_binder target_argument in
      let copied = Subst.align_arrow_codomains source left target right in
      let binder, _, body = arrow copied in
      check (Ident.same binder target_binder) "Recursive binder not aligned";
      check (get_id copied = get_id body) "Recursive sharing lost";
      let _, _, body = arrow left in
      check (get_id left = get_id body) "Recursive source changed";
      Format.printf "Recursive type sharing is preserved@."
  end in
  ()
;;
[%%expect{|
Batched dependent arrows preserve all type relations
Nested argument binders retain ordinary freshening
Capture collision falls back to ordinary substitution
Recursive type sharing is preserved
|}]
