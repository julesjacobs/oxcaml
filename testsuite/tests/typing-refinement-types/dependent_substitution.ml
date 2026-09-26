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
original: true
copy: true
copy of copy: true
saved: true
|}]
