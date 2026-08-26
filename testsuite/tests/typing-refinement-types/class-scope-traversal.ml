(* TEST
 include ocamlcommon;
 expect;
*)

open Types

let () =
  let escaped = Ident.create_local "escaped" in
  let binder = Ident.create_local "x" in
  let safe = Predef.type_int in
  let predicate =
    { rexp_desc = Rexp_var escaped;
      rexp_type = Predef.type_bool; rexp_loc = Location.none }
  in
  let refine binder = Ctype.newty (Trefine
    {ref_binder = binder; ref_payload = safe; ref_pred = predicate})
  in
  let sign =
    { csig_self = safe; csig_self_row = safe;
      csig_vars = Vars.empty; csig_meths = Meths.empty }
  in
  let empty = Cty_signature sign in
  let path = Path.Pident escaped in
  let contexts ty =
    [Cty_constr (path, [ty], empty);
     Cty_constr (path, [], Cty_arrow (Nolabel, ty, empty));
     Cty_signature {sign with csig_self = ty};
     Cty_signature {sign with csig_self_row = ty};
     Cty_signature {sign with csig_vars = Vars.singleton "v"
       (Asttypes.Immutable, Asttypes.Virtual, ty)};
     Cty_signature {sign with csig_meths = Meths.singleton "m"
       (Mpublic, Asttypes.Virtual, ty)};
     Cty_arrow (Nolabel, ty, empty);
     Cty_arrow (Nolabel, safe, Cty_signature {sign with csig_self = ty})]
  in
  let find = Ctype.refinement_scope_escape_class_type
    (Ident.Set.singleton escaped)
  in
  List.iter (fun cty -> assert (Option.is_some (find cty)))
    (contexts (refine binder));
  List.iter (fun cty -> assert (find cty = None)) (contexts safe);
  List.iter (fun cty -> assert (find cty = None)) (contexts (refine escaped));;
[%%expect{|
|}]
