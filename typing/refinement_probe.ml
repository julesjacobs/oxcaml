(* See refinement_probe.mli. *)

open Typedtree

let refined_head env ty =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Types.Trefine _ -> true
  | _ -> false

(* Pattern-bound variables are reported against the environment in which the
   binding is live: the body of the [let] that binds them, the right-hand
   side of their [match]/[function] case, or — for module-level bindings —
   the structure's final environment. *)
let report_pattern_bindings ppf env pat =
  List.iter
    (fun (id, (name : string Location.loc), _, _, _) ->
       match Env.find_value (Path.Pident id) env with
       | { val_type; _ } ->
           let val_type = Subst.Lazy.force_type_expr val_type in
           if refined_head env val_type then
             Format.fprintf ppf
               "@[<2>%a:@ refined environment entry: %s :@ %a@]@."
               Location.print_loc name.loc (Ident.name id)
               Printtyp.type_expr val_type
       | exception Not_found -> ())
    (pat_bound_idents_full pat)

let iterator ppf =
  let expr sub (e : expression) =
    if refined_head e.exp_env e.exp_type then
      Format.fprintf ppf "@[<2>%a:@ refined head on expression:@ %a@]@."
        Location.print_loc e.exp_loc Printtyp.type_expr e.exp_type;
    (* Every recorded obligation is reported — one line per marker, so a
       double record at a site shows up as a duplicated line. *)
    List.iter
      (fun (extra, loc, _) ->
         match extra with
         | Texp_refinement_obligation ty ->
             Format.fprintf ppf "@[<2>%a:@ refinement obligation:@ %a@]@."
               Location.print_loc loc Printtyp.type_expr ty
         | _ -> ())
      e.exp_extra;
    (match e.exp_desc with
     | Texp_let (_, vbs, body) ->
         List.iter
           (fun vb -> report_pattern_bindings ppf body.exp_env vb.vb_pat)
           vbs
     | Texp_letmutable (vb, body) ->
         report_pattern_bindings ppf body.exp_env vb.vb_pat
     | _ -> ());
    Tast_iterator.default_iterator.expr sub e
  in
  let case : type k . Tast_iterator.iterator -> k case -> unit =
    fun sub c ->
      report_pattern_bindings ppf c.c_rhs.exp_env c.c_lhs;
      Tast_iterator.default_iterator.case sub c
  in
  let structure sub (str : structure) =
    List.iter
      (fun item ->
         match item.str_desc with
         | Tstr_value (_, vbs) ->
             List.iter
               (fun vb ->
                  report_pattern_bindings ppf str.str_final_env vb.vb_pat)
               vbs
         | _ -> ())
      str.str_items;
    Tast_iterator.default_iterator.structure sub str
  in
  { Tast_iterator.default_iterator with expr; case; structure }

let implementation ppf str =
  let iterator = iterator ppf in
  iterator.structure iterator str
