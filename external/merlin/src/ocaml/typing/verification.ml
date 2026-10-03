(* Refinement-subsumption sites, recorded by the type checker and discharged
   by the verifier.  A site is found by the physical identity of its
   [Tmod_constraint] or [Tmod_apply] descriptor, which copies of the module
   expression share.  Sites that the verifier does not reach are discharged
   at the end with no facts. *)
type site_entry =
  { desc : Typedtree.module_expr_desc;
    site : Typedtree.refinement_site;
    mutable consumed : bool }

let sites = ref []

let reset_refinement_sites () = sites := []

let attach_refinement_site desc site =
  sites := { desc; site; consumed = false } :: !sites

let find_refinement_site desc =
  List.find_map
    (fun entry ->
      if entry.desc == desc && not entry.consumed
      then begin
        entry.consumed <- true;
        Some entry.site
      end
      else None)
    !sites

let unconsumed_refinement_sites () =
  List.rev
    (List.filter_map
       (fun entry ->
         if entry.consumed then None
         else begin
           entry.consumed <- true;
           Some entry.site
         end)
       !sites)

let has_refinement_sites () = !sites <> []

let refinement_site_unavailable (site : Typedtree.refinement_site) =
  match site.rs_obligations with
  | [] -> ()
  | o :: _ ->
      Location.raise_errorf ~loc:o.ro_value_loc
        "Refinement verification is unavailable in this compiler"

let unavailable ~interface structure =
  Option.iter refinement_site_unavailable interface;
  List.iter (fun entry -> refinement_site_unavailable entry.site) !sites;
  let iterator =
    { Tast_iterator.default_iterator with
      expr = (fun self expression ->
        List.iter (function
          | Typedtree.Texp_refine, loc, _ ->
            Location.raise_errorf ~loc
              "Refinement verification is unavailable in this compiler"
          | Typedtree.Texp_subsumption _, loc, _ ->
              Location.raise_errorf ~loc
                "Refinement verification is unavailable in this compiler"
          | Typedtree.Texp_refinement { target; _ }, loc, _ ->
            (match Types.get_desc
                (Ctype.expand_head expression.Typedtree.exp_env target) with
             | Types.Trefine _ ->
                 Location.raise_errorf ~loc
                   "Refinement verification is unavailable in this compiler"
             | _ -> ())
          | _ -> ()) expression.Typedtree.exp_extra;
        Tast_iterator.default_iterator.expr self expression)
    }
  in
  iterator.structure iterator structure

let verifier =
  ref (fun ~whole_unit:_ ~interface structure ->
      unavailable ~interface structure)
let install verify = verifier := verify

let with_sites f =
  Fun.protect ~finally:reset_refinement_sites f

let run structure =
  with_sites (fun () -> !verifier ~whole_unit:false ~interface:None structure)

let run_unit ?interface structure =
  with_sites (fun () -> !verifier ~whole_unit:true ~interface structure)

let termination = ref (fun ~self:_ ~fn:_ ~measure ->
  Location.raise_errorf ~loc:measure.Typedtree.exp_loc
    "Numerical termination verification is unavailable in this compiler")

let install_termination check = termination := check
let check_termination ~self ~fn ~measure = !termination ~self ~fn ~measure
