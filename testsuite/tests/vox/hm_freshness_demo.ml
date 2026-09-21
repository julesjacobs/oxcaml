(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml hm_declarative.ml hm_type_proofs.ml hm_environment_spec.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_freshness_proofs.ml hm_freshness_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_finite_spec
open Hm_freshness_proofs
module E = Hm_environment_spec

let (mixed_boundary @ total) : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (g : node Pref.t) @ immutable ->
    {u : unit | not (p === q) && not (p === r) && not (p === g)
      && not (q === r) && not (q === g) && not (r === g)} -> {u : unit | true} @ ghost =
  fun p q r g premise -> ghost_ (
    let var : desc = Var in let arrow = Arrow (p, q) in
    let low = cell var 0 in let high = cell var 1 in let product = cell arrow 1 in
    let generic = {low with level = Generic} in cell_def var 0; cell_def var 1; cell_def arrow 1;
    let h0 = H.empty () in let h1 = H.put h0 p low in let h2 = H.put h1 q high in
    let h3 = H.put h2 r product in let h = H.put h3 g generic in
    let a = Free p in let b = Free q in let rhs = Branch (r, a, b) in
    tree_root_def a; tree_root_def b; tree_root_def rhs;
    finite_def h a; finite_def h b; finite_def h rhs;
    Level_unifier_spec.observe_def h p; Level_unifier_spec.observe_def h q; Level_unifier_spec.observe_def h r;
    below_def h r 1; at_level_def h r;
    let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total = fun x ->
      ordered_def h x; children_below_def h var 0; children_below_def h var 1; children_below_def h arrow 1;
      below_def h p 1; below_def h q 1; at_level_def h p; at_level_def h q;
      let u = () in u in
    let trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (not (H.mem h x) || finite h t)} @ immutable) @ total = fun x ->
      if x === r then rhs else (
        let t = Free x in tree_root_def t; finite_def h t; Level_unifier_spec.observe_def h x; t) in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let t = trees x in readback t in
    let values : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in rho x === readback t}) @ total = fun x ->
      rho_def x; let t = trees x in
      readback_def t; readback_def a; readback_def b;
      let u = () in u in
    let no = E.No_templates in let parameter = Parameter g in let boundary = Boundary p in
    let ts1 = E.Template_binding (parameter, no) in let ts = E.Template_binding (boundary, ts1) in
    let empty : E.env = E.Empty in let env1 = E.Bind (g, empty) in let env = E.Bind (p, env1) in
    E.env_at_def h 0 env ts; E.env_at_def h 0 env1 ts1; E.env_at_def h 0 empty no;
    root_def parameter; root_def boundary; template_def h parameter; template_def h boundary;
    generic_desc_def h g var; finite_node_def h p; at_level_def h p; below_def h p 0;
    E.boundary_bound_def h 0 parameter; E.boundary_bound_def h 0 boundary;
    let u = () in environment_avoids h 0 1 order trees rho values rhs env ts (u);
    generalized_names_def h 0 rhs; generalized_names_def h 0 a; generalized_names_def h 0 b;
    below_def h q 0; at_level_def h q;
    let names = generalized_names h 0 rhs in
    let no_names = A.No_names in let one_name = A.Name (q, no_names) in join_def no_names one_name;
    let _selected : {u : unit | names === one_name} = u in u)
