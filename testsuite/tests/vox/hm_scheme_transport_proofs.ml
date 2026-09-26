open Copy_spec
module D = Hm_declarative
module T = Hm_type_proofs
module A = Hm_abstraction
module F = Hm_freshness_proofs
module P = Hm_template_instance_proofs
module S = Hm_substitution
module E = Hm_environment_spec
module M = Level_mgu_spec

let rec (embed_substitute @ total) :
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (t : ty) @ immutable ->
    {u : unit | S.substitute_type delta (D.embed t) === D.embed (M.substitute delta t)} @ ghost = fun delta t -> ghost_ (
    D.embed_def t; let mono = D.embed t in S.substitute_type_def delta mono;
    M.substitute_def delta t; let next = M.substitute delta t in D.embed_def next;
    (match t with Variable _ | Boolean | Word64 -> () | List_type a -> embed_substitute delta a; () | Function (a, b) -> embed_substitute delta a; embed_substitute delta b; ());
    ())

let rec (substitute_body @ total) : (names : A.names) @ immutable ->
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (E.boundary_member schema p) || M.substitute delta (rho p) === tau p})) @ total ->
    {u : unit | P.parameters_in names schema} ->
    {u : unit | S.substitute_type delta (P.body names rho schema) === P.body names tau schema} @ ghost =
  fun names delta rho tau schema equal premise -> ghost_ (
    P.parameters_in_def names schema;
    P.body_def names rho schema; P.body_def names tau schema;
    let mono = P.body names rho schema in S.substitute_type_def delta mono;
    match schema with
    | Boundary p -> equal p; E.boundary_member_def schema p; let t = rho p in embed_substitute delta t; ()
    | Parameter p -> let z = D.Z in A.abstract_free_def names z p;
      (match A.position names p with None -> () | Some i ->
        D.add_def z i; let parameter = D.Parameter i in S.substitute_type_def delta parameter; ())
    | Constant _ | Word_constant _ -> ()
    | Indirect (_, child) | List_template (_, child) ->
      let next : ((p : node Pref.t) @ immutable ->
        {u : unit | not (E.boundary_member child p) || M.substitute delta (rho p) === tau p}) @ total = fun p ->
        equal p; E.boundary_member_def schema p; () in
      substitute_body names delta rho tau child next (); ()
    | Product (_, a, b) ->
      let left : ((p : node Pref.t) @ immutable ->
        {u : unit | not (E.boundary_member a p) || M.substitute delta (rho p) === tau p}) @ total = fun p ->
        equal p; E.boundary_member_def schema p; () in
      let right : ((p : node Pref.t) @ immutable ->
        {u : unit | not (E.boundary_member b p) || M.substitute delta (rho p) === tau p}) @ total = fun p ->
        equal p; E.boundary_member_def schema p; () in
      substitute_body names delta rho tau a left ();
      substitute_body names delta rho tau b right (); ())

let (substitute_scheme @ total) :
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (E.boundary_member schema p) || M.substitute delta (rho p) === tau p})) @ total ->
    {u : unit | S.substitute_scheme delta (P.scheme rho schema) === P.scheme tau schema} @ ghost =
  fun delta rho tau schema equal -> ghost_ (
    let names = F.template_names schema in
    let included : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None || not (A.position names p === None)}) @ total =
      fun _p -> () in
    P.parameters_subset schema names included; substitute_body names delta rho tau schema equal ();
    P.scheme_def rho schema; P.scheme_def tau schema; let sigma = P.scheme rho schema in
    S.substitute_scheme_def delta sigma; ())

let rec (substitute_context @ total) :
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schemas : E.templates) @ immutable ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (E.environment_boundary schemas p) || M.substitute delta (rho p) === tau p})) @ total ->
    {u : unit | S.substitute_context delta (P.context rho schemas) === P.context tau schemas} @ ghost =
  fun delta rho tau schemas equal -> ghost_ (
    P.context_def rho schemas; P.context_def tau schemas;
    let g = P.context rho schemas in S.substitute_context_def delta g;
    match schemas with E.No_templates -> () | E.Template_binding (schema, rest) ->
    let first : ((p : node Pref.t) @ immutable ->
      {u : unit | not (E.boundary_member schema p) || M.substitute delta (rho p) === tau p}) @ total = fun p ->
      equal p; E.environment_boundary_def schemas p; () in
    let tail : ((p : node Pref.t) @ immutable ->
      {u : unit | not (E.environment_boundary rest p) || M.substitute delta (rho p) === tau p}) @ total = fun p ->
      equal p; E.environment_boundary_def schemas p; () in
    substitute_scheme delta rho tau schema first; substitute_context delta rho tau rest tail; ())
