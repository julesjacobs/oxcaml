module D = Hm_declarative
module T = Hmc_templates
module A = Hmc_ground_arguments
module G = Hmc_ground_type

type descriptor = {key : A.key; definition : T.definition; earlier : T.catalog; ty : G.t}

let[@def] (valid @ total) (d : descriptor @ immutable) = ghost_ (
  T.valid d.earlier && T.definition_valid d.earlier d.definition
  && d.key.A.owner === T.rank d.earlier
  && A.length d.key.A.arguments === D.arity d.definition.T.scheme
  && G.mono d.ty === D.open_scheme d.definition.T.scheme (A.declarative d.key.A.arguments))

type instance = {d : descriptor | valid d}
type result = Unknown_template | Wrong_arity | Instance of instance [@@inductive]

let (scheme_valid @ total) : (earlier : T.catalog) @ immutable -> (d : T.definition) @ immutable ->
    {u : unit | T.definition_valid earlier d} ->
    {u : unit | D.scheme_wf D.Z d.T.scheme && Hmc_no_free.scheme d.T.scheme} @ ghost =
  fun earlier d premise -> ghost_ (
    T.definition_valid_def earlier d; D.scheme_wf_def D.Z d.T.scheme;
    match d.T.scheme with D.Forall (k, ty) ->
      D.typed_def (D.add k D.Z) (D.weaken_context k (T.context earlier)) d.T.source ty d.T.derivation; ())

let (request @ total) : (catalog : {c : T.catalog | T.valid c}) @ immutable ->
    (index : D.index) @ immutable -> (args : A.t) @ immutable ->
    {r : result | match r with
      | Unknown_template -> D.lookup (T.context catalog) index === None
      | Wrong_arity -> (match D.lookup (T.context catalog) index with None -> false
        | Some s -> not (A.length args === D.arity s))
      | Instance d -> T.selection catalog index === Some {T.definition = d.definition; earlier = d.earlier}
        && d.key.A.arguments === args
        && D.lookup (T.context catalog) index === Some d.definition.T.scheme
        && D.present (T.rank catalog) d.key.A.owner} @ immutable = fun catalog index args ->
  match T.select catalog index () with
  | None -> Unknown_template
  | Some selected ->
    let definition = selected.T.definition in let earlier = selected.T.earlier in
    if Hm_elaboration_check.index_equal (A.length args) (D.arity definition.T.scheme) then (
      ghost_ (scheme_valid earlier definition ());
      let scheme : {s : D.scheme | D.scheme_wf D.Z s && Hmc_no_free.scheme s} = refine_ definition.T.scheme in
      let actuals : {a : A.t | A.length a === D.arity scheme} = refine_ args in
      let ty = A.instantiate scheme actuals in
      let key = {A.owner = T.rank earlier; arguments = args} in
      let out = {key; definition; earlier; ty} in
      ghost_ (valid_def out);
      let out : instance = refine_ out in Instance out)
    else Wrong_arity
