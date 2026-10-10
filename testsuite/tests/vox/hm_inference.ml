module D = Hm_declarative
module M = Hm_inference_model
module V = Verified_hm

type payload = {input : D.term @@ ghost; ty : Copy_spec.ty option; evidence : V.Spec.evidence @@ ghost}
type result = {r : payload | V.Spec.inferred r.input r.ty r.evidence &&
  match r.ty with None -> true | Some ty -> V.Spec.has_type r.input ty r.evidence}

let[@def] (source @ total) (out : result @ immutable) = ghost_ out.input
let[@def] (inferred_type @ total) (out : result @ immutable) = out.ty

let infer : (input : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {out : result | source out === input} @ immutable = fun input ->
  let answer = V.infer_type input in
  let ty = V.read_type (borrow_ answer) in
  let out : result = {input = ghost_ input; ty; evidence = ghost_ answer.#evidence} in
  ghost_ (source_def out);
  out

let (sound @ total) : (out : result) @ immutable -> (ty : Copy_spec.ty) @ immutable ->
    {u : unit | inferred_type out === Some ty} ->
    {d : D.typing | D.typed D.Z D.Empty_context (source out) (D.embed ty) d} @ immutable ghost =
  fun out ty premise -> ghost_ (
    source_def out; inferred_type_def out; V.Spec.typing out.input ty out.evidence ())

let rec (substitution_agrees @ total) :
    (delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (ty : Copy_spec.ty) @ immutable ->
    {u : unit | M.substitute delta ty === Level_mgu_spec.substitute delta ty} @ ghost =
  fun delta ty -> ghost_ (
    M.substitute_def delta ty; Level_mgu_spec.substitute_def delta ty;
    match ty with
    | Copy_spec.List_type a -> substitution_agrees delta a
    | Copy_spec.Function (a, b) -> substitution_agrees delta a; substitution_agrees delta b
    | Copy_spec.Variable _ | Copy_spec.Boolean | Copy_spec.Word64 -> ())

let (principal @ total) : (out : result) @ immutable ->
    (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | D.typed D.Z D.Empty_context (source out) (D.embed target) typing} ->
    (claim : bool) ->
    (use : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match inferred_type out with None -> false | Some ty -> target === M.substitute delta ty} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun out target typing premise claim use -> ghost_ (
    source_def out; inferred_type_def out;
    let accept : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match out.ty with None -> false | Some ty -> target === Level_mgu_spec.substitute delta ty} ->
      {u : unit | claim}) @ total = fun delta _factor ->
        (match out.ty with None -> () | Some ty -> substitution_agrees delta ty);
        use delta () in
    V.principal_evidence out.input out.ty out.evidence target typing premise claim accept)

let (rejected @ total) : (out : result) @ immutable ->
    (target : Copy_spec.ty) @ immutable -> (typing : D.typing) @ immutable ->
    {u : unit | inferred_type out === None &&
      D.typed D.Z D.Empty_context (source out) (D.embed target) typing} -> {u : unit | false} @ ghost =
  fun out target typing premise -> ghost_ (
    let impossible : ((delta : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match inferred_type out with None -> false | Some ty -> target === M.substitute delta ty} ->
      {u : unit | false}) @ total = fun _delta contradiction -> () in
    principal out target typing () false impossible)
